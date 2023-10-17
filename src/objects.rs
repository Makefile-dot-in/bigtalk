use std::{rc::Rc, mem::size_of, any::TypeId};
use gc::{Gc, GcCell, Trace, unsafe_empty_trace, GcCellRef, GcCellRefMut};
use gc_derive::{Trace, Finalize};
use fnv::FnvHashMap;
use mopa::{Any, mopafy}; // needed for mixed trait Any


use crate::vm::{const_sym, VM};

pub(crate) mod meta;
mod string;
mod numericish;
mod collections;
pub(crate) mod exn;

use string::StringObj;
use std::fmt;

use self::exn::misc_exception;

trait AnyTrace: Any + Trace + fmt::Debug + 'static {}
mopafy!(AnyTrace);
impl<T: Any + Trace + fmt::Debug> AnyTrace for T {}

pub type Symbol = u32;

pub type RetValue = Result<Value, Value>;



#[derive(Trace, Finalize, Clone, Debug)]
enum ValueData {
	Unboxed([u8; 16]), // unboxed values are allowed up to 128 bits
	// this may have to be updated when 256-bit CPUs hit the market
	MultVal(Gc<GcCell<Vec<Value>>>),
	BoxedData(Gc<dyn AnyTrace>),
}

type CMsgHandler = unsafe extern "C" fn(vm: &mut VM, recvr: Value, argv: *mut Value) -> RetValue;
type RustMsgHandler<const ARGC: usize> = fn(vm: &mut VM, recvr: Value, args: &mut [Value; ARGC]) -> RetValue;

#[derive(Clone, Debug)]
pub struct BigtalkFn {
	pub instrs: Rc<[u8]>,
	pub nlocals: usize,
	pub stacksize: usize
}

// we do it this way because working with unsized types is h.
#[derive(Clone, Copy)]
pub struct TypeErasedHandler {
	call: unsafe fn(f: *const (), vm: &mut VM, recvr: Value, args: &mut [Value]) -> RetValue,
	func: *const (),
}

impl std::fmt::Debug for TypeErasedHandler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeErasedHandler")
			.field("call", &(self.call as *const ()))
			.field("func", &self.func).finish()
    }
}

impl TypeErasedHandler {
	// SAFETY: very unsafe. verify the length of args first.
	#[inline(always)]
	unsafe fn call<const ARGC: usize>(f: *const (), vm: &mut VM, recvr: Value, args: &mut [Value]) -> RetValue {
		let argptr = args.as_mut_ptr() as *mut [Value; ARGC];
		// this is safe on platforms where function pointers have the same layout as regular pointers
		// aka the vast majority of platforms
		let realfnptr: RustMsgHandler<ARGC> = std::mem::transmute(f);
		(realfnptr)(vm, recvr, &mut *argptr)
	}

	pub const fn new<const ARGC: usize>(handler: RustMsgHandler<ARGC>) -> Self {
		Self {
			call: Self::call::<ARGC>,
			func: handler as *const (),
		}
	}

	// SAFETY: safe to call if args is of the correct arity
	unsafe fn invoke(&self, vm: &mut VM, recvr: Value, args: &mut [Value]) -> RetValue {
		(self.call)(self.func, vm, recvr, args)
	}
}


/// A handler for messages.
/// Clones are relatively inexpensive, they're just needed due to an Rc.
#[derive(Finalize, Clone)]
pub enum MessageBackend {
	RustNativeFn(TypeErasedHandler),
	CNativeFn(CMsgHandler),
	NormalFn(BigtalkFn),
}

impl std::fmt::Debug for MessageBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RustNativeFn(arg0) => f.debug_tuple("RustNativeFn").field(arg0).finish(),
            Self::CNativeFn(arg0) => f.debug_tuple("CNativeFn").field(&(*arg0 as *const ())).finish(),
            Self::NormalFn(arg0) => f.debug_tuple("NormalFn").field(arg0).finish(),
        }
    }
}

#[derive(Finalize, Clone, Debug)]
pub struct MessageHandler {
	pub argc: usize,
	pub backend: MessageBackend,
}

impl MessageHandler {
	pub(crate) fn invoke(&self, vm: &mut VM, recvr: Value, args: &mut [Value]) -> RetValue {
		if self.argc != args.len() {
			todo!()
		}
		match &self.backend {
			// SAFETY: safe because we checked above :pleading_face:
			// (I can't use the actual emoji here because rust-analyzer will break otherwise)
			MessageBackend::RustNativeFn(f) => unsafe {
				f.invoke(vm, recvr, args)
			},
			// SAFETY: safe if cfn doesn't do funny shit
			MessageBackend::CNativeFn(cfn) => unsafe {
				cfn(vm, recvr, args.as_mut_ptr())
			},
			MessageBackend::NormalFn(BigtalkFn { instrs, nlocals, stacksize }) => todo!(),
		}
	}
}


// SAFETY: this won't segfault because it's an empty implementation
// of Trace. We can't #[derive(Trace)] because Trace is not implemented for
// for<'a> fn(&'a Value, &'a mut [Value]), so we do this
// since MessageHandler contains no Gc objects.
unsafe impl Trace for MessageHandler {
	unsafe_empty_trace!();
}

#[derive(Trace, Finalize, Debug)]
pub struct Class {
	pub(crate) name: String,
	pub(crate) field_names: Vec<String>,
	pub(crate) superclass: Option<Gc<GcCell<Class>>>,
	pub(crate) class_fields: Vec<Value>,
	pub(crate) class_field_names: Vec<String>,
	pub(crate) instance_msgs: FnvHashMap<Symbol, MessageHandler>,
	pub(crate) class_msgs: FnvHashMap<Symbol, MessageHandler>
}
impl Class {
	fn clsname(&self) -> &str {
		&self.name
	}
    pub(crate) fn get_instance_message(this: Gc<GcCell<Self>>, selector: Symbol) -> Option<MessageHandler> {
		let refr = this.borrow();
		if let Some(handler) = refr.instance_msgs.get(&selector) {
			return Some(handler.clone());
		}

		
		match refr.superclass.as_ref() {
			Some(sp) => Self::get_instance_message(Gc::clone(&sp), selector),
			None => None,
		}
    }

	
	
	pub(crate) fn get_class_message(this: Gc<GcCell<Self>>, selector: Symbol) -> Option<MessageHandler> {
		let refr = this.borrow();
		if let Some(handler) = refr.class_msgs.get(&selector) {
			return Some(handler.clone());
		}

		match refr.superclass.as_ref() {
			Some(sp) => Self::get_class_message(Gc::clone(&sp), selector),
			None => None
		}
    }

	pub(crate) fn is_superclass(this: Gc<GcCell<Self>>, superclass: &Gc<GcCell<Self>>) -> bool {
		let Some(spcls) = this.borrow().superclass.clone() else {
			return false
		};
		
		Self::is_superclass(spcls, &superclass)
	}

	fn send_message(this: Gc<GcCell<Self>>, vm: &mut VM, mut finder: impl FnMut(Gc<GcCell<Self>>, Symbol) -> Option<MessageHandler>, sel: Symbol, recvr: Value, args: &mut [Value]) -> RetValue {
		match finder(this.clone(), sel) {
			Some(h) => h.invoke(vm, recvr, args),
			None => finder(this, const_sym("doesNotUnderstand:withArgs:"))
				.expect("message not understood")
				.invoke(vm, recvr, args)
		}
	}

	fn instance_field_names<'a>(&'a self) -> Box<dyn Iterator<Item = &str> + 'a> {
		Box::new(self.field_names.iter().map(String::as_str)) as Box<dyn Iterator<Item = &str>>
	}

	fn class_field_names<'a>(&'a self) -> Box<dyn Iterator<Item = &str> + 'a> {
		Box::new(self.field_names.iter().map(String::as_str)) as Box<dyn Iterator<Item = &str>>
	}
}

pub(crate) struct ConstClassDef {
	pub(crate) name: &'static str,
	pub(crate) superclass: Option<&'static ConstClassDef>,
	pub(crate) field_names: &'static [&'static str],
	pub(crate) instancemsgs: &'static [(Symbol, usize, TypeErasedHandler)],
	pub(crate) classmsgs: &'static [(Symbol, usize, TypeErasedHandler)],
}

/// defines a native message (efficient style)
const fn defmsg<const ARGC: usize>(selector: &'static str, handler: RustMsgHandler<ARGC>) -> (Symbol, usize, TypeErasedHandler) {
	let selectorb = selector.as_bytes();
	let selector_arity = if !selectorb[0].is_ascii_alphanumeric() {
		1
	} else {
		let mut n = 0;
		let mut idx = 0;
		while idx < selectorb.len() {
			if selectorb[idx] == b':' {	
				n += 1;
			}
			idx += 1;
		}
		n
	};
	if ARGC != selector_arity {
		panic!("arity mismatch");
	}
	(const_sym(selector), ARGC, TypeErasedHandler::new(handler))
}

#[derive(Trace, Finalize, Clone, Debug)]
pub struct Value {
	pub data: ValueData,
	pub cls: Gc<GcCell<Class>>,
}

impl Value {
	#[inline]
	pub fn downcast<T: Trace + fmt::Debug + 'static>(&self) -> Option<&T> {
		match &self.data {
			ValueData::BoxedData(data) => data.downcast_ref::<T>(),
			_ => None
		}
	}

	pub fn upcast<T: Trace + fmt::Debug + 'static>(value: Box<T>, cls: Gc<GcCell<Class>>) -> Self {
		Self {
			data: ValueData::BoxedData((value as Box<dyn AnyTrace>).into()),
			cls
		}
	}

	#[inline]
	pub fn instanceof(&self, cls: &Gc<GcCell<Class>>) -> bool {
		Class::is_superclass(self.cls.clone(), cls)
	}


	
	#[inline]
	pub(crate) fn nilcls(c: Gc<GcCell<Class>>) -> Self {
		Self {
			data: ValueData::Unboxed([0; 16]),
			cls: c,
		}
	}

	#[inline]
	pub fn nil(vm: &VM) -> Self {
		Self::nilcls(vm.classtbl.nil.clone())
	}

	#[inline]
	pub fn to_symbol(&self, vm: &VM) -> Option<Symbol> {
		if !self.instanceof(&vm.classtbl.symbol) {
			return None
		}

		let ValueData::Unboxed(a) = self.data else {
			return None // shouldn't ever happen but just to be safe
		};

		Some(Symbol::from_ne_bytes(a[0..size_of::<Symbol>()].try_into().ok()?))
	}

	#[inline]
	pub fn from_symbol(vm: &VM, sym: Symbol) -> Self {
		let mut data = [0; 16];
		data[0..size_of::<Symbol>()].copy_from_slice(&sym.to_ne_bytes());
		Self {
			data: ValueData::Unboxed(data),
			cls: vm.classtbl.symbol.clone(),
		}
	}

	pub fn to_str(&self, vm: &VM) -> Option<&str> {
		if !self.instanceof(&vm.classtbl.string) {
			return None
		}

		let obj = self.downcast::<Box<StringObj>>()?;
		Some(&obj.inner)
		
	}

	pub fn from_string(vm: &VM, s: String) -> Self {
		let boxed_s = s.into_boxed_str();
		Self::upcast(Box::new(StringObj::from_boxed_string(boxed_s)), vm.classtbl.string.clone())	
	}

	pub fn from_class(vm: &VM, c: Gc<GcCell<Class>>) -> Self {
		Self::upcast(Box::new(c), vm.classtbl.class.clone())
	}
	

	pub fn with_class(mut self, c: Gc<GcCell<Class>>) -> Self {
		self.cls = c;
		self
	}

	pub fn to_class(&self, vm: &VM) -> Option<&Gc<GcCell<Class>>> {
		if !self.instanceof(&vm.classtbl.class) {
			return None
		}

		self.downcast::<Gc<GcCell<Class>>>()
	}


	pub(crate) fn from_boolcls(cls: Gc<GcCell<Class>>, b: bool) -> Self {
		Self {
			data: ValueData::Unboxed((b as u128).to_ne_bytes()),
			cls
		}
	}

	#[inline]
	pub fn from_bool(vm: &VM, b: bool) -> Self {
		Self::from_boolcls(if b {
			vm.classtbl.btrue.clone()
		} else {
			vm.classtbl.bfalse.clone()
		}, b)
	}

	pub fn to_bool(&self, vm: &VM) -> Option<bool> {
		if !self.instanceof(&vm.classtbl.boolean) {
			return None;
		}

		let ValueData::Unboxed(ub) = self.data else {
			return None;
		};

		Some(u128::from_ne_bytes(ub) != 0)
	}

	pub fn from_i64(vm: &VM, n: i64) -> Self {
		Self::from_i64cls(vm.classtbl.integer.clone(), n)
	}

	pub(crate) fn from_i64cls(c: Gc<GcCell<Class>>, n: i64) -> Self {
		let mut data = [0; 16];
		data[0..size_of::<i64>()].copy_from_slice(&n.to_ne_bytes());
		Self {
			data: ValueData::Unboxed(data),
			cls: c,
		}
	}

	pub fn to_i64(&self, vm: &VM) -> Option<i64> {
		if !self.instanceof(&vm.classtbl.integer) {
			return None;
		}

		let ValueData::Unboxed(ub) = self.data else {
			return None;
		};

		Some(i64::from_ne_bytes(ub[0..size_of::<i64>()].try_into().ok()?))
	}

	
	pub fn from_f64(vm: &VM, f: f64) -> Self {
		let mut data = [0; 16];
		data[0..size_of::<f64>()].copy_from_slice(&f.to_ne_bytes());
		Self {
			data: ValueData::Unboxed(data),
			cls: vm.classtbl.float.clone(),
		}
	}

	pub fn to_f64(&self, vm: &VM) -> Option<f64> {
		if !self.instanceof(&vm.classtbl.integer) {
			return None;
		}

		let ValueData::Unboxed(ub) = self.data else {
			return None;
		};

		Some(f64::from_ne_bytes(ub[0..size_of::<f64>()].try_into().ok()?))
	}

	pub fn from_vec(vm: &VM, vec: Vec<Value>) -> Self {
		Self::multval_from_vec(vm, vec, vm.classtbl.array.clone())
	}

	pub fn multval_from_vec(vm: &VM, vec: Vec<Value>, cls: Gc<GcCell<Class>>) -> Self {
		Self {
			data: ValueData::MultVal(Gc::new(GcCell::new(vec))),
			cls
		}
	}

	// TODO: what the fuck
	pub fn as_super(mut self) -> Self {
		let oldcls = self.cls.clone();
		if oldcls.borrow().superclass.is_some() {
			self.cls = oldcls.borrow().superclass.as_ref().unwrap().clone();
		}
		self
	}
	
	pub fn to_vec(&self, vm: &VM) -> Option<GcCellRefMut<'_, Vec<Value>>> {
		if !self.instanceof(&vm.classtbl.array) {
			return None;
		}

		let ValueData::MultVal(vec) = &self.data else {
			return None;
		};

		Some(vec.borrow_mut())
	}

	pub fn get_field(&self, vm: &VM, field: u16) -> RetValue {
		let ValueData::MultVal(vec) = &self.data else {
			return Err(misc_exception(vm, format!("expected field, got {:?}", self.data)));
		};
		Ok(vec.borrow()[field as usize].clone())
	}

	pub fn set_field(&self, vm: &VM, field: u16, newval: Value) -> Result<(), Value> {
		let ValueData::MultVal(vec) = &self.data else {
			return Err(misc_exception(vm, format!("expected field, got {:?}", self.data)));
		};

		vec.borrow_mut()[field as usize] = newval;
		Ok(())
	}

	pub fn from_char(vm: &VM, c: char) -> Self {
		let mut data = [0; 16];
		data[0..size_of::<char>()].copy_from_slice(&(c as u32).to_ne_bytes());
		Self {
			data: ValueData::Unboxed(data),
			cls: vm.classtbl.chr.clone(),
		}
	}

	pub fn send_message(&self, vm: &mut VM, sel: Symbol, recvr: Value, args: &mut [Value]) -> RetValue {
		Class::send_message(self.cls.clone(), vm, Class::get_class_message, sel, recvr, args)
	}
}
