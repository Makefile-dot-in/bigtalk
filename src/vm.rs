use std::mem::ManuallyDrop;
use std::task::{Waker, Wake, Context, RawWakerVTable, RawWaker};
use std::future::Future;
use std::sync::{Mutex, Arc};
use std::pin::Pin;
use std::{collections::HashMap, cell::RefCell};
use std::rc::Rc;
use std::iter;
use fnv::FnvHashMap;
use futures::executor::LocalPool;

use gc::{Gc, GcCell};

use crate::objects::{Symbol, Value, BigtalkFn, RetValue, Class, ConstClassDef, TypeErasedHandler, MessageHandler, MessageBackend};
use crate::objects::meta::ALL_CLASSDEFS;
use crate::objects::exn::{misc_exception, message_not_understood};
use crate::opcodes::get_num;
use crate::{opcodes, index_or_push};

/// this is where predefined syms go
/// it's like the Sims, but more boring
const SYMS: [&str; 24] = [
	"subclass:",
	"doesNotUnderstand:withArgs:",
	"new",
	"fromString:",
	"raise",
	"+",
	"-",
	"/",
	"*",
	"Root",
	"Object",
	"UndefinedObject",
	"Class",
	"Exception",
	"MessageNotUnderstood",
	"TypeException",
	"MiscException",
	"Symbol",
	"String",
	"Boolean",
	"True",
	"False",
	"Number",
	"Integer",
];

const fn is_str_eq(one: &str, other: &str) -> bool {
	let one = one.as_bytes();
	let other = other.as_bytes();
	if one.len() != other.len() { return false }
	let len = one.len();
	let mut idx = 0;
	while idx < len {
		if one[idx] != other[idx] {
			return false
		}
		idx += 1
	}
	true
}

pub(crate) const fn const_sym(name: &str) -> Symbol {
	let mut idx = 0;
	while idx < SYMS.len() {
		if is_str_eq(SYMS[idx], name) {
			return idx as Symbol;
		}
		
		idx += 1;
	}
	panic!("not a const sym")
}



#[derive(Clone, Debug)]
struct ByteRetData {
	execbody: Rc<[u8]>, // the actual instructions
	pc: usize, // program counter
	stackpos: usize, // location on the stack (with data and return information and whatnot)
	uid: u64, // unique ID (used by blocks)
	retpos: usize,  // place to return to on the return stack
}

impl ByteRetData {
	#[inline(always)]
	fn get_num<T, const N: usize>(&mut self, conv: impl FnOnce([u8; N]) -> T, instr: &str) -> Result<T, String> {
		if self.pc + N >= self.execbody.len() {
			return Err(format!("error while getting arg for {instr}: expected arg, got EOF. state: {self:?}"));
		}
		self.pc += N;
		Ok(conv(self.execbody[self.pc..self.pc + N].try_into().unwrap()))
	}

	fn update_from(&mut self, other: &Self) {
		self.pc = other.pc;
		self.stackpos = other.pc;
	}
}

enum RetData {
	Bytecode(ByteRetData),
	Native,
}

type GcClass = Gc<GcCell<Class>>;
pub(crate) struct ClassTbl {
	pub root: GcClass,
	pub nil: GcClass,
	pub class: GcClass,
	pub object: GcClass,
	pub block: GcClass,
	pub exn: GcClass,
	pub exn_msg_not_understood: GcClass,
	pub exn_type: GcClass,
	pub exn_misc: GcClass,
	pub symbol: GcClass,
	pub string: GcClass,
	pub chr: GcClass,
	pub boolean: GcClass,
	pub btrue: GcClass,
	pub bfalse: GcClass,
	pub number: GcClass,
	pub integer: GcClass,
	pub float: GcClass,
	pub array: GcClass,
}

pub struct VM {
	stack: Vec<Value>,
	retstack: Vec<RetData>,
	symtbl: Vec<Box<str>>,
	globalvars: FnvHashMap<Symbol, Value>,
	last_id: u64,
	trueval: Value,
	falseval: Value,
	nil: Value,
	one: Value,
	zero: Value,
	two: Value,
	pub(crate) classtbl: ClassTbl,
}

impl VM {
	pub fn new() -> Self {
		let classes = Self::make_classes(ALL_CLASSDEFS);
		let mut globalvars = FnvHashMap::default();
		for class in classes.iter() {
			globalvars.insert(const_sym(&class.borrow().name), Value::upcast(Box::new(class.clone()), classes[1].clone()));
		}
		let [root,
			 nil,
			 class,
			 object,
			 block,
			 exn,
			 exn_msg_not_understood,
			 exn_type,
			 exn_misc,
			 symbol,
			 string,
			 chr,
			 boolean,
			 btrue,
			 bfalse,
			 number,
			 integer,
			 float,
			 array] = classes;
		
		Self {
			stack: Vec::new(),
			retstack: Vec::new(),
			symtbl: SYMS.iter()
				.map(|sym| (*sym).to_owned().into_boxed_str())
				.collect(),
			globalvars,
			last_id: 0,
			trueval: Value::from_boolcls(btrue.clone(), true),
			falseval: Value::from_boolcls(bfalse.clone(), false),
			nil: Value::nilcls(nil.clone()),
			zero: Value::from_i64cls(integer.clone(), 0),
			one: Value::from_i64cls(integer.clone(), 1),
			two: Value::from_i64cls(integer.clone(), 2),
			classtbl: ClassTbl {
				root,
				nil,
				class,
				object,
				block,
				exn,
				exn_msg_not_understood,
				exn_type,
				exn_misc,
				symbol,
				string,
				chr,
				boolean,
				btrue,
				bfalse,
				number,
				integer,
				float,
				array,
			}
		}
	}

	fn get_id(&mut self) -> u64 {
		self.last_id += 1;
		self.last_id
	}
	
	fn make_classes<const N: usize>(defs: [&ConstClassDef; N]) -> [GcClass; N] {
		fn compmsgs_to_runmsgs(msg: &[(Symbol, usize, TypeErasedHandler)])
							   -> FnvHashMap<Symbol, MessageHandler> {
			let mut retval = FnvHashMap::default();

			for (sym, argc, handler) in msg {
				retval.insert(*sym, MessageHandler {
					argc: *argc,
					backend: MessageBackend::RustNativeFn(*handler)
				});
			}
			retval
		}
		let mut clstbl: HashMap<*const ConstClassDef, GcClass> = HashMap::new();
		defs.map(|clsdef| {
			let cls = Gc::new(GcCell::new(Class {
				name: clsdef.name.to_owned(),
				field_names: clsdef.field_names.iter()
					.map(|s| (*s).to_owned())
					.collect::<Vec<_>>(),
				superclass: clsdef.superclass
					.map(|d| clstbl[&(d as *const ConstClassDef)].clone()),
				class_fields: vec![],
				class_field_names: vec![],
				instance_msgs: compmsgs_to_runmsgs(clsdef.instancemsgs),
				class_msgs: compmsgs_to_runmsgs(clsdef.classmsgs),
			}));
			clstbl.insert(clsdef as *const ConstClassDef, cls.clone());
			cls
		})
	}

	pub fn sym_of_str(&mut self, s: Box<str>) -> Symbol {
		if let Some(i) = self.symtbl.iter().position(|sym| sym == &s) {
			return i as u32;
		}
		
		self.symtbl.push(s);
		(self.symtbl.len() - 1) as u32
			
	}

	#[inline(always)]
	fn get_self(&self, execref: &ByteRetData) -> Value {
		self.stack[execref.stackpos].clone()
	}

	#[inline(always)]
	fn get_local(&self, execref: &ByteRetData, n: usize) -> Value {
		self.stack[execref.stackpos + 1 + n].clone()
	}

	#[inline(always)]
	fn set_local(&mut self, execref: &ByteRetData, n: usize, v: Value) {
		self.stack[execref.stackpos + 1 + n] = v;
	}

	fn send(&mut self, execref: &mut ByteRetData, argc: usize, sel: Symbol) -> Result<(), Value> {
		let stacklen = self.stack.len();
		let argstart = stacklen - argc - 1;
		let cls = self.stack[argstart].cls.clone();
		let handler = Class::get_instance_message(cls.clone(), sel)
			.ok_or_else(|| message_not_understood(self, sel, cls.clone()))?;
		let RetData::Bytecode(brd) = self.retstack.last_mut().expect("retstack underflow") else {
			panic!("retstack-run() divergence {:?}", execref);
		};
		
		brd.update_from(&execref);
		
		match handler.backend {
			MessageBackend::NormalFn(bfn) => {
				for _ in 0..bfn.nlocals {
					self.stack.push(Value::nil(self));
				}
				*execref = ByteRetData {
					execbody: bfn.instrs,
					pc: 0,
					stackpos: argstart,
					uid: self.get_id(),
					retpos: self.retstack.len()
				};
				self.retstack.push(RetData::Bytecode(execref.clone()));
			},
			_ => {
				self.retstack.push(RetData::Native);
				let recvr = self.stack[argstart].clone();
				let args = &mut *self.stack[argstart+1..stacklen].to_vec();
				let retval = match handler.invoke(self, recvr, args) {
					Ok(v) => v,
					Err(e) => if Gc::ptr_eq(&e.cls, &self.classtbl.integer) {
						let num = e.to_i64(&self).unwrap();
						if num <= 0 {
							let RetData::Bytecode(b) = self.retstack.last_mut().expect("return stack underflow") else {
								panic!("expected bytecode on the return stack")
							};
							self.simple_ret(b);
							return Ok(());
						}
						return Err(Value::from_i64(&self, num - 1));
					} else {
						return Err(e)
					}
				};
				self.stack.truncate(argstart);
				self.stack.push(retval)
			}
		}
		Ok(())
	}

	fn simple_ret(&mut self, idx: usize) -> Option<Value> {
		self.stack.truncate(execref.stackpos);
		self.retstack.truncate(execref.retpos - 1);
		match self.retstack.last().expect("return stack underflow") {
			RetData::Bytecode(b) => *execref = b.clone(),
			RetData::Native => return Some(self.stack.pop().expect("stack underflow")),
		}
		None
	}
	
	fn ret(&mut self, execref: &mut ByteRetData) -> Result<(), Value> {
		let native_rets = self.retstack[execref.retpos..].iter().filter(|ret| matches!(ret, RetData::Native)).count();
		if let Some(pos) = self.retstack[execref.retpos..].iter().rposition(|ret| matches!(ret, RetData::Native)) {
			return Err(Value::from_i64(&self, native_rets as i64));
		}
		
		
		
		
		Ok(())
	}
	
	fn run(&mut self) -> RetValue {
		let mut execref = match self.retstack.last().unwrap() {
			RetData::Bytecode(data) => data.clone(),
			RetData::Native => {
				self.retstack.pop();
				return Ok(self.stack.pop().expect("return stack underflow"));
			}
		};
		loop {
			use opcodes::*;
			match execref.execbody[execref.pc] {
				PUSH_NIL => {
					self.stack.push(self.nil.clone());
				},
				PUSH_SELF => {
					self.stack.push(self.get_self(&execref));
				},
				PUSH_SUPER => {
					self.stack.push(self.get_self(&execref).as_super());
				},
				PUSH_TRUE => {
					self.stack.push(self.trueval.clone());
				},
				PUSH_FALSE => {
					self.stack.push(self.falseval.clone());
				},
				PUSH_ZERO => {
					self.stack.push(self.zero.clone());
				},
				PUSH_ONE => {
					self.stack.push(self.one.clone());
				},
				PUSH_TWO => {
					self.stack.push(self.two.clone());
				},
				PUSH_CHAR => {
					let c = execref.get_num(u32::from_be_bytes, "PUSH_CHAR").unwrap();
					Value::from_char(self, char::from_u32(c)
									 .expect("char decoding error in PUSH_CHAR"));
				},
				PUSH_INT => {
					let c = execref.get_num(i64::from_be_bytes, "PUSH_INT").unwrap();
					self.stack.push(Value::from_i64(self, c));
				},
				PUSH_FLOAT => {
					let c = execref.get_num(f64::from_be_bytes, "PUSH_FLOAT").unwrap();
					self.stack.push(Value::from_f64(self, c));
				},
				PUSH_FIELD => {
					let c = execref.get_num(u16::from_be_bytes, "PUSH_FIELD").unwrap();
					self.stack.push(self.get_self(&execref).get_field(self, c)?);
				},
				PUSH_LOCAL => {
					let c = execref.get_num(u16::from_be_bytes, "PUSH_LOCAL").unwrap();
					self.stack.push(self.get_local(&execref, c as usize));
				},
				PUSH_STRING => {
					let len = execref.get_num(u16::from_be_bytes, "PUSH_STRING").unwrap();
					let s = String::from_utf8(execref.execbody[execref.pc..execref.pc + len as usize].to_vec()).unwrap();
					execref.pc += len as usize;
					self.stack.push(Value::from_string(self, s));
				},
				PUSH_GLOBAL => {
					let sym = execref.get_num(Symbol::from_be_bytes, "PUSH_GLOBAL").unwrap();
					self.stack.push(self.globalvars[&sym].clone());
				},
				PUSH_ARRAY => {
					let len = execref.get_num(u16::from_be_bytes, "PUSH_ARRAY").unwrap() as usize;
					if self.stack.len() < len {
						panic!("{execref:?} {:?}: stack underflow", self.stack);
					}
					let mut vec = self.stack[self.stack.len() - len..self.stack.len()].to_vec();
					vec.truncate(self.stack.len() - len);
					self.stack.push(Value::from_vec(&self, vec));
				},
				PUSH_BLOCK => {
					todo!()
				},
				PUSH_SYMBOL => {
					let sym = execref.get_num(Symbol::from_be_bytes, "PUSH_SYMBOL").unwrap();
					self.stack.push(Value::from_symbol(self, sym));
				},
				STORE_FIELD => {
					let field = execref.get_num(u16::from_be_bytes, "STORE_FIELD").unwrap();
					let newval = self.stack.pop();
					self.get_self(&execref).set_field(self, field, newval.unwrap()).unwrap();
				},
				STORE_LOCAL => {
					let nlocal = execref.get_num(u16::from_be_bytes, "STORE_LOCAL").unwrap() as usize;
					let newval = self.stack.pop();
					self.set_local(&execref, nlocal, newval.unwrap());
				},
				POP => drop(self.stack.pop().expect("stack underflow")),
				DUP => {
					let tos = self.stack.last().expect("stack underflow").clone();
					self.stack.push(tos);
				},
				SEND => {
					let sel = execref.get_num(Symbol::from_be_bytes, "SEND").unwrap();
					let argc = execref.get_num(u32::from_be_bytes, "SEND").unwrap() as usize;
					self.send(&mut execref, argc, sel)?;
				},
				SEND_PLUS => self.send(&mut execref, 2, const_sym("+"))?,
				SEND_MINUS => self.send(&mut execref, 2, const_sym("-"))?,
				SEND_DIV => self.send(&mut execref, 2, const_sym("/"))?,
				SEND_MUL => self.send(&mut execref, 2, const_sym("*"))?,
				RETURN => {
					let retval = self.stack.pop();
				
					
				},
				RETURN_SELF => todo!(),
				RETURN_BLOCK => todo!(),
				_ => todo!()
			}
		}
	}
}
