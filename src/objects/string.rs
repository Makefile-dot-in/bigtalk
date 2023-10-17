use gc::{unsafe_empty_trace, Finalize, Trace};

use super::exn::type_exception2;
use super::{defmsg, ConstClassDef, Value};
use super::meta::ROOT_CLASS;

#[repr(transparent)]
#[derive(Debug)]
pub struct StringObj {
	pub inner: str,
}

impl StringObj {
	pub fn from_boxed_string(s: Box<str>) -> Box<Self> {
		// SAFETY:this is safe because we are casting
		// a pointer to a transparent struct over a str to a pointer to a str,
		// the safety of which is guaranteed by the language.
		unsafe {
			Box::from_raw(Box::into_raw(s) as *mut StringObj)
		}
	}

	pub fn into_boxed_string(self: Box<Self>) -> Box<str> {
		// SAFETY: safe because we're doing the same as above
		// but in reverse
		unsafe {
			Box::from_raw(Box::into_raw(self) as *mut str)
		}
	}
}

impl Clone for Box<StringObj> {
    fn clone(&self) -> Self {
		// SAFETY: safe because cast from repr(transparent) to contained object (I think)
		unsafe {
			StringObj::from_boxed_string(std::mem::transmute::<_, &Box<str>>(self).clone())
		}
    }
}

impl Finalize for StringObj {}
unsafe impl Trace for StringObj {
	unsafe_empty_trace!();
}



pub(super) const SYMBOL_CLASS: ConstClassDef = ConstClassDef {
	name: "Symbol",
	superclass: Some(&ROOT_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[
		defmsg("fromString:", |vm, class, [s]| {
			let s = s.downcast::<Box<StringObj>>()
				.expect("fromString: needs to have a string");
			let class = class.to_class(vm).unwrap().clone();
			let boxed_str = vm.sym_of_str(s.inner.to_owned().into_boxed_str());
			Ok(Value::from_symbol(vm, boxed_str).with_class(class))
		})
	]
};

pub(crate) const STRING_CLASS: ConstClassDef = ConstClassDef {
	name: "String",
	superclass: Some(&ROOT_CLASS),
	field_names: &[],
	instancemsgs: &[
		defmsg("+", |vm, this, [other]| {
			let this = this.to_str(vm)
				.ok_or_else(|| type_exception2(vm, &vm.classtbl.string, &this))?;
			let other = other.to_str(vm)
				.ok_or_else(|| type_exception2(vm, &vm.classtbl.string, &other))?;
			Ok(Value::from_string(vm, format!("{}{}", this, other)))
		})
	],
	classmsgs: &[]
};

pub(crate) const CHAR_CLASS: ConstClassDef = ConstClassDef {
	name: "Char",
	superclass: Some(&ROOT_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[],
};

