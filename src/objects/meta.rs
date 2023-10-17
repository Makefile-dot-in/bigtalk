
use gc::{Gc, GcCell, Finalize, unsafe_empty_trace, Trace};
use gc_derive::{Trace, Finalize};
use std::iter;
use std::rc::Rc;

use super::{defmsg, ConstClassDef, Value, ValueData, Class, numericish, collections};
use super::{exn, string};
use exn::{type_exception2, misc_exception};

// note: superclasses must precede their children
pub(crate) const ALL_CLASSDEFS: [&ConstClassDef; 19] = [
	&ROOT_CLASS,
	&NIL_CLASS,
	&CLASS_CLASS, // must be fixed in this position
	&OBJECT_CLASS,
	&BLOCK_CLASS,
	&exn::EXCEPTION,
	&exn::MESSAGE_NOT_UNDERSTOOD,
	&exn::TYPE_EXCEPTION,
	&exn::MISC_EXCEPTION,
	&string::SYMBOL_CLASS,
	&string::STRING_CLASS,
	&string::CHAR_CLASS,
	&numericish::BOOLEAN_CLASS,
	&numericish::TRUE_CLASS,
	&numericish::FALSE_CLASS,
	&numericish::NUMBER_CLASS,
	&numericish::INTEGER_CLASS,
	&numericish::FLOAT_CLASS,
	&collections::ARRAY_CLASS
];



pub(super) const ROOT_CLASS: ConstClassDef = ConstClassDef {
    name: "Root",
    superclass: None,
	field_names: &[],
    instancemsgs: &[],
    classmsgs: &[
		defmsg("subclass:", |vm, recvr, [newname]| {
			todo!()
		}),
	],
};


pub(super) const NIL_CLASS: ConstClassDef = ConstClassDef {
	name: "UndefinedObject",
	field_names: &[],
	superclass: Some(&ROOT_CLASS),
	instancemsgs: &[],
	classmsgs: &[]
};

pub(super) const CLASS_CLASS: ConstClassDef = ConstClassDef {
	name: "Class",
	field_names: &[],
	superclass: Some(&ROOT_CLASS),
	instancemsgs: &[
		defmsg("doesNotUnderstand:withArgs:", |vm, this, [selector, args]| {
			let selector = selector.to_symbol(vm)
				.ok_or_else(|| type_exception2(vm, &vm.classtbl.symbol, selector))?;
			let args = args.to_vec(vm)
				.ok_or_else(|| misc_exception(vm, format!("args must be an arraylike, got {}", &args.cls.borrow().name)))?;
			let class = this.to_class(vm).unwrap().clone();

			Class::send_message(class, vm, Class::get_class_message, selector, this, &mut **args)
		})
	],
	classmsgs: &[]
};

pub(super) const OBJECT_CLASS: ConstClassDef = ConstClassDef {
	name: "Object",
	field_names: &[],
	superclass: Some(&ROOT_CLASS),
	instancemsgs: &[],
	classmsgs: &[
		// new is implemented on Object because Root knows nothing about its actual layout
		defmsg("new", |vm, class, []| {
			let class = class.to_class(vm).unwrap().clone(); // failure shouldn't be possible in normal operation
			let fields = iter::repeat(Value::nil(vm))
				.take(class.borrow().field_names.len())
				.collect::<Vec<_>>();
			Ok(Value {
				data: ValueData::MultVal(Gc::new(GcCell::new(fields))),
				cls: class,
			})
		})
	],
};

#[derive(Trace, Finalize, Debug)]
pub struct BlockObject {
	stalloc: usize,
	argstart: usize,
	argn: usize,
	pc: usize,
	uid: usize,
	retpos: usize,
}

pub(super) const BLOCK_CLASS: ConstClassDef = ConstClassDef {
    name: "Block",
    superclass: Some(&ROOT_CLASS),
    field_names: &[],
    instancemsgs: &[],
    classmsgs: &[],
};
