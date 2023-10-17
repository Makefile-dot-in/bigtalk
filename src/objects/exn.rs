
use gc::{Gc, GcCell, Finalize, unsafe_empty_trace, Trace};
use std::iter;

use crate::vm::VM;

use super::{defmsg, ConstClassDef, Value, ValueData, Class, Symbol};
use super::meta::OBJECT_CLASS;

pub(super) const EXCEPTION: ConstClassDef = ConstClassDef {
    name: "Exception",
    superclass: Some(&OBJECT_CLASS),
    field_names: &[],
    instancemsgs: &[
		defmsg("raise", |_, this, []| {
			Err(this)
		})
	],
    classmsgs: &[],
};

pub(super) const MESSAGE_NOT_UNDERSTOOD: ConstClassDef = ConstClassDef {
	name: "MessageNotUnderstood",
	superclass: Some(&EXCEPTION),
	field_names: &["selector", "class"],
	instancemsgs: &[],
	classmsgs: &[],
};

pub(crate) fn message_not_understood(vm: &VM, sym: Symbol, class: Gc<GcCell<Class>>) -> Value {
	let sym = Value::from_symbol(vm, sym);
	let class = Value::from_class(vm, class);
	Value::multval_from_vec(vm, vec![sym, class], vm.classtbl.exn_msg_not_understood.clone())
}

pub(super) const TYPE_EXCEPTION: ConstClassDef = ConstClassDef {
	name: "TypeException",
	superclass: Some(&EXCEPTION),
	field_names: &["expected", "actual"],
	instancemsgs: &[],
	classmsgs: &[],
};


pub(crate) fn type_exception(vm: &VM, expected: Gc<GcCell<Class>>, actual: Gc<GcCell<Class>>) -> Value {
	let expected = Value::from_class(vm, expected);
	let actual = Value::from_class(vm, actual);
	Value::multval_from_vec(vm, vec![expected, actual], vm.classtbl.exn_type.clone())
}

pub(crate) fn type_exception2(vm: &VM, expected: &Gc<GcCell<Class>>, actual: &Value) -> Value {
	type_exception(vm, expected.clone(), actual.cls.clone())
}

pub(super) const MISC_EXCEPTION: ConstClassDef = ConstClassDef {
	name: "MiscError",
	superclass: Some(&EXCEPTION),
	field_names: &["message"],
	instancemsgs: &[],
	classmsgs: &[]
};

pub(crate) fn misc_exception(vm: &VM, message: String) -> Value {
	let message = Value::from_string(vm, message);
	Value::multval_from_vec(vm, vec![message], vm.classtbl.exn_misc.clone())
}
