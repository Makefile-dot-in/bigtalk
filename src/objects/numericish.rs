
use gc::{Gc, GcCell, Finalize, unsafe_empty_trace, Trace};
use std::iter;

use crate::vm::VM;

use super::{defmsg, ConstClassDef, Value, ValueData, Class, Symbol};
use super::meta::ROOT_CLASS;

pub(crate) const BOOLEAN_CLASS: ConstClassDef = ConstClassDef {
	name: "Boolean",
	superclass: Some(&ROOT_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[]
};

pub(crate) const TRUE_CLASS: ConstClassDef = ConstClassDef {
	name: "True",
	superclass: Some(&BOOLEAN_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[],
};

pub(crate) const FALSE_CLASS: ConstClassDef = ConstClassDef {
	name: "False",
	superclass: Some(&BOOLEAN_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[],
};

pub(crate) const NUMBER_CLASS: ConstClassDef = ConstClassDef {
	name: "Number",
	superclass: Some(&ROOT_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[],
};

pub(crate) const INTEGER_CLASS: ConstClassDef = ConstClassDef {
	name: "Integer",
	superclass: Some(&NUMBER_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[],
};

pub(crate) const FLOAT_CLASS: ConstClassDef = ConstClassDef {
	name: "Float",
	superclass: Some(&NUMBER_CLASS),
	field_names: &[],
	instancemsgs: &[],
	classmsgs: &[],
};
