use gc::{unsafe_empty_trace, Finalize, Trace};

use super::exn::type_exception2;
use super::{defmsg, ConstClassDef, Value};
use super::meta::ROOT_CLASS;

pub(crate) const ARRAY_CLASS: ConstClassDef = ConstClassDef {
    name: "Array",
    superclass: Some(&ROOT_CLASS),
    field_names: &[],
    instancemsgs: &[],
    classmsgs: &[],
};
