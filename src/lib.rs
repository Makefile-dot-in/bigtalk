#![feature(const_mut_refs, const_option)]
pub mod parser;
pub mod compiler;
pub mod opcodes;
pub mod analyzer;
pub mod vm;
pub mod objects; 

fn index_or_push<T>(vec: &mut Vec<T>, cmp: impl Fn(&T) -> bool, ins: impl FnOnce() -> T) -> usize {
	if let Some(i) = vec.iter().position(cmp) {
		return i;
	}

	vec.push(ins());
	vec.len() - 1
} 
