use crate::opcodes::{array_of_iter, get_num};


fn skip_itref<E>(mut iter: impl Iterator, n: usize, err: impl FnOnce() -> E) -> Result<(), E> {
	for _ in 0..n {
		let Some(_) = iter.next() else {
			return Err(err());
		};
	}
	Ok(())
}

#[inline(always)]
fn ec<'a>(pc: u16, msg: &'a str) -> impl 'a + Fn() -> (u16, String) {
	move || (pc, msg.to_owned())
}

fn get_stack_size<'a>(mut instrs: impl Iterator<Item = &'a mut u8>) -> Result<usize, (u16, String)> {
	use crate::opcodes::*;
	let mut stacksize = 0;
	let mut curstacksize: usize = 0;
	let mut pc = 0;
	while let Some(instr) = instrs.next() {
		curstacksize = curstacksize.checked_add_signed(match *instr {
			PUSH_NIL => 1,
			PUSH_SELF => 1,
			PUSH_SUPER => 1,
			PUSH_TRUE => 1,
			PUSH_FALSE => 1,
			PUSH_ZERO => 1,
			PUSH_ONE => 1,
			PUSH_TWO => 1,
			PUSH_CHAR => 1,
			PUSH_INT => {
				skip_itref(instrs.by_ref(), 8, ec(pc, "PUSH_INT: expected arg"))?;
				1
			},
			PUSH_FLOAT => {
				skip_itref(instrs.by_ref(), 8, ec(pc, "PUSH_FLOAT: expected arg"))?;
				1
			},
			PUSH_FIELD => {
				skip_itref(instrs.by_ref(), 2, ec(pc, "PUSH_FIELD: expected arg"))?;
				1
			},
			PUSH_LOCAL => {
				skip_itref(instrs.by_ref(), 2, ec(pc, "PUSH_LOCAL: expected arg"))?;
				1
			},
			PUSH_STRING => {
				let len = get_num(instrs.by_ref().map(|x| *x), u16::from_be_bytes, || (pc, "PUSH_STRING: expected arg".to_owned()))?;
				skip_itref(instrs.by_ref(), len as usize, ec(pc, "PUSH_STRING: expected arg"))?;
				1
			},
			PUSH_GLOBAL => {
				skip_itref(instrs.by_ref(), 4, ec(pc, "PUSH_GLOBAL: expected arg"))?;
				1
			},
			PUSH_ARRAY => {
				let len = get_num(instrs.by_ref().map(|x| *x), u16::from_be_bytes, || (pc, "PUSH_ARRAY: expeected arg".to_owned()))?;
				1 - len as isize
			},
			PUSH_BLOCK => {
				let block_ninstr = get_num(instrs.by_ref().map(|x| *x), u16::from_be_bytes, ec(pc, "PUSH_BLOCK: expected arg"))?;
				// skip the next 2 args
				skip_itref(instrs.by_ref(), 2 * 2, ec(pc, "PUSH_BLOCK: expected arg"))?;
				// this is hacky but meh
				let refs: [&mut u8; 2] = array_of_iter(instrs.by_ref().take(2))
					.ok_or_else(ec(pc, "PUSH BLOCK: expected arg"))?;
				let stacksize = get_stack_size(instrs.by_ref().take(block_ninstr as usize))?;
				for (byte, refr) in stacksize.to_be_bytes().into_iter().zip(refs) {
					*refr = byte
				}
				1
			},
			PUSH_SYMBOL => {
				skip_itref(instrs.by_ref(), 4, ec(pc, "PUSH_SYMBOL: expected arg"))?;
				1
			},
			STORE_FIELD => {
				skip_itref(instrs.by_ref(), 2, ec(pc, "STORE_FIELD: expected arg"))?;
				-1
			},
			STORE_LOCAL => {
				skip_itref(instrs.by_ref(), 2, ec(pc, "STORE_LOCAL: expected arg"))?;
				-1
			},
			POP => -1,
			DUP => 1,
			SEND => {
				skip_itref(instrs.by_ref(), 4 /* a single u32 */, ec(pc, "SEND: expected arg"))?;
				let nargs = get_num(instrs.by_ref().map(|x| *x), u16::from_be_bytes, ec(pc, "SEND: expected arg"))?;
				1 - nargs as isize
			},
			SEND_PLUS => -2 + 1,
			SEND_MINUS => -2 + 1,
			SEND_DIV => -2 + 1,
			SEND_MUL => -2 + 1,
			RETURN | RETURN_BLOCK => {
				(curstacksize == 1).then_some(())
					.ok_or_else(ec(pc, "RETURN: there must be exactly one item on the stack before returning"))?;
				-1
			},
			RETURN_SELF => {
				(curstacksize == 0).then_some(())
					.ok_or_else(ec(pc, "RETURN_SELF: there must be exactly zero items on the stack when running RETURN_SELF"))?;
				0
			},
			i => return Err((pc, format!("no such instruction: {i}")))
		}).ok_or_else(ec(pc, "stack underflow detected"))?;
		if curstacksize > stacksize { stacksize = curstacksize }
		pc += 1;
	}
	Ok(stacksize)
}
