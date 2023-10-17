pub const PUSH_NIL:     u8 = 0;
pub const PUSH_SELF:    u8 = 1;
pub const PUSH_SUPER:   u8 = 2;
pub const PUSH_TRUE:    u8 = 3;
pub const PUSH_FALSE:   u8 = 4;
pub const PUSH_ZERO:    u8 = 5;
pub const PUSH_ONE:     u8 = 6;
pub const PUSH_TWO:     u8 = 7;
pub const PUSH_CHAR:    u8 = 8;
pub const PUSH_INT:     u8 = 9;
pub const PUSH_FLOAT:   u8 = 10;
pub const PUSH_FIELD:   u8 = 11;
pub const PUSH_LOCAL:   u8 = 12;
pub const PUSH_STRING:  u8 = 13;
pub const PUSH_GLOBAL:  u8 = 14;
pub const PUSH_ARRAY:   u8 = 15;
pub const PUSH_BLOCK:   u8 = 16;
pub const PUSH_SYMBOL:  u8 = 17;
pub const STORE_FIELD:  u8 = 18;
pub const STORE_LOCAL:  u8 = 19;
pub const POP:          u8 = 20;
pub const DUP:          u8 = 21;
pub const SEND:         u8 = 22;
pub const SEND_PLUS:    u8 = 23;
pub const SEND_MINUS:   u8 = 24;
pub const SEND_DIV:     u8 = 25;
pub const SEND_MUL:     u8 = 26;
pub const RETURN:       u8 = 27;
pub const RETURN_SELF:  u8 = 28;
pub const RETURN_BLOCK: u8 = 29;

fn add_int<T, I>(start: usize, arr: &mut [u8], i: T, f: fn(T) -> I)
where I: IntoIterator<Item = u8> {
	for (i, b) in f(i).into_iter().enumerate() {
		arr[start+i] = b;
	}
}

pub(crate) fn array_of_iter<T, const N: usize>(mut it: impl Iterator<Item = T>) -> Option<[T; N]> {
	let mut retval: [Option<T>; N] = [0; N].map(|_| None);

	for elem in retval.iter_mut() {
		*elem = Some(it.next()?);
	}
	it.next().is_none().then_some(())?;
	Some(retval.map(|x| x.unwrap()))
}



pub(crate) fn get_num<T, E, const N: usize>(iter: impl Iterator<Item = u8>, conv: impl FnOnce([u8; N]) -> T, err: impl FnOnce() -> E) -> Result<T, E> {
	Ok(conv(array_of_iter(iter.take(N)).ok_or_else(err)?))
}

pub trait Bytecodable {
	fn add_byte(&mut self, b: &[u8]);
	fn change_idx(&mut self, idx: u16, val: u8);
	fn symi_of_sym(&mut self, s: &str) -> u32;
	fn pos(&self) -> u16;
	
	fn push_nil(&mut self) {
		self.add_byte(&[PUSH_NIL]);
	}

	fn push_self(&mut self) {
		self.add_byte(&[PUSH_SELF]);
	}

	fn push_super(&mut self) {
		self.add_byte(&[PUSH_SUPER])
	}

	fn push_true(&mut self) {
		self.add_byte(&[PUSH_TRUE]);
	}

	fn push_false(&mut self) {
		self.add_byte(&[PUSH_FALSE]);
	}

	fn push_char(&mut self, c: char) {
		// 1 instr + 4 char
		let mut arr: [u8; 5] = [0; 5];
		arr[0] = PUSH_CHAR;
		add_int(1, &mut arr, c as u32, u32::to_be_bytes);
		self.add_byte(&arr);
	}

	fn push_int(&mut self, i: i64) {
		match i {
			0 => self.add_byte(&[PUSH_ZERO]),
			1 => self.add_byte(&[PUSH_ONE]),
			2 => self.add_byte(&[PUSH_TWO]),
			_ => {
				// 1 instr + 8 i64
				let mut arr: [u8; 9] = [0; 9];
				arr[0] = PUSH_INT;
				add_int(1, &mut arr, i, i64::to_be_bytes);
				self.add_byte(&arr);
			}
		}
	}

	fn push_float(&mut self, f: f64) {
		// 1 instr + 8 f64
		let mut arr: [u8; 9] = [0; 9];
		arr[0] = PUSH_FLOAT;
		add_int(1, &mut arr, f, f64::to_be_bytes);
		self.add_byte(&arr);
	}

	fn push_field(&mut self, i: u16) {
		// 1 instr + 2 u16
		let mut arr: [u8; 3] = [0; 3];
		arr[0] = PUSH_FIELD;
		add_int(1, &mut arr, i, u16::to_be_bytes);
		self.add_byte(&arr);
	}

	fn push_local(&mut self, i: u16) {
		// 1 instr + 2 u16
		let mut arr: [u8; 3] = [0; 3];
		arr[0] = PUSH_LOCAL;
		add_int(1, &mut arr, i, u16::to_be_bytes);
		self.add_byte(&arr);
	}

	fn push_string(&mut self, s: &str) {
		// 1 instr + 2 u16
		let mut arr: [u8; 3] = [0; 3];
		arr[0] = PUSH_STRING;
		add_int(1, &mut arr, s.len() as u16, u16::to_be_bytes);
		self.add_byte(&arr);
		self.add_byte(s.as_bytes());
	}

	fn push_symbol(&mut self, s: &str) {
		// 1 instr + 4 u32
		let mut arr: [u8; 5] = [0; 5];
		arr[0] = PUSH_SYMBOL;
		add_int(1, &mut arr, self.symi_of_sym(s), u32::to_be_bytes);
		self.add_byte(&arr);
	}

	fn push_global(&mut self, s: &str) {
		// 1 instr + 2 u16
		let mut arr: [u8; 3] = [0; 3];
		arr[0] = PUSH_GLOBAL;
		add_int(1, &mut arr, self.symi_of_sym(s), u32::to_be_bytes);
		self.add_byte(&arr);
	}
	
	fn push_array(&mut self, l: u16) {
		// 1 instr + 2 u16
		let mut arr: [u8; 3] = [0; 3];
		arr[0] = PUSH_ARRAY;
		add_int(1, &mut arr, l, u16::to_be_bytes);
		self.add_byte(&arr);
	}

	fn push_block<T, F>(&mut self, argn: u16, argstart: u16, mut add_block: F) -> T
	where F: FnMut(&mut Self) -> T {
		// 1 u8 (instr)
		// + 2 u16 (nextinstr)
		// + 2 u16 (n args)
		// + 2 u16 (arg start ilocals)
		// + 2 u16 (stack size)
		let initpos = self.pos() + 1;
		let mut arr: [u8; 9] = [0; 9];
		arr[0] = PUSH_BLOCK;
		add_int(3, &mut arr, argn, u16::to_be_bytes);
		add_int(5, &mut arr, argstart, u16::to_be_bytes);
		self.add_byte(&arr);
		let val = add_block(self);
		let curpos = self.pos();
		for (i, b) in (curpos - initpos).to_be_bytes().into_iter().enumerate() {
			self.change_idx(initpos+(i as u16), b);
		}
		val
	}

	fn store_field(&mut self, i: u16) {
		let mut arr: [u8; 3] = [0; 3];
		arr[0] = STORE_FIELD;
		add_int(1, &mut arr, i, u16::to_be_bytes);
		self.add_byte(&arr);
	}

	fn store_local(&mut self, i: u16) {
		let mut arr: [u8; 3] = [0; 3];
		arr[0] = STORE_LOCAL;
		add_int(1, &mut arr, i, u16::to_be_bytes);
	}

	fn pop(&mut self) {
		self.add_byte(&[POP]);
	}

	fn dup(&mut self) {
		self.add_byte(&[DUP]);
	}

	fn send(&mut self, sym: &str, nargs: u16) {
		// 1 instr + 4 u32 + 2 u16
		match (sym, nargs) {
			("+", 2) => self.add_byte(&[SEND_PLUS]),
			("-", 2) => self.add_byte(&[SEND_MINUS]),
			("/", 2) => self.add_byte(&[SEND_DIV]),
			("*", 2) => self.add_byte(&[SEND_MUL]),
			_ => {
				let mut arr: [u8; 7] = [0; 7];
				arr[0] = SEND;
				add_int(1, &mut arr, self.symi_of_sym(sym), u32::to_be_bytes);
				add_int(5, &mut arr, nargs, u16::to_be_bytes);
				self.add_byte(&arr);
			}
		}
	}	

	fn ret(&mut self) {
		self.add_byte(&[RETURN]);
	}

	fn return_self(&mut self) {
		self.add_byte(&[RETURN_SELF]);
	}

	fn return_block(&mut self) {
		self.add_byte(&[RETURN_BLOCK]);
	}
}

