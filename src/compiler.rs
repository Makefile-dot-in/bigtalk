mod opcodes {
	pub const PUSH_NIL:     u8 = 0;
	pub const PUSH_SELF:    u8 = 1;
	pub const PUSH_TRUE:    u8 = 2;
	pub const PUSH_FALSE:   u8 = 2;
	pub const PUSH_ZERO:    u8 = 3;
	pub const PUSH_ONE:     u8 = 4;
	pub const PUSH_TWO:     u8 = 5;
	pub const PUSH_CHAR:    u8 = 6;
	pub const PUSH_INT:     u8 = 7;
	pub const PUSH_FLOAT:   u8 = 8;
	pub const PUSH_FIELD:   u8 = 9;
	pub const PUSH_LOCAL:   u8 = 10;
	pub const PUSH_STRING:  u8 = 11;
	pub const PUSH_GLOBAL:  u8 = 12;
	pub const PUSH_ARRAY:   u8 = 13;
	pub const PUSH_BLOCK:   u8 = 14;
	pub const STORE_FIELD:  u8 = 15;
	pub const STORE_LOCAL:  u8 = 16;
	pub const POP:          u8 = 17;
	pub const SEND:         u8 = 18;
	pub const SEND_PLUS:    u8 = 19;
	pub const SEND_MINUS:   u8 = 20;
	pub const SEND_DIV:     u8 = 21;
	pub const SEND_MUL:     u8 = 22;
	pub const RETURN:       u8 = 23;
	pub const RETURN_BLOCK: u8 = 24;

	fn add_int<T, I>(start: usize, arr: &mut [u8], i: T, f: fn(T) -> I)
	where I: IntoIterator<Item = u8> {
		for (i, b) in f(i).into_iter().enumerate() {
			arr[start+i] = b;
		}
	}
	
	pub trait Bytecodable {
		fn add_byte(&mut self, b: &[u8]);
		fn change_idx(&mut self, idx: u16, val: u8);
		fn pos(&self) -> u16;
		
		fn push_nil(&mut self) {
			self.add_byte(&[PUSH_NIL]);
		}

		fn push_self(&mut self) {
			self.add_byte(&[PUSH_SELF]);
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
			// 1 instr + 8 i64
			let mut arr: [u8; 9] = [0; 9];
			arr[0] = PUSH_INT;
			add_int(1, &mut arr, i, i64::to_be_bytes);
			self.add_byte(&arr);
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

		fn push_global(&mut self, s: &str) {
			// 1 instr + 2 u16
			let mut arr: [u8; 3] = [0; 3];
			arr[0] = PUSH_GLOBAL;
			add_int(1, &mut arr, s.len() as u16, u16::to_be_bytes);
			self.add_byte(&arr);
			self.add_byte(s.as_bytes());
		}

		fn push_array(&mut self, l: u16) {
			// 1 instr + 2 u16
			let mut arr: [u8; 3] = [0; 3];
			arr[0] = PUSH_ARRAY;
			add_int(1, &mut arr, l, u16::to_be_bytes);
			self.add_byte(&arr);
		}

		fn push_block<F>(&mut self, mut add_block: F)
		where F: FnMut(&mut Self) {
			// 1 instr + 2 u16
			let mut arr: [u8; 3] = [0; 3];
			arr[0] = PUSH_BLOCK;
			let initpos = self.pos() + 1;
			self.add_byte(&arr);
			add_block(self);
			for (i, b) in initpos.to_be_bytes().into_iter().enumerate() {
				self.change_idx(initpos+(i as u16), b);
			}
		}
	}
}

