mod opcodes {
	const PUSH_NIL: u8 = 0;
	const PUSH_SELF: u8 = 1;
	const PUSH_TRUE: u8 = 2;
	const PUSH_FALSE: u8 = 2;
	const PUSH_CHAR: u8 = 3;
	const PUSH_INT: u8 = 4;
	const PUSH_FLOAT: u8 = 5;
	const PUSH_FIELD: u8 = 6;
	const PUSH_LOCAL: u8 = 7;
	const PUSH_LITERAL: u8 = 8;
	const PUSH_GLOBAL: u8 = 9;
	const PUSH_ARRAY: u8 = 10;
	const STORE_FIELD: u8 = 11;
	const STORE_LOCAL: u8 = 12;
	const POP: u8 = 13;
	const SEND: u8 = 14;
	
}
