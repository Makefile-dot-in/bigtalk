
use std::collections::HashSet;
use std::hash::Hash;

use crate::{opcodes::Bytecodable, parser::{Expr, MessageDef, Location, PureExpr}};
use crate::index_or_push;

pub struct CompileError(String, Location);

fn statcomperr(l: Location, msg: &str) -> CompileError {
	CompileError(msg.to_owned(), l)
}

fn firstdup<T: Hash + Eq>(iter: impl Iterator<Item = T>) -> Option<T> {
	let mut set = HashSet::new();
	for ele in iter {
		if set.contains(&ele) {
			return Some(ele);
		}
		set.insert(ele);
	}
	None
}

pub type CompRes<T> = Result<T, CompileError>;

enum Var<'a> {
	Local(u16),
	Field(u16),
	Global(&'a str)
}

struct MessageCompiler<'a, 'b, 'c> {
	refpt: usize,
	locals: Vec<String>,
	fields: &'c [String],
	buf: &'b mut Vec<u8>,
	symbol_store: &'a mut Vec<String>
}



impl<'a, 'b, 'c> Bytecodable for MessageCompiler<'a, 'b, 'c> {
    fn add_byte(&mut self, b: &[u8]) {
        self.buf.extend_from_slice(b);
    }

    fn change_idx(&mut self, idx: u16, val: u8) {
        self.buf[self.refpt + (idx as usize)] = val;
    }

    fn symi_of_sym(&mut self, s: &str) -> u32 {
		index_or_push(self.symbol_store, |sym| s == &*sym, || s.to_owned()) as u32
    }

    fn pos(&self) -> u16 {
        self.buf.len() as u16
    }
}

impl<'a, 'b, 'c> MessageCompiler<'a, 'b, 'c> {
	fn compile_message(&mut self, MessageDef { args, vardecl, body, .. }: MessageDef) -> CompRes<()> {
		firstdup(args.iter()).map_or(Ok(()), |var| Err(CompileError(format!("duplicate argument name: {var}"), (0, 0))))?;
		firstdup(vardecl.iter()).map_or(Ok(()), |var| Err(CompileError(format!("duplicate local name: {var}"), (0, 0))))?;
		self.locals.extend(args);
		self.locals.extend(vardecl);
		
		if !self.pop_intersperse(body.iter())? {
			self.return_self();
		}
		Ok(())
	}

	fn pop_intersperse<'d>(&mut self, mut exprs: impl Iterator<Item = &'d Expr>) -> CompRes<bool> {
		let Some(e) = exprs.next() else { return Ok(false) };
		self.compile_expr(e)?;
		let mut expr = None;
		for ele in exprs {
			self.pop();
			expr = Some(ele);
			self.compile_expr(ele)?;
		}
		Ok(matches!(expr, Some((PureExpr::Return(_), _))))
	}
	
	fn compile_expr(&mut self, (pexpr, loc): &Expr) -> CompRes<()> {
		use PureExpr::*;
		match pexpr {
			Assignment(var, expr) => self.compile_assign(var, &expr, *loc)?,
			Return(expr) => self.compile_return(&expr)?,
			Str(s) => self.push_string(s),
			Symbol(s) => self.push_symbol(s),
			Char(c) => self.push_char(*c),
			Array(a) => self.compile_array(a, *loc)?,
			Int(i) => self.push_int(*i),
			Float(f) => self.push_float(*f),
			Variable(n) => self.compile_varref(&*n, *loc)?,
			MsgSend(rec, selector, args) => self.compile_send(&rec, &*selector, &*args, *loc)?,
			Block(args, tempvars, exprs) => self.compile_block(&*args, &*tempvars, &*exprs, *loc)?,
			True => self.push_true(),
			False => self.push_false(),
			Nil => self.push_nil(),
			SelfRef => self.push_self(),
			Super => self.push_super(),
		}
		Ok(())
	}

	fn get_var<'d>(&mut self, s: &'d str, l: Location) -> CompRes<Var<'d>> {
		let Some(c) = s.chars().next() else {
			return Err(statcomperr(l, "a variable name must have at least one character"));
		};

		if c.is_uppercase() {
			Ok(Var::Global(s))
		} else if let Some(ilocal) = self.locals.iter().rposition(|l| &*l == s) {
			Ok(Var::Local(ilocal as u16))
		} else if let Some(ifield) = self.fields.iter().rposition(|l| &*l == s) {
			Ok(Var::Field(ifield as u16))
		} else {
			Err(statcomperr(l, "variable not found"))
		}
	}

	fn compile_assign(&mut self, var: &str, expr: &Expr, l: Location) -> CompRes<()> {
		self.compile_expr(expr)?;
		match self.get_var(var, l)? {
			Var::Local(ilocal) => {
				self.store_local(ilocal);
			},
			Var::Field(ifield) => {
				self.store_field(ifield)
			},
			Var::Global(_) => return Err(statcomperr(l, "cannot assign to globals")),
		}

		Ok(())
	}

	fn compile_return(&mut self, expr: &Expr) -> CompRes<()> {
		if let (PureExpr::SelfRef, _) = expr {
			self.return_self();
			return Ok(());
		}
		self.compile_expr(expr)?;
		self.ret();
		Ok(())
	}

	fn compile_array(&mut self, a: &[Expr], l: Location) -> CompRes<()> {
		for expr in a {
			self.compile_expr(expr)?;
		}
		let n = a.len()
			.try_into()
			.map_err(|e| CompileError(format!("{} (does your array literal exceed 65535 elements?)", e), l))?;
		self.push_array(n);
		Ok(())
	}

	fn compile_varref(&mut self, n: &str, l: Location) -> CompRes<()> {	
		match self.get_var(n, l)? {
			Var::Local(ilocal) => self.push_local(ilocal),
			Var::Field(ifield) => self.push_field(ifield),
			Var::Global(s) => self.push_global(s)
		}
		Ok(())
	}

	fn compile_send(&mut self, rec: &Expr, selector: &str, args: &[Expr], l: Location) -> CompRes<()> {
		self.compile_expr(rec)?;
		for arg in args {
			self.compile_expr(arg)?;
		}
		self.send(selector, args.len().try_into()
				  .map_err(|e| CompileError(format!("{e} (are you sending a message with more than 65.5k arguments?"), l))?);
		Ok(())
	}

	fn compile_block(&mut self, args: &[String], tempvars: &[String], exprs: &[Expr], l: Location) -> CompRes<()> {
		let startn = self.locals.len();
		let argn: u16 = args.len().try_into().map_err(|e| CompileError(format!("{e} (do you have more than 65535 arguments?)"), l))?;

		self.push_block(startn as u16, argn, |mpars| {
			mpars.locals.extend_from_slice(args);
			mpars.locals.extend_from_slice(tempvars);

			if !mpars.pop_intersperse(exprs.iter())? {
				mpars.push_nil();
				mpars.return_block();
			}
			
			mpars.locals[startn..].iter_mut().for_each(|l| l.truncate(0)); // prevents other blocks from reusing the variables by accident
			Ok(())
		})
	}


}
