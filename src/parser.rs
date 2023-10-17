use std::collections::HashSet;
use std::fmt;
#[derive(PartialEq, Debug, Clone)]
pub enum PureExpr {
    Assignment(String, Box<Expr>),
    Return(Box<Expr>),
    Str(String),
    Symbol(String),
    Char(char),
    Array(Vec<Expr>),
    Int(i64),
    Float(f64),
    Variable(String),
    MsgSend(Box<Expr>, String, Vec<Expr>),
    Block(Vec<String>, Vec<String>, Vec<Expr>),
    True,
    False,
    Nil,
    SelfRef,
    Super,
}
pub type Location = (u32, u32);
pub type Expr = (PureExpr, Location);

#[derive(PartialEq, Eq, Debug)]
pub struct ParseError(String, Location);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}-{}", self.0, self.1.0, self.1.1)
    }
}

#[derive(Debug)]
pub struct MessageDef {
    pub selector: String,
    pub args: Vec<String>,
    pub vardecl: Vec<String>,
    pub body: Vec<Expr>,
}

impl MessageDef {
    pub fn from_code(code: &str) -> Result<Self, ParseError> {
        let mut parser = Parser::new(code);
        let (selector, args) = parser.parse_meth_sign()?;
        let vardecl = parser
            .read_tempvardecl()?
			.unwrap_or(Vec::new());
        let body = parser.parse_stmts(None)?;
        Ok(Self {
            selector,
            args,
            vardecl,
            body,
        })
    }
}

struct Parser<'a> {
    code: &'a str,
    idx: usize,
    location: Location,
    specialchars: HashSet<char>,
}

impl<'a> Parser<'a> {
    // New
    pub fn new(code: &'a str) -> Self {
        Self {
            code,
            idx: 0, 
            location: (0, 0),
            specialchars: SPECIAL_CHARACTERS.chars().collect(),
        }
    }

    // Errors
    fn err(&self, msg: String) -> ParseError {
        ParseError(msg, self.location)
    }

    fn unexpected_eof(&self) -> ParseError {
        self.err("unexpected EOF".to_string())
    }

    fn unexpected_char(&self, c: char) -> ParseError {
        self.err(format!("unexpected char: '{c}'"))
    }

    #[inline]
    fn unexpected(&self, res: Option<char>) -> ParseError {
        match res {
            Some(c) => self.unexpected_char(c),
            None => self.unexpected_eof(),
        }
    }

    // Utility functions for reading
    #[inline]
    fn lookahead(&self, idx: usize) -> Option<char> {
        self.code.get(self.idx..)?.chars().nth(idx)
    }

    fn advance(&mut self) -> Option<char> {
        let Some(c) = self.lookahead(0) else { return None };
        let (line, cidx) = self.location;
        if c == '\n' {
            self.location = (line + 1, 0);
        } else {
            self.location = (line, cidx + 1);
        }
        self.idx += c.len_utf8();
        Some(c)
    }

    #[inline]
    fn expect(&mut self, c: char) -> Result<char, ParseError> {
        match self.advance() {
            Some(r) if r == c => Ok(r),
            Some(r) => return Err(self.unexpected_char(r)),
            None => return Err(self.unexpected_eof()),
        }
    }

    fn mb_expect_str(&mut self, s: &str) -> bool {
        let Some(sl) = self.code.get(self.idx..) else {
			return false;
		};
        let retval = sl.starts_with(s);
        if retval {
            self.idx += s.len();
        }
        retval
    }

    #[inline]
    fn advance_if(&mut self, f: impl FnOnce(&mut Parser, char) -> bool) -> Option<char> {
        let Some(c) = self.lookahead(0) else { return None };
        if f(self, c) {
            self.advance()
        } else {
            None
        }
    }

    fn lookahead_str(&self, idx: usize, n: usize) -> Option<String> {
        Some(
            self.code
                .get(self.idx..)?
                .chars()
                .skip(idx)
                .take(n)
                .collect::<String>(),
        )
    }

    fn with_advance<T>(&mut self, action: impl FnOnce(&mut Self, Option<char>) -> T) -> T {
		let a = self.advance();
        action(self, a)
    }

    /// Constructs a type with the location attached.
    fn with_loc<T>(&self, pe: T) -> (T, Location) {
        (pe, self.location)
    }

    // Reader functions for reading various tokens
    fn read_ident(&mut self) -> Option<String> {
        let mut s = self.advance_if(|_, c| c.is_alphabetic())?.to_string();
        while let Some(c) = self.advance_if(|_, c| c.is_alphabetic()) {
            s.push(c);
        }

        (!s.is_empty()).then_some(s)
    }

    fn read_specchar(&mut self) -> Option<String> {
        let mut s = String::new();
        while let Some(c) = self.advance_if(|sel, c| sel.specialchars.contains(&c)) {
            s.push(c);
        }

        (!s.is_empty()).then_some(s)
    }

    fn read_string(&mut self) -> Result<String, ParseError> {
        self.expect('\'')?;

        let mut s = String::new();
        while let Some(c) = self.advance() {
            match c {
                '\'' => match self.advance_if(|_, c| c == '\'') {
                    Some('\'') => s.push('\''),
                    _ => return Ok(s),
                },
                _ => s.push(c),
            }
        }
        Err(self.unexpected_eof())
    }

    fn read_symbol(&mut self) -> Result<String, ParseError> {
        let Some(fst) = self.lookahead(0) else {
			return Err(self.unexpected_eof());
		};

        let symbuf = match fst {
            'a'..='z' | 'A'..='Z' => {
                let mut symbuf = String::new();
                while let Some(c) = self.advance_if(|_, c| c.is_alphanumeric() || c == ':') {
                    symbuf.push(c);
                }
                symbuf
            }
            c if self.specialchars.contains(&c) => {
                self.read_specchar().ok_or_else(|| self.unexpected_eof())?
            }
            '\'' => self.read_string()?,
            _ => return Err(self.unexpected_char(fst)),
        };
        Ok(symbuf)
    }

    fn read_tempvardecl(&mut self) -> Result<Option<Vec<String>>, ParseError> {
        self.skip_whitespace()?;
        let Some('|') = self.lookahead(0) else {
			return Ok(None);
		};
        self.advance();

       let mut vars = Vec::new();
        loop {
            self.skip_whitespace()?;
            let Some(ident) = self.read_ident() else {
				self.expect('|')?;
				break;
			};
            vars.push(ident);
        }
        (!vars.is_empty())
            .then_some(Some(vars))
            .ok_or_else(|| self.err("|vars...| must declare at least one variable".to_string()))
    }

    fn skip_comment(&mut self) -> Result<(), ParseError> {
        self.expect('"')?;

        while let Some(c) = self.advance() {
            if c == '"' {
                return Ok(());
            }
        }
        return Err(self.unexpected_eof());
    }

    fn skip_horizontal_whitespace(&mut self) {
        while let Some('\t' | ' ') = self.lookahead(0) {
			self.advance();
		}
    }

    fn skip_whitespace(&mut self) -> Result<(), ParseError>{
        while let Some(c@(' ' | '\t' | '\n' | '"')) = self.lookahead(0) {
			if c == '"' {
				self.skip_comment()?;
			} else {
				self.advance();
			}
		}
		Ok(())
    }

    fn parse_num(&mut self) -> Result<PureExpr, ParseError> {
        let mut numbuf = String::new();
        let mut radix: u32 = 10;
        let mut seen_period = false;
        let mut seen_r = false;
		let mut seen_e = false;
        while let Some(c) = self.lookahead(0) {
            match c {
                'r' if !seen_r && !seen_period => {
					seen_r = true;
                    radix = numbuf
                        .parse::<u32>()
                        .map_err(|e| self.err(format!("error parsing radix {numbuf}: {e}")))?;
                    if radix > 36 {
                        return Err(
                            self.err(format!("radix cannot be larger than 36, got {radix}"))
                        );
                    }
                    numbuf = String::new();
					self.advance();
                },
                '.' if !seen_period => {
                    if self.lookahead(1).map(char::is_alphanumeric) == Some(true) {
						seen_period = true;
                        self.with_advance(|_, _| numbuf.push('.'))
                    } else {
                        break;
                    }
                },
				'e' if seen_period && !seen_e => {
					if self.lookahead(1).map(char::is_alphanumeric) == Some(true) {
						seen_e = true;
						self.with_advance(|_, _| numbuf.push('e'))
					} else {
						break;
					}
				},
                c if c.is_digit(radix) => self.with_advance(|_, _| numbuf.push(c)),
                _ => break,
            }
        }

        if seen_period {
            if radix != 10 {
                return Err(self.err(format!("error trying to parse number {numbuf} with radix {radix}: floats must be decimal")));
            }

            let float = numbuf
                .parse::<f64>()
                .map_err(|e| self.err(format!("error parsing float {numbuf}: {e}")))?;

            Ok(PureExpr::Float(float))
        } else {
            let int = i64::from_str_radix(&numbuf, radix).map_err(|e| {
                self.err(format!(
                    "error parsing int {numbuf} with radix {radix}: {e}"
                ))
            })?;
            Ok(PureExpr::Int(int))
        }
    }

    fn parse_variable(&mut self) -> Result<PureExpr, ParseError> {
        let identbuf = self.read_ident().ok_or_else(|| self.unexpected_eof())?;
        let pexpr = match identbuf.as_str() {
            "true" => PureExpr::True,
            "false" => PureExpr::False,
            "nil" => PureExpr::Nil,
            "super" => PureExpr::Super,
            "self" => PureExpr::SelfRef,
            _ => PureExpr::Variable(identbuf),
        };
        Ok(pexpr)
    }

    #[inline]
    fn parse_charlit(&mut self) -> Result<PureExpr, ParseError> {
        self.advance().ok_or_else(|| self.unexpected_eof())?;
        Ok(PureExpr::Char(
            self.advance().ok_or_else(|| self.unexpected_eof())?,
        ))
    }

    fn parse_array_element(&mut self) -> Result<PureExpr, ParseError> {
        use PureExpr::*;
        loop {
            let Some(c) = self.lookahead(0) else {
				return Err(self.unexpected_eof());
			};

            let expr = match c {
                '"' => {
                    self.skip_comment()?;
                    continue;
                }
                ' ' | '\t' | '\n' => {
                    self.skip_whitespace()?;
                    continue;
                }
                '#' if self.lookahead(1) == Some('(') => {
                    Array(self.with_advance(|p, _| p.parse_lit_array())?)
                }
                '#' => Symbol(self.with_advance(|p, _| p.read_symbol())?),
                '(' => Array(self.parse_lit_array()?),
                '$' => self.parse_charlit()?,
                // we unwrap here v because if the peek was successful .next() should b
                '0'..='9' => self.parse_num()?,
                'a'..='z' | 'A'..='Z' | '\'' => Symbol(self.read_symbol()?),
                c if self.specialchars.contains(&c) => Symbol(self.read_symbol()?),
                _ => return Err(self.unexpected_char(c)),
            };
            return Ok(expr);
        }
    }

    fn parse_lit_array(&mut self) -> Result<Vec<Expr>, ParseError> {
        self.expect('(')?;

        let mut exprs = Vec::new();
        loop {
            let expr = match self.lookahead(0) {
                Some(')') => {
                    self.advance();
                    break;
                }
                Some(_) => self.parse_array_element()?,
                None => {
                    self.advance();
                    return Err(self.unexpected_eof());
                }
            };

            exprs.push(self.with_loc(expr));
        }
        return Ok(exprs);
    }

    fn parse_block(&mut self) -> Result<PureExpr, ParseError> {
        self.expect('[')?;
        let mut args = Vec::new();
        self.skip_whitespace()?;
        if self.lookahead(0) == Some(':') {
			loop {
				match self.lookahead(0) {
					Some(':') => {
						self.advance();
					},
					Some('|') => {
						self.advance();
						break;
					},
					res => return Err(self.unexpected(res)),
				}
				
				let Some(ident) = self.read_ident() else {
					return Err(self.unexpected(self.lookahead(0)));
				};
				args.push(ident);
				
				self.skip_whitespace()?
			}
        }
        let vardecl = self.read_tempvardecl()?.unwrap_or_else(|| Vec::new());
        let stmts = self.parse_stmts(Some(']'))?;
        Ok(PureExpr::Block(args, vardecl, stmts))
    }

    fn parse_primary(&mut self) -> Result<Option<PureExpr>, ParseError> {
        use PureExpr::*;
        loop {
            let c = match self.lookahead(0) {
                Some(c) => c,
                None => return Ok(None),
            };
            let expr = match c {
                '#' if self.lookahead(1) == Some('(') => {
                    Array(self.with_advance(|p, _| p.parse_lit_array())?)
                }
                '#' => Symbol(self.with_advance(|p, _| p.read_symbol())?),
                '$' => self.parse_charlit()?,
                '0'..='9' => self.parse_num()?,
                'a'..='z' | 'A'..='Z' => self.parse_variable()?,
                '\'' => Str(self.read_string()?),
                '(' => {
                    self.advance();
                    let ex = self.parse_expr()?.ok_or_else(|| self.unexpected_eof())?.0;
                    self.expect(')')?;
                    ex
                }
                '{' => Array(self.with_advance(|p, _| p.parse_stmts(Some('}')))?),
                '[' => self.parse_block()?,
                ' ' | '\t' | '\n' => {
					self.advance();
					continue
				},
				'"' => {
					self.skip_comment()?;
					continue
				},
                _ => return Err(self.unexpected_char(c)),
            };
            return Ok(Some(expr));
        }
    }

    fn parse_unary(&mut self) -> Result<Option<Expr>, ParseError> {
        let Some(mut expr) = self.parse_primary()?.map(|e| self.with_loc(e)) else {
			return Ok(None);
		};
        loop {
            self.skip_whitespace()?;
            let startidx = self.idx;
            let Some(msg) = self.read_ident() else {
				return Ok(Some(expr));
			};

            if self.lookahead(0) == Some(':') {
                self.idx = startidx;
                return Ok(Some(expr));
            }

            expr = self.with_loc(PureExpr::MsgSend(Box::new(expr), msg, Vec::new()));
        }
    }

    fn parse_binary(&mut self) -> Result<Option<Expr>, ParseError> {
        let Some(mut expr) = self.parse_unary()? else {
			return Ok(None);
		};

        loop {
            self.skip_whitespace()?;
            let Some(msg) = self.read_specchar() else {
				return Ok(Some(expr));
			};

            let Some(snd) = self.parse_unary()? else {
				return Err(self.unexpected_eof());
			};
            expr = self.with_loc(PureExpr::MsgSend(Box::new(expr), msg, vec![snd]));
        }
    }

    fn parse_keyword(&mut self) -> Result<Option<Expr>, ParseError> {
        let Some(mut expr) = self.parse_binary()? else {
			return Ok(None);
		};
        let mut selector = String::new();
        let mut args = Vec::new();
        loop {
            let startidx = self.idx;
            let Some(msg) = self.read_ident() else {
				break;
			};

            let Some(_) = self.advance_if(|_, c| c == ':') else {
				self.idx = startidx;
				break;
			};

            let Some(snd) = self.parse_binary()? else {
				return Err(self.unexpected_eof());
			};

            selector.push_str(&msg);
            selector.push(':');
			args.push(snd);
        }

        if !args.is_empty() {
            expr = self.with_loc(PureExpr::MsgSend(Box::new(expr), selector, args));
        }

        Ok(Some(expr))
    }

    fn parse_return(&mut self) -> Result<Option<Expr>, ParseError> {
        self.skip_whitespace()?;
        let Some('^') = self.advance_if(|_, c| c == '^') else {
			return self.parse_keyword();
		};

        let Some(expr) = self.parse_keyword()? else {
			return Err(self.unexpected_eof());
		};
        Ok(Some(self.with_loc(PureExpr::Return(Box::new(expr)))))
    }

    fn parse_assignment(&mut self) -> Result<Option<Expr>, ParseError> {
        self.skip_whitespace()?;
        let startidx = self.idx;
        let Some(ident) = self.read_ident() else {
			return self.parse_return();
		};
        self.skip_whitespace()?;

        if !self.mb_expect_str(":=") && !self.mb_expect_str("_") {
            self.idx = startidx;
            return self.parse_return();
        }

        let Some(rhs) = self.parse_return()? else {
			return Ok(None);
		};

        Ok(Some(
            self.with_loc(PureExpr::Assignment(ident, Box::new(rhs))),
        ))
    }

    #[inline]
    fn parse_expr(&mut self) -> Result<Option<Expr>, ParseError> {
        self.parse_assignment()
    }

    fn parse_stmts(&mut self, sentinel: Option<char>) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        loop {
            if self
                .advance_if(|_, c| sentinel.map(|exp| exp == c) == Some(true))
                .is_some()
            {
                return Ok(exprs);
            }
            let Some(expr) = self.parse_expr()? else {
				return Ok(exprs);
			};
            self.skip_whitespace()?;
            exprs.push(expr);
            self.advance_if(|_, c| c == '.');
        }
    }

    fn parse_meth_sign(&mut self) -> Result<(String, Vec<String>), ParseError> {
        self.skip_horizontal_whitespace();
        let Some(c) = self.lookahead(0) else {
            return Err(self.unexpected_eof());
        };

        let (mut is_keyword, mut selector) =
            match (c.is_alphabetic(), self.specialchars.contains(&c)) {
                (true, false) => (
                    true,
                    self.read_ident().ok_or_else(|| self.unexpected_eof())?,
                ),
                (false, true) => (
                    false,
                    self.read_specchar()
                        .ok_or_else(|| self.unexpected_char(c))?,
                ),
                (false, false) => return Err(self.unexpected_char(c)),
                (true, true) => {
                    return Err(self.err(format!(
                        "parser bug! {c} cannot be both alphabetic and a special character"
                    )))
                }
            };

        is_keyword = is_keyword && self.advance_if(|_, c| c == ':').is_some();
        self.skip_horizontal_whitespace();
        let mut paramnames = Vec::new();
        if !is_keyword {
            self.expect('\n')?;
            return Ok((selector, paramnames));
        }

        selector.push(':');
        let arg = self.read_ident().ok_or_else(|| self.unexpected_eof())?;
        paramnames.push(arg);
        loop {
			self.skip_horizontal_whitespace();
            if self.advance_if(|_,c| c == '\n').is_some() {
                return Ok((selector, paramnames));
            }
            let Some(ident) = self.read_ident() else {
                return Err(self.unexpected_eof());
            };

            self.expect(':')?;
            selector.push_str(&ident);
            selector.push(':');

            self.skip_horizontal_whitespace();
            let Some(arg) = self.read_ident() else {
                return Err(self.unexpected(self.lookahead(0)));
            };
            paramnames.push(arg);
        }
    }
}

static SPECIAL_CHARACTERS: &str = r"+/*\~<=>@%|&?!";

#[cfg(test)]
mod test {
	use super::MessageDef;
    use super::Parser;
    use super::PureExpr;

    macro_rules! assert_parseexpr {
        ($input:expr, $pattern:pat) => {
            match Parser::new($input).parse_expr() {
                Ok(Some(($pattern, _))) => println!("{} is {}", $input, stringify!($pattern)),
                o => panic!(
                    "{} doesn't match {}, instead was {:?}",
                    $input,
                    stringify!($pattern),
                    o
                ),
            }
        };
    }

    #[test]
    fn num() {
        use PureExpr::*;
        assert_parseexpr!("2", Int(2));
        assert_parseexpr!("3.5", Float(3.5));
        assert_parseexpr!("16rFF", Int(255));
        assert_parseexpr!("8r777", Int(0o777));
        assert_parseexpr!("2.0e2", Float(2.0e2));
    }

    #[test]
    fn string() {
        use PureExpr::*;
        macro_rules! assert_comparestr {
            ($input:expr, $pattern:expr) => {
                match Parser::new($input).parse_expr() {
                    Ok(Some((Str(s), _))) if s == $input => {
                        println!("{} = {}", $input, stringify!($pattern))
                    }
                    o => panic!(
                        "{} doesn't match {}, instead was {:?}",
                        $input,
                        stringify!($pattern),
                        o
                    ),
                }
            };
        }
        assert_comparestr!(
            r#"'amogus sus sus sus sus amogus'"#,
            "amogus sus sus sus sus amogus"
        );
        assert_comparestr!(
            r#"'Hi! I''m make, and I did your mother yesterday!'"#,
            "Hi! I'm make, and I did your mother yesterday!"
        );
    }

    #[test]
    fn symbol() {
        use PureExpr::*;
        macro_rules! assert_comparesym {
            ($input:expr, $pattern:expr) => {
                match Parser::new($input).parse_expr() {
                    Ok(Some((Symbol(s), _))) if s == $input => {
                        println!("{} = {}", $input, stringify!($pattern))
                    }
                    o => panic!(
                        "{} doesn't match {}, instead was {:?}",
                        $input,
                        stringify!($pattern),
                        o
                    ),
                }
            };
        }
        assert_comparesym!("#test:with:", "test:with:");
        assert_comparesym!("#'hi amogus'", "hi amogus");
        assert_comparesym!("#hewo", "hewo");
    }

    #[test]
    fn array() {
        // this is where the real stuff happens
        use PureExpr::*;
        match Parser::new("#(2 3)").parse_expr() {
            Ok(Some((Array(v), _))) => match v[..] {
                [(Int(2), _), (Int(3), _)] => {}
                _ => panic!("unexpected value: Ok(Some((Array({v:?}))))"),
            },
            v => panic!("unexpected value: {v:?}"),
        }
        match Parser::new("#(this:message:is:cool: 2 3 $) hahahaamogus)").parse_expr() {
            Ok(Some((Array(v), _))) => match &v[..] {
                [(Symbol(sym), _), (Int(2), _), (Int(3), _), (Char(')'), _), (Symbol(sym2), _)] => {
                    if sym == "this:message:is:cool:" && sym2 == "hahahaamogus" {}
                }
                _ => panic!("unexpected value: Ok(Some((Array({v:?}))))"),
            },
            e => panic!("unexpected value: {e:?}"),
        }
    }
/*
    #[test]
    fn realgame() {
        use PureExpr::*;
        let code = r#"from: x to: y
|z w|
z _ 9.
w _ 8.0.
z := instancevar message: #((2 + 4) message:with:colons: $' 'your_mom'"sussy baka"2.5e3 (5 6)).
(x >= 10)ifTrue:[Transcript show: 'i love ur mom'].
^ [:x | x + 2] value: 3
"#;
        let MessageDef {
            selector,
            args,
            vardecl,
            body,
        } = MessageDef::from_code(code).unwrap();
        assert_eq!(selector, "from:to:");
        assert_eq!(args, vec!["x", "y"]);
        assert_eq!(vardecl, vec!["z", "w"]);
        assert_eq!(
            body,
            vec![
                (Assignment("z".to_string(), (Int(9), (1, 1))), (1, 5)),
                (Assignment("r".to_string(), (Int(9), (1, 1))), (1, 5)),
            ]
        )
    }*/
}
// whoa
// my tests run
