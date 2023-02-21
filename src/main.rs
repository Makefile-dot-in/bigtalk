use bigtalk::parser::MessageDef;
use std::io::{stdin, Read};

fn main() -> Result<(), String> {
	let mut code = String::new();
	stdin().read_to_string(&mut code)
		.map_err(|x| x.to_string())?;
	let msgdef = MessageDef::from_code(&code)
		.map_err(|x| x.to_string())?;
	println!("{msgdef:?}");
	Ok(())
}
