# tack

very silly toy language

```rs
fn main(): i32 {
	return 42;
}
```

## Features
- own lexer & parser
- very bad x86 codegen
- literally nothing else

## TODO

- [X] functions
- [X] variables
- [ ] proper types (only supports i32 and bool atm)
- [X] if statements
- [ ] else statements
- [ ] else if 
- [X] while statements
- [ ] variable scoping in statements
- [ ] string support
- [ ] structs
- [ ] external functions? (not writing my own malloc)
- [ ] pointers