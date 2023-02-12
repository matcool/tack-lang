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
- [X] else statements
- [X] else if 
- [X] while statements
- [X] variable scoping in statements
- [X] string support
- [X] structs
- [ ] external functions? (not writing my own malloc)
- [X] pointers
- [ ] better build system (use temp folder maybe)
- [ ] import
- [X] structs as function args
- [ ] arrays