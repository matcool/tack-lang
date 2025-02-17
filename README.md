# tack

very silly toy language

```rs
fn main(): i32 {
	return 42;
}
```

## Roadmap

- [X] functions
- [X] variables
- [ ] floats
- [X] if statements
- [X] else statements
- [X] else if 
- [X] while statements
- [ ] for loops
- [X] string
- [X] structs
- [X] structs as function args
- [X] pointers
- [X] import
- [X] arrays
- [X] struct literals
	```rs
	struct Point {
		x: i32;
		y: i32;
	}
	fn main(): i32 {
		let point: Point = Point { x: 10, y: 20 };
		return point.x;
	}
	```
- [ ] struct methods
	```rs
	struct Point {
		x: i32;
		y: i32;

		fn dist_squared(self): i32 {
			return self.x * self.x + self.y * self.y;
		}
	}

	fn main(): i32 {
		let point: Point = Point { x: 10, y: 20 };
		return point.dist_squared();
	}
	```
- [ ] standard library
- [ ] error handling (Result?, `try expr`?)
- [ ] block expressions, yield statements
- [ ] basic generics
	```rs
	// not sure on syntax still
	fn add<T>(a: T, b: T): T {
		return a + b;
	}
	fn main(): i32 {
		// also not sure
		return add<i32>(1, 2);
	}
	```