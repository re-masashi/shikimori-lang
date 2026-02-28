
## Primitives

```
i8  i16  i32  i64
u8  u16  u32  u64
f32 f64
bool
usize
str   // fat pointer { *u8, len }
```

---

## Pointers & Arrays

```
*T         // raw pointer
[]T        // slice. fat pointer { *T, len }
[N]T       // fixed size array, N must be comptime constant
?T         // optional. sugar for union { some: T, none }
```

---

## Top-Level Declarations

### Functions

```
fn name[A, B](arg: A, other: B) -> RetType {
    // body
}
```

### Structs

```
struct Name[A, B] {
    field: Type,
    other: Type,

    fn new(x: i32) -> Name { ... }          // static. no self
    fn get(self, ) -> i32 { self.x }         // instance. self is reference
    fn set(self, val: i32) { self.x = val; } // mutation through self (self is always ref)
}
```

### Tagged Unions

```
union Name[A] {
    variant_a: i32,
    variant_b: f32,
    nothing,              // unit variant

    fn is_int(self) -> bool { ... }
}
```

### Interfaces

```
interface Drawable {
    fn draw(self);
    fn bounds(self) -> Rect;
}
```

A type satisfies an interface by having all the required methods. No explicit `impl` declaration. Interfaces are used as types: `fn render(d: interface Drawable)` takes any type that satisfies `Drawable`. interface A + B + C is also valid.

`interface Drawable` is a fat pointer. { *data, *vtable }

### Extern

```
extern c_malloc(size: usize) -> *u8;
extern c_free(ptr: *u8);
```

For structs across FFI boundaries: define the same struct with the same field order on both sides.

---

## Imports

```
use "path/to/file"                  // import all
use "path/to/file" (Foo as F, bar)  // selective import with optional rename
```

Everything is public. 

---

## Variables

```
let x: i32 = 5;      // explicit type
let x = 5;           // inferred (local inference only)
let x = myfn();      // inferred from return type
```

Type inference is local; function signatures are always annotated.

---

## Control Flow

### If

```
if condition {
    ...
} else if other {
    ...
} else {
    ...
}
```

### Match

```
match value {
    variant_a(x) => { ... },
    variant_b(y) => { ... },
    nothing      => { ... },
    _            => { ... },   // wildcard / default
}
```

Match arms support:
- Union variant destructuring: `variant(x)`
- Literal values: `42`, `true`, `"hello"`
- Wildcard: `_`

No complex patmat. 

### Loops

```
loop { ... }                        // infinite
while condition { ... }
for item in iterable { ... }
for i in 0..n { ... }               // range
```

Labeled breaks:

```
:outer loop {
    loop {
        break :outer;
    }
}
```

### Defer

```
fn example() {
    let buf = alloc(1024);
    defer free(buf);        // runs when scope exits, in reverse order
    // ...
}
```

---

## Methods

First argument determines dispatch:

```
struct Foo {
    fn static_method(x: i32) -> Foo { ... }   // no self = static
    fn instance(self, x: i32) -> i32 { ... }           // self = instance method
}

Foo.static_method(5);   // static call
foo.instance(0);         // instance call. sugar for Foo.instance(foo, 0)
```

`self` is always a reference. 
Method references (`let f = foo.method`) are invalid (for now).

---

## Comptime

### Comptime Builtins

```
@typename(T)           -> string    // "MyStruct"
@typeid(T)             -> usize     // unique integer per type
@sizeof(T)             -> usize     // size in bytes
@alignof(T)            -> usize     // alignment
@fields(T)             -> []Field   // struct field metadata
@variants(T)           -> []Variant // union variant metadata
@has_field(T, "name")  -> bool
@field(val, "name")               // access field by name at comptime
```

Built-in comptime structs (comptime-only, not available at runtime):

```
struct Field {
    name:   string,
    type:   Type,
    offset: usize,
}

struct Variant {
    name: string,
    type: Type,
}
```

`Type` is an opaque comptime handle.

### Comptime Expressions

```
comptime let x = @sizeof(i32) * 2;
```

### Comptime If. prunes dead branches entirely

```
fn serialize[T](val: T) -> []u8 {
    comptime if @typeid(T) == @typeid(i32) {
        return int_to_bytes(val);
    } else {
        return generic_serialize(val);
    }
}
```

### Comptime For. loop unrolled at compile time

```
fn print_fields[T](val: T) {
    comptime for field in @fields(T) {
        let f = @field(val, field.name);
        print(field.name);
        print(f);
    }
}
```

User-defined `comptime fn`: TODO

---

## Macros

Syntax macros  take token trees, produce AST nodes. Argument kinds: `expr`, `stmt`, `type`, `ident`.

```
macro assert(cond: expr, msg: expr) {
    if (!cond) { panic(msg); }
}
```

No recursive macros or repetition patterns yet.

---

## Memory & Allocators

No GC, no borrow checker. You own your pointers. Dangling pointers are your problem.

Allocators are structs with function pointers. The stdlib provides a default heap allocator. Custom allocators are first class citizens. Pass them around like any other value.

```
extern c_malloc(size: usize) -> *u8;
extern c_free(ptr: *u8);
```

---

## Error Handling

`?T` is `union { some: T, none }`. Use `match` to unwrap.

`?` propagation operator is planned but not in scope yet.

---

## Operator Overloading

Defined via special methods. Not required now. Added later.

```
__add__, __sub__, __mul__, __div__
__eq__, __lt__, __gt__
__index__
```

---

## Explicitly Out of Scope (for now)

- `?` error propagation operator
- Method references / bound closures
- User-defined `comptime fn`
- Macros with repetition / recursion
- `Vec` / dynamic array stdlib type
- `distinct` types
- Tuples
- Type aliases
