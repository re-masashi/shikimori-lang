```
Map[K, V]  →  ForAll{
  vars: [(0,"K"), (1,"V")],
  body: TyNamed{"Map", Struct, [TyVar{0,"K"}, TyVar{1,"V"}]}
}
```

Instantiated:
```
Map[str, i32]  →  TyNamed{"Map", Struct, [TyNamed{"str", Primitive, []}, TyNamed{"i32", Primitive, []}]}
```

```
Result[T, E]  →  ForAll{
  vars: [(0,"T"), (1,"E")],
  body: TyNamed{"Result", Union, [TyVar{0,"T"}, TyVar{1,"E"}]}
}
```

Instantiated:
```
Result[i32, str] → TyNamed{"Result", Union, [TyNamed{"i32", Primitive, []}, TyNamed{"str", Primitive, []}]}
```

```
fn zip[A, B](a: A, b: B) -> Pair[A, B]  →  ForAll{
  vars: [(0,"A"), (1,"B")],
  body: FnTy{
    args: [TyVar{0,"A"}, TyVar{1,"B"}],
    return_type: TyNamed{"Pair", Struct, [TyVar{0,"A"}, TyVar{1,"B"}]}
  }
}
```

Instantiated:
```
FnTy{
  args: [TyNamed{"i32", Primitive, []}, TyNamed{"str", Primitive, []}],
  return_type: TyNamed{"Pair", Struct, [TyNamed{"i32", Primitive, []}, TyNamed{"str", Primitive, []}]}
}
```

Pass 1:
Collect top level names.
Register their signatures.

Pass 2:
Check

