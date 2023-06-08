# Why Cat

**C** with **A**lgebraic **T**ypes

# What is Cat

A rust transpiler that converts Cat source code to C source code.

Use the power of algebraic data types with C idioms.

Cat is both a C API and an extension to C that transforms to the aformentioned C API.

There is a 1:1 matching for lines written in Cat and C, allowing the Cat transpiler to act as a front end for clang or gcc.

## Inspiration

https://github.com/hodefoting/oi/

## Building

First you need to build PEGLT
```
$ cd PEGLT
$ mkdir build
$ cd build
$ cmake .. -DCMAKE_INSTALL_PREFIX=../install -B ../
$ make -j {num of threads}
$ make install
```

Next, you build cat-cc
```
$ mkdir build
$ cd build
$ cmake ../ -B ../build
$ make
```

# Features

## Sum and Product Types and Match Statements

```c
  // product types
  type point = float * float

  // sum type
  // Every constructor may individually either carry no data or carry data. We
  // call constructors that carry no data constant; and those that carry data,
  // non-constant.

  // constant
  type peff = ENormal | ENotVery | Esuper

  // non-constant
  type shape =
    | Point of point
    | Circle of point * float /* center and radius */
    | Rect of point * point   /* lower-left and upper-right corners */

  // a closure with a match statement that returns a point type
  // dont need a return, automatically return the last statement
  point center(shape s) {
      match (s):
        | Point p -> p
        | Circle (p, _) -> p
        | Rect ((x1, y1), (x2, y2)) -> ((x2 +. x1) /. 2.0, (y2 +. y1) /. 2.0)
  }

  // recursive variant types
  type intlist = Nil | Cons of int * intlist

  intlist lst3 = Cons(3, Nil) /* [3] */
  intlist lst123 = Cons(1, Cons(2, lst3)) /* [1 [2 [3]]] */

  int sum (intlist l) {
      match (l):
        | Nil -> 0
        | Cons (h, t) -> h + sum t
  }


  int length (intlist l) {
      match (l):
        | Nil -> 0
        | Cons (_, t) -> 1 + length t
  }


  bool empty(intlist l) {
      match (l):
        | Nil -> true
        | Cons _ -> false
  }

  // Parameterized Variants (early binding variants)
  type mylist <a> = Nil | Cons of a * mylist <a>
  type option <a> = None | Some of a

  type val <a> = Type of a
  val<int> int_val = 9;
  val<double> double_val = 3.14;


  // multiple type parameters
  // if both aren't used, error
  type pair <a, b> = first of a * second of b

  mylist<int> lst3 = Cons (3, Nil)          /* similar to [3] */
  mylist<char *> lst_hi = Cons ("hi", Nil)  /* similar to ["hi"] */

  int length (mylist<a> l) {
      match (l):
        | Nil -> 0
        | Cons (_, t) -> 1 + length t
  }

  // compiler will check if 'a is a number type because of the `h + sum t`
  int sum (mylist<a> l) {
      match (l):
        | Nil -> 0
        | Cons (h, t) -> h + sum t
  }

  // sum types are typed-unions so 2 variants can have the same constant or non-constant
  type new_variant <a, b> = None | Pair of pair<a, b>
  type new_variant2 = None | Pair of pair<int, double>

  typedef struct S S;
  // no template structs
  struct S {
      const char *string;
      new_variant<int, double> p1;
      new_variant2 p2;
  };

  S s = {.string = "asdasd", .p1 = new_variant(1, 2.3), .p2 = None}
  match (s.p1):
    | Pair (f, s) -> ...
    | None -> ...
```

## Binding

```c
  int c =
      int x = random() in
      switch(x) {
          case (x % 2 == 0): x / 2;
          case (x % 2 == 1): x / 3;
          default: x;
      }

  printf("%d\n", c);
```

## Closures
```c
  for (int i = 0; i < 10; ++i) {
      int a = 0;
      int add(void)[i a]{
          a += 1;
          i += 2;
          printf("a: %d i: %d\n", a, i);
      }

      add();
  }
```

## Various function programming techniques (currying, point free, pipeline operator, partial application)
```c
  type pair <a, b> = first of a | second of b
  pair<int, int> *p = (sizeof(pair<int, int>) * 10) |> malloc

  for (int i = 0; i < 10; ++i) {
      match (p[i]):
        // can't use point free syntax if regular function call
        | first (x) -> printf("first: %d\n", x);
        | second (x) -> printf("second: %d\n", x);
  }

  int arr[10];
  // '.' placeholder for arguments
  // if no . symbol, pipelined argument(s) goes at the end
  fun add = . + .

  // [a b] special syntax to pipeline multiple things at once
  // can add :KEYS which are basically one-word comments
  int sum = [arr :size (sizeof(int) * 10)] |> memset . 0 . |> foldl add 0

  // currying
  fun memset_curry = memset . 0 (sizeof(int) * 10)
  int sum = arr |> memset_curry |> foldl 0

  length arr |> printf "length: %d"
```
