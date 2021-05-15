# Type System

The type system is what allows [JSSAT IR][jssat-ir] optimizations to be very
effective. It is designed with the intent to model Javascript code in such a way
that high level optimizations are made trivial.

[jssat-ir]: ../jssat-ir.md

# Specialization

In Javascript, the `+` operator can have different meanings, such as
concatenation versus addition. For example, looking at `add` alone, we have no
idea if we should perform concatenation or numeric addition.

```js
function add(a, b) {
  return a + b;
}

add("a", "b"); // "ab"
add(1, 2); // 3
```

In JSSAT IR, this is trivial to model: we simply give placeholder types to `a`
and `b` (named `A` and `B` below), and will expect that when the function is
called we will know the types of these parameters.

# Exact Types

In Typescript, we can represent exactly the string `"asdf"` with the type
constant `"asdf"`. This is in stark contrast to lower level languages, where the
only way to represent a string is with the datatype `string` - a datatype that
can represent every single string possible. This is a problem, as code that does
not make use of every possible string must accomodate all strings, despite maybe
not making use of all strings. Following are primary examples of why exact types
are useful.

## Advanced return types

Often, functions perform operations on data. Consider the following:

```js
function add(a, b) {
  return a + b;
}
```

In strongly typed programming languages, we can implement this using generics.
We assume `a` and `b` share the ability to be added to one another, and infer
the return type from the result type of that ability. In Javascript, we do not
have the luxury of asking the users to annotate their functions with generics,
nor can we expect `a` and `b` to be even remotely similar. However, we still
want to be able to know what `add` will return for any `a` and `b`.

Thus, exact return types allow us to say that the return type of `add` in the
most comically trivial fashion: `a + b`. In JSSAT IR, this would be:

```jssat
fn @add(%a, %b) {
    --- %a: A
    --- %b: B
    %result = EvaluateStringOrNumericBinaryExpression %a, "+", %b
    --- %result: EvaluateStringOrNumericBinaryExpression %a, "+", %b
    ret %result
    --- -> %result
}
```

In the following function, we assert that the return type is `%result`. The type
of `%result` is the resultant type of calling the expression
`EvaluateStringOrNumericBinaryExpression %a, "+", %b`.

## Set optimizations

For example, consider the following function that switches based on a string
key:

```js
switch (key) {
    case "FunctionDeclaration": ... break;
    case "Expression": ... break;
    default: break;
}
```

If we were to to use string comparisons as specified, this would end up
performing a large amount of work. Possible optimizations would be to use a
constant pool of strings, but this is a large amount of overhead.

With exact types, we can do significantly better. If we know that the value of
`key` is never observed, and that it is limited to only being the values that
can match the switch statement, we can assume that key must be in the set
`{ "FunctionDeclaration", "Expression" }`. This is a set of two elements. We can
thus optimize the expression into the numbers `0 | 1`. This has the effect of
being far more CPU-friendly and performant.

## Exact Maps

In Javascript, often objects will usually be extremely similar, and not require
the highly dynamic nature of an object. For example:

```js
function makePoint(x, y) {
  return { x: x, y: y };
}

let origin = makePoint(0, 0);
let up = makePoint(0, 1);
```

In this example, we know with certainty that `origin` and `up` will _never_ have
a property other than `x` and `y` on them. Thus, it would be good if we could
explicitly annotate this. Explicit maps allow us to model that the map supports
lookup from `x -> Integer` and `y -> Integer`, allowing us to model the object
as a `Record { x -> Integer, y -> Integer }`. This will allow us to pack what
use to be a hashmap into just 8 bytes, with constant lookup for the properties.

# Dependent types

JSSAT makes heavy use of dependent types to assist in narrowing down the list of
possible paths to take. For example, it is not uncommon to see polymorphic
functions, as Javascript does not have generics.

```js
function unwrap(x) {
  if (x.type === "Some") return x.value;
  throw new Error("No value in `x`!");
}
```

At first glance, this may not seem like a polymorphic function, but it certainly
is. We can say that if `x` is not like a `Record { type -> String("Some") }`,
then it will always throw an Error. Otherwise, it will return `x.value`. In
pseudo-Typescript, we could annotate the function as follows:

```ts
declare function unwrap<X>(x: X & { type: "Some" }): X["value"];
declare function unwrap<X>(x: X): never;
```

The first case is if `x` is like a `{ type: "Some" }`. If that is true, then the
return type of the function will be `x.value`. Otherwise, we will return an
error.

# Turing complete types

Tangentially related to exact types, the turing completeness of the type system
becomes apparent when more complicated things get involved. The primary reason
for desiring turing completeness is so that type information encoded in

In Javascript, we can effortlessly write code that may be difficult to wrap ones
head around in a strongly typed language.

```js
function search(x) {
  while (x.a) {
    x = x.a;
  }
  return x;
}
```

Here, we know that we perform `x.a` some number of times until the condition
`x.a` is falsy. Modelling _exactly_ this function in Typescript is impossible,
without cutting corners, yet possible in JSSAT IR.

To demonstrate JSSAT IR's recursive types, let's start by attempting to unwrap
the function one layer at a time:

```ts
function search_one<X: { a?: A }, A>(x: X): if `x.a` then A else X {
  if (x.a) {
    return x.a;
  }
  return x;
}

function search_two<X: { a?: A }, A: { a?: A2 }, A2>(x: X):
    if `x.a` then
        if `x.a.a` then
            A2
        else
            A
    else
        X
{
  if (x.a) {
      if (x.a.a) {
          return x.a.a
      }
      return x.a;
  }
  return x;
}
```

An obvious pattern is beginning to emerge. JSSAT IR allows us to express this
exact recursiveness using recursive types

# Context-aware types

In Javascript, we may reuse variables for completely unrelated purposes.

```js
function deep(x) {
  let a = x.a;
  a = compute(a);
  return a;
}
```

In traditional languages, we would have to model the type of `a` as
`x.a | compute(x.a)`, and if say `x : { a |-> Int }, compute : a -> String`,
then the type of `a` would be `Int | String`, requiring that we construct a
discriminated union when storing or using `a`. This would be inefficient, so we
use _context aware types_ to model this. Context aware typing means that after
every instruction or expression, we construct a new view on the types based on
the previous view of the types.

```js
function deep(x) {
  // x : X
  let a = x.a;
  // x : X
  // a : X.a
  a = compute(a);
  // x : X
  // a : X.a
  // a' : compute(a)
  return a;
  // deep(x) -> a'
}
```

This knowledge can assist the optimization process into generating code that
shadows `a` rather than overwrites it.
