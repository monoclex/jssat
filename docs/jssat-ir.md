# JSSAT IR

JSSAT IR, the JavaScript Static Analysis Tool Intermediate Representation, aims
to model Javascript in an intermediate form that is much easier to statically
analyze, as well as optimize. It is inspired by LLVM IR in most respects.
Following is a list of design goals which JSSAT IR needed to meet in order to be
suitable for usage:

- [Easy to iterate over and optimize](#optimizability)
- [Suitable target for ECMAScript](#ecmascript-compliance); must be easy
  translate ECMAScript constructs into JSSAT IR.

And what follows these goals, are the [design decisions](#design) to meet them.

## Optimizability

To enable JSSAT IR to be easy to optimize, we borrow concepts from LLVM IR.
Among these concepts are:

- SSA form for all scalar register values
- Infinite registers
- Phi instructions for control flow

## ECMAScript Compliance

One goal for JSSAT IR is to be easily targettable from ECMAScript. Thus, it must
be simple to port ECMAScript instruction calls to JSSAT IR. Thus, the JSSAT IR
instruction set primarily consists of ECMAScript functions.

# Design

JSSAT IR can be thought of as typeless, similar to Javascript. However, type
information is inferred from the code and annotated onto the relevant functions.

## Overview:

- **Identifiers**

  Identifiers in JSSAT IR begin with `%` (e.g. `%register`). This is to
  differentiate identifiers from instructions. Anonymous identifiers are
  automatically generated, being with `%.`, and are followed by numbers (e.g.
  `%.0`).

  Functions and globals are prefixed with `@`. Anonymous functions are prefixed
  with `@.`, followed by numbers (e.g. `@.0`). The distinction between `%` and
  `@` exists to differentiate top level identifiers with local identifiers.

- **Code layout**

  All code is only present in JSSAT IR functions. A JSSAT IR program is composed
  of functions. Execution begins in `@main`.

- **Function definition**

  JSSAT IR functions have a fixed number of formal parameters.

- **Maps**

  Maps are the terms for objects with dynamic key value lookup behavior. Maps
  are only used to store user-facing values, and are capable of storing an
  unknown amount of properties. Maps may often be optimized to records when
  applicable. Maps are often stored inside of records, as they only serve to
  store user properties.

- **Records**

  Records are mappings of keys to values, but have only a fixed amount of keys.
  They are often used to record engine-known information (such as the prototype
  object) and store Maps inside for user values.

- **GC promotion**

  Any value in a register can be promoted to the GC. This means that the
  lifetime of the value will be exclusively handled by the garbage collector.
  Copying around a GC promoted value is equivalent to copying a pointer.

- **Debug information**

  Any statement and function may be accompanied by debug information, to assist
  the reader in determining any reasoning behind what the compiler may do. Debug
  information is written preceding the element (function or statement) with
  triple hyphens `---`s.

- **References**

  JSSAT IR has the notion of pointers and references in order to reduce the
  amount of allocation and copying that occurs. These references are on the
  stack, and similar to LLVM's `alloca`. Allocating memory on the stack is done
  with the `newptr` operation. No size is needed as that will be computed based
  on usage. Passing around the register will copy the pointer into the methods
  that use the pointer. The `getptr` and `setptr` instructions will load the
  value stored in the pointer into a register, and set the value of a register
  into the pointer.

## Relevance to Javascript

JSSAT IR is designed to be a compromise between ease of translation from
Javascript into JSSAT IR, and optimizability. The following section details
compromises made in order to enable translatability.

- **`arguments` object**

  In Javascript, it is known that any function can have be treated as if it has
  any amount of arguments. To compensate for this behavior, every function call
  has one formal parameter for accepting Javascript arguments referred to as
  `%arguments`. It is expected to be an [arguments exotic object][args-exotic].
  The body of the function is expected to pick out values from the function if
  it desires to use them. Optimization passes may optimize this overhead out.

  [args-exotic]: https://tc39.es/ecma262/#sec-arguments-exotic-objects

- **Execution Contexts**

  In Javascript, an Execution Context is what records the binding from variables
  to values. These are handled with the usage of records, with embedded maps if
  necessary for dynamic allocation of variables. Optimizations may extract
  elements of the record out into registers if possible, as well as possibly
  eliminating them if possible.

- **`new.target`**

  This property is accomodated for within the execution context passed to the
  function.

- **Completion Records**

  Completion records are in extensive use throughout the ECMAScript
  specification, and are also used in JSSAT IR to model control flow.

# Instructions

## CreateMappedArgumentsObject
