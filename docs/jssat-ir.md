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
  `%.0`). Full identifiers are identifiers with both a name and an anonymous ID,
  and are emitted by JSSAT (e.g. `%name.3`).

  Functions and globals are prefixed with `@`. Anonymous functions are prefixed
  with `@.`, followed by numbers (e.g. `@.0`). The distinction between `%` and
  `@` exists to differentiate top level identifiers with local identifiers. Full
  versions of thesse combine the two elements (e.g. `@main.0`).

- **Code layout**

  All code is only present in JSSAT IR functions. A JSSAT IR program is composed
  of functions. Execution begins in `@main`.

- **Function definition**

  JSSAT IR functions have a fixed number of formal parameters.

- **Records**

  Records are mappings of keys to values. They serve as the foundation for
  objects, arrays, symbols, and other record-like values. A register that holds
  a record holds a pointer to a hashmap.

- **Debug information**

  Any statement and function may be accompanied by debug information, to assist
  the reader in determining any reasoning behind what the compiler may do. Debug
  information is written preceding the element (function or statement) with
  triple hyphens `---`s.

- **References**

  JSSAT IR has the notion of pointers and references in order to reduce the
  amount of allocation and copying that occurs. These references are on the
  stack, and similar to LLVM's `alloca`. Allocating memory on the stack is done
  with the `newptr` operation, and allocates enough to store a record. Passing
  around the register will copy the pointer into the methods that use the
  pointer. It behaves like a normal record.

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

## Regular Instructions

Regular instructions are ones that cannot be defined in terms of another
instruction. These are handled by JSSAT. Following is a list of instructions
with example usage:

- **`make_primitive`**

  ```
  %record = make_primitive record
  %list = make_primitive list
  %char = make_primitive '\x65\x00'
  %undef = make_primitive undefined
  %null = make_primitive null
  %boolean = make_primitive true
  %number = make_primitive 6.9
  %bigint = make_primitive 123456789012345678901234567890n
  ```

  The `make_primitive` instruction creates one of the core runtime types,
  described below:

  | Type      | Behavior                                                |
  | :-------- | ------------------------------------------------------- |
  | Record    | A key value lookup for arbitrary keys and values        |
  | List      | Pointer to a resizable collection of arbitrary elements |
  | Char      | A UTF-16 character, which takes up two bytes.           |
  | Undefined | The ECMAScript `undefined` type                         |
  | Null      | The ECMAScript `null` type                              |
  | Boolean   | The ECMAScript `boolean` type                           |
  | Number    | The ECMAScript `number` type                            |
  | BigInt    | The ECMAScript `bigint` type                            |

  The record and list types are unique to JSSAT, and are used to implement the
  following ECMAScript types:

  | ECMAScript Type | JSSAT IR Type                        |
  | :-------------- | :----------------------------------- |
  | Object          | Record                               |
  | String          | Record, with inner List of `char`s   |
  | Symbol          | Record, with inner Number identifier |

- **`is_type`**

  ```
  fn @is_record(%v) {
    %is_rec = is_type record %v
    ret %is_rec
  }
  ```

  The `is_type` instruction determines if the type of a register is of the type
  specified. The `is_type` instruction only on JSSAT IR types:

  - `record`
  - `list`
  - `char`
  - `undefined`
  - `null`
  - `boolean`
  - `number`
  - `bigint`
  - `ref`

- **`rec_get`**

  ```
  %value = make_primitive "hi"
  %object = make_primitive record
  record_set %object, [[Value]], %value
  %get_value = record_get %object, [[Value]]
  ```

  The `rec_get` instruction will attempt to get the value at the key specified.
  Internal slots are valid keys, just like in the `rec_set` instruction. The
  `rec_get` instruction returns a reference, so the reference must be
  dereferenced via `ref_deref` to use the value. If there was no entry, the
  reference will be empty, and can be checked for with `ref_isempty`. It is
  intended for `rec_get` to be used as the building block for various ECMAscript
  abstract operations, and to not be used directly.

- **`rec_set`**

  ```
  %key = make_primitive "name"
  %value = make_primitive "john"
  %person = make_primitive record
  rec_set %person, %key, %john
  ```

  The `rec_set` instruction will, given the a key, insert the value into the
  record at the specified key. If a value already exists, it will be
  overwritten. If no value exists, the value specified will be inserted. a value
  into the record specified. Valid values for keys include internal slots, which
  can be referenced by using double bracket notation, just like the ECMA262
  notation.

  ```
  %number = make_primitive 6.9
  %record = make_primitive record
  rec_set %record, [[NumberData]], %number
  ```

- **`rec_alloca`**

  ```
  %ptr = rec_alloca
  ```

  The `rec_alloca` instruction will allocate a record on the stack. Any
  references to a record on the stack after the method returns is UB. The
  `rec_alloca` instruction is used to reduce allocations and GC pressure.

- **`list_insert`**

  ```
  %list = make_primitive list
  %fav_number = make_primitive 6.9
  %idx = make_primitive 0
  list_insert %list, %idx, %fav_number
  ```

  The `list_insert` instruction will insert a single element into a list. If the
  index is not a valid integer, this is UB.

- **`list_get`**

  ```
  fn @first(%list) {
    %idx = make_primitive 0
    %element.0 = list_get %list, %idx
    -- WARNING: may cause UB
    %element.1 = ref_deref %element.0
    ret %element.1
  }
  ```

  The `list_get` instruction returns a single element of a list. If the index is
  not a valid integer, this is UB. This instruction returns a reference to the
  element in the list, which will be empty if no item in the list exists at that
  index.

- **`list_len`**

  ```
  fn @length(%list) {
    %len = list_len %list
    ret %len
  }
  ```

  The `list_len` instruction returns the length of the list specified.

- **`ref_deref`**

  ```
  fn @pull_description(%record) {
    %name_ref = rec_get %record, [[Description]]
    -- WARNING: may cause UB
    %name = ref_deref %name_ref
    ret %name
  }
  ```

  The `ref_deref` instruction will dereference a reference, to get the value
  located at the reference. Dereferencing an empty reference is UB. The above
  code example will invoke UB if `%record` does not have a value in the
  `[[Description]]` internal slot.

- **`ref_isempty`**

  ```
  fn @has_description(%record) {
    %name_ref = rec_get %record, [[Description]]
    %has_name = ref_isempty %name_ref
    ret %has_name
  }
  ```

  The `ref_isempty` instruction returns a boolean, which determines if a
  reference is valid and points to something. If the return value is `true`, the
  reference is safe to dereference. If the return value is `false`, the
  reference is null and it is UB to dereference the reference.

## Control Flow

JSSAT IR control flow is extremely similar to LLVM IR control flow. It is
handled by grouping instructions into blocks, with a label to name the block.
Every block must end with a control flow instruction, listed below. `call` is
not considered a control flow instruction, as control flow ends up at the same
place.

| Instruction | Description                                                   |
| :---------- | ------------------------------------------------------------- |
| `ret`       | Exits the current function, may return a value to the caller. |
| `jmp`       | Jumps to a label, can perform conditional jumps.              |
| `phi`       | Conditionally selects a value in a register.                  |

- **`ret`**

  Exits the function immediately.

  ```jssat
  fn @hi() {
    ret
  }
  ```

  If given a value, it will return that value to the caller.

  ```jssat
  fn @hi() {
    %hello = make_primitive 6.9
    ret %hello
  }
  ```

- **`jmp`**

  Jumps to a block unconditionally.

  ```jssat
  fn @loop() {
  %block.0:
    jmp %block.1
  %block.1:
    jmp %block.0
  }
  ```

  Conditionally jumps to a block

  ```jssat
  fn @maybe_loop(%do_loop) {
    %block.0:
      jmp %block.1
    %block.1:
      jmp %block.0, %do_loop
      ret
  }
  ```

- **`phi`**

  Selects a value for a register depending on the path of execution. The
  following example will conditionally select `2` or `3` depending on if
  `%use_two` is true or false.

  ```jssat
  fn @get_num(%use_two) {
  %.0:
    jmp %two, %use_two
  %three:
    %num.0 = make_primitive 3
    jmp %end
  %two:
    %num.1 = make_primitive 2
    jmp %end
  %end:
    %num.2 = phi %three -> %num.0, %two -> %num.1
    ret %num.2
  }
  ```

  It is far more common to see `rec_alloca` used over phi, as LLVM will optimize
  the stack allocation into registers with `phi` in the majority of cases.

## ECMAScript Abstract Operations

In ECMAScript, there are many specification defined abstract operations. In
JSSAT IR, these instructions can be notated via a `$` prefix to the instruction.
For example, the following JSSAT IR invokes [`ToBoolean`][to-bool] with the
parameter `%x`.

```jssat
%result = $ToBoolean %x
```

All ECMAScript abstract operations are capable of being lowered into equivalent
JSSAT IR that does not make use of these abstract operations. Core ECMAScript
operations, such as [`Type(x)`][typex] will be implemented as a JSSAT IR
instruction.

[typex]: https://tc39.es/ecma262/#sec-ecmascript-data-types-and-values
