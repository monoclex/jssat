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

- **Data declarations**

  Data declarations are similar to structs in Rust or C. Records may be
  optimized into data declarations if the JSSAT IR optimization process finds it
  capable of being done.

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

- **Types**

  JSSAT IR types are not well defined. This is to allow the optimizer lots of
  free range in what it thinks is possible.

- **Garbage Collection Strategies**

  JavaScript code often has lots of objects and arrays in use for various
  purposes. Often, an allocation per object creates lots of pressure in
  situations where objects are just used to group together pieces of data. This
  is combatted in JavaScript engines by having a young nursery generation. JSSAT
  IR aims to optimize away this behavior, and has several strategies for garbage
  collection. Instantiation of a list, record, or data definition requires the
  GC strategy to be specified.

## Loosely Typed Nature

JSSAT IR is loosely typed. This makes emitting JSSAT IR extremely easy. One of
the downsides of this is that a very complicated type system must exist to
handle assigning a valid type to each register, but this would be a necessity
regardless in order to perform optimizations such as mangling of record
properties.

If an instruction requires a specific type of argument but an invalid type is
passed, it is considered UB. The compiler is not obligated to do anything, but
it may warn the user of such. Invalid optimizations that malform the input into
UB are considered wrong, and should be reported. All emitted code should be in
the form of an ECMAScript Abstract Operation, and thus migitate the potential
room for error emitting JSSAT IR instructions.

## GC Strategies

Instructions such as `make_primitive` include garbage collection strategies to
potentially reduce the amount of garbage generated in special cases. Following
are a list of each garbage collection strategy, and their proper usage.

- **Tracing**

  ```jssat
  -- `%rec` is already marked as root upon creation
  %rec = make_primitive.record.tracing
  -- this instruction is not needed, but useful for other scenarios
  gc_tracing_markroot %rec
  -- usage of the object
  gc_tracing_unmarkroot %rec
  ```

  The `tracing` strategy refers to usage of a traditional, bulky, tracing
  garbage collector. The `gc_tracing_markroot` and `gc_tracing_unmarkroot`
  instructions are used to mark when a reference is considered reachable as a
  root. Any objects unreachable via roots are subject to garbage collection at
  an unspecified time.

- **Reference Counting**

  ```jssat
  %rec = make_primitive.record.refcount
  -- cloning and incrementing the reference
  %rec.1 = gc_refcount_clone %rec
  -- usage of the object
  gc_refcount_end %rec
  ```

  The `refcount` strategy uses reference counting to determine the liveness of
  an object. Every time the reference is copied via `gc_refcount_clone`, a
  number is incremented to signify the amount of live references to the object.
  Optimizations may remove these explicit clones if they are proved to become
  unnecessary.

  One thing to be concerned about is how objects inside of records behave. For
  these scenarios, when the parent object gets freed, it will cause all all
  fields to be freed as well, which will in turn free any inner ref counted
  items. Any cycles with ref counting are not considered for a reference
  counting GC.

- **Region**

  ```jssat
  %region = gc_region_make
  %rec = make_primitive.record.region %region
  -- usage of the object
  gc_region_end %region
  ```

  The `region` stragey allocates an arbitrary amount of memory to use for the
  creation of objects. It grows as much as necessary. Upon the `end_region`
  instruction, all the memory claimed by the region will be deallocated. This
  has the effect of making allocations and cleanup extremely cheap but
  imprecise. It does not incur the costs of root tracing like a tracing garbage
  collector, and does not have the runtime costs of reference counting.

A drawback of having multiple GC strategies is that functions need to
monomorphised with the right GC strategy. This harms use cases where functions
are stored inside of objects and passed around, as every function that utilizes
the function stored inside the object must be use the same GC strategy, which
will often end up being tracing GC, or in the optimal case, region GC for
cross-boundary objects.

Unfortunately, it is difficult to polymorphise all garbage collection strategies
into a single strategy that works. At this time, this remains an intentionally
unsolved problem until more time can be put into the idea. The current,
theorized solution, would be to wrap all GC objects. Then, we can utilize a
union between the desired garbage collector's type of object and the
polymorphised object type to determine which object belongs to which strategy.

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

# Structure

### Top Level

At the top level contains only global declarations, function declarations,
extern function declarations, and data definitions.

```jssat
-- global
global @global_name

-- function
fn @fn_name() {
  ret
}

-- function with args
fn @fn_with_args(%a, %b) {
  ret %a
}

-- extern function
extern fn @extern_fn(%a: Any);

-- data definition
data X {
  field_name: Number,
  generics: Reference<X>,
}
```

The most notably unique oddity of JSSAT IR is the complete lack of types. This
allows the compiler to give more defined types to registers as deemed fit.

In addition to function declarations with a body, external functions can be
declared. External functions must have their parameters annotated, and if there
is a type mismatch, it will be a compiletime error. The optimization process in
JSSAT IR will preserve as much as possible about each object, including
disallowing the renaming of field names in records.

```jssat
extern fn @external_fn(%args);
```

Data definitions are used when optimizing extremely simple records. All it
defines is the field names, and the types for that data. They are similar to
structs in Rust, except passed by reference by default.

### Function Body

Inside JSSAT IR functions are instructions that return a value, instructions
that do not return a value, and blocks.

Instructions that do not return a value include control flow operators such as
`jmp` and `ret`.

```jssat
fn @example(%a) {
  ret
}
```

Instructions that return a value include `ref_isempty`, and nearly every other
instruction.

```jssat
fn @is_empty_ref(%r) {
  %empty = ref_isempty %r
  ret %empty
}
```

Blocks are similar to the LLVM concept of blocks. Blocks are prefixed with an
identifier then colon, and all blocks must end with a control flow operation to
another block. All instructions are contained within blocks. There is an
implicit block at the beginning of all functions if not explicitly shown.

```jssat
fn @conditional(%r) {
  %empty = ref_isempty %r
  jmpif %empty -> %full, %end
%full:
  %t.0 = make_primitive.undefined
  ret %t.0
%end:
  %t.1 = make_primitive.null
  ret %t.1
}
```

### Instruction semantics

The notation for instructions is not designed rigorously to be parsed. It is
just devised as a tool of visualizing the output of what JSSAT sees. However,
this subsection will descibe the intent with the formatting of instructions.

Instructions vary in formatting from one another. Generally, the format for
instructions is the name of the instruction, followed by commas for each
register being passed in. An example is as follows:

```jssat
instruction_name %arguments, %passed, %in
```

Functions that behave generically, such as `make_primitive` and `assert_type`,
represent this generic behavior by appending a dot to denote the specific type
of the instruction, e.g. `make_primitive.undefined` vs `make_primitive.boolean`.

Instructions that typically have some fixed, non-user-supplied arguments do not
need commas. However, this may be revisited later.

User supplied arguments are separated with commas inbetween each value.

Instructions that wish to describe the intent of a block affecting a value do so
with the `->` notation, e.g.

```jssat
jmpif %register_is_true -> %jump_to_this_block, %else_jump_to_this_block

%v = phi %if_this_block_was_entered -> %take_this_value, %otherwise_if_this_block_was_entered -> %take_that_value
```

# Instructions

## Regular Instructions

Regular instructions are ones that cannot be defined in terms of another
instruction. These are handled by JSSAT. Following is a list of instructions
with example usage:

- **`make_primitive`**

  ```
  %record = make_primitive.record
  %list = make_primitive.list
  %data = make_primitive.data DataName
  %char = make_primitive.char '\x65\x00'
  %undef = make_primitive.undefined
  %null = make_primitive.null
  %boolean = make_primitive.bool true
  %number = make_primitive.number 6.9
  %bigint = make_primitive.bigint 123456789012345678901234567890
  ```

  The `make_primitive` instruction creates one of the core runtime types,
  described below:

  | Type      | Behavior                                                |
  | :-------- | ------------------------------------------------------- |
  | Record    | A key value lookup for arbitrary keys and values        |
  | List      | Pointer to a resizable collection of arbitrary elements |
  | Data      | Similar to structs in Rust.                             |
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

- **`assert_type`**

  ```jssat
  fn @ref_isempty(%ref) {
    assert_type.ref %ref
    %is_empty = ref_isempty = %ref
    ret %is_empty
  }
  ```

  The `assert_type` instruction will assert at compile time that a given
  register is of some specific type. The available set of types to assert is
  limited to allow the JSSAT IR optimization process to optimize as much as
  possible. At this time, the valid types that are assertable are:

  | Type      | Keyword  | Example                  |
  | :-------- | -------- | ------------------------ |
  | Reference | `ref`    | `assert_type.ref %.0`    |
  | Region    | `region` | `assert_type.region %.0` |

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

  ```jssat
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

  ```jssat
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

  ```jssat
  %number = make_primitive 6.9
  %record = make_primitive record
  rec_set %record, [[NumberData]], %number
  ```

- **`data_get`**

  ```jssat
  %data = make_primitive.data X
  %value = make_primtive.number 6.9
  data_set %data, [[field_name]], %value
  %get_value = data_get %data, [[field_name]]
  ```

  The `data_get` instruction will get the value at the key specified. If the key
  is not specified in the data definition, this is UB.

- **`data_set`**

  ```jssat
  %data = make_primitive.data X
  %value = make_primtive.number 6.9
  data_set %data, [[field_name]], %value
  ```

  This will set the value at the field to the data specified.

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
| `jmp`       | Jumps to a label unconditionally.                             |
| `jmpif`     | Conditionally jumps to a label.                               |
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

- **`jmpif`**

  Conditionally jumps to a block.

  ```jssat
  fn @maybe_loop(%do_loop) {
    %block.0:
      jmp %block.1
    %block.1:
      jmpif %do_loop -> %block.0, %block.2
    %block.2:
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

### Macro ECMAScript Abstract Operation

In ECMAScript, there are operations which introduce control flow, for example,
[`ReturnIfAbrupt`][ret-abr]. These instructions look like an instruction, but
have conditional logic inside of them. For example, the following function
appears linear, but is not.

```jssat
fn @a(%x) {
  $ReturnIfAbrupt %x
  %y = make_primitive.bool true
  %result = $NormalCompletion %y
  ret %result
}
```

The above code will produce a conditional jump, rather than a linear flow of
instructions.

[ret-abr]:
