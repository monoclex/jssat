# Contributing

Thanks for being interested in contributing! To get started, see the
[developing](#developing) section. Be sure to follow the
[coding guidelines](#guidelines) when submitting code. Below are a list of the
areas which you can consider contributing to, as well as a few tasks to help
illustrate what needs to be done:

## Frontend

The frontend is the part of the compiler that turns JavaScript into JSSAT IR.
JavaScript is parsed by SWC and JSSAT IR is emitted. It also encompasses the
user-facing part of the compiler, such as options or reading from source code.

### Flags:

```toml
# If you're working on JS -> JSSAT IR
defaults = ["link-swc"]
# If you're working on the user facing parts
defaults = []
```

### Tasks:

- **Parse more JavaScript**

  Translate the [ECMAScript Specification](https://tc39.es/ecma262/), and emit
  JSSAT IR that follows it.

- **Report untranslatable behavior**

  The JSSAT IR Builder API may not have the concept or notation of certain
  things found in the ECMAScript Specification. Reporting these as issues will
  bring awareness to what needs to be supported.

- **Nicer to use interface**

  Right now a hardcoded string is used with the JavaScript to compile. It'd be
  nice to change this.

## DevOps

Having the compiler be tested and binaries being distributed on every commit is
beneficial for ensuring that the code and PRs work, as well as letting people
try out the compiler.

### Tasks:

- **Setup CI/CD**

  There is none at the moment, so some would be nice.

- **Build on Windows/Mac**

  At the moment, the compiler is only geared for linux as I primarily develop on
  linux. If CI/CD could build and test the compiler on windows and mac, that
  would be excellent for ensuring that the compiler is cross platform.

- **Run Test262**

  Running the Official Test262 ECMAScript Conformance Test Suite as part of the
  testing phase would help ensure that there would be no regressions for every
  commit.

## Runtime

The runtime is work related to the library that gets shipped with every
JSSAT-compiled program.

### Tasks:

- **Garbage Collection**

  There is no GC at the moment. All GC state can be tied to the JSSAT `Runtime`
  struct. It needs to be thought out how the code generation from the backend
  will talk to the runtime.

- **ECMAScript Operations**

  The ECMAScript Specification specifies some operations without a rigid
  algorithm, such as [substring](https://tc39.es/ecma262/#substring),
  [floor](https://tc39.es/ecma262/#eqn-floor), and potentially various others.
  Implementation of these must be done at the runtime level.

- **Objects**

  For objects with dynamic keys, the runtime should provide `HashMap`-based APIs
  to allow interaction with these objects. This would then be used by things
  such as [`JSON.parse()`](https://tc39.es/ecma262/#sec-json.parse) to create
  dynamic objects from user input.

## Backend

The backend encompasses all of the compiler that turns JSSAT IR into LLVM IR.
There are numerous tasks to be done in this area. In order to work in this area,
being able to build and compile with LLVM is useful for testing your changes.

### Flags:

```toml
# To test the symbolic execution engine & annotater
defaults = ["link-swc"]
# If you're working on LLVM generation & to make sure evreything works:tm:
defaults = ["link-swc", "link-llvm"]
```

### Tasks:

- Generalize function arguments after numerous invocations
- Perform least fixed point after running into conditionally recursive code
- Add union types to support multiple return values
- Build struct types from objects that support recursion of itself
- Hooking into the runtime for garbage collection of objects
- Get objects with dynamic non-const keys working
- Perform escape analysis to determine objects that can be on the stack vs GC'd
- ...probably a lot more i'll have to do :')

# Developing

JSSAT requires a special runtime library to be included in the binary of the
compiler. This is used when compiling JSSAT IR into LLVM IR. In order for the
compiler to build, the runtime library must be present. You must build the
runtime library before the compiler. The cargo makefile will do this
automatically.

Here are the commands to build the project and runtime automatically:

**Debug**

```shell
cargo make dev
```

**Release**

```shell
cargo make release
```

# Guidelines

Below are a small set of (expanding) guidelines I've found useful.

- **Assert invariants when feasible.**

  Often there will be invariants in the code. Using `debug_assert!()` to assert
  that those invariants are met is helpful for debugging.

- **When zipping two iterators, assert that the length of both iterators are
  equal.**

  I've zipped two iterators of different lengths and had to dive in and figure
  out why a function was missing arguments.

- **When inserting elements into a `HashMap` or similar, assert that there are
  no existing elements.**

  Often, insertions into collections will be made upon the assumption that the
  element does not already exist in the collection. Using the `.expect_none()`
  method helps make this assertion easy to do.
