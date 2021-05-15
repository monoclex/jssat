# JSSAT Documentation

JSSAT is a **J**ava**S**cript **S**tatic **A**nalysis **T**ool. Given some
Javascript code, it will determine the precise types of every variable and
function signature with respect to usage and lifetime.

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Benefits](#benefits)
  - [Make Javascript code faster](#make-javascript-code-faster)
  - [Aggressive, exactly correct minification](#aggressive-exactly-correct-minification)
  - [Detect runtime errors at compiletime](#detect-runtime-errors-at-compiletime)
  - [Generate exact types for functions](#generate-exact-types-for-functions)
- [Drawbacks](#drawbacks)
  - [Must be a script](#must-be-a-script)
- [More reading](#more-reading)

## Benefits

JSSAT can be beneficial in the following areas:

### Make Javascript code faster

Current technologies that embed Javascript must embed a Javascript runtime face
drastic benefits and drawbacks:

- **High memory usage**

  High performance Javascript engines must learn about your code in order to
  generate code that accomodates towards it. An example of such optimization
  would be [V8's Shapes and Inline Caches][v8-shapes]. This is necessary to
  achieve just decent performance, but comes at a high price to pay. In
  addition, Javascript has lots of runtime knowledge tha could be known entirely
  at compile time and simply prevented - for example, readonly properties.
  Overall, an interpreted runtime has a lot of cost associated to it.

  With JSSAT, its type analysis can perform similar optimizations at compile
  time, avoiding the runtime costs associated with dynamic optimization.

- **Large binary size**

  At the time of writing, JavaScriptCore `libjavascriptcoregtk-4.1.so.0` is 24M.
  V8 9.2.1 is 22M. A high performance Javascript engine comes with a high base
  cost. This means higher binary sizes for Electron-based applications, such as
  Discord, Spotify, Visual Studio Code, and more.

  With JSSAT, the Javascript code can be transformed at compile time into
  assembly, removing the need for a bulky runtime.

- **Slow performance**

  High performance Javascript engines can get decently fast, after a period of
  running the code and re-optimizing it. However, there is still large startup
  times incurred due to the need to parse, verify, and compile the code at
  runtime.

  With JSSAT, all of this is performed at compile time, eliminating the need to
  parse, verify, and compile the code at runtime.

JSSAT solves all of these issues by using the rich type information to produce
code that performs the identical task, but more efficiently. The speicfic ways
it achieves this is outlined in the [optimization][optimization] section of the
documentation.

[v8-shapes]: https://mathiasbynens.be/notes/shapes-ics
[optimizations]: ./optimization

### Aggressive, exactly correct minification

Current minification technology has a difficult time refactoring the fields of
objects to change their name as it's hard to determine if an object's properties
should be mangled, or if doing so would break the application. JSSAT can
determine objects with constant properties and mangle these properties
correctly, without fail, every time.

### Detect runtime errors at compiletime

JSSAT uses its extremely rich type information to determine where errors may
occur in functions, and in what circumstances. This information propagates to
the top level of the program, and if not caught, will throw a compile time error
with detailed information about the specific circumstance that may cause that
behavior.

### Generate exact types for functions

JSSAT produces extremely rich type information about your code, and can use this
type information to generate Typescript type definitions for functions within
your code. The generated typings can be used for verification, or as starting
ground for hand-written typings.

## Drawbacks

Unfortunately, JSSAT is not the end all be all of JS problems.

### Must be a script

JSSAT only supports scripts. This is because JSSAT must have complete knowledge
over what pieces of code are used in order to perform accurate narrowing. In the
future, there may be partial support for modules but it is unlikely at this
time.

### Must be compiletime

## More reading

- [JSSAT Architecture](./architecture.md)
