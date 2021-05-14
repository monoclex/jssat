# Optimizations

There are three kinds of optimizations present in JSSAT:

- **Base optimizations**

  Base optimizations are applied regardless of code backend, as the optimization
  is useful regardless of backend. [Dead code elimination][opt-dce], the
  elimination of code paths that never get hit, is a good example of such an
  optimization.

- **Native optimizations**

  Native optimizations refer to optimizations that occur when targetting the
  native backend. An example of such an optimization would be
  [specialization][opt-specialization], the duplication of code in cases where a
  more exact type is found for a parameter. This is more commonly known as as
  "templates" or "generics" in some programming languages.

- **Bundle optimizations**

  Bundle optimizations refer to optimizations that occur when targetting a
  Javascript bundle. An example of such an optimization would be
  [mangling][opt-mangling], where long property names are not desirable as it
  increases code size, as well as also making it slightly easier to reverse
  engineer code.

[opt-dce]: ./dead-code-elimination.md
[opt-specialization]: ./specialization.md
[opt-mangling]: ./mangling.md
