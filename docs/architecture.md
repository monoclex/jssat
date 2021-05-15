# JSSAT Architecture

The JSSAT Architecture describes how JSSAT is built to handle an input of
Javascript code, and output various different types of code.

## Overview

The JSSAT Architecture can be organized into three distinct pieces. At the heart
of JSSAT is the [JSSAT type system][type-system]. The JSSAT type system allows
the optimization phases to optimize as well as they do, and cannot exist without
[JSSAT IR][jssat-ir] being able to assist in cleanly modelling the types.

1.  **Javascript frontend**

    The frontend takes input Javascript, validates the Javascript, and produces
    JSSAT IR. It does this in a few steps:

    1.  **Parsing into an AST**

        Parsing of Javascript into a valid AST is handled by [swc][swc]. Any
        parse errors are reported as compilation errors.

    2.  **Traversing the AST**

        Next, the AST is transformed into JSSAT IR, by walking over the AST and
        emitting the corresponding instructions for each Javascript expression
        and statement. Type information is not introduced at this stage.

        For detailed information about how the AST is traversed and what JSSAT
        IR is emitted, see [AST traversal][ast-traversal] for more information.

    3.  **Annotation of types**

        Finally, the JSSAT IR is fully annotated with rich type information.
        This is a separate, very involved process, which is responsible for a
        decent amount of the time spent in JSSAT.

2.  **Backend**

    The backend takes JSSAT IR, and produces the desired output. The primary
    reason backends are able to optimize is due to the rich type information
    present in the JSSAT IR, which make relationships between objects and types
    very apparent. There are three distinct backends:

    - **Type definition backend**

      Produces Typescript `.d.ts` typings.

    - **Native backend**

      Emits native code using LLVM.

    - **Bundle backend**

      Optimizes the code to the minimum size possible.

    All backends utilize a set of optimizations in order to achieve their goal,
    which is covered in detail in the [optimization][optimization] section of
    the documentation.

[type-system]: ./type-system
[jssat-ir]: ./jssat-ir.md
[swc]: https://github.com/swc-project/swc
[ast-traversal]: ./ast-traversal.md
[optimization]: ./optimization
