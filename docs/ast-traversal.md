# AST Traversal

JSSAT IR is generated when walking over a Javascript AST. There is no type
information emitted, all type information is inferred in a separate stage.

## Code Setup

Before traversing the AST, there is a base amount of JSSAT IR inserted into
`@main` to prepare the code to be executed. Unused code will get optimized away
in a later process if necessary. The following abstract ECMAScript operations
are performed in `@main`, for all programs:

1. **Initialization of the `@surrounding_agent` global**

   The `@surrounding_agent` global is a record with the following fields:

   - `execution_context_stack`, a stack that contains the execution contexts.
     The top of this stack is referred to as the running execution context.

   The global is defined at this time.

2. [**InitializeHostDefinedRealm()**][init-host-realm]

   The JSSAT IR host does not require the use of an exotic object to serve as
   the realm's global object (step 7), nor does it require that the `this`
   binding in the realm's global scope return an object other than the global
   object (step 8).

   Step 11 allows for host-defined global object properties to be injected.
   JSSAT behaves as such:

   - If `node` or `browser` compatibility is enabled, the global object will
     include JSSAT-defined globals for those platforms.

   - If no compatibility is desired, only a `print` function will be added to
     the global object.

   The realm returned by `InitializeHostDefinedRealm` will be passed in as the
   parameter `realm` in the following step.

3. [**ParseScript(sourceText, realm, hostDefined)**][parse-script]

   `sourceText` will be defined as the source code passed to JSSAT IR.
   `hostDefined`

   At the point of emitting JSSAT IR, we will already have confirmed that the
   syntax has no parse errors. Thus, we can simply return the script record.

4. [**ScriptEvaluation(scriptRecord)**][script-eval]

   Using the script record previously specified, the `Script` is now executed
   according to the [runtime semantics for Scripts][rt-sem].

[init-host-realm]: https://tc39.es/ecma262/#sec-createrealm
[parse-script]: https://tc39.es/ecma262/#sec-parse-script
[script-eval]: https://tc39.es/ecma262/#sec-runtime-semantics-scriptevaluation
[rt-sem]: https://tc39.es/ecma262/#sec-scripts

## Bodies

In ECMAScript, items with an inner `StatementList` will be put inside its own
JSSAT IR function. Debug information will be included to note the position in
the source code that the statement list begins and ends at. The specific bodies
that satisfy this are the following:

- Block
- FunctionBody
- ConciseBody
- GeneratorBody
- AsyncGeneratorBody
- AsyncFunctionBody
- AsyncConciseBody

## Generation

JSSAT IR generation occurs by traversing over the AST, beginning with `Script`.
Each element in the `Script` is recursively traversed over. Then, for every
instance of runtime semantics specified in the ECMAScript specification, the
relevant instructions are emitted at compile time to handle such.

### Body Generation

As mentioned in [bodies](#bodies), a JSSAT IR function is emitted per body. The
template of each body is as follows:

```jssat
fn @body_name(%environment_record) -> CompletionRecord;
```

Upon evaluation of a function object, the `[[Call]]` internal method will be set
to a reference to a function with the following signature:

```jssat
fn @__call__(%F, %this_argument, %arguments_list) -> CompletionRecord;
```

Where `%F` is the function object being called, `%this_argument` is the current
`this` value, and `%arguments_list` is the list of arguments to the function.
The environment record of the function is
