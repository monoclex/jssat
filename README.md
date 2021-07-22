<div align="center">
    <h1>JSSAT</h1>
</div>

JSSAT is a compiler that turns JavaScript into LLVM IR, without sacrificing
performance or size as traditional methods do. To do this, it primarily utilizes
symbolic execution to explore a program along all possible paths, and generates
the corresponding LLVM IR.
[A much more in-depth explanation can be found here (please do read it, it's everything this README should be and more)](https://sirjosh3917.com/posts/jssat-compiling-javascript-to-llvm-ir).

### ⚠️ JSSAT is in an early stage of development.

Currently, we can compile a simple `print("Hello, World!")` but there is much
that needs to be done.

## Usage

At this time, the JavaScript program is hardcoded as a string, and there is
currently no support for node-like programs yet.

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) for information on contributing.

## License

The license is the same as Rust/LLVMs (MIT + Apache), so you should be able to
use this freely.
