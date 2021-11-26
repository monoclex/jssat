# Domino the Debugger

Domino, named in homage of my favorite cat, is the one-stop-shop for debugging
JSSAT IR. Currently, these are the feature requirements for it to:

- handle massive amounts of profiling data from the JSSAT interpreter
- show the change in records and other data structures as it flows through the
  code
- map JSSAT IR to the source code that generated it, and show the original LISP
- show the callstack of the program over time

Overall, it is intended to be a complete solution for debugging JSSAT IR.

## Usage

Domino is intended to be used after interpreting a program. To start Domino,
call `domino::launch` after interpretation:

```rs
let listen_url = "127.0.0.1:8000";
println!("starting domino on http://{listen_url}");
domino::launch(listen_url, &interpreter.moment.into());
```

If Domino is stuck on `contacting backend...` (it should disappear instantly),
check the `Network` tab. The `/overview` request must succeed before the page
loads.

To navigate around the callstack, use the arrow keys. `ArrowUp` and `ArrowDown`
advance backwards and forwards a single moment in time. A moment is sampled on
every instruction, so this will advance you to exactly the previous or next
instruction the interpreter interprets. `ArrowLeft` and `ArrowRight` advance
backwards and forwards an entire instruction at a time. This will step over
going inside of calls. However, if there is no "next instruction" to step over
to (e.g. if an assertion fails inside of a function), Domino will step a single
moment at a time instead.
