# Domino the Debugger

Domino, named in homage of my favorite cat, is the one-stop-shop for debugging
JSSAT IR. Currently, these are the feature requirements for it to:

- handle massive amounts of profiling data from the JSSAT interpreter
- show the change in records and other data structures as it flows through the
  code
- map JSSAT IR to the source code that generated it, and show the original LISP
- show the callstack of the program over time

Overall, it is intended to be a complete solution for debugging JSSAT IR.
