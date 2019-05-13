So we're going for a bit different of a methodology for implementing a compiler. Rather than going directly from language -> binary in one step, we will instead be implementing many smaller compilers, each one going to a lower level than the last. Eventually we will compile to a assembly-like language with a full vm.

What's cool about this is that it's very easy to get started, and to get results. In theory, if every compiler takes input from stdin, and outputs to stdout, you can create a full compiler by using bash pipes: `cat input.lisp | compilerA | compilerB | compilerC > output`

What is _also_ cool is that this means that assignments are very simple: implement the next component of this chain! The downside to this is that there's a lot of repeated parsing work, but hopefully the languages we translate will be simple enough that this won't be an issue.

As far as the languages, here is what I'm thinking:

- Start with C
- Compile that C down to effectively 3-address code (with the exception of multidimensional arrays)
- Compile multidimensional arrays

So... without further ado, here are the proposed assignments:

- Week 1: Typechecking
- Week 2: 3-address code
- Week 3: Multidimensional arrays
- Week 4: Optimization
- Week 5: _Basic_ Codegen
  - basically this means that we aren't going to handle the stack, or procedure params,
    or pointers
- Week 6: Procedure Parameters
- Week 7: Structs and maybe unions
