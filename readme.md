## Compiling gracefully

Alternative title: *Calculating compilers categorically*

Talk for [Haskell Love 2020](https://haskell.love/).

Links:

*   [Slides (PDF)](http://conal.net/talks/compiling-gracefully.pdf)
*   [Video](https://www.youtube.com/watch?v=wvQbpS6wBa0)
*   Incomplete draft paper: [*Calculating compilers categorically*](http://conal.net/papers/calculating-compilers-categorically/).

**Abstract**:

<blockquote>
This talk revisits the classic exercise of compiling a programming language to a stack-based virtual machine.
The main innovation is to factor the exercise into two phases: translation into standard a algebraic vocabulary, and a stack-oriented interpretation of that vocabulary.
The first phase is independent of stack machines and has already been justified and implemented in a much more general setting.
The second phase captures the essential nature of stack-based computation simply and generally, giving rise to a collection of simple, uniform algebra problems whose solution is a compiler.
</blockquote>
