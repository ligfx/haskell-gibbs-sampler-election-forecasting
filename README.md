This is my project to replicate in Haskell the Gibbs sampler I implemented in my thesis in R. Since random numbers are implemented as a Monad, this project should expose me to a lot of the trickier concepts in Haskell.

Thoughts so far:

- Use `Control.Applicative`, it's useful for all sorts of things.
- If you can do something with `Control.Monad`, you can probably do it with `Control.Applicative`.
- `Control.Arrow` has some very useful functions, but does not interact well with monads (unless you use the Kleisli functions). I implemented an Applicative fanout `(&&&)` operator in `gibbs/helpers.hs` to get around this.
- I use the [Hasklig](https://github.com/i-tu/Hasklig) fonts with useful ligatures for Haskell symbols
