[TaintedExample1.hs](/TaintedExample1.hs) and [TaintedExample2.hs](/TaintedExample2.hs) are my first forays into Type level programming with the help of a friendly user Pantsu from the [Functional Programming Discord](https://discord.com/invite/K6XHBSh).

I've been looking to represent the proof rules of Gentzen's [Natural Deduction] in Haskell to try out writing rewrite rules. I've been meaning to get my hands dirty with tree rewrite rules and it seemed like the best starting point.

Initially I had tried something like this

```haskell
{-# LANGUAGE GADTs -}

data Expr a where
  Base :: a -> Expr a
  Uncurse :: TaintedExpr a -> Expr b -> Expr b

data TaintedExpr a where
  Base :: a -> TaintedExpr a
  And :: Expr a -> TaintedExpr b ->
TaintedExpr (a,b)
  AndB :: TaintedExpr a -> TaintedExpr -> TaintedExpr (a,b)
```

And ran into the issue that I wanted to "Uncurse" expressions.

First things I noticed with this mirrored GADT approach:
- Brittle duplication of practically identical types
- Doesn't handle constructors accepting both Untained and Tainted expressions.
- Do you export both?

The whole point of this exercise was to build up a GADT for ND proofs that can track assumptions and discharge them. The Uncurse constructor is supposed to represent an implication.

The datakind language extension is still over my head. I've seen a [talk on Servant](https://www.youtube.com/watch?v=gMDiKOuwLXw) before but I still need to grok it.

Eisenberg from Tweag.io actually did a [dissertation on dependant types in Haskell](https://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf)

[This approach has helped in making an equ signature.](https://stackoverflow.com/a/55563794) Now we just need a binding.

One thing to note is how to handle equality of proof trees (tree structure) and isomorphisms between proofs. The GADT aspect of this might mess it up.
[Another thing to read](https://stackoverflow.com/59/constraintkinquestions/313171ds-explained-on-a-super-simple-example/31328543#31328543)

http://www.cs.ox.ac.uk/jeremy.gibbons/wg21/meeting62/gibbons-indexed.pdf
https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Typeable.html

This might help https://github.com/eddywestbrook/hobbits
