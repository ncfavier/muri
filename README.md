# muri :: Type -> Maybe Term

muri takes a Haskell type, and generates a Haskell term with that type, if possible.

Equivalently, under the [propositions-as-types correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence), muri is a theorem prover for intuitionistic propositional logic. It takes a proposition as input, and generates a proof of that proposition.

The subset of Haskell used is such that:

- types are limited to type variables, product types `(a, b)`, sum types `Either a b` and function types `a -> b`,
- returned values are built using lambda abstraction, function application, pair construction and the `Left` and `Right` type constructors for `Either`,
- the functions `fst`, `snd`, `either` and `curry` have their usual meanings,
- `afterLeft = (. Left)`, `afterRight = (. Right)`, and `afterConst = (. const)` (these functions will be removed, eventually).

muri was inspired by Lennart Augustsson's [Djinn](https://github.com/augustss/djinn), and is partly based on "Contraction-Free Sequent Calculi for Intuitionistic Logic" by Roy Dyckhoff.

## Usage

```sh
muri TYPE
```

muri currently doesn't have a build system, so you'll have to compile it yourself, or run it with `runhaskell Main.hs` or similar.
