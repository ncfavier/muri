# muri :: Type -> Maybe Term

muri takes a Haskell type, and generates a Haskell term with that type, if possible.

Equivalently, under the [propositions-as-types correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence), muri is a theorem prover for intuitionistic propositional logic. It takes a proposition as input, and produces a proof of that proposition, if one exists.

The subset of Haskell used is as follows:

- input types are limited to type variables, product types `(a, b)`, sum types `Either a b` and function types `a -> b`,
- output values are built using lambda abstraction, function application, pair construction and the `Left` and `Right` type constructors for `Either`,
- the functions `fst`, `snd`, `either` and `curry` have their usual definitions,
- `afterLeft = (. Left)`, `afterRight = (. Right)`, and `afterConst = (. const)` (these functions will be removed, eventually).

muri was inspired by Lennart Augustsson's [Djinn](https://github.com/augustss/djinn), and is partly based on "Contraction-Free Sequent Calculi for Intuitionistic Logic" by Roy Dyckhoff.

## Usage

```sh
muri TYPE
```

muri currently doesn't have a build system, so you'll have to compile it yourself, or run it with `runhaskell Main.hs` or similar.

## Examples

```sh
$ muri 'a -> (b, c) -> (b, (a, c))'
\a -> \b -> (fst b, (a, snd b))
```

```sh
$ muri '(a, b) -> Either a b'
\a -> Left (fst a)
```

```sh
$ muri 'Either (a, c) (b, c) -> (Either a b, c)'
\a -> either (\b -> (Left (fst b), snd b)) (\c -> (Right (fst c), snd c)) a
```

```sh
$ muri '(Either (a -> f) a -> f) -> f'
\a -> afterLeft a (afterRight a)
```

```sh
$ muri '(a -> b) -> (Either (a -> f) b -> f) -> f'
\a -> \b -> afterLeft b (\c -> afterRight b (a c))
```

```sh
$ muri '(Either (a -> f) (b -> f) -> f) -> ((a, b) -> f) -> f'
\a -> \b -> afterLeft a (\c -> afterRight a (curry b c))
```

```sh
$ muri 'a -> b'
Impossible.
```
