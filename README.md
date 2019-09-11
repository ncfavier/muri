# muri :: Type -> Maybe Term

muri takes a Haskell type, and generates a Haskell term with that type, if possible.

Equivalently, under the [propositions-as-types correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence), muri is a theorem prover for intuitionistic propositional logic. It takes a proposition as input, and produces a proof of that proposition, if one exists.

Input types are limited to type variables, product types `(a, b)`, sum types `Either a b` and function types `a -> b`.

Output terms are built using lambda abstraction, function application, `let` and `case` expressions, pair construction and the `Left` and `Right` type constructors.

muri was inspired by Lennart Augustsson's [Djinn](https://github.com/augustss/djinn), and uses the LJT calculus from "Contraction-Free Sequent Calculi for Intuitionistic Logic" by Roy Dyckhoff to ensure termination.

## Usage

```sh
muri TYPE
```

muri currently doesn't have a build system, so you'll have to compile it yourself, or run it with `runhaskell` or similar.

## Examples

```sh
$ muri 'a -> (b, c) -> (b, (a, c))'
\a -> \b -> let (c, d) = b in (c, (a, d))
```

```sh
$ muri '(a, b) -> Either a b'
\a -> let (b, c) = a in Left b
```

```sh
$ muri 'Either (a, c) (b, c) -> (Either a b, c)'
\a -> case a of { Left b -> let (a, c) = b in (Left a, c); Right c -> let (a, b) = c in (Right a, b) }
```

```sh
$ muri '(Either (a -> f) a -> f) -> f'
\a -> (\b -> a (Left b)) (\c -> a (Right c))
```

```sh
$ muri '(a -> b) -> (Either (a -> f) b -> f) -> f'
\a -> \b -> (\c -> b (Left c)) (\f -> (\d -> b (Right d)) (a f))
```

```sh
$ muri '(Either (a -> f) (b -> f) -> f) -> ((a, b) -> f) -> f'
\a -> \d -> (\b -> a (Left b)) (\h -> (\c -> a (Right c)) ((\e -> \f -> d (e, f)) h))
```

```sh
$ muri '((a -> b) -> c) -> ((a, b -> c) -> b) -> c'
\a -> \b -> a (\f -> (\c -> \d -> b (c, d)) f (\e -> a (\_ -> e)))
```

```sh
$ muri 'a -> b'
Impossible.
```

## To do

- rewrite output terms nicely (e.g. rewrite `\a -> let (b, c) = a in b` to `\(b, c) -> b`)
