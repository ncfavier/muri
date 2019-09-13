# muri :: Type -> Maybe Term

muri takes a Haskell type, and generates a Haskell term with that type (or a more general one), if possible.

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

```
$ muri 'a -> (b, c) -> (b, (a, c))'
\a -> \(b, c) -> (b, (a, c))
```

```
$ muri '(a, b) -> Either a b'
\(a, b) -> Left a
```

```
$ muri 'Either (a, c) (b, c) -> (Either a b, c)'
\a -> case a of { Left (b, c) -> (Left b, c); Right (b, c) -> (Right b, c) }
```

```
$ muri '(Either (a -> f) a -> f) -> f'
\a -> a (Left (\c -> a (Right c)))
```

```
$ muri '(a -> b) -> (Either (a -> f) b -> f) -> f'
\a -> \b -> b (Left (\c -> b (Right (a c))))
```

```
$ muri '(Either (a -> f) (b -> f) -> f) -> ((a, b) -> f) -> f'
\a -> \b -> a (Left (\e -> a (Right (\f -> b (e, f)))))
```

```
$ muri '((a -> b) -> c) -> ((a, b -> c) -> b) -> c'
\a -> \b -> a (\f -> b (f, \e -> a (\_ -> e)))
```

```
$ muri 'a -> b'
Impossible.
```
