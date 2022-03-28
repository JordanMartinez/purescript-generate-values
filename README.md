# purescript-generate-values

Generate random values. This project was originally forked from [`purescript-quickcheck`](https://github.com/purescript/purescript-quickcheck) due to
1. discoverability issues - when trying to generate random values, quickcheck isn't necessarily what one thinks of.
2. enabling generation with effects - `Gen` could not be run in another monad. This library re-exposes that as `GenT`.

## Installation

```
spago install generate-values
```

## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-generate-values).
