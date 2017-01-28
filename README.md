# purescript-coui

Experiments with comonads for modelling UIs. Inspired by [paf31](https://github.com/paf31)'s
[purescript-comonad-ui](https://github.com/paf31/purescript-comonad-ui) and as exercies to understand comonads.

The difference with the original are it support side effects in response to user actions
by using CoT transformer to handle user action directly rather than using Pairing as demonstrate by paf31.
Also i implements the structure for running the component largely taken from Slamdata's [Halogen](https://github.com/slamdata/purescript-halogen).

I am still not clear how to combine the component, one can be achieved by taking their Day convolution.
But, it might possible to us to focus them instead.

Credit goes to
- [paf31](https://github.com/paf31)
- Slamdata's [Halogen](https://github.com/slamdata/purescript-halogen)
