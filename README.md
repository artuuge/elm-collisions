## ELM-COLLISIONS

The present repository contains a collection of examples illustrating [Elm](http://elm-lang.org/) programming language.

### INSTRUCTIONS

There has been a significant upgrade `Elm: 0.16 -> 0.17` which can be perceived as a deviation of the design choices of the language from the **Functional Reactive Programming** paradigm: [farewell-to-frp](http://elm-lang.org/blog/farewell-to-frp).
For this reason, the `Elm-0.16` implementation of the examples has been moved to a sub-directory `obsolete`. The new examples are written in `Elm-0.17.1` and they can be found in the sub-directory `0.17`. One may check everything out by going to [http://elm-lang.org/try](http://elm-lang.org/try), where it should normally be possible to cut-and-paste the raw code and run it in a straightforward way.

If you wish to compile the examples locally, install Elm on your machine, and execute:
```
elm make filename.elm
```
This creates an `index.html` which can be opened in a browser.
