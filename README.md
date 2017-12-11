# text-editor-challenge

## Challenge specs

https://www.hackerrank.com/challenges/simple-text-editor/problem

## running the app

If you have stack installed...

run the tests:
```
stack test
```

compile:
```
stack install
```

Run:
```
stack exec text-editor-challenge <optional-file-path>
```

## layout

The core logic of the app is in `library/Editor.hs`. This is where the bulk of the logic is, and so it is heavily tested, and free of IO/effects.

Most of the input effects are in `library/Parser.hs` and this logic I mostly tested manually, and is covered by a single happy-path integration test.

The output effects are mostly in `executable/Main.hs` and were all just tested manually.

The tests are located in `test-suite/Main.hs`.

The default input file is locate at `data/input-sample.txt`, and is the same input as specified in the challenge.


## approach


I chose to have the app read from a file, since the challenge didn't specify the input method. If I were going to mess around with it more,
I'd add an additional interactive input option to run it as a repl. The challenge mentioned only reading in the number of operations as
specified by the first integer in the input. I but I chose to not implement that behavior, since it makes more sense for a serialized input like a
stream than it does for a file, which has a definitive end-of-input. But adding in that restriction would only require changing a couple lines in
the parser.

The general idea of having a functional core of business logic that is heavily tested, and then a shell of IO logic that is sparsly tested, is how I
approach design in any language (but it's easier in Haskell since the effects are reified in the IO type).

I like to refactor my code such that the names for things are self-explanatory, and then I add comments to explain the
reasoning behind decisions (in theory at least). But Haskell can get so concise that more extensive comments might become necessary. In general
I'm flexible with comments, and will write more or less depending on team standards.

The pattern I used of passing around a State type, could have been refactored with the StateT monad, but it didn't seem necessary.

At first I was thinking that I'd need to write a property test to handle edge cases arising from any particular ordering of commands,
but by the end I felt like the unit tests I had written were thorough enough. For the most part any change to the core bits of logic
(like `Editor.perform`) will break a test, so I feel good about the test coverage.
