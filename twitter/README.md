# Twitter Bot
The twitter bot embeds the [JSON exported](../data/README.md) from the analysis tool. It randomly sorts the sentences and tweets one every half hour.

## [Main.hs](Main.hs)
All of the code for the command-line utility to load the JSON and post it to Twitter are in this file.

## [TweetsData.hs](TweetsData.hs)
A separate file which loads the JSON as a Haskell value at compile time. That way it ends up being embedded in the executable binary when compiled.

## [package.yaml](package.yaml)
Project definition with dependency list and compile flags.
