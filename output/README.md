# Output from Analysis

## [alice-adjectives.txt](alice-adjectives.txt)

How often does "Alice" appear adjacent to an adjective? This text file is the output of the analysis program for this question.

It shows:
* how many times "Alice" occurs
* how many times "Alice" occurs next to an adjective
* the adjectives adjacent to "Alice" with occurence counts in descending order
* all instances of "Alice" with adjacent adjectives and part-of-speech classification
* all instances of "Alice" with surrouding words with part-of-speech classification

Uses the Haskell `chatter` library for part-of-speech tagging and chunking. See details [here](https://hackage.haskell.org/package/chatter).

## [top-words.txt](top-words.txt)

Show all words in the text with occurrence count ordered by descending occurrence count then by alphabetical order (if occurrence counts are identical).

Words have been:
* stripped of punctuation except for the apostrophe and
* converted to lower case

## [top-words-non-stop.txt](top-words-non-stop.txt)

Similar to top-words above, but shows non-stop words in the text with occurrence count ordered by descending occurrence count then by alphabetical order (if occurrence counts are identical).

Stop word list is taken from [snowball_original.txt](https://github.com/igorbrigadir/stopwords/blob/21fb2ef149216e3c8cac097975223604ae1e2310/en/snowball_original.txt).
