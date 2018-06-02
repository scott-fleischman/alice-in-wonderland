# Analysis

## [app/Main.hs](app/Main.hs)
Stub file used to indicate the entry point for the command-line application.

## [source](https://github.com/scott-fleischman/alice-in-wonderland/tree/master/analysis/source)
Contains the bulk of the analysis code.

### [source/Alice.hs](source/Alice.hs)
Contains `main` function which outputs information for twitter bot and debugging purposes.

### [source/Alice/Parse.hs](source/Alice/Parse.hs)
Parses the raw Gutenberg text file into detailed structure including chapters and paragraphs with various formatting settings.

### [source/Alice/Pdf.hs](source/Alice/Pdf.hs)
Loads the PDF document and extracts text. Relies on the [`pdf-toolbox-document`](http://hackage.haskell.org/package/pdf-toolbox-document) Haskell library. I ended up not using this in the final analysis because the PDF document format is too low-level for my purposes and I didn't trust the quality of the text extraction.

As I understand it, PDFs represent text using glyphs and positional offsets, so text extraction relies on glpyh (x, y) coordinates to determine where spaces and paragraphs are. Using the PDF for text information then seems less reliable than the raw text file version, which already has end-line characters for paragraphs and explicit spaces.

### [source/Alice/Render.hs](source/Alice/Render.hs)
Given the structure of the text, output chapters, paragraphs and words using the appropriate characters. One thing in particular we do is output actual em-dash characters (`—`) instead of two dashes (`--`).

### [source/Alice/Sentence.hs](source/Alice/Sentence.hs)
Parse paragraphs into sentences. This is done by iteratively expressing patterns which give sentence divisions, then manually checking the results from debug output. Then repeat this process until the sentence divisions are high quality.

### [source/Alice/TextFile.hs](source/Alice/TextFile.hs)
Simple loading of text file and path to its location.

### [source/Alice/Tweets.hs](source/Alice/Tweets.hs)
Defines structure of JSON for outputting sentence data as Twitter threads to be loaded by the Twitter bot program.

### [source/Alice/Words.hs](source/Alice/Words.hs)
Use the [chatter NLP library](https://hackage.haskell.org/package/chatter) to chunk and tag words with part of speech labels.

## [test/Main.hs](test/Main.hs)
Test suite run on every build.

## [package.yaml](package.yaml)
Project definition with dependency list and compiler options.
