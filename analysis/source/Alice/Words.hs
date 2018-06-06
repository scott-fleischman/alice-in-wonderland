{-# LANGUAGE OverloadedStrings #-} -- interpret string literals as different types

module Alice.Words where

-- our library code
import qualified Alice.Parse
import qualified Alice.Render
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile

import qualified Data.Foldable as Foldable -- loop over data structures
import qualified Data.List as List -- linked list processing
import qualified Data.Map as Map -- map (tree) library
import qualified Data.Ord as Ord -- data comparison code
import           Data.Semigroup ((<>)) -- abstract string concatenation
import           Data.Sequence (Seq) -- lists with efficient append and prepend
import qualified Data.Sequence as Seq -- sequence library
import qualified Data.Set as Set -- set library for efficient membership lookup
import           Data.Text (Text) -- string type
import qualified Data.Text as Text -- string library
import qualified Data.Text.IO as Text.IO -- print strings
import           NLP.Corpora.Conll (Tag) -- part-of-speech Tag definition
import qualified NLP.Corpora.Conll -- conll library code
import qualified NLP.POS -- part of speech library
import           NLP.Types.Tree (POS) -- part of speech type definition
import qualified NLP.Types.Tree -- part of speech library
import           Prelude hiding (words) -- hide the 'words' identifier so we can use it in our code

-- create the list of top words file
createTopWordsFile :: FilePath -> FilePath -> FilePath -> IO ()
createTopWordsFile stopWordsFile rawOutputPath nonStopOutputPath = do
  -- load the gutenberg text file
  text <- Alice.TextFile.loadText Alice.TextFile.textFilePath

  -- parse the body using our paragraph structures
  body <-
    case Alice.Parse.parseBody text of
      Left err -> error (show err) -- crash upon failure
      Right result -> return result

  stopWordsSet <- fmap (Set.fromList . filter (not . Text.null) . Text.lines) $ Text.IO.readFile stopWordsFile

  let
    chapters = Alice.Structure.bodyChapters body

    -- extract words including the "later" edition text
    words = Foldable.foldMap (Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition . Alice.Structure.chapterParagraphs) chapters

    -- count occurrences and make into printable text (read bottom-up)
    makeWordsOutput
      = Text.intercalate "\n" -- add endline characters after each line
      . fmap (\(k, v) -> Text.concat [k, " ", (Text.pack . show) v]) -- print a single line with in the format "word 123"
      . countOccurences -- count the number of times each word occurs
      -- ^^^ start processing here

    -- all words including stop words, converted to lowercase
    rawWords = fmap (Text.toLower . Alice.Structure.wordText) words

    -- word list with stop words filtered out
    nonStopWords = Seq.filter (\x -> not (Set.member x stopWordsSet)) rawWords

  -- write the text output to the files
  Text.IO.writeFile rawOutputPath (makeWordsOutput rawWords)
  Text.IO.writeFile nonStopOutputPath (makeWordsOutput nonStopWords)

-- Given a list (or something like a list) of words, count each time it occurs.
-- Return a pair of the word with its occurrence count.
-- Results are sorted by descending occurrence count.
countOccurences :: Ord a => Foldable t => Functor t => t a -> [(a, Int)]
countOccurences
  -- (read this code bottom-up)
  = List.sortOn (Ord.Down . snd) -- sort by descending occurrence count
  . Map.toList -- convert the map into a list of pairs of word and counts
  . Foldable.foldr (\(k, v) m -> Map.insertWith (+) k v m) Map.empty -- for each word, add it to the map or increase the count in the map
  . fmap (\x -> (x, 1 :: Int)) -- given each word the initial count of 1
  -- ^^^ start processing here, and read each line upwards

-- write the Alice adjectives analysis file output
createPartOfSpeechFile :: FilePath -> IO ()
createPartOfSpeechFile outputPath = do
  -- load Gutenberg text
  text <- Alice.TextFile.loadText Alice.TextFile.textFilePath

  -- parse body
  body <-
    case Alice.Parse.parseBody text of
      Left err -> error (show err) -- crash on parse failure
      Right result -> return result

  let chapters = Alice.Structure.bodyChapters body
  tagger <- NLP.POS.defaultTagger -- use the default NLP tagger in the chatter library
  let
    -- get the chunked, tagged sentences for each chapter (read the following code bottom-up)
    getChapterSentences
      = NLP.POS.tag tagger -- tag word parts of speech using the NLP library
      . Alice.Render.renderAllWords -- render all words as flat text, thereby normalizing the text output
      . Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition -- break into words including later text additions
      . Alice.Structure.chapterParagraphs -- get paragraphs
      -- ^^^ start here and read upwards

    -- add context around each word (3 words before and 3 words afterwards)
    contextualizeTaggedSentence (NLP.Types.Tree.TaggedSent tags)
      = Alice.Sentence.contextualizeItems (,,) (Alice.Structure.BeforeCount 3) (Alice.Structure.AfterCount 3)
      $ Seq.fromList tags -- convert words into the Seq data structure (efficient prepend and append)

    -- find the words that match "Alice"
    findAlices = filter isAliceTriple . Foldable.toList

    -- get all of the "Alice" words (tagged with part of speech) with surrounding word context from all chapters
    allAlices :: [(Seq (POS Tag), POS Tag, Seq (POS Tag))]
    allAlices = Foldable.foldMap (Foldable.foldMap (findAlices . contextualizeTaggedSentence) . getChapterSentences) chapters

    -- find adjectives in the context around each Alice word
    aliceAdjectives = filter hasContext . fmap takeAdjacentAdjectives $ allAlices

    -- calculate
    aliceAdjectiveSet
      = Text.concat
      . fmap (\(x, c) -> x <> " " <> (Text.pack . show) c <> "\n")
      . countOccurences
      . fmap NLP.Types.Tree.showPOStok
      . Foldable.toList
      $ Foldable.foldMap getAdjacentAdjectives allAlices

    -- print the output in a text file format
    output = Text.concat
      [ "All alices: " <> (Text.pack . show . length) allAlices <> "\n"
      , "Alices adjacent to adjectives: " <> (Text.pack . show . length) aliceAdjectives <> "\n"
      , "\n"
      , "Adjectives around Alice with total counts:\n"
      , aliceAdjectiveSet
      , "\n"
      , "\n"
      , "\n"
      , "Alice + adjectives:\n"
      , Text.intercalate "\n" $ fmap printTriple aliceAdjectives -- print each alice+adj one per line
      , "\n"
      , "\n"
      , "\n"
      , "\n"
      , "All Alices:\n"
      , Text.intercalate "\n" $ fmap printTriple allAlices -- print all alice occurrences with context one per line
      ]
  Text.IO.writeFile outputPath output -- write the file

-- does this contain "Alice"
isAliceTriple :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Bool
isAliceTriple (_, pos, _) = isAlicePOS pos

-- does this word have any surrounding words?
hasContext :: (Seq a, a, Seq a) -> Bool
hasContext (before, _, after) = (not . Seq.null) before || (not . Seq.null) after

-- make a printable version of this triple, with POS tagging
printTriple :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Text
printTriple (before, pos, after)
  = Text.intercalate " " . Foldable.toList
  $ fmap NLP.Types.Tree.printPOS before
  Seq.>< Seq.singleton (NLP.Types.Tree.printPOS pos)
  Seq.>< fmap NLP.Types.Tree.printPOS after

-- Only consider context around word that are adjectives.
-- This processes outward from the word itself, so it only considers adjectives adjacent to the word.
takeAdjacentAdjectives
  :: (Seq (POS Tag), POS Tag, Seq (POS Tag))
  -> (Seq (POS Tag), POS Tag, Seq (POS Tag))
takeAdjacentAdjectives (before, pos, after) = (Seq.takeWhileR isAdjectivePOS before, pos, Seq.takeWhileL isAdjectivePOS after)

-- returns a flat list of adjacent adjectives
getAdjacentAdjectives :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Seq (POS Tag)
getAdjacentAdjectives = (\(before, _, after) -> before Seq.>< after) . takeAdjacentAdjectives

-- does this word have adjacent adjectives?
isAdjacentAdjective :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Bool
isAdjacentAdjective = (> 0) . Seq.length . getAdjacentAdjectives

-- get a textual version of the token
getPOSToken :: POS Tag -> Text
getPOSToken = NLP.Types.Tree.showTok . NLP.Types.Tree.posToken

-- this "Alice"?
isAlicePOS :: POS Tag -> Bool
isAlicePOS = (== "Alice") . getPOSToken

-- is this Part of Speech tag an adjective
isAdjectivePOS :: POS Tag -> Bool
isAdjectivePOS = isAdjectiveTag . NLP.Types.Tree.posTag

-- is this tag an adjective?
isAdjectiveTag :: Tag -> Bool
isAdjectiveTag NLP.Corpora.Conll.JJ = True
isAdjectiveTag NLP.Corpora.Conll.JJR = True
isAdjectiveTag NLP.Corpora.Conll.JJS = True
isAdjectiveTag _ = False
