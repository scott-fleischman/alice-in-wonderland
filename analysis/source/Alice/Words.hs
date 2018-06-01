{-# LANGUAGE OverloadedStrings #-}

module Alice.Words where

import qualified Alice.Parse
import qualified Alice.Render
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import           Data.Semigroup ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           NLP.Corpora.Conll (Tag)
import qualified NLP.Corpora.Conll
import qualified NLP.POS
import           NLP.Types.Tree (POS)
import qualified NLP.Types.Tree
import           Prelude hiding (words)

createTopWordsFile :: FilePath -> IO ()
createTopWordsFile outputPath = do
  text <- Alice.TextFile.loadText Alice.TextFile.textFilePath
  body <-
    case Alice.Parse.parseBody text of
      Left err -> error (show err)
      Right result -> return result

  let
    chapters = Alice.Structure.bodyChapters body
    words = Foldable.foldMap (Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition . Alice.Structure.chapterParagraphs) chapters
    output
      = Text.intercalate "\n"
      . fmap (\(k, v) -> Text.concat [k, " ", (Text.pack . show) v])
      . countOccurences
      . fmap (Text.toLower . Alice.Structure.wordText)
      $ words
  Text.IO.writeFile outputPath output

countOccurences :: Ord a => Foldable t => Functor t => t a -> [(a, Int)]
countOccurences
  = List.sortOn (Ord.Down . snd)
  . Map.toList
  . Foldable.foldr (\(k, v) m -> Map.insertWith (+) k v m) Map.empty
  . fmap (\x -> (x, 1 :: Int))

createPartOfSpeechFile :: FilePath -> IO ()
createPartOfSpeechFile outputPath = do
  text <- Alice.TextFile.loadText Alice.TextFile.textFilePath
  body <-
    case Alice.Parse.parseBody text of
      Left err -> error (show err)
      Right result -> return result

  let chapters = Alice.Structure.bodyChapters body
  tagger <- NLP.POS.defaultTagger
  let
    getChapterSentences
      = NLP.POS.tag tagger
      . Alice.Render.renderAllWords
      . Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition
      . Alice.Structure.chapterParagraphs
    contextualizeTaggedSentence (NLP.Types.Tree.TaggedSent tags)
      = Alice.Sentence.contextualizeItems (,,) (Alice.Structure.BeforeCount 3) (Alice.Structure.AfterCount 3)
      $ Seq.fromList tags
    findAlices = filter isAliceTriple . Foldable.toList

    allAlices :: [(Seq (POS Tag), POS Tag, Seq (POS Tag))]
    allAlices = Foldable.foldMap (Foldable.foldMap (findAlices . contextualizeTaggedSentence) . getChapterSentences) chapters

    aliceAdjectives = filter hasContext . fmap takeAdjacentAdjectives $ allAlices
    aliceAdjectiveSet = Text.concat . fmap (\(x, c) -> x <> " " <> (Text.pack . show) c <> "\n") . countOccurences . fmap NLP.Types.Tree.showPOStok . Foldable.toList $ Foldable.foldMap getAdjacentAdjectives allAlices

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
      , Text.intercalate "\n" $ fmap printTriple aliceAdjectives
      , "\n"
      , "\n"
      , "\n"
      , "\n"
      , "All Alices:\n"
      , Text.intercalate "\n" $ fmap printTriple allAlices
      ]
  Text.IO.writeFile outputPath output

isAliceTriple :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Bool
isAliceTriple (_, pos, _) = isAlicePOS pos

hasContext :: (Seq a, a, Seq a) -> Bool
hasContext (before, _, after) = (not . Seq.null) before || (not . Seq.null) after

printTriple :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Text
printTriple (before, pos, after)
  = Text.intercalate " " . Foldable.toList
  $ fmap NLP.Types.Tree.printPOS before
  Seq.>< Seq.singleton (NLP.Types.Tree.printPOS pos)
  Seq.>< fmap NLP.Types.Tree.printPOS after

takeAdjacentAdjectives
  :: (Seq (POS Tag), POS Tag, Seq (POS Tag))
  -> (Seq (POS Tag), POS Tag, Seq (POS Tag))
takeAdjacentAdjectives (before, pos, after) = (Seq.takeWhileR isAdjectivePOS before, pos, Seq.takeWhileL isAdjectivePOS after)

getAdjacentAdjectives :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Seq (POS Tag)
getAdjacentAdjectives = (\(before, _, after) -> before Seq.>< after) . takeAdjacentAdjectives

isAdjacentAdjective :: (Seq (POS Tag), POS Tag, Seq (POS Tag)) -> Bool
isAdjacentAdjective = (> 0) . Seq.length . getAdjacentAdjectives

getPOSToken :: POS Tag -> Text
getPOSToken = NLP.Types.Tree.showTok . NLP.Types.Tree.posToken

isAlicePOS :: POS Tag -> Bool
isAlicePOS = (== "Alice") . getPOSToken

isAdjectivePOS :: POS Tag -> Bool
isAdjectivePOS = isAdjectiveTag . NLP.Types.Tree.posTag

isAdjectiveTag :: Tag -> Bool
isAdjectiveTag NLP.Corpora.Conll.JJ = True
isAdjectiveTag NLP.Corpora.Conll.JJR = True
isAdjectiveTag NLP.Corpora.Conll.JJS = True
isAdjectiveTag _ = False
