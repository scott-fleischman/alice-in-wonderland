{-# LANGUAGE OverloadedStrings #-} -- can interpret literal strings as different types

module Alice where

-- Include most of our library code
import qualified Alice.Parse
import qualified Alice.Render
import qualified Alice.Sentence
import qualified Alice.Structure
import qualified Alice.TextFile
import qualified Alice.Tweets
import qualified Alice.Words

import qualified Data.Char as Char -- character checking (is space, letter)
import qualified Data.Foldable as Foldable -- abstract loop processing over data structures
import qualified Data.Maybe as Maybe -- library for dealing with optional values
import           Data.Sequence (Seq) -- Sequence is a list-like structure with efficient append and prepend operations (internally a tree)
import qualified Data.Sequence as Seq -- library for Sequence
import qualified Data.Set as Set -- set library (internally a tree)
import           Data.Text (Text) -- core string type
import qualified Data.Text as Text -- string library
import qualified Data.Text.IO as Text.IO -- printing strings to output
import           Prelude hiding (words) -- Prelude is always included; hide the 'words' identifier so we can use it as a variable name

-- run the code with a path to the text file to load
run :: FilePath -> IO ()
run path = do
  -- load the Alice in Wonderland text file
  text <- Alice.TextFile.loadText path

  -- print the number of characters
  putStrLn $ "Text length = " ++ show (Text.length text)

  -- parse the structure of the document
  body <-
    case Alice.Parse.parseBody text of
      -- crash with message on failure
      Left err -> error (show err)

      -- success!
      Right result -> return result

  -- get the chapter list
  let chapters = Alice.Structure.bodyChapters body

  -- print chapter count
  putStrLn $ "\nChapter Count: " ++  (show . length) chapters

  -- print each chapter using the 'printChapterSentences' function
  mapM_ printChapterSentences chapters

-- debug function to print the set of all punctuation trailing a word
printSuffixSet :: Alice.Structure.Body -> IO ()
printSuffixSet body = do
  let
    chapters = Alice.Structure.bodyChapters body
    allWords :: Seq Alice.Structure.Word
    allWords
      = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition
      $ Foldable.foldMap Alice.Structure.chapterParagraphs chapters
    suffixSet
      = Set.fromList
      . fmap Alice.Structure.wordSuffix
      . filter
        (\x
          -> (not . Text.null . Alice.Structure.wordSuffix) x
          && Alice.Structure.wordLast x == Alice.Structure.NotLastWordInParagraph
        )
      . Foldable.toList
      $ allWords
  mapM_ Text.IO.putStrLn suffixSet

-- debug function to print chapter data (right now modified to print sentences longer than 280 characters)
printChapterSentences :: Alice.Structure.Chapter -> IO ()
printChapterSentences (Alice.Structure.Chapter number (Alice.Structure.ChapterTitle titleText) _contents paragraphs) = do
  putStr $ "Chapter " ++ show number ++ ". "
  Text.IO.putStrLn titleText
  putStrLn ""
  let
    chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition paragraphs
    sentences = Alice.Sentence.parseAllSentences chapterWords
    sentenceTexts = fmap (\(Alice.Structure.Sentence words) -> Alice.Render.renderAllWords words) sentences
    longSentenceTexts = filter ((>280) . Text.length) $ Foldable.toList sentenceTexts
    chunks = fmap (Alice.Render.chunkRendering 280) longSentenceTexts
    invalidChunks = filter (\x -> length (Seq.findIndicesL ((> 280) . Text.length) x) > 0) chunks
  mapM_ (Text.IO.putStrLn . Text.append "\n" . Text.intercalate "\n" . Foldable.toList) invalidChunks
  putStrLn "\n\n\n"

-- debug function to print a chapter text (here modified to print words with context and trailing period, exclamation point or question mark characters)
printChapter :: Alice.Structure.Chapter -> IO ()
printChapter (Alice.Structure.Chapter number title _contents paragraphs) = do
  putStrLn $ show number ++ ". " ++ show title
  putStrLn ""
  let
    chapterWords = Alice.Sentence.allParagraphWords Alice.Structure.LaterEdition paragraphs
    getSuffix = Alice.Structure.wordSuffix . Alice.Structure.wordContextWord
    getWordLast = Alice.Structure.wordLast . Alice.Structure.wordContextWord
  mapM_ printWordContextLn
    . filter
      (\wordContext
        -> (Maybe.isJust . Text.find (\c -> elem c ['.','!','?']) . getSuffix) wordContext
        && getWordLast wordContext == Alice.Structure.NotLastWordInParagraph
      )
    . Foldable.toList
    . Alice.Sentence.contextualizeWords (Alice.Structure.BeforeCount 3) (Alice.Structure.AfterCount 5)
    $ chapterWords
  putStrLn "\n\n\n"

-- print a word from our internal structure
printWord :: Alice.Structure.Word -> IO ()
printWord = Text.IO.putStr . Alice.Render.renderWord

-- print a word from our internal structure on its own line
printWordLn :: Alice.Structure.Word -> IO ()
printWordLn word = do
  printWord word
  Text.IO.putStr "\n"

-- print a word on a single line with surrounding context
printWordContextLn :: Alice.Structure.WordContext -> IO ()
printWordContextLn (Alice.Structure.WordContext before word after) =
  Text.IO.putStrLn $
    Text.intercalate "   "
      [ Alice.Render.renderAllWords before
      , "   [[ "
      , Alice.Render.renderWord word
      , " ]]   "
      , Alice.Render.renderAllWords after
      ]

-- debug function to print a string with some other value (say, a number)
printPair :: Show a => (Text, a) -> IO ()
printPair (t, a) = do
  Text.IO.putStr t
  putStr "\t"
  print a

-- debug function to print only punctuation
printIfContainsNonLetter :: Text -> IO ()
printIfContainsNonLetter input =
  if Text.any (not . Char.isLetter) input
    then Text.IO.putStrLn input
    else return ()

-- debug function to print a paragraph as a list of words with spaces, removing all paragraph structure (indent, etc.)
printParagraphFlat :: Alice.Structure.ParagraphFormat -> IO ()
printParagraphFlat para = do
  let optionalText = Alice.Sentence.flattenParagraphFormat Alice.Structure.LaterEdition para
  case optionalText of
    Just text -> do
      Text.IO.putStrLn text
      putStrLn ""
    Nothing -> return ()

-- debug function to print the paragraph structure that we parsed as well as words in the paragraph
printParagraphFormat :: Alice.Structure.ParagraphFormat -> IO ()
printParagraphFormat (Alice.Structure.ParagraphFormatPlain text) = do
  Text.IO.putStrLn text
  putStrLn ""
printParagraphFormat (Alice.Structure.ParagraphFormatIndented texts) = do
  mapM_ Text.IO.putStrLn texts
  putStrLn ""
printParagraphFormat (Alice.Structure.ParagraphFormatLaterEdition texts) = do
  putStrLn "  [later edition:]"
  mapM_ Text.IO.putStrLn texts
  putStrLn ""
printParagraphFormat Alice.Structure.ParagraphFormatStarDivision = do
  putStrLn " * * * (star divison) * * * "
  putStrLn ""
printParagraphFormat Alice.Structure.ParagraphFormatChorusMarker = do
  putStrLn "   --CHORUS.-- "
  putStrLn ""

-- main entry point
main :: IO ()
main = do
  -- create the tweets JSON file for our twitter bot
  Alice.Tweets.createTweetsFile "data/tweets.json"

  -- print top words file
  Alice.Words.createTopWordsFile "output/top-words.txt"

  -- print adjective analysis file
  Alice.Words.createPartOfSpeechFile "output/alice-adjectives.txt"

  -- uncomment next line to print debug info on the Gutenberg text file
  -- run Alice.TextFile.textFilePath
