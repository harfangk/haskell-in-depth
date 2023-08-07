import Control.Monad
import Data.Char
import Data.List (group, sort, sortBy)
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import System.Environment

type Entry = (T.Text, Int)

type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs = (head xs, length xs)
    cleanWord = T.dropAround $ not . isLetter

allWords :: Vocabulary -> [Text]
allWords = map fst

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (sum $ map snd vocab, length vocab)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

allWordsReport :: Vocabulary -> Text
allWordsReport vocab = fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab = fmt $ "Total number of words: " +| total |+ "\nNumber of unique words: " +| unique |+ "\n"
  where
    (total, unique) = wordsCount vocab

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num =
  fmt $ nameF "Frequent words" $ blockListF' "" fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t, n) = "" +| t |+ ": " +| n |+ ""

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] -> processTextFile fname True (read num)
    [fname, num] -> processTextFile fname False (read num)
    _ -> putStrLn "Usage: vocab3 [a] filename freq_words_num"
