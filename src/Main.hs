import Foto
import System.Environment
import Data.List

main :: IO ()
main = do
  -- receber argumentos
  args <- getArgs

  if head args == "-t"
    then do
      runTests
    else do
      -- reads file content
      content <- readFile (head args)
      let l = lines content

      -- fiter comments on image binary
      let filteredLines = filter (\x -> head x /= '#') l

      -- get flags for modifications
      let flags = drop 2 args

      -- fetch data for creating Photo
      let e = take 2 filteredLines
      let tipo = head e
      let height = (read $ head $ words $ last e) :: Int
      let width = (read $ last $ words $ last e) :: Int
      let maxValue = (read $ last $ take 3 filteredLines) :: Int
      let pixels = drop 3 filteredLines
      let pixelsInt = (map read $ (words . unwords) pixels) :: [Int]
      let p = Photo tipo width height maxValue pixelsInt

      let final_output = foldl (\acc f -> applyChanges acc f) p flags
      writeFile (last $ take 2 args) $ mountOutput final_output
