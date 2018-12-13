import Foto
import System.Environment
import Data.List

main :: IO ()
main = do
  -- receber argumentos
  args <- getArgs

  if (head args == "-t")
    then do
      runTests
    else do
      -- lê conteúdo do arquivo
      content <- readFile (head args)
      let l = lines content

      -- filtra comentários
      let filteredLines = filter (\x -> head x /= '#') l

      -- gera lista de flags
      let flags = drop 2 args

      -- criação do objeto photo
      let e = take 2 filteredLines
      let tipo = head e
      let height = (read $ head $ words $ last e) :: Int
      let width = (read $ last $ words $ last e) :: Int
      let maxValue = (read $ last $ take 3 filteredLines) :: Int
      let pixels = drop 3 filteredLines
      let pixelsInt = (map read $ (words . unwords) pixels) :: [Int]
      let p = Photo tipo width height maxValue pixelsInt



      --putStrLn $ show e
      let final_output = foldl (\acc f -> applyChanges acc f) p flags
      writeFile (last $ take 2 args) $ mountOutput final_output
