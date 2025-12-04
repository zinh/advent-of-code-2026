module Main where
import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ls = lines contents
      fn (pos, n0) line = let r = parseLine line
                              newPos = rotate pos r
                           in
                             if newPos == 0 then (newPos, n0 + 1) else (newPos, n0)
  print (foldl fn (50, 0) ls)
