module Lib (rotate, parseLine, Rotation(..), Direction(..)) where

data Direction = L | R deriving (Show, Eq)
data Rotation = Rotation Direction Integer deriving (Show, Eq)

parseLine :: String -> Rotation
parseLine ('L' : digits) = Rotation L (read digits::Integer)
parseLine ('R' : digits) = Rotation R (read digits::Integer)
parseLine _ = error "Unexpected Input"

rotate :: Integer -> Rotation -> Integer
rotate position (Rotation L step) = let newPosition = position - (step `mod` 100)
  in
    if newPosition >= 0
       then newPosition
       else 100 + newPosition

rotate position (Rotation R step) = let newPosition = position + (step `mod` 100)
  in
    if newPosition < 100
       then newPosition
       else newPosition - 100
