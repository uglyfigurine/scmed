module Rubyx
(
-- * Data Types 
  Position
, Color(..)
, Tile(..)
, Face(..)
, Cube(..)
, Axis(..)
, Direction(..)
, Move
-- * smart constructors
, position
, move
-- * functions
, validateIndex
, initialCube
) where

import Data.Validation
import GHC.Base

data Position = Position {
                x :: Int
              , y :: Int
              , z :: Int
              } deriving (Show)

position :: Int -> Int -> Int -> Validation (NonEmpty String) Position
position x y z = Position <$> vx <*> vy <*> vz 
                  where
                    vx = validationNel $ toEither $ validateIndex x
                    vy = validationNel $ toEither $ validateIndex y
                    vz = validationNel $ toEither $ validateIndex z

validateIndex :: Int -> Validation String Int
validateIndex i 
  | i < 0     = Failure $ "Negative value found " <> show i
  | i > 2     = Failure $ "Value greater than 2 found " <> show i
  | otherwise = Success i

data Color = White | Green | Yellow | Orange | Red | Blue deriving (Show)

data Tile = Tile Color Position deriving (Show)

data Face = Face [Tile] deriving (Show)

data Cube = Cube {
            front   :: Face
          , back    :: Face
          , top     :: Face
          , bottom  :: Face
          , right   :: Face
          , left    :: Face
          } deriving (Show)

data Axis = X | Y | Z deriving (Show)
data Direction = Clockwise | AntiClockwise deriving (Show)

data Move = Move Int Axis Direction deriving (Show)
move i a d = Move <$> validateIndex i <*> pure a <*> pure d

initialCube = Cube  <$> frontFace 
                    <*> backFace
                    <*> topFace
                    <*> bottomFace
                    <*> rightFace
                    <*> leftFace
                    where 
                      frontFace   = Face <$> (liftA $ Tile White)  <$> (sequenceA $ position <$> [0, 1, 2] <*> [0, 1, 2] <*> [2])
                      backFace    = Face <$> (liftA $ Tile Green)  <$> (sequenceA $ position <$> [0, 1, 2] <*> [0, 1, 2] <*> [0])
                      topFace     = Face <$> (liftA $ Tile Yellow) <$> (sequenceA $ position <$> [0, 1, 2] <*> [2]       <*> [0, 1, 2])
                      bottomFace  = Face <$> (liftA $ Tile Orange) <$> (sequenceA $ position <$> [0, 1, 2] <*> [0]       <*> [0, 1, 2])
                      rightFace   = Face <$> (liftA $ Tile Blue)   <$> (sequenceA $ position <$> [2]       <*> [0, 1, 2] <*> [0, 1, 2])
                      leftFace    = Face <$> (liftA $ Tile Red)    <$> (sequenceA $ position <$> [0]       <*> [0, 1, 2] <*> [0, 1, 2])



