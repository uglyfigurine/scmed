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
import GHC.Base (NonEmpty)
import System.Random

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

randomRDef :: (Enum a, RandomGen g) => (a, a) -> g -> (a, g)
randomRDef (lw, up) g = (toEnum a, g') where
      (a, g') = randomR (fromEnum lw, fromEnum up) g

randomDef :: (RandomGen g, Bounded a, Random a) => g -> (a, g)
randomDef g = randomR (minBound, maxBound) g

data Axis = X | Y | Z deriving (Show, Enum, Bounded)
instance Random Axis where
  randomR = randomRDef
  random  = randomDef

data Direction = Clockwise | AntiClockwise deriving (Show, Enum, Bounded)
instance Random Direction where
  randomR = randomRDef
  random  = randomDef

data Move = Move Int Axis Direction deriving (Show)
move :: Int -> Axis -> Direction -> Validation String Move
move i a d = Move <$> validateIndex i <*> pure a <*> pure d

-- randomMoves :: (RandomGen g) => Int -> g -> [Validation String Move]
randomMoves n g = fmap move' randomInputs
                  where
                    move' = uncurry3 move
                    randomInputs = zip3 (take n $ randomRs (0, 2) g :: [Int])
                                        (take n $ randoms g :: [Axis])
                                        (take n $ randoms g :: [Direction])

uncurry3 f = \(x,y,z) -> f x y z

applyMove :: Cube -> Move -> Cube
applyMove cube (Move i X d) = undefined
applyMove cube (Move i Y d) = undefined
applyMove cube (Move i Z d) = undefined

{-
-- revisit this instance
-- "Takes a range (lo,hi) and a random number generator g, and returns a random value uniformly distributed in the closed interval [lo,hi], together with a new generator."
instance Random Move where
  randomR _ = random

  random g = (Move rInt rAxis rDirection, g''')
    where
     (rInt, g')         = random g   -- :: RandomGen g => (Int, g)
     (rAxis, g'')       = random g'  -- :: RandomGen g => (Axis, g)
     (rDirection, g''') = random g'' -- :: RandomGen g => (Direction, g)
-}

initialCube = Cube  <$> frontFace
                    <*> backFace
                    <*> topFace
                    <*> bottomFace
                    <*> rightFace
                    <*> leftFace
                    where
                      frontFace   = Face <$> (fmap $ Tile White)  <$> (sequenceA $ position <$> [0, 1, 2] <*> [0, 1, 2] <*> [2])
                      backFace    = Face <$> (fmap $ Tile Green)  <$> (sequenceA $ position <$> [0, 1, 2] <*> [0, 1, 2] <*> [0])
                      topFace     = Face <$> (fmap $ Tile Yellow) <$> (sequenceA $ position <$> [0, 1, 2] <*> [2]       <*> [0, 1, 2])
                      bottomFace  = Face <$> (fmap $ Tile Orange) <$> (sequenceA $ position <$> [0, 1, 2] <*> [0]       <*> [0, 1, 2])
                      rightFace   = Face <$> (fmap $ Tile Blue)   <$> (sequenceA $ position <$> [2]       <*> [0, 1, 2] <*> [0, 1, 2])
                      leftFace    = Face <$> (fmap $ Tile Red)    <$> (sequenceA $ position <$> [0]       <*> [0, 1, 2] <*> [0, 1, 2])

