module Snake where

-- Imports --
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(...), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)


-- Types --
data Game = Game
    { _snake  :: Snake,         -- Snake as a sequence of points
      _dir    :: Direction,     -- Direction of snake
      _food   :: Coord,         -- Location of the food
      _foods  :: Stream Coord,  -- Infinite list of food locations  
      _dead   :: Bool,          -- Game over flag
      _paused :: Bool,          -- Paused flag
      _score  :: Int,           -- Player score
      _frozen :: Bool           -- Freeze to disallow duplicate turns
    } deriving (Show)

type Coord = V2 Int
type Snake = Seq Coord

data Stream a = a :| Stream as
    deriving (Show)

data Direction
    = North | East | South | West
    deriving (Eq, Show)


-- Core functions --
-- Step forward in time
step :: Game -> Game
step g = fromMaybe g $ do
    guard (not $ g ^. paused || g ^. dead)
    let g' = g & frozen .~ False
    return . fromMaybe (move g') $ die g' <|> eatFood g'

-- Possibly die if next head position is disallowed
die :: Game -> Maybe Game

-- Possibly eat food if next head position is food
eatFood :: Game -> Maybe Game

-- Move snake along 
move :: Game -> Game

-- Turn game direction (orthogonally)
turn :: Direction -> Game -> Game

-- Initialise a paused game with random food location
initGame :: IO Game