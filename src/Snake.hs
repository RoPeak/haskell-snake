module Snake where

-- Imports
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(...), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)


-- Types
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