module UI where

-- Imports --
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Snake

import Brick
    (App(..), AttrMap, BrickEvent(..), EventM, Next, Widget,
    customMain, neverShowCursor, continue, halt, hLimit,
    vLimit, vBox, hBox, padRight, padLeft, padTop, padAll,
    Padding(..), withBorderStyle, str, attrMap, withAttr,
    emptyWidget, AttrName, on, fg, (<+>))

import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))


-- Types --
-- Each Tick measures the passing of time
data Tick = Tick

-- Named resources
type Name = ()

-- Each Cell on the grid
data Cell = Snake | Food | Empty

-- App definition
app :: App Game Tick Name
app = App { appDraw = drawUI,
            appChooseCursor = neverShowCursor,
            appHandleEvent = handleEvent,
            appStartEvent = return,
            appAttrMap = const theMap  
          }


-- Handling events --
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g


-- Drawing --
drawUI :: Game -> [Widget Name]
drawUI g = 
    [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
    $ vBox [ drawScore (g ^. score),
             padTop (Pad 2) $ drawGameOver (g ^. dead)
            ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Score")
    $ C.hCenter
    $ padAll 1
    $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
    if dead
        then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
        else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Snake")
    $ vBox rows
    where
        rows         = [hBox $ cellsInRow r | r <- [height-1, height-2..0]]
        cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
        drawCoord    = drawCell . cellAt
        cellAt c
            | c `elem` g ^. snake = Snake
            | c == g ^. food = Food
            | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str " "

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = undefined


-- Main --
main :: IO ()
main = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100000  -- Decides how fast the game is
    g <- initGame
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

