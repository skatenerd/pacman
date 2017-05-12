module Main where
import qualified Graphics.Gloss as GL
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Graphics.Gloss.Geometry.Line as LN
import Lib
import Data.Set
import qualified Data.List as L
import Data.Maybe
import Safe
import Debug.Trace
import Data.List.Extras

data GameState

thedisplay = (GL.InWindow "Pac Man Game" (windowSize, windowSize) (10, 10))

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show, Enum)
data WorldState = WorldState { wsInputDirection :: [Direction], wsPlayerLocation :: (Float, Float), wsMap :: GameMap, wsTick :: Int, wsEnemyPosition :: (Float, Float) }
data Segment = VerticalSegment { fixedX :: Float, startY :: Float, endY :: Float } | HorizontalSegment { fixedY :: Float, startX :: Float, endX :: Float } deriving (Ord, Eq, Show)
data GameMap = GameMap { gmTunnels :: Set Segment }

defaultMap :: GameMap
defaultMap = GameMap { gmTunnels = fromList [
  (VerticalSegment { fixedX = 0, startY = -200, endY = 200 } ),
  (HorizontalSegment { fixedY = 0, startX = -200, endX = 200 }),
  (HorizontalSegment { fixedY = 200, startX = -450, endX = 450 }),
  (VerticalSegment { fixedX = 100, startY = -450, endY = 450 }),
  (VerticalSegment { fixedX = -200, startY = -450, endY = 450 })
] }

keyToDirection G.KeyDown = Just DirDown
keyToDirection G.KeyLeft = Just DirLeft
keyToDirection G.KeyUp = Just DirUp
keyToDirection G.KeyRight = Just DirRight
keyToDirection _ = Nothing

segmentToPath VerticalSegment {fixedX = fixedX, startY = startY, endY = endY } = [(fixedX, startY), (fixedX, endY)]
segmentToPath HorizontalSegment {fixedY = fixedY, startX = startX, endX = endX } = [(startX, fixedY), (endX, fixedY)]

drawWorld world = return $  GL.Pictures (tunnelOutlines ++ [playerPicture, enemyPicture])
                  where playerPicture = (uncurry GL.translate) (wsPlayerLocation world) $ GL.Pictures [(GL.Color GL.yellow (GL.circleSolid 20)), mouth]
                        mouthAngle = if even $ ticks `div` 90
                                       then fromIntegral $ mod ticks 90
                                       else fromIntegral $ 90 - (mod ticks 90)
                                     where ticks = (wsTick world) * 10
                        mouth = GL.Color GL.black $ GL.thickArc 0 mouthAngle 10 20
                        tunnelOutlines = fmap drawTunnel $ toList $ gmTunnels $ wsMap world
                        enemyPicture = (uncurry GL.translate) (wsEnemyPosition world) $ GL.Color GL.red $ GL.circleSolid 20

drawTunnel segment = G.Polygon [a,b,d,c]
  where shiftedLeft = shiftLeft segment
        shiftedRight = shiftRight segment
        [a,b] = segmentToPath shiftedLeft
        [c,d] = segmentToPath shiftedRight



shiftLeft vs@VerticalSegment{fixedX = fixedX} = vs {fixedX = (fixedX - 20)}
shiftLeft hs@HorizontalSegment{fixedY = fixedY} = hs {fixedY = (fixedY + 20)}

shiftRight vs@VerticalSegment{fixedX = fixedX} = vs {fixedX = (fixedX + 20)}
shiftRight hs@HorizontalSegment{fixedY = fixedY} = hs {fixedY = (fixedY - 20)}

advanceWorldEvent :: G.Event -> WorldState -> IO WorldState
advanceWorldEvent (G.EventKey (G.SpecialKey pressedKey) toState _ _) world = return $ updateForDirection pressedKey toState world
advanceWorldEvent _ world = return world

updateForDirection :: G.SpecialKey -> G.KeyState -> WorldState -> WorldState
updateForDirection pressedKey G.Down world = addPressedKey pressedKey world
updateForDirection pressedKey G.Up world = if isJust direction
                                             then world{ wsInputDirection = L.delete (fromJust direction) (wsInputDirection world)}
                                             else world
                                           where direction = keyToDirection pressedKey


addPressedKey pressedKey world@WorldState{wsInputDirection=wsInputDirection} = if isJust direction
                                                                                 then world{ wsInputDirection = (fromJust direction) : wsInputDirection }
                                                                                 else world
                                                                               where direction = keyToDirection pressedKey

getBaseDelta Nothing = (0,0)
getBaseDelta (Just DirUp) = (0, 10)
getBaseDelta (Just DirRight) = (10, 0)
getBaseDelta (Just DirDown) = (0, -10)
getBaseDelta (Just DirLeft) = (-10, 0)

bestAIMove world@WorldState{wsEnemyPosition=wsEnemyPosition, wsPlayerLocation=wsPlayerLocation, wsMap=wsMap} = argmin distanceAfterDirection directions
  where directions = [DirUp ..]
        distanceAfterDirection direction = distance (wsPlayerLocation) (positionAfterMove (return direction) wsEnemyPosition wsMap)

distance (a,b) (c,d) = sqrt((a-c)**2 + (b-d)**2)


loopAround value min max
  |  (value < min) = max + (value - min)
  |  (value > max) = min + (value - max)
  | otherwise = value

windowSize = 800
boundLocation (a,b) = ((loopAround a (-400.0) (400.0)), (loopAround b (-400.0) (400.0)))

positionAfterMove (Just direction) startingPosition map = boundLocation nextLocation
  where delta = getBaseDelta $ return direction
        advanceLocation (a,b) (c,d) = (a+c, b+d)
        proposedLocation = advanceLocation startingPosition delta
        nextLocation = if legalMove
                         then proposedLocation
                         else startingPosition
        tunnelList = toList $ gmTunnels map
        legalMove = any (alongSegment startingPosition proposedLocation) tunnelList

positionAfterMove Nothing startingPosition _ = startingPosition

advanceWorldTime seconds world = return $ world { wsPlayerLocation = nextPlayerLocation, wsTick = (wsTick world) + 1, wsEnemyPosition = nextEnemyPosition }
  where nextEnemyPosition = positionAfterMove (return (bestAIMove world)) (wsEnemyPosition world) (wsMap world)
        nextPlayerLocation = positionAfterMove (headMay (wsInputDirection world)) (wsPlayerLocation world) (wsMap world)

alongSegment a b s@HorizontalSegment {fixedY = fixedY, startX = startX, endX = endX } = yMatches && withinSegment
  where yMatches = ((snd a) == fixedY) && ((snd b) == fixedY)
        withinSegment = (isBetween (fst a) startX endX) && (isBetween (fst b) startX endX)

alongSegment a b s@VerticalSegment {fixedX = fixedX, startY = startY, endY = endY } = xMatches && withinSegment
  where xMatches = ((fst a) == fixedX) && ((fst b) == fixedX)
        withinSegment = (isBetween (snd a) startY endY) && (isBetween (snd b) startY endY)

isBetween candidate a b = (candidate <= (max a b)) && (candidate >= (min a b))


main :: IO ()
main = G.playIO thedisplay GL.blue 10 (WorldState [] (0,0) defaultMap 0 (300, 200)) drawWorld advanceWorldEvent advanceWorldTime

