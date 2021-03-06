module BoardUpdate (
                    getC,
                    setC,
                    getNeighbors,
                    hasNeighbors,
                    validPos,
                    validPosBool,
                    isWinner,
                    winnerExists,
                    declareDraw,
                    indices
                    ) where
import Board
import Data.Maybe
import Control.Monad

--Valid indices

indices = [(x, y) | x <- [0..3], y <- [0..11]] :: [(Int, Int)]

--Index helper functions

nextIndex' :: Int -> Maybe Int
nextIndex' curr
  |curr == 11 = Just 0
  |(curr > 11) || (curr < 0) = Nothing
  |otherwise = Just (curr + 1)

prevIndex' :: Int -> Maybe Int
prevIndex' curr
  |curr == 0 = Just 11
  |(curr > 11) || (curr < 0) = Nothing
  |otherwise = Just (curr - 1)

belowIndex' :: Int -> Maybe Int
belowIndex' curr
  |(curr >= 3) || (curr < 0) = Nothing
  |otherwise = Just (curr + 1)

aboveIndex' :: Int -> Maybe Int
aboveIndex' curr
  |(curr <= 0) || (curr > 3) = Nothing
  |otherwise = Just (curr - 1)

getIndex :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int, Int) -> Maybe (Int, Int)
getIndex f g (coords) = maybeListToMaybeTuple [f (fst coords), g (snd coords)]

maybeListToMaybeTuple :: [Maybe a] -> Maybe (a, a)
maybeListToMaybeTuple maybeList
  |(validList (sequence maybeList)) && (length result == 2) = Just (result !! 0, result !! 1)
  |otherwise = Nothing
  where
    validList Nothing = False
    validList _ = True
    result = catMaybes maybeList

upperLeftIndex :: (Int, Int) -> Maybe (Int, Int)
upperLeftIndex = getIndex aboveIndex' prevIndex' 

aboveIndex :: (Int, Int) -> Maybe (Int, Int)
aboveIndex = getIndex aboveIndex' return

upperRightIndex :: (Int, Int) -> Maybe (Int, Int)
upperRightIndex = getIndex aboveIndex' nextIndex'

nextIndex :: (Int, Int) -> Maybe (Int, Int)
nextIndex = getIndex return nextIndex'

lowerRightIndex :: (Int, Int) -> Maybe (Int, Int)
lowerRightIndex = getIndex belowIndex' nextIndex'

belowIndex :: (Int, Int) -> Maybe (Int, Int)
belowIndex = getIndex belowIndex' return

lowerLeftIndex :: (Int, Int) -> Maybe (Int, Int)
lowerLeftIndex = getIndex belowIndex' prevIndex'

prevIndex :: (Int, Int) -> Maybe (Int, Int)
prevIndex = getIndex return prevIndex'

validPos :: (Int, Int) -> Maybe (Int, Int)
validPos (x, y)
  |x `elem` [0..3] && y `elem` [0..11] = Just (x,y)
  |otherwise = Nothing

validPosBool :: (Int, Int) -> Bool
validPosBool (x, y) = case validPos (x, y) of
  Just p -> True
  Nothing -> False
    
--getter helper functions

getStatus :: (Int, Int) -> Board -> Status
getStatus (x, y) board = (board !! x !! y)

getStatusWithfn f (x, y) board
  |isNothing (f (x,y)) = Nothing
  |otherwise = Just (getStatus (fromJust (f (x, y))) board)

--getter functions

getTL = getStatusWithfn upperLeftIndex
getT = getStatusWithfn aboveIndex
getTR = getStatusWithfn upperRightIndex
getR = getStatusWithfn nextIndex
getBR = getStatusWithfn lowerRightIndex
getB = getStatusWithfn belowIndex
getBL = getStatusWithfn lowerLeftIndex
getL = getStatusWithfn prevIndex
getC = getStatusWithfn validPos

--setter helper functions

updateBoard :: (Int, Int) -> Status -> Board -> Board
updateBoard (x, y) status board
  | (upperRows, thisRow : lowerRows ) <- splitAt x board,
    (leftCells, thisCell: rightCells) <- splitAt y thisRow
             = upperRows ++ (leftCells ++ status : rightCells) : lowerRows
  | otherwise = error "index out of range" -- Should never happen

updateBoardfn :: ((Int, Int) -> Maybe (Int, Int)) -> (Int, Int) -> Status -> Board -> Maybe Board
updateBoardfn f (x,y) status board
  | isNothing (f (x, y)) = Nothing
  | otherwise = Just (updateBoard (fromJust (f (x, y))) status board)
                
--setter function

setC = updateBoardfn validPos

--return all neighbor Statuses of a board position
getNeighbors :: (Int, Int) -> Board -> [[Maybe Status]]
getNeighbors (x, y) board = (map . map) ($ board) ((map . map) ($ (x, y)) neighbors)
                           where neighbors = [[getTL, getT, getTR],
                                              [getL,  getC, getR ],
                                              [getBL, getB, getBR]]
                                             
hasNeighbors pos board = any (/= Empty)  (flatNeighbors pos board) where
  flatNeighbors (x, y) board = map fromJust
                             (filter (/= Nothing)
                              (concat
                               (getNeighbors (x, y) board)))
                             
isWinner :: Board -> Status -> Bool
isWinner board status = any (== True)
                        (map (isWinningSpace board status)
                         indices) where
  isWinningSpace board status (i, j) = ((a (i, j)) &&
                                        (a (i, j `addMod12` 1)) &&
                                        (a (i, j `addMod12` 2)) &&
                                        (a (i, j `addMod12` 3))) ||

                                       ((a (0, j)) &&
                                        (a (1, j)) &&
                                        (a (2, j)) &&
                                        (a (3, j))) ||
                        
                                       ((a (0, j)) &&
                                        (a (1, j `addMod12` 1)) &&
                                        (a (2, j `addMod12` 2)) &&
                                        (a (3, j `addMod12` 3))) ||
                        
                                       ((a (0, j)) &&
                                        (a (1, j `addMod12` (-1))) &&
                                        (a (2, j `addMod12` (-2))) &&
                                        (a (3, j `addMod12` (-3))))
    where
      m = (map . map) (== status) board
      addMod12 a b = (a + b) `mod` 12
      a (i, j) = m !! i !! j

winnerExists :: Board -> Bool
winnerExists board
  |isWinner board X || isWinner board O = True
  |otherwise = False

declareDraw board = not ((canWin board X) || (canWin board O))
  where
    canWin board status = isWinner (fillEmptyWithStatus board status) status
      where
        fillEmptyWithStatus board status = (map . map)
                                           (replaceEmptyWith status)
                                           board
          where
            replaceEmptyWith status space
              |space == Empty = status
              |otherwise = space
