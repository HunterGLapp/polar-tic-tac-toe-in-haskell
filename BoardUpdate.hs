module BoardUpdate (getTL, getT, getTR,
                    getL,  getC, getR,
                    getBL, getB, getBR,
                    
                    setTL, setT, setTR,
                    setL,  setC, setR,
                    setBR, setB, setBL,
                    
                    getNeighbors,
                    hasNeighbors,
                    validPos,
                    validPosBool,
                    isWinner,
                    showWinner,
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
                
--setter functions

setTL = updateBoardfn upperLeftIndex
setT = updateBoardfn aboveIndex
setTR = updateBoardfn upperRightIndex
setR = updateBoardfn nextIndex
setBR = updateBoardfn lowerRightIndex
setB = updateBoardfn belowIndex
setBL = updateBoardfn lowerLeftIndex
setL = updateBoardfn prevIndex
setC = updateBoardfn validPos

--return all neighbor Statuses of a board position
getNeighbors :: (Int, Int) -> Board -> [[Maybe Status]]
getNeighbors (x, y) board = (map . map) ($ board) ((map . map) ($ (x, y)) neighbors)
                           where neighbors = [[getTL, getT, getTR],
                                              [getL,  getC, getR ],
                                              [getBL, getB, getBR]]
                                             

putNeighbors (x, y) board = putStr (showNeighbors (getNeighbors (x, y) board)) where
  showNeighbors neighbors =  foldl1 (++)
                           (map (++ "\n")
                           (map show
                           (map
                           (map showMaybeStatus)
                           neighbors)))

hasNeighbors pos board = any (/= Empty)  (flatNeighbors pos board) where
  flatNeighbors (x, y) board = map fromJust
                             (filter (/= Nothing)
                              (concat
                               (getNeighbors (x, y) board)))
                             
isWinner :: Board -> Status -> Bool
isWinner board status = any (== True) (map ($ status ) (map ($ board) (map  winningPos indices))) where
  winningPos (x, y) board status = any (==True) (map ($ status) (map allMine (getNeighborfns (x, y) board)))
  getNeighborfns (x, y) board = (map . map) ($ board)
                              ((map . map) ($ (x, y))
                               winningNeighborFunctions) where
   winningNeighborFunctions = [[getT, getC, getB],
                              [getL, getC, getR],
                              [getTL, getC, getBR],
                              [getTR, getC, getBL]]
  allMine maybeStatuses myMark = all (== True) (map ($ myMark) (map (spaceIsMine) maybeStatuses)) where
  spaceIsMine maybeStatus status
    |isNothing (maybeStatus) = False
    |fromJust maybeStatus == status = True
    |otherwise = False


showWinner :: Board -> String
showWinner board
  |isWinner board X && isWinner board O = "Something went wrong. Two winners"
  |isWinner board X = "X wins!"
  |isWinner board O = "O wins!"
  |otherwise = "No winner yet"


winnerExists :: Board -> Bool
winnerExists board
  |isWinner board X || isWinner board O = True
  |otherwise = False


declareDraw board = not ((canWin board X) || (canWin board O)) where
  canWin board status = isWinner (fillEmptyWithStatus board status) status where
    fillEmptyWithStatus board status = (map . map) (replaceEmptyWith status) board where
      replaceEmptyWith status space
        |space == Empty = status
        |otherwise = space
