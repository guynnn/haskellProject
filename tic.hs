module Main where

 
import Data.List as List
import Data.Char

-------------------------------DataTypes:---------------------------------------

data Player = PlayerX | PlayerO deriving (Eq, Show)
data Piece = X | O deriving (Eq, Show)
type Tile = Maybe Piece
type Location = Int
type Board = [(Location, Tile)]

data GameState = GameState {
    board :: Board,
    turn :: Player,
    winner :: Maybe Player,
    record :: Record Move
} deriving (Show)

data Rank = N | P | D deriving Eq

data BoardState = BoardState {
    curBoard :: Board,
    rank :: Rank,
    choice :: Board
}

data Tree a = Node a [Tree a] | Leaf a deriving Eq
type Move = (String, Location)
data Record a = Element a (Record a) | Empty deriving (Eq)

---------------------Functor, Monad & others :)--------------------------------

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a sons) = Node (f a) (map (fmap f) sons)


instance Functor Record where
    fmap f Empty = Empty
    fmap f (Element m record) = Element (f m) (fmap f record)


instance Applicative Record where
    pure a = Element a Empty
    Empty <*> _ = Empty
    (Element f r) <*> Empty = Empty
    (Element f r1) <*> (Element b r2) = Element (f b) (fmap f r2)


instance Foldable Record where
    foldMap f Empty = mempty
    foldMap f (Element a r) = (f a) `mappend` (foldMap f r)

joinR :: Record a -> Record a -> Record a
joinR Empty r = r
joinR (Element a Empty) r = (Element a r)
joinR (Element a r1) r2 = Element a (joinR r1 r2) 


concatRecord :: Record (Record a) -> Record a
concatRecord r = foldl joinR Empty r 


instance Monad Record where
    return a = Element a Empty
    record >>= f = concatRecord $ fmap f record

instance Monoid (Record a) where
    mempty = Empty
    mappend = joinR

instance (Show a) => Show (Record a) where
   show Empty = "End"
   show (Element a r) = show a ++ " -> " ++ (show r)

-------------------------------Examples-----------------------------------------

{- some Examples of using Monad, Monoid and applicative:
**Monad**:
  let x = Element (2 :: Int) (Element (1 :: Int) Empty)
      y = Element (3 :: Int) (Element (6 :: Int) Empty)
      w = Element (4 :: Int) (Element (9 :: Int) Empty)
      z = Element x (Element y Empty)
      m = x >>= (\s-> return (s+1))

**Monoid**:
     -- Identity law:
  putStrLn $ "Record obeys the Identity law w.r.t \"Empty\""
  print $ (x `mappend` mempty) == x  &&  (mempty `mappend` x) == x
  
  -- Associative law:
  putStrLn $ "Record is associative."
  print $ (x `mappend` y) `mappend` w == x `mappend` (y `mappend` w)

**Applicative**:

  let f = \x -> x + 2
      g = \x -> x - 2
      a = Element f Empty
      b = Element g Empty
  
  print $  a <*> x
  print $  b <*> w  

for a demo of this examples please go to https://repl.it/repls/CleverClosedVariables
-}


---------------------------other Record functions-------------------------------

addToRecord :: Record Move -> Move -> Record Move
addToRecord record@(Empty) move = Element move record
addToRecord (Element m record) move = Element m (addToRecord record move)


recordToStr :: Record Move -> String
recordToStr Empty = "---------------"
recordToStr (Element m record) = "---------------\n" ++ (fst m) ++ " : " ++ (show $ snd m) ++ "\n" ++ (recordToStr record) 

------------------------------The Main Program----------------------------------

winStates :: [[Int]]
winStates = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]]

makeTriple [] = []
makeTriple xs = take 3 xs : makeTriple (drop 3 xs)

emptyBoard :: Board
emptyBoard =  [(0, Nothing), (1, Nothing), (2, Nothing),
               (3, Nothing), (4, Nothing), (5, Nothing),
               (6, Nothing), (7, Nothing), (8, Nothing)]

initialState :: GameState
initialState = GameState {
    board = emptyBoard,
    turn = PlayerX,
    winner = Nothing,
    record = Empty
}

toString :: Board -> String
toString b =
    let l = fmap (maybe "." show) $ fmap snd b
        threes = makeTriple l
        withCols = fmap (List.intersperse "|") threes
        withRows = List.intersperse ["\n-----\n"] $ withCols
    in concat $ concat withRows


intersection :: (Eq a) => [a] -> [a] -> Bool
intersection [] ys = False
intersection (x:xs) ys = (elem x ys) || intersection xs ys


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs


isWinState :: Piece -> Board -> Bool
isWinState p b =
    -- find all subset of the given piece locations that their length is 3
    let locations = filter (\x -> length x == 3) $ List.subsequences [ x | (x,y) <- b, y == Just p]
    in intersection locations winStates


didWin :: Board -> Maybe Player
didWin board
  | isWinState X board = Just PlayerX
  | isWinState O board = Just PlayerO
  | otherwise = Nothing


playerToPiece :: Player -> Piece
playerToPiece PlayerX = X
playerToPiece PlayerO = O


isValidDigit :: Char -> Bool
isValidDigit a
  | elem a (take 9 ['0'..]) = True
  | otherwise = False


step :: Location -> GameState -> GameState
step location (GameState board turn winner record)
  | snd (board !! location) == Nothing =
                       let piece = playerToPiece turn
                           board' = replaceNth location (location, Just piece) board
                           turn' = if turn == PlayerX then PlayerO else PlayerX
                           winner' = didWin board'
                           move = (show turn, location)
                           record' = addToRecord record move  
                       in GameState board' turn' winner' record'
  | otherwise = GameState board turn winner record


emptyIndexes :: Board -> [Int]
emptyIndexes b = [x | (x, y)<- b, y == Nothing]


chooseNext :: [BoardState] -> Board
chooseNext nodes = let ranks = fmap rank nodes
                   in if (elem P ranks) then head [(curBoard bs) | bs <- nodes, (rank bs) == P]
                      else head [(curBoard bs) | bs <- nodes, (rank bs) == D]


diff :: Board -> Board -> Location
diff board1 board2 = head [loc1 | (loc1, val1) <- board1, (loc2, val2) <- board2, loc1 == loc2, val1 /= val2]


swap :: Player -> Player -> Bool
swap p1 p2 = if p1 == p2 then False else True


rankNode :: [Rank] -> Rank
rankNode ranks = if (any (P==) ranks) then N
               else if (any (D==) ranks) then D
               else P 


rankLeaf :: Board -> Rank
rankLeaf board = if (isWinState X board) || (isWinState O board) then P else D


initBoardState :: Board -> BoardState
initBoardState board = BoardState {curBoard = board, rank = P, choice = board}


isLeaf :: Board -> Bool
isLeaf board = let idxs = emptyIndexes board
               in if (null idxs) || (isWinState X board) || (isWinState O board) then True
                  else False


boardTree :: Board -> Piece -> Tree Board
boardTree board p = let idxs = emptyIndexes board
                 in if (isLeaf board) then Leaf board
                    else let values = [(loc, Just p) | loc <- idxs]
                             boards = replicate (length idxs) board
                             p' = if p == X then O else X
                             pieces = replicate (length idxs) p'
                             sons = zipWith3 replaceNth idxs values boards
                             subTree = zipWith boardTree sons pieces
                         in Node board subTree


toGameTree :: Tree Board -> Tree BoardState
toGameTree = fmap initBoardState


updateRankAndChoice :: Tree BoardState -> BoardState
updateRankAndChoice (Leaf (BoardState b r c)) = let rank' = rankLeaf b
                                      in BoardState {curBoard = b, rank = rank', choice = c}
updateRankAndChoice (Node (BoardState b r c) sons) = let subTree = fmap updateRankAndChoice sons
                                                         rank' = rankNode (fmap rank subTree)
                                                         choice' = chooseNext subTree
                                           in BoardState {curBoard = b, rank = rank', choice = choice'}


nextMove :: Board -> Piece -> Int
nextMove board p = let nextBoard = choice $ updateRankAndChoice $ toGameTree $ boardTree board p
                   in  diff board nextBoard


makeMove :: GameState -> Bool -> IO (GameState)
makeMove (GameState board turn' winner record) pc_turn = do
      putStrLn $ toString board
      let state = GameState {board = board, turn = turn', winner = winner, record = record}
      if (null $ emptyIndexes board) then do
                                          putStrLn "\ndraw!\n"
                                          return state
      else 
        case winner of -- check for winner!
             Nothing -> do -- no winner
                 if pc_turn then do putStrLn "\nPC move:\n" -- PC's turn:
                                    let piece = playerToPiece turn'
                                        state' = step (nextMove board piece) state
                                    makeMove state' False 
                 else do -- user's turn:
                     putStrLn $ show turn' ++ "'s enter a digit (0-8):"
                     loc <- getLine
                     if (length loc) > 1 then do 
                        putStrLn "Error: more than one digit!"
                        makeMove state False
                     else if not (isValidDigit (head loc)) then do
                        putStrLn "Error: invalid digit!"
                        makeMove state False
                     else --valid move:
                        let state' = step (Data.Char.digitToInt (head loc)) state 
                        in makeMove state' (swap (turn state') (turn state)) 
             Just player -> do -- game over
                putStrLn $ show player ++ " has won!\n"
                let record_new = fmap (\x-> if (fst x) /= (show turn') then ((fst x) ++ " :)", (snd x)) else x) record
                putStrLn ("The moves were:\n\n" ++ (recordToStr record_new))  
                return state


main = do
    makeMove initialState False


