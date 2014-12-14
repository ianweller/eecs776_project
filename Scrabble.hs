-- Ian Weller / 2428096
-- EECS 776 final project: Scrabble
-- You should probably not distribute this software because some lawyers might get mad

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import System.IO
import System.Random

-- Type for storing the Scrabble game state
data ScrabbleGame = ScrabbleGame { board :: [[Char]]
                                 , racks :: [[Char]]
                                 , scores :: [Int]
                                 , bag :: [Char]
                                 , turn :: Int
                                 } deriving (Show)

-- remove element from a list by index
removeElement n xs = [ xs !! (x - 1) | x <- [1..length xs], x - 1 /= n ]

-- randomly shuffle a list
shuffle :: [a] -> StdGen -> [a]
shuffle [] g = []
shuffle xs g =
    let (n,_) = randomR (0,length xs - 1) g
    in  xs !! n : shuffle (removeElement n xs) g

-- the letter set
-- in racks, blanks are represented with '_', but are represented on the board
-- by the lowercase form of the letter.
letterset = concatMap (uncurry $ concatMap . replicate)
    [(1,"KJXQX"),(2,"_BCMPFHVWY"),(3,"G"),(4,"LSUD"),(6,"NRT"),(8,"O"),(9,"AI"),(12,"E")]
letterscore c
    | elem c "QZ"         = 10
    | elem c "JX"         = 8
    | elem c "K"          = 5
    | elem c "FHVWY"      = 4
    | elem c "BCMP"       = 3
    | elem c "DG"         = 2
    | elem c "EAIONRTLSU" = 1
    | elem c "_"          = 0

-- newgame p: set up a new board with p players
newgame :: Int -> StdGen -> ScrabbleGame
newgame p g =
    --let bag = chunksOf 7 $ shuffle letterset
    let bag = chunksOf 7 (shuffle letterset g)
    in  ScrabbleGame { board = replicate 15 $ replicate 15 ' '
                     , racks = take p bag
                     , scores = replicate p 0
                     , bag = concat $ drop p bag
                     , turn = 1
                     }

-- printgame sg p: print the game board and the rack for player p
printgame :: ScrabbleGame -> Int -> IO ()
printgame sg p = do
    putStrLn boardedge
    mapM putStrLn [ boardrow sg (board sg !! (n - 1)) n | n <- [1..15] ]
    putStrLn boardedge
    putStrLn $ "P" ++ show (turn sg) ++ ": " ++ rackformat (racks sg !! ((turn sg) - 1))

rackformat s = concat $ intersperse " " [ [c] ++ "(" ++ show (letterscore c) ++ ")" | c <- s ]

boardedge = "+" ++ (replicate 30 '-') ++ "+"

-- in these functions, n = row, m = column (starting at 1!)
boardrow :: ScrabbleGame -> String -> Int -> String
boardrow sg r n = "|" ++ concat [ boardcell (r !! (m - 1)) n m | m <- [1..15] ] ++ "|" ++ (extradata sg n)

extradata :: ScrabbleGame -> Int -> String
extradata sg r
    -- show player names above scores
    | r == 3 = (++) "  " $ concat $ intersperse " " [ leftpad ("P" ++ show p) 4 | p <- [1..length $ racks sg] ]
    -- show scores
    | r == 4 = (++) "  " $ concat $ intersperse " " [ leftpad (show score) 4 | score <- scores sg ]
    -- letters remaining
    | r == 6 = (++) "  " $ "Letters remaining: " ++ show (length $ bag sg)
    | otherwise = ""

boardcell :: Char -> Int -> Int -> String
boardcell c n m
    | c == ' ' && n == 8 && m == 8 = boardcolor "\xff0a" n m
    | c == ' '                     = boardcolor "  " n m
    | otherwise                    = [fullwidth c]

boardcolor :: String -> Int -> Int -> String
boardcolor s n m = colorstr s (cellcolor n m)

data Bonus = DoubleWord | TripleWord | DoubleLetter | TripleLetter

cellbonus :: Int -> Int -> Maybe Bonus
cellbonus n m
    | n == 8 && m == 8                             = Just DoubleWord
    | elem n [2,14] && elem m [2,14]               = Just DoubleWord
    | elem n [1,8,15] && elem m [1,8,15]           = Just TripleWord
    | elem n [2,6,10,14] && elem m [2,6,10,14]     = Just TripleLetter
    | elem n [7,9] && elem m [7,9]                 = Just DoubleLetter
    | n == m                                       = Just DoubleWord
    | n == (16 - m)                                = Just DoubleWord
    | elem n [1,4,8,12,15] && elem m [1,4,8,12,15] = Just DoubleLetter
    | elem n [3,7,9,13] && elem m [3,7,9,13]       = Just DoubleLetter
    | otherwise                                    = Nothing

bonuscolor (Just DoubleWord) = Just Pink
bonuscolor (Just TripleWord) = Just Red
bonuscolor (Just DoubleLetter) = Just Cyan
bonuscolor (Just TripleLetter) = Just Blue
bonuscolor Nothing = Nothing

cellcolor n m = bonuscolor $ cellbonus n m

----------------------------------------
-- character rendering functions / stuff
data Color = Pink | Red | Cyan | Blue | White

colorstr s (Just c) = (colorstart c) ++ s ++ colorend
colorstr s Nothing = s
colorstart Pink = "\x1b[45m"
colorstart Red = "\x1b[41m"
colorstart Cyan = "\x1b[46m"
colorstart Blue = "\x1b[44m"
colorstart White = "\x1b[47m"
colorend = "\x1b[0m"

fullwidth :: Char -> Char
fullwidth = (chr . (65248+) . ord)

leftpad s n = (replicate (n - length s) ' ') ++ s

----------------------------------------

data Command = Quit

getcommand :: IO Command
getcommand = do
    putStrLn "Commands: quit"
    putStr "> "
    hFlush stdout
    command_str <- getLine
    case (map toLower command_str) of
        "quit" -> return Quit
        _ -> do { putStrLn "Invalid command."; getcommand }

gameloop :: ScrabbleGame -> IO ()
gameloop sg = do
    printgame sg 0
    command <- getcommand
    case command of
        Quit -> return ()

main = do
    putStr "How many players? "
    hFlush stdout
    num_players <- getLine
    when (read num_players < 1) $ error "too few players"
    when (read num_players > 4) $ error "too many players"
    g <- getStdGen
    gameloop $ newgame (read num_players) g
