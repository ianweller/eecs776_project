-- Ian Weller / 2428096
-- EECS 776 final project: Scrabble
-- You should probably not distribute this software because some lawyers might get mad

import Data.Char
import Data.List.Split
import System.Random

-- Type for storing the Scrabble game state
data ScrabbleGame = ScrabbleGame { board :: [[Char]]
                                 , racks :: [[Char]]
                                 , scores :: [Int]
                                 , bag :: [Char]
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
    | c == 'K'            = 5
    | elem c "FHVWY"      = 4
    | elem c "BCMP"       = 3
    | elem c "DG"         = 2
    | elem c "EAIONRTLSU" = 1
    | c == '_'            = 0

-- newgame p: set up a new board with p players
newgame :: Int -> StdGen -> ScrabbleGame
newgame p g =
    --let bag = chunksOf 7 $ shuffle letterset
    let bag = chunksOf 7 (shuffle letterset g)
    in  ScrabbleGame { board = replicate 15 $ replicate 15 ' '
                     , racks = take p bag
                     , scores = replicate p 0
                     , bag = concat $ drop p bag
                     }

-- printgame sg p: print the game board and the rack for player p
printgame :: ScrabbleGame -> Int -> IO ()
printgame sg p = do
    putStrLn boardedge
    mapM putStrLn $ map (\n -> boardrow (board sg !! (n - 1)) n) [1..15]
    putStrLn boardedge

boardedge = "+" ++ (replicate 30 '-') ++ "+"

-- in these functions, n = row, m = column (starting at 1!)
boardrow :: String -> Int -> String
boardrow r n = "|" ++ concatMap (\m -> boardcell (r !! (m - 1)) n m) [1..15] ++ "|"

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

main = do
    g <- getStdGen
    let game = newgame 2 g
    printgame game 0
