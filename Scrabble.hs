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
removeElement n xs = (\(l,r) -> l ++ drop 1 r) $ splitAt n xs

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

boardrow :: String -> Int -> String
boardrow r n = "|" ++ r ++ "|"

fullwidth s = s

main = do
    g <- getStdGen
    let game = newgame 2 g
    printgame game 0
