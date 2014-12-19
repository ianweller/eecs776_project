-- Ian Weller / 2428096
-- EECS 776 final project: Scrabble
-- You should probably not distribute this software because some lawyers might get mad

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import System.Random

jointuple a b = (a,b)
replacel n l with = concat[take n l,take (length l - n) with,drop (n + length with) l]
replace n l with = replacel n l [with]
replacef n l f = replace n l (f (l !! n))
-- http://stackoverflow.com/a/5121537
mayberead = fmap fst . listToMaybe . reads

-- Type for storing the Scrabble game state
data ScrabbleGame = ScrabbleGame { board :: [[Char]]
                                 , racks :: [[Char]]
                                 , scores :: [Int]
                                 , bag :: [Char]
                                 , turn :: Int
                                 , dict :: [String]
                                 } deriving (Eq,Ord)

instance Show ScrabbleGame where
    show sg = concat $ intersperse "\n" (["    " ++ map fullwidth ['a'..'o'],boardedge]
                                      ++ [ boardrow (board sg !! (n - 1)) n | n <- [1..15] ]
                                      ++ [boardedge,"P" ++ show (turn sg) ++ ": " ++ rackformat (currentrack sg)])
              where boardedge = "   +" ++ (replicate 30 '-') ++ "+"
                    rackformat s = concat $ intersperse " " [ [c] ++ "(" ++ show (letterscore c) ++ ")" | c <- s ]
                    -- in these functions, n = row, m = column (starting at 1!)
                    -- i have no idea why n comes before m. i was not very awake when writing those bits apparently
                    boardrow r n = concat [if (n < 10) then " " ++ (show n) else show n," |"
                                             , concat [ boardcell (r !! (m - 1)) n m | m <- [1..15] ],"|"
                                             , (extradata n)]
                    extradata r
                        -- show player names above scores
                        | r == 3 = (++) "  " $ concat $ intersperse " " [ leftpad ("P" ++ show p) 4 | p <- [1..length $ racks sg] ]
                        -- show scores
                        | r == 4 = (++) "  " $ concat $ intersperse " " [ leftpad (show score) 4 | score <- scores sg ]
                        -- letters remaining
                        | r == 6 = (++) "  " $ "Letters remaining: " ++ show (length $ bag sg)
                        | otherwise = ""
                    boardcell c n m
                        | c == ' ' && n == 8 && m == 8 = boardcolor "\xff0a" n m
                        | c == ' '                     = boardcolor "  " n m
                        | otherwise                    = [fullwidth c]
                    boardcolor s n m = colorstr s (cellcolor n m)

-- newgame p: set up a new board with p players
newgame :: Int -> StdGen -> [String] -> ScrabbleGame
newgame p g words =
    --let bag = chunksOf 7 $ shuffle letterset
    let bag = chunksOf 7 (shuffle letterset g)
    in  ScrabbleGame { board = replicate 15 $ replicate 15 ' '
                     , racks = take p bag
                     , scores = replicate p 0
                     , bag = concat $ drop p bag
                     , turn = 1
                     , dict = words
                     }

currentrack sg = racks sg !! ((turn sg) - 1)
players sg = length (racks sg)

-- remove element from a list by index
removeElement n xs = [ xs !! (x - 1) | x <- [1..length xs], x - 1 /= n ]

-- randomly shuffle a list
shuffle :: [a] -> StdGen -> [a]
shuffle [] g = []
shuffle xs g =
    let (n,_) = randomR (0,length xs - 1) g
    in  xs !! n : shuffle (removeElement n xs) g

-- the letter set
letterset = concatMap (uncurry $ concatMap . replicate)
    [(1,"KJXQX"),(2,"BCMPFHVWY"),(3,"G"),(4,"LSUD"),(6,"NRT"),(8,"O"),(9,"AI"),(12,"E")]
_letterscore c
    | elem c "QZ"         = 10
    | elem c "JX"         = 8
    | elem c "K"          = 5
    | elem c "FHVWY"      = 4
    | elem c "BCMP"       = 3
    | elem c "DG"         = 2
    | elem c "EAIONRTLSU" = 1
letterscore = (_letterscore . toUpper)

-- this isn't the OSPD but it'll do. maybe
validwords :: IO [String]
validwords = do
    let validword s = and [all (flip elem ['a'..'z']) s,length s <= 15]
    f <- readFile "/usr/share/dict/words"
    return $ filter validword $ lines f

scoreplay :: ScrabbleGame -> Play -> Either String (ScrabbleGame,Int)
scoreplay sg (Play _word (x,y) dir)
    | not $ elem (map toLower _word) (dict sg) = Left "That's not a word!"
    | otherwise                                =
        either (\msg -> Left msg)
               (\(sg,score) -> Right (ScrabbleGame { board = board sg
                                                   , racks = replace ((turn sg) - 1) (racks sg) $
                                                             (currentrack sg) ++ (take (7 - (length $ currentrack sg)) (bag sg))
                                                   , scores = replacef ((turn sg) - 1) (scores sg) $ (+) score
                                                   , bag = drop (7 - (length $ currentrack sg)) (bag sg)
                                                   , turn = (mod (turn sg) (players sg)) + 1
                                                   , dict = dict sg
                                                   }, score))
               $ scorer sg 0 id n m word
    -- in these functions, n = row, m = column (starting at 1!)
    -- Vertical: n increases; Horizontal: m increases
    where
        word = map toUpper _word
        n = y
        m = colnum x
        errorchecks = [ (n > 0 && m > 0,"The word would fall off the board.")
                      , (length word > 0,"You somehow entered a 0-length word and we are all confused.")
                      ] ++ firstplaychecks ++ hchecks ++ vchecks
        firstplaychecks = if board sg == (replicate 15 $ replicate 15 ' ')
                              then [ (dir == Horizontal,"The first play must be horizontal.")
                                   , (n == 8,"The first play must be on row 8.")
                                   , (n <= 8 && n + (length word) >= 9,"The first play must cover the square h8.")
                                   ]
                              else []
        hchecks = if dir == Horizontal then [(m + (length word) <= 16,"The word would fall off the board.")] else []
        vchecks = if dir == Vertical   then [(n + (length word) <= 16,"The word would fall off the board.")] else []
        errormsg = snd <$> find (\(cond,msg) -> cond == False) errorchecks
        scorer sg score mult n m [] = Right (sg,mult score)
        scorer sg score mult n m (c:cs) =
            if currentc == ' '
                then if elem c (currentrack sg) then scorer' else Left "You don't have the letters to play that word."
                else if currentc == c then scorer' else Left "There's another letter in the way."
            where currentc = ((board sg) !! (n - 1)) !! (m - 1)
                  (score',mult') = scoreletter sg c n m
                  sg' = ScrabbleGame { board = replace (n - 1) (board sg) $ replace (m - 1) (board sg !! (n - 1)) c
                                     , racks = if currentc == c then racks sg
                                                   else replace ((turn sg) - 1) (racks sg) $ delete c (currentrack sg)
                                     , scores = scores sg
                                     , bag = bag sg
                                     , turn = turn sg
                                     , dict = dict sg
                                     }
                  n' = if dir == Vertical then n + 1 else n
                  m' = if dir == Horizontal then m + 1 else m
                  scorer' = scorer sg' (score + score') (mult' . mult) n' m' cs

-- returns (score,function that represents multiplicative word bonus)
scoreletter :: ScrabbleGame -> Char -> Int -> Int -> (Int,Int -> Int)
scoreletter sg c n m = (lettermult base,wordmult)
    where base = letterscore c
          bonus = fromMaybe Letter $ snd <$> cellbonus n m
          lettermult = if bonus == Letter then fromMaybe id $ bonusmult <$> fst <$> cellbonus n m else id
          wordmult = if bonus == Word then fromMaybe id $ bonusmult <$> fst <$> cellbonus n m else id

data BonusMult = Double | Triple
    deriving (Show,Eq,Ord)
data BonusType = Word | Letter
    deriving (Show,Eq,Ord)

bonusmult b
    | b == Double = (*) 2
    | b == Triple = (*) 3

cellbonus n m
    | n == 8 && m == 8                             = Just (Double,Word)
    | elem n [2,14] && elem m [2,14]               = Just (Double,Word)
    | elem n [1,8,15] && elem m [1,8,15]           = Just (Triple,Word)
    | elem n [2,6,10,14] && elem m [2,6,10,14]     = Just (Triple,Letter)
    | elem n [7,9] && elem m [7,9]                 = Just (Double,Letter)
    | n == m                                       = Just (Double,Word)
    | n == (16 - m)                                = Just (Double,Word)
    | elem n [1,4,8,12,15] && elem m [1,4,8,12,15] = Just (Double,Letter)
    | elem n [3,7,9,13] && elem m [3,7,9,13]       = Just (Double,Letter)
    | otherwise                                    = Nothing

bonuscolor (Double,Word) = Pink
bonuscolor (Triple,Word) = Red
bonuscolor (Double,Letter) = Cyan
bonuscolor (Triple,Letter) = Blue

cellcolor n m = bonuscolor <$> cellbonus n m

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

data Direction = Horizontal | Vertical
    deriving (Show,Eq,Ord)
data Command = QuitCommand | PlayCommand Play
data Play = Play { word :: String
                 , xy :: (Char,Int)
                 , dir :: Direction
                 } deriving (Show,Eq,Ord)

colnum x = (ord $ toLower x) - (ord 'a') + 1
readdir d
    | elem d ["h","H","right","across"] = Just Horizontal
    | elem d ["v","V","down"]           = Just Vertical
    | otherwise                         = Nothing

getcommanderror :: String -> ScrabbleGame -> IO Command
getcommanderror msg sg = do
    putStrLn msg
    getcommand sg
invalidcommand = getcommanderror "Invalid command."

getcommand :: ScrabbleGame -> IO Command
getcommand sg = do
    putStrLn "Commands: play [word] xy [h|v] | quit"
    putStr "> "
    hFlush stdout
    command_str <- getLine
    let command = words $ map toLower command_str
    if length command > 0
        then case (command !! 0) of
            "quit" -> return QuitCommand
            "play" -> do
                if length command == 4
                    then do
                        let xy = command !! 2
                        let x = xy !! 0
                        let y = mayberead $ drop 1 xy
                        let dir = readdir $ command !! 3
                        if (elem x ['a'..'o']) || (elem (fromMaybe 0 y) [1..15])
                            then if dir == Nothing
                                 then getcommanderror "Invalid dir -- use \"h\" or \"v\"." sg
                                 else return $ PlayCommand (Play { word = command !! 1
                                                                 , xy = (x,fromMaybe 0 y)
                                                                 , dir = fromMaybe Horizontal dir
                                                                 })
                            else getcommanderror "Invalid xy -- x must be in ['a'..'o'], y must be in [1..15]." sg
                    else invalidcommand sg
            _ -> invalidcommand sg
        else invalidcommand sg

gameloop = gameloop' True

gameloop' :: Bool -> ScrabbleGame -> IO ()
gameloop' showboard sg = do
    when showboard (putStrLn $ show sg)
    command <- getcommand sg
    case command of
        QuitCommand -> return ()
        PlayCommand p -> do
            let result = scoreplay sg p
            either (\msg -> do { putStrLn msg; gameloop' False sg })
                   (\(sg',score) -> do { putStrLn $ "Scored " ++ (show score) ++ " points!"; gameloop sg' })
                   result

main = do
    putStr "How many players? "
    hFlush stdout
    num_players <- getLine
    when (read num_players < 1) $ error "too few players"
    when (read num_players > 4) $ error "too many players"
    g <- getStdGen
    words <- validwords
    gameloop (newgame (read num_players) g words)
