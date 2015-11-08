import Prelude hiding (Left, Right)
import System.Random
import Data.List
import Text.Printf
import Data.Char (toLower)
import System.IO
type Table = [[Int]]
data Move = Up | Down | Left | Right

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Start"
    table <- initialize
    mainLoop table
    where initialize :: IO Table
          initialize = do table' <- addNumber $ replicate 4 [0, 0, 0, 0]
                          addNumber table'			 
          mainLoop :: Table -> IO ()
          mainLoop table = do printTable table
                              if filter ( == 2048) (concat table) /= [] then putStrLn "Finish" else gameFunction table
          gameFunction :: Table -> IO ()
          gameFunction table
            | (getEmpty table == [] && canMove table == False) = do putStr "Game Over"
            | otherwise = do putStr "Please input [Up, Down, Left, Right]"
                             new_table <- newTable table
                             if table == new_table 
                             then do putStr "Please chose other one\n"
                                     mainLoop table 
                             else do new <- addNumber new_table 
                                     mainLoop new
          canMove :: Table -> Bool
          canMove table = sum ( map (length . getEmpty . flip move table) [Up, Down, Left, Right] ) > 0
          printTable :: Table -> IO ()
          printTable table = do let showRow :: [Int] -> String
                                    showRow = concatMap(printf "%5d")
                                mapM_(putStrLn . showRow) table
          addNumber :: Table -> IO Table
          addNumber table = do let choose :: [a] -> IO a
                                   choose xs = do i <- randomRIO (0, length xs-1)
                                                  return (xs !! i)
                                   setNumber :: Table -> (Int, Int) -> Int -> Table
                                   setNumber table (row, col) val = fst ++ [mid] ++ post
                                                                where fst = take row table
                                                                      mid = take col (table !! row) ++ [val] ++ drop (col + 1) (table!!row)
                                                                      post = drop (row + 1) table                                           
                               target <- choose (getEmpty table)
                               value <- choose [2, 4]
                               return (setNumber table target value)
          getEmpty :: Table -> [(Int, Int)]
          getEmpty table = let singleRow n = zip (replicate 4 n) [0 .. 3]
                               coordinates = concatMap singleRow [0 .. 3]
                           in filter (\(row, col) -> (table!!row) !! col == 0 ) coordinates

          newTable :: Table -> IO Table
          newTable table = do let moves :: [([Char], Move)]
                                  moves = zip ["up", "down", "left", "right"][Up, Down, Left, Right]
                                  moveKey :: IO Move
                                  moveKey = do input <- getLine
                                               case lookup (map toLower input) moves of
                                                   Just x -> return x
                                                   Nothing -> do putStr "Plese choose [Up, Down, Left, Right]"
                                                                 moveKey
                              key <- moveKey
                              let new_table = move key table   
                              return new_table
          move :: Move -> Table -> Table
          move Left = map compute
          move Right = map (reverse . compute . reverse)
          move Up = transpose . move Left . transpose
          move Down = transpose . move Right . transpose
          compute :: [Int] -> [Int]
          compute xs = computed ++ space
                     where combine (x:y:xs) | x == y = x*2 : combine xs
                                            | otherwise = x : combine (y:xs)
                           combine x = x
                           computed  = combine $ filter(/= 0) xs
                           space = replicate (length xs - length computed) 0