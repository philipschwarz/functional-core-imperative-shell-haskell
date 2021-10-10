module Main where

import Lib

type Pos = (Int,Int)
type Board = [Pos]

pulsar :: Board
pulsar = [(4, 2),(5, 2),(6, 2),(10, 2),(11, 2),(12, 2),          
                 (2, 4),(7, 4),( 9, 4),(14, 4),          
                 (2, 5),(7, 5),( 9, 5),(14, 5),          
                 (2, 6),(7, 6),( 9, 6),(14, 6),   
          (4, 7),(5, 7),(6, 7),(10, 7),(11, 7),(12, 7),
          (4, 9),(5, 9),(6, 9),(10, 9),(11, 9),(12, 9),          
                 (2,10),(7,10),( 9,10),(14,10),          
                 (2,11),(7,11),( 9,11),(14,11),          
                 (2,12),(7,12),( 9,12),(14,12),
          (4,14),(5,14),(6,14),(10,14),(11,14),(12,14)]

main :: IO ()
main = life(pulsar)

life :: Board -> IO ()  
life b = do cls              
            showcells b  
            goto (width + 1, height + 1)            
            wait 500000
            life (nextgen b)

-------------------------------------------------

cls :: IO ()
cls = putStr "\ESC[2J"
            
showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

-------------------------------------------------

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p                  
                  putStr xs
                  
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" 
                             ++ show x ++ "H")
                                    
-------------------------------------------------

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),                   
                isEmpty b p,                   
                liveneighbs b p == 3]
                
-------------------------------------------------
            
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

isEmpty :: Board -> Pos -> Bool 
isEmpty b p = not (isAlive b p)

liveneighbs :: Board -> Pos -> Int 
liveneighbs b = length . filter(isAlive b) . neighbs

isAlive :: Board -> Pos -> Bool 
isAlive b p = elem p b 

-------------------------------------------------

neighbs :: Pos -> [Pos] 
neighbs (x,y) = map wrap [(x-1, y-1), (x,   y-1),             
                          (x+1, y-1), (x-1, y  ),             
                          (x+1,   y), (x-1, y+1),             
                          (x,   y+1), (x+1, y+1)]

wrap :: Pos -> Pos 
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

width :: Int
width = 20

height :: Int 
height = 20
       
            
            


