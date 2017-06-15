--------------------------------------------------
-- Skeleton for Coursework 1, SDC/AI, 2016-2017 --
-- "Binary Image Manipulations"                 --
-- "By Naeem Rashid  UOB # 14031252"            --
--------------------------------------------------



module Cw2016 where
import Char
import Hugs.Prelude

type BPic = [String]


bpic :: Int -> BPic
bpic 1 = [" XXX ",
          "X   X",
          "XXXXX",
          "X   X",
          "X   X"]

bpic 2 = [" XXX ",
          "X   X",
          "XXXXX",
          "X X",
          "X   X"]

bpic 3 = ["X      XXX   XX   XX  XXX ",
          "X     X   X  X X X X  X  X",
          "X     XXXXX  X  X  X  XXX ",
          "X     X   X  X     X  X   ",
          "XXXXX X   X  X     X  X   "]

bpic 4 = ["    XXXXXXX XX ",
          "  XXXXXXXXXXXXX",
          "    XXX XXXXXXX",
          "    XXX     XXX",
          "X   XXX      XX",
          "X   XX    X  XX",
          "X   XX   XX  X ",
          "    XX   XX  X ",
          "    XXXXXXX    ",
          "    XXXXXXX    ",
          "    XXX XXX    ",
          "    XX   XX  XX",
          "    XXX XXX  XX",
          "    XXX  XX  XX",
          "    XX   XX  XX",
          "    XXX      XX",
          "    XXX      XX",
          "  XXXXXXXXXXXXX",
          " XXXXXXXXXXXXXX",
          "  XXXXXXXXXXX X"]

bpic 5 = ["               ",
          "      XX       ",
          "    XXXXXX     ",
          "  XXXXXXXXXX   ",
          "XXXXXXXXXXXXXX ",
          "  XXXXXXXXXX   ",
          "    XXXXXX     ",
          "      XX       ",
          "               ",
          "               ",
          "               "]

bpic 6 = ["               ",
          "               ",
          "               ",
          "               ",
          "      XX       ",
          "    XXXXXX     ",
          "  XXXXXXXXXX   ",
          "XXXXXXXXXXXXXX ",
          " XXXXXXXXXX    ",
          "  XXXXXX       ",
          "   XX          "]



-----------------------------------------------------------
--                           Task 1                      --
-----------------------------------------------------------
showPic :: BPic -> IO()
showPic listOfString = putStr(concat(map(++"\n") listOfString))

sizePic :: BPic -> (Int , Int)
sizePic [] = (0,0);
sizePic list = ((maximum(map (length ) list)) , (length list) )

isEqual :: [Int]-> Bool
isEqual [] = False
isEqual list = length list==(length(filter (head (list)==) list))

isPic :: BPic -> Bool
isPic [] = False
isPic listOfString = isEqual(map (length ) listOfString) 


-----------------------------------------------------------
--                           Task 2                      --
-----------------------------------------------------------
compChar :: Char -> Char
compChar c 
	| c==' ' = 'X'
	|otherwise = ' '

compString :: String -> String
compString [] = []
compString str = map (compChar) str

compPic :: BPic -> BPic
compPic [] = []
compPic list = map (compString) list

-----------------------------------------------------------
--                           Task 3                      --
-----------------------------------------------------------
charMatch :: Char -> Char ->Char
charMatch x y 
	| x==' ' && y==' ' = ' '
	|otherwise = 'X'

unionString :: String -> String -> String
unionString [] y = []
unionString x [] = []
unionString (x:xt) (y:yt) =(charMatch x y): unionString xt yt   
unionPic :: BPic -> BPic -> BPic
unionPic [] y = []
unionPic x [] = []
unionPic (x:xt) (y:yt) =(unionString x y): unionPic xt yt

isecCharMatch :: Char -> Char ->Char
isecCharMatch  x y 
	| x=='X' && y=='X' = 'X'
	|otherwise = ' '

isecString :: String -> String -> String
isecString [] y = []
isecString x [] = []
isecString (x:xt) (y:yt) =(isecCharMatch x y): isecString xt yt 

isecPic :: BPic -> BPic -> BPic
isecPic [] y = []
isecPic x [] = []
isecPic (x:xt) (y:yt) =(isecString x y): isecPic xt yt

-----------------------------------------------------------
--                           Task 4                      --
-----------------------------------------------------------
rollup ::  BPic -> BPic
rollup [] = []
rollup (x:xt) = xt ++ [x]

rolldown ::  BPic -> BPic
rolldown [] = [] 
rolldown x = (last x):(take ((length x)-1) x )

rollPic :: Int -> BPic -> BPic
rollPic x list
 | x==0 = list
 | list==[] = []
 | x>0 = (rollPic (x-1) (rollup list))
 | otherwise = (rollPic (x+1) (rolldown list))



-----------------------------------------------------------
--                           Task 5                      --
-----------------------------------------------------------
moveLStr :: String -> String
moveLStr [] = []
moveLStr (x:xt) = xt ++ [x]

moveRStr :: String -> String
moveRStr [] = []
moveRStr x = (last x):(take ((length x)-1) x )

moveright :: BPic -> BPic
moveright [] = []
moveright list = map moveRStr list 

moveleft :: BPic -> BPic
moveleft [] = []
moveleft list = map moveLStr list 

movePic :: Int -> BPic -> BPic
movePic x list
 | x==0 = list
 | list==[] = []
 | x>0 = (movePic (x-1) (moveright list))
 | otherwise = (movePic (x+1) (moveleft list))

fusePic :: BPic -> BPic -> BPic
fusePic _[] = []
fusePic []_ = []
fusePic(x:xt)(y:yt)=(x ++ y):fusePic xt yt

-----------------------------------------------------------
--                           Task 6                      --
-----------------------------------------------------------
shiftPic :: Int -> Int -> BPic -> BPic
shiftPic x y [] = []
shiftPic x 0 list = movePic x list
shiftPic 0 y list = rollPic y list
shiftPic x y list = rollPic y (movePic x list)

-----------------------------------------------------------
--                           Task 7                      --
-----------------------------------------------------------
--
-- Main changes are in rollup , rolldown, moveRStr,moveLStr
-- other functions have same functionality but different by name 

rollup_e ::  BPic -> BPic
rollup_e [] = []
rollup_e (x:xt) = xt 

rolldown_e ::  BPic -> BPic
rolldown_e [] = [] 
rolldown_e x = take ((length x)-1) x 

rollPic_e :: Int -> BPic -> BPic
rollPic_e x list
 | x==0 = list
 | list==[] = []
 | x>0 = (rollPic_e (x-1) (rollup_e list))
 | otherwise = (rollPic_e (x+1) (rolldown_e list))



moveLStr_e :: String -> String
moveLStr_e [] = []
moveLStr_e (x:xt) = xt

moveRStr_e :: String -> String
moveRStr_e [] = []
moveRStr_e x = take ((length x)-1) x 

moveright_e :: BPic -> BPic
moveright_e [] = []
moveright_e list = map moveRStr_e list 

moveleft_e :: BPic -> BPic
moveleft_e [] = []
moveleft_e list = map moveLStr_e list 

movePic_e :: Int -> BPic -> BPic
movePic_e x list
 | x==0 = list
 | list==[] = []
 | x>0 = (movePic_e (x-1) (moveright_e list))
 | otherwise = (movePic_e (x+1) (moveleft_e list))

shiftPic_e :: Int -> Int -> BPic -> BPic
shiftPic_e x y [] = []
shiftPic_e x 0 list = movePic_e x list
shiftPic_e 0 y list = rollPic_e y list
shiftPic_e x y list = rollPic_e y (movePic_e x list)



-----------------------------------------------------------
--                           Task 9                      --
-----------------------------------------------------------
mirror_h :: BPic -> BPic
mirror_h [] = [] 
mirror_h list = reverse list

mirror_y :: BPic -> BPic
mirror_y [] = [] 
mirror_y list = map reverse  list

-----------------------------------------------------------
--                           Task 10                      --
-----------------------------------------------------------
pickCol :: BPic -> String
pickCol [] = []
pickCol (x:xt) = (head x): pickCol xt 
dropCol :: BPic -> BPic
dropCol [] = []
dropCol (x:xt)= (drop 1 x): dropCol xt  
moveCol :: (BPic, BPic) -> (BPic, BPic)
moveCol ([] , [] ) = ( [], [] )
moveCol ([] , y) = ([] , y)
moveCol (x , y ) = ((dropCol x) , (pickCol x ):y )
picPairs :: BPic -> [(BPic, BPic)]
picPairs pic = iterate moveCol (pic, [])
checker :: (BPic,BPic) -> Bool
checker (x, y) =  length x/=(length(filter (""==) x))
rotOnce :: BPic -> BPic
rotOnce [] = []
rotOnce list = snd(head(dropWhile(checker ) (picPairs list)))


pickCol' :: BPic -> String
pickCol' [] = []
pickCol' (x:xt) =  pickCol' xt ++ [(last x)]
dropCol' :: BPic -> BPic
dropCol' [] = []
dropCol' (x:xt) = (take ((length x)-1) x): dropCol' xt
moveCol' :: (BPic, BPic) -> (BPic, BPic)
moveCol' ([] , [] ) = ( [], [] )
moveCol' ([] , y) = ([] , y)
moveCol' (x , y ) = ((dropCol' x) , (pickCol' x ):y )
picPairs' :: BPic -> [(BPic, BPic)]
picPairs' pic = iterate moveCol' (pic, [])
rotOnce' :: BPic -> BPic
rotOnce' [] = []
rotOnce' list = snd(head(dropWhile(checker ) (picPairs' list)))



rotPic :: Int -> BPic -> BPic
rotPic x list 
	| x==0 = list
	|x>0 = rotPic (x-1) (rotOnce list)
	|otherwise = rotPic (x+1) (rotOnce' list)


-----------------------------------------------------------
--                           Task 8                      --
-----------------------------------------------------------
filldeL :: Char -> Int -> String
filldeL x 0 = [] 
filldeL x num = x:(filldeL x (num-1) )
filledPic :: Char -> (Int , Int) -> BPic
filledPic x (0,_) = []
filledPic x (_,0) = []
filledPic x (h , y) = (filldeL x h): filledPic x (h , (y-1) )
framePic_v ::  BPic -> BPic
framePic_v  list = fusePic (fusePic(fusePic (filledPic '|' (1,length(list))) (filledPic ' ' (1,length(list)))) list) (fusePic (filledPic ' ' (1,length(list))) (filledPic '|' (1,length(list)))) 
framePic_h :: BPic -> BPic
framePic_h [] = []
framePic_h list = (filldeL '-' (length (head list)) ):(filldeL ' ' (length (head list)) ):list  ++ [(filldeL ' ' (length (head list))) ]++[(filldeL '-' (length (head list)))]
framePic :: BPic -> BPic 
framePic list = framePic_v  (framePic_h  list)







 



