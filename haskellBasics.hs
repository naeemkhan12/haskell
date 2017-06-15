slope (x1,y1) (x2,y2)
       |otherwise = dy/dx
     where dy = y2-y1
           dx = x2-x1


recipro :: (RealFloat a) => a -> a
recipro x 
       | x==0 = 0
     | x==1 = 1
     | otherwise = 1/x
abst x
    | x < 0 = -(x)
  | otherwise = x
signNumb x
    | x < 0 = (-1)
  | x > 0 = 1
  | x == 0 = 0

  
sign x =
  if x<0
  then -1
  else if x>0
  then 1
  else 0
differ x y z
  | x == y && y==z = True
  | otherwise = False
maxthree x y z
  | x>=y && x>=z = x
  | y>=x && y>=z = y
  | otherwise = z
intTostr x
  | x==1 = "One"
  | x==2 = "Two"
  | x==3 = "Three"
  | x==4 = "four"
  | x==5 = "Five"
  | otherwise= error "Out of Limit"
  
  
--enum::Int -> Int
enumfromto x = x : enumfromto (x+1)
--fact::Int->fact
fact x
 | x==0 = 1
 | x==1 = 1
 | otherwise = x*fact(x-1)

addAll c
  | c==0 = 1
  | otherwise =fact(c) + addAll(c-1)
remainder m n
  | m < n = m
  | otherwise = remainder(m-n) n

devide m n
  | m < n = 0
  | otherwise = 1+devide(m-n) n
powerkar m n
  | m==1 = 1
  |m==0 = 0
  | n==1 = m
  | n==0 = 1
  |otherwise = m*powerkar m (n-1) 
replicat:: Int -> a -> [a]
replicat n l
  | n<=0 = []
  |otherwise=l:replicat(n-1) l
zipper :: [a] -> [b] -> [(a,b)]
zipper _[] = []
zipper []_ = []
zipper(x:xt)(y:yt)=(x,y):zipper xt yt

pairs xs = zip xs (tail xs)

zipperM :: (Eq a, Eq b, Eq c) => [a] -> [b] -> [c] -> [(a,b,c)]
zipperM []_[] = []
zipperM _[][] = []
zipperM [][]_ = []
zipperM (x:xt)(y:yt)(z:zt)= (x,y,z):zipperM xt yt zt 
dotProduct (x1,y1)(x2,y2)
    |(x1*x2)<0 && (y1*y2)<0 = ((x1*x2),(y1*y2))
    |(x1*x2)<0  = (-(x1*x2),(y1*y2))
    |(y1*y2)<0 =  ((x1*x2),-(y1*y2))
{-myOR (h:t)
  | h==True = True
  |otherwise=h:myOR(h:t)
-}
qsort (h:t) =
 smaller++ [h]++ larger
  where smaller = qsort[y|y <- t,y<=h]
        larger = qsort[x|x <- t,x>h]
{-mapper :: (a -> b) -> [a] -> [b]
mapper f [] = []
mapper f (h:t)
  if f h == True
    then h: mapper f tail
    else
     then mapper f t 
-}      

mapper1 :: (a -> b) -> [a] -> [b]
mapper1 f [] = []
mapper1 f(h:t) = f h : mapper1 f t

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (h:t) = h+ sum2 t

mult :: [Int] -> Int
mult [] = 0
mult (h:t) = h* mult t

maxi :: [Int] -> Int
maxi  [] = 0
maxi (h:t)=   max h (maxi t)

mini :: [Int] -> Int
mini  [] = 0
mini (h:t)=   min h (maxi t)
foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f b [] = b
foldR f b (h:t) = f h (foldR f b t)

foldL :: (a -> b -> a) -> a -> [b] -> a
foldL f b [] = b
foldL f b (h:t) = f (foldL f b t ) h

letter x = [" AAA ","A   A","AAAAA","A   A","A   A"]

showMat :: Char -> IO()
showMat b= putStr(concat(map(++"\n")(letter 'a')))


sHoriz :: Int -> String -> String
sHoriz 0 _ = ""
sHoriz _ "" = "" 
sHoriz y (x:xs) = (replicate y x) ++ sHoriz y xs

--Class work horizintal Stretch

hStretchChar :: Int -> Char -> String
hStretchChar i ch = replicate i ch

hStretchString :: Int -> String -> String
hStretchString i str = concat (map (hStretchChar i) str )


hStretchList :: Int -> [String] -> [String]
hStretchList i strlist = map (hStretchString i) strlist


-- verticalStretch

vStretchStr :: Int -> String -> String
vStretchStr i str = concat(replicate i (str ++"\n"))

vstretchList :: Int -> [String] -> [String]
vstretchList i strList = map(vStretchStr i) strList

stretch :: Int -> Int ->[String]->[String]
stretch i j strList= vstretchList j (hStretchList i strList)


showMat2 :: [String]->IO()
showMat2 strlist = putStr(concat strlist)

leftBorder :: Char -> String -> String
leftBorder c x = c:x 

leftBorderList :: Char ->[String] ->[String]
leftBorderList c list = map(leftBorder c) list

rightBorderList :: String -> [String] -> [String]
rightBorderList c list = map(++c) list

borderLR :: Char -> String -> [String] ->[String]
borderLR c s list = rightBorderList s  (leftBorderList c list)


toStringList :: Int -> String-> [String]
toStringList i [] = [] 
toStringList i str = take i str : (toStringList i (drop i str))   

--stitch1 :: Int -> [String] ->String -> [String]
--stitch1 i lis1 lis2 =  zipWith (++)  lis1 (toStringList( (i-(head of lis1)) lis2))

--map (ch:)strlist for left
--map(++"ch")strlist for right
-- fold max map length strlist

--zipwith (++) list1 list2