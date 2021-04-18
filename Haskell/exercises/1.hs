perimetro :: Float -> Float 
perimetro r = 2*r*pi 

dist :: (Double,Double) -> (Double,Double) -> Double 
dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

primUlt :: [a] -> (a,a) 
primUlt l = (head l,last l) 

multiplo :: Int -> Int -> Bool
multiplo a b = mod a b == 0 

truncaImpar :: [a] -> [a] 
truncaImpar l = if mod (length l) 2 == 0 then l else tail l 

max2 :: Int -> Int -> Int 
max2 a b = if a>b then a else b 

max3 :: Int -> Int -> Int -> Int
max3 a b c = if (max2 a c)>b then max2 a c else b 

nRaizes :: Float -> Float -> Float -> Int 
nRaizes a b c | b^2 -4*a*c == 0 = 1
              | b^2 -4*a*c > 0 = 2                             
              | otherwise = 0 

raizes :: Float -> Float -> Float -> [Float]
raizes a b c | nRaizes a b c == 0 = []                                      
             | nRaizes a b c == 1 = [-b/2*a]                                
             | nRaizes a b c == 2 = [(-b + sqrt((b)^2 -4*a*c))/2*a,(-b - sqrt((b)^2 -4*a*c))/2*a]


testehora :: (Int,Int) -> Bool 
testehora (h,m) | h+m < 0 = False 
                | h+m < 83 = True
                | otherwise = False

comphoras :: (Int,Int) -> (Int,Int) -> (Int,Int)
comphoras (h1,m1) (h2,m2) | h1 > h2 = (h1,m1)
                              | h1 < h2 = (h2,m2)              
                              | m1 > m2 = (h1,m1)              
                              | otherwise = (h2,m2)

convhoras :: (Int,Int) -> Int
convhoras (hx,mx) = hx*60 + mx 

convmin :: Int -> (Int,Int)
convmin mi = (div mi 60,mod mi 60)

difhoras :: (Int,Int) -> (Int,Int) -> Int
difhoras (ho1,min1) (ho2,min2) = abs(ho1*60 + min1 - (ho2*60 + min2))

adicmin :: Int -> (Int,Int) -> (Int,Int)
adicmin minx (hoy,miny) | minx + miny > 60 = (hoy + 1, minx + miny - 60)                                      
                        | minx + miny < 60 = (hoy, minx + miny)

data Hora = H Int Int deriving (Show,Eq)

testehora2 :: Hora -> Bool 
testehora2 (H h m) = h < 24 && m < 60 && h >= 0 && m >= 0

comphoras2 :: Hora -> Hora -> Hora
comphoras2 hora1@(H h1 m1) hora2@(H h2 m2) | h1 > h2 = hora1
                                           | h1 < h2 = hora2              
                                           | m1 > m2 = hora1              
                                           | otherwise = hora2

convhoras2 :: Hora -> Int
convhoras2 (H hx mx) = hx*60 + mx 

convmin2 :: Int -> Hora
convmin2 mi = (H (div mi 60) (mod mi 60))

difhoras2 :: Hora -> Hora -> Int
difhoras2 (H ho1 min1) (H ho2 min2) = abs(ho1*60 + min1 - (ho2*60 + min2))

adicmin2 :: Int -> Hora -> Hora
adicmin2 minx (H hoy miny) | minx + miny > 60 = (H (hoy + 1) (minx + miny - 60))                                      
                           | minx + miny < 60 = (H hoy (minx + miny))
