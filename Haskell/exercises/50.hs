enumFromto :: Int -> Int -> [Int]
enumFromto a b = if a == b
                 then [b]
                 else a : (enumFromto (a+1) b) 

enumFromThenTo2 :: Int -> Int -> Int -> [Int]
enumFromThenTo2 a b c = if a > c 
                       then []
                       else a : (enumFromThenTo2 (a+(b-a)) (b+(b-a)) c)

concat2 :: [a] -> [a] -> [a]
concat2 [] b = b 
concat2 a [] = a
concat2 (h:t) b = h : (concat2 t b) 

find :: [a] -> Int -> a 
find (h:t) 0 = h
find (h:t) a = (find t (a-1))

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (h:t) = concat2 (reverse2 t) [h] 

take2 :: Int -> [a] -> [a]
take2 _ [] = []
take2 0 (h:t) = []
take2 n (h:t) = h : (take2 (n-1) t)

drop2 :: Int -> [a] -> [a]
drop2 _ [] = []
drop2 0 l = l
drop2 n (_:t) = (drop2 (n-1) t)
