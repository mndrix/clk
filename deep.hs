data Line = Full String | Partial String
    deriving (Show)
instance Eq Line where
    (==) (Full    _) (Full    _) = True
    (==) (Partial _) (Partial _) = True
    (==) _           _           = False

deepTake :: Integer -> [[a]] -> [a]
deepTake _ [] = []
deepTake n (x:xs) = deepTake_ n x xs
    where deepTake_ 0  _  _ = []
          deepTake_ _ [] [] = []
          deepTake_ n [] (y:ys) = deepTake_ n y ys
          deepTake_ n (x:xs) ys = x:( deepTake_ (n-1) xs ys )

-- combine adjacent partial lines together
squish :: [Line] -> [Line]
squish []  = []
squish [x] = [x]
squish ((Partial a):(Partial b):zs) = squish $ (Partial (a++b)):zs
squish (x:xs) = x:( squish xs )
