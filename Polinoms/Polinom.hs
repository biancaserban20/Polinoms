module Polinom (Termen, Polinom, createTermen, fromString, toString, addPolinoms, multiplyPolinoms, normalizePolinom, derivatePolinom, addIO, mulIO, derivateIO , normalizeIO )
 where

import Data.List
import System.IO
--import Data.List.Split
import Data.Char


data Termen = Termen {  coeficient :: Double ,
                        text :: String
                        }

type Polinom = [Termen]



----- !! This constructor (instead of the implicit one) sorts the text when creating a new Termen ---------

createTermen :: Double -> String -> Termen
createTermen x text = Termen x (sort text)


---------------------------------------------------------------------------
instance Eq Termen where
 t1 == t2 = ((text t1) == (text t2)) && ((coeficient t1) == (coeficient t2))

instance Ord Termen where
 compare t1 t2
  | (text t1) /= (text t2) =  compare (text t1) (text t2)
  | otherwise = compare (coeficient t1) (coeficient t2)



showL :: String -> String
showL [] = []
showL [x] = [x]
showL (h : t) = if n > 1 then [h] ++ "^" ++ (show n) ++ (showL $ drop n (h:t))
                         else [h] ++ (showL $ drop n (h:t))
                          where
                           rep = takeWhile (==h) (h:t)
                           n = length rep

showC :: Double -> String
showC x
 | x <= 0 = " " ++ (show x)
 | x > 0 = " +" ++ (show x)


instance Show Termen where
 show (Termen x y) = showC(x) ++ showL(y)



---------------------------------------------
transformText :: String -> String
transformText text = transformTextAux text []

transformTextAux :: String -> String -> String
transformTextAux [] acc = sort acc
transformTextAux (x : '^' : rest ) acc =  transformTextAux (dropWhile isDigit rest) (acc ++ (replicate numar x))
                                           where numar  = read (takeWhile isDigit rest) :: Int
transformTextAux (x : rest) acc = transformTextAux rest ( acc ++ [x])

-----------------------------------------------

coef :: String -> Double
coef "" = 1
coef "-" = -1
coef ('+' : rest ) = coef rest
coef text = read text :: Double


termenFromString :: String ->  Termen
termenFromString text = createTermen  (coef (takeWhile (not . isLetter) text) )  (transformText $ dropWhile (not . isLetter) text )

----------------------------------------------
-- "fromString" transform a given String in a Polinomyal, just by listing all the elements
-- This function does not "normalize" the given Polinomyal
-- The  < & delete "" & > from the function is there because the "split" behaves strangely when the first termen of the polinomyal is negative
-- example: split ( keepDelimsL $ oneOf "+-" ) "-20xy+3abx" ======> ["" , "-20xy" , "+3abx"]  (We have to get rid of that first weird element)


fromString :: String -> Polinom
fromString text = sort $ foldl (\acc x -> acc ++ [(termenFromString x)] ) [] $ splitString [] $ filter (\x -> x /= ' ')  text

splitString :: [String] -> String -> [String]
splitString acc [] = acc
splitString acc (x : xs) = splitString (acc ++ [ ( [x] ++ takeWhile (\a -> (a /= '+') && (a /= '-')) xs) ] ) (dropWhile  (\a -> (a /= '+') && (a /= '-')) xs)

---------------------------------------------
-- "normalizePolinom" is a function that:
-- -> gets rid of terms that have the coefficient 0 ( e.g :  0xy+2x^2y^4 = 2x^2y^4)
-- -> combine the coefficients of terms that have the same "text" ( e.g : -3xyz + 10xyz + 6xyz^2 = 7xyz + 6xyz^2)
-- -> because our Polynom is a sorted list of Termens, and Termens are ordered with respect to the "text" of the Termen, the combining can occur only and only between consecutive Termens

normalizePolinom :: Polinom -> Polinom
normalizePolinom [] = []
normalizePolinom [ x ]
 | coeficient x == 0 = []
 | otherwise = [ x ]
normalizePolinom ( (Termen 0 _ ) : rest ) = normalizePolinom rest
normalizePolinom ( (Termen x t1) : (Termen y t2) : rest)
 | t1 == t2 = normalizePolinom ( [ (Termen (x+y) t1) ] ++ rest   )
 | otherwise = [ (Termen x t1)] ++ (normalizePolinom ( (Termen y t2) : rest))


-------------------------------------------

toString :: Polinom -> String
toString [] = "0"
toString [x] = show(x)
toString ( x : xs ) = show(x) ++ (toString xs)


----------------------------------------------
-- "addPolinoms" takes advantage of the fact that the Polinoms are sorted lists of Termens.
-- This helps us find a lot easier similar Termens (that have equal "texts") from the 2 Polinoms
-- The result of the function is also a sorted list of Termens
-- VERY IMPORTANT! "addPolinoms" behaves properly only when it is used on normalized Polinoms!

addPolinoms :: Polinom -> Polinom -> Polinom
addPolinoms [] p = p
addPolinoms p [] = p
addPolinoms (x : xs) (y : ys)
 | text x < text y = [x] ++ ( addPolinoms xs (y:ys) )
 | text x > text y = [y] ++ ( addPolinoms (x:xs) ys )
 | text x == text y = [createTermen (coeficient x  +  coeficient y) (text x)] ++ ( addPolinoms xs ys)

-----------------------------------------------
multiplyTermens :: Termen -> Termen -> Termen
multiplyTermens x y = createTermen (coeficient x * coeficient y)  ( (text x) ++ (text y) )

multiplyTP :: Termen -> Polinom -> Polinom
--multiplyTP x [] = []
--multiplyTP x (y : ys) = [( multiplyTermens x y)] ++ (multiplyTP x ys)
multiplyTP termen poli =sort $ map (multiplyTermens termen)  poli


multiplyPolinoms :: Polinom -> Polinom -> Polinom
multiplyPolinoms [] _ = []
multiplyPolinoms _ [] = []
multiplyPolinoms [t] p = multiplyTP t p
multiplyPolinoms p [t] = multiplyTP t p
multiplyPolinoms (x : xs) ys = addPolinoms (multiplyTP x ys) (multiplyPolinoms xs ys)

-----------------------------------------------
-- "derivatePolinom" takes a variable and a Polinom and derivates the Polinom with respect to that variable


derivateTerm :: Char -> Termen -> Termen
derivateTerm x term
 | elem x (text term) = createTermen ( (coeficient term) * ( fromIntegral   (occurences x (text term)) ) ) ( delete x (text term) )
 | otherwise  = (Termen 0 "")
  where
   occurences = \x list -> length $ filter (==x) list


derivatePolinom :: Char -> Polinom -> Polinom
--derivatePolinom _ [] = []
--derivatePolinom x [y] = [derivateTerm x y]
--derivatePolinom x (y : ys) = addPolinoms [(derivateTerm x y)] ( derivatePolinom x ys)

derivatePolinom var poli = normalizePolinom $ sort $ map (\x -> derivateTerm var x) poli

----------------------------------------------

normalizeIO :: IO ()

normalizeIO = do
 putStrLn "Carefully enter your polinom here in order to be normalized:"
 input <- getLine
 let poli = normalizePolinom $ fromString input
 putStrLn (toString poli)
 return()



derivateIO :: IO ()

derivateIO = do
 putStrLn "Carefully enter your polinom so we can derivate it:"
 input <- getLine
 if (input == "")
  then do
        putStrLn "You inserted the null polinom that is equal to 0. The result is 0, regardless the variable. Try again!"
        derivateIO
  else do
        putStrLn "Enter the variable with respect to which we derivate the polinom:"
        inputvar <- getLine
        if (length inputvar) /= 1
         then do
               putStrLn "You entered either multiple characters or no characters"
               return()
         else do
               let poli = derivatePolinom (head inputvar) (normalizePolinom $ fromString input)
               putStrLn "The result of the derivation is:"
               putStrLn (toString $ normalizePolinom poli)
               return()


addIO :: IO ()

addIO = do
 putStrLn "Enter the first polinom:"
 input1 <- getLine
 putStrLn "Enter the second polinom:"
 input2 <- getLine
 let p1 = normalizePolinom $ fromString input1
 let p2 = normalizePolinom $ fromString input2
 putStrLn "The sum result is:"
 putStrLn $ toString $ addPolinoms p1 p2


mulIO :: IO ()

mulIO = do
 putStrLn "Enter the first polinom:"
 input1 <- getLine
 putStrLn "Enter the second polinom:"
 input2 <- getLine
 let p1 = normalizePolinom $ fromString input1
 let p2 = normalizePolinom $ fromString input2
 putStrLn "The multiplication result is:"
 putStrLn $ toString $ multiplyPolinoms p1 p2


----------------------------------------------
