module Foto
(
  Photo(..)
 ,applyChanges
 ,mountOutput
 ,runTests
) where

import Test.QuickCheck

data Photo a = Photo {  tipo :: String
                     , width :: Int
                     , height :: Int
                     , maxValue :: Int
                     , pixels :: [Int]
                     } deriving (Eq,Show)

instance Arbitrary (Photo a)
  where
    arbitrary = do
      t <- arbitrary
      w <- arbitrary
      h <- arbitrary
      arbitraryMaxValue <- choose (0,255)
      arbitraryPixels <- vectorOf (h*w) (choose (0,arbitraryMaxValue))
      return $ Photo t w h arbitraryMaxValue arbitraryPixels

applyChanges :: Photo a -> String -> Photo a
applyChanges (Photo t w h mx p) n = (Photo t wx hx mx px)
  where px = concat $ map (\x -> prepare n x) xs
        xs = case n of
          "-fh" -> flipHorizontal w p
          "-fv" -> flipVertical w p
          "-gs" -> applyColor w n p
          "-rc" -> applyColor w n p
          "-bc" -> applyColor w n p
          "-gc" -> applyColor w n p
          "-hw" -> halfWidth w p
          "-hh" -> halfHeight w p
        wx = if n == "-hw"
              then (div w 2)
              else w
        hx = if n == "-hh"
              then (div h 2)
              else h

mountOutput :: Photo a -> String
mountOutput (Photo t w h mx p) =
  t ++ "\n" ++ (show w) ++ " " ++ (show h)
  ++ "\n" ++ (show mx) ++ "\n" ++ unlines fPixels
  where
    fPixels = map show p

-- prepare rows
prepare :: String -> [[Int]] -> [Int]
prepare n zs
      | n == "-fh" = (concat . reverse) zs
      | otherwise = concat zs

-- split into w rows
splitLines :: Int -> [[Int]] -> [[[Int]]]
splitLines _ [] = []
splitLines w xs = fst s : splitLines w (snd s)
  where s = splitAt w xs

-- decompose the list into list of integers [r, g, b]
decompor :: [Int] -> [[Int]]
decompor [] = []
decompor xs = (take 3 xs)  : (decompor (drop 3 xs))

flipHorizontal :: Int -> [Int] -> [[[Int]]]
flipHorizontal w p = (splitLines w . decompor) p

flipVertical :: Int -> [Int] -> [[[Int]]]
flipVertical w p = ((splitLines w . reverse . decompor) p)

applyColor ::  Int -> String -> [Int] -> [[[Int]]]
applyColor w n p
  | n == "-gs" = (splitLines w . applyGray . decompor) p
  | n == "-rc" = (splitLines w . applyRed . decompor) p
  | n == "-bc" = (splitLines w . applyBlue . decompor) p
  | n == "-gc" = (splitLines w . applyGreen . decompor) p

applyGray :: [[Int]] -> [[Int]]
applyGray zs = [ [s,s,s] | [r, g, b] <- zs, let s = div (r+g+b) 3]

applyRed :: [[Int]] -> [[Int]]
applyRed zs = [[s,0,0] | [r, g, b] <- zs, let s = div (r+g+b) 3]

applyBlue :: [[Int]] -> [[Int]]
applyBlue zs = [[0,0,s] | [r, g, b] <- zs, let s = div (r+g+b) 3]

applyGreen :: [[Int]] -> [[Int]]
applyGreen zs = [[0,s,0] | [r, g, b] <- zs, let s = div (r+g+b) 3]

halfWidth :: Int -> [Int] -> [[[Int]]]
halfWidth w p = [zs | x <- ((splitLines w . decompor) p),
                      let zs = applyHalfWidth x]

applyHalfWidth :: [[Int]] -> [[Int]]
applyHalfWidth [] = []
applyHalfWidth (x1:x2:xs) =
  (zipWith (\a b -> div (a+b) 2) x1 x2) : applyHalfWidth xs

halfHeight :: Int -> [Int] -> [[[Int]]]
halfHeight w p = applyHalfHeight [x | x <- ((splitLines w .decompor) p)]

applyHalfHeight :: [[[Int]]] -> [[[Int]]]
applyHalfHeight [] = []
applyHalfHeight (x1:x2:xs) = mergeLines x1 x2 : applyHalfHeight xs --x1 and x2 are rows. Merge x1 x2

mergeLines :: [[Int]] -> [[Int]] -> [[Int]]
mergeLines x1 x2 = [ zipWith (\a b -> div (a+b) 2) x y | (x,y) <- zip x1 x2 ]

----- Run tests -----
runTests = do
  quickCheck prop_2flipsH_equal
  quickCheck prop_2flipsV_equal
  quickCheck prop_length_equal_wxh
  quickCheck prop_maxvalue_pixel
  quickCheck prop_same_ratio

----- Define properties for QuickCheck -----

prop_2flipsH_equal :: Photo a -> Property
prop_2flipsH_equal (Photo t w h m p) =
  w > 0 && (mod w 3 == 0) && h > 0 ==>
  (Photo t w h m p) == (foldl (\acc f -> applyChanges acc f) (Photo t w h m p) ["-fh","-fh"])

-- Results are not good. Investigate
prop_2flipsV_equal :: Photo a -> Property
prop_2flipsV_equal (Photo t w h m p) =
  w > 0 && (mod w 3 == 0) && h > 2 && even h ==>
  (Photo t w h m p) == (foldl (\acc f -> applyChanges acc f) (Photo t w h m p) ["-fv","-fv"])

prop_length_equal_wxh :: Photo a -> Property
prop_length_equal_wxh (Photo t w h m p) =
  w > 0 && h > 0 ==> length p == w * h

prop_maxvalue_pixel :: Photo a -> Property
prop_maxvalue_pixel (Photo t w h m p) =
  w > 0 && h > 0 && m > 0 ==> length (filter (\x -> x > m) p) == 0

prop_same_ratio :: Photo a -> Property
prop_same_ratio (Photo t w h m p) =
  w > 0 && h > 0 && even w && even h ==>
   (div h w) == (div h2 w2)
  where
    (Photo t2 w2 h2 m2 p2) =
       (foldl (\acc f -> applyChanges acc f) (Photo t w h m p) ["-hh","-hw"])
