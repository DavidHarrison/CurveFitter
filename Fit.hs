-- file: Fit.hs

import Data.List (minimumBy, inits, tails)

type Point                    = (Double,Double)
type Function                 = (Double -> Double)
type Cost                     = Double
type CostFunction             = (Function -> Cost)
type Condition                = ([Coefficient] -> Bool)
type Coefficient              = Double
type CartesianCoord           = [Component]
type Component                = Double
type PolarCoord               = [Double]
type Angle                    = Double
type Radius                   = Double

startingDelta   = 10
finalDelta      = 0.0001
reduceDelta     = (\x -> x / 10)
movesPerCircle  = 6
finalCost       = 2

-- ps: points to be fitted to
-- cf: cost function
-- con: conditition under which to yield current function
fit :: [Point] -> CostFunction -> Condition -> [Coefficient]
fit ps cf con = refineFit ps cf con [0]

-- ps: points to be fitted to
-- cf: cost function
-- con: conditition under which to yield current function
-- coefs: coefficients of current power series function
refineFit :: [Point] -> CostFunction -> Condition
             -> [Coefficient] -> [Coefficient]
refineFit ps cf con coefs
  | con coefs = coefs
  | otherwise = refineFit ps cf con $ minimizeCost ps cf (coefs ++ [0])
                                                   startingDelta
                                                   finalDelta

-- ps: points to be fitted to
-- cf: cost function
-- coefs: coefficients of current polynomial function
-- sa: starting delta for optimization moves
-- fa: final delta for optimization moves
minimizeCost :: [Point] -> CostFunction -> [Coefficient]
                -> Double -> Double -> [Coefficient]
minimizeCost ps cf coefs sd fd
  -- base case: starting delta is less than the final delta
  | sd < fd = coefs
  -- base case for delta: cost of current position is less than that of all
  --                      moves
  | (cf $ ctof coefs) <= (cf $ ctof minCostMove)
    = minimizeCost ps cf coefs (reduceDelta sd) fd
  -- otherwise, recurse on the minimum cost move
  | otherwise = minimizeCost ps cf minCostMove sd fd
    where minCostMove = minimumBy (\x y -> compare (cf $ ctof x) (cf $ ctof y))
                        $ moves sd coefs

-- coefficients to function
-- cs: coefficients of the polynomial function
ctof :: [Coefficient] -> Function
ctof [] x = 0
ctof cs x = ((last cs) * x^((length cs) - 1)) + ((ctof (init cs)) x)

-- cs: cartesian coordinates representing coefficients of a polynomial function
-- r: radius of moves
moves :: Double -> [Coefficient] -> [[Coefficient]]
moves r cs = map (zipWith (+) cs) $ moveVectors r (length cs)

-- r: radius
-- l: length of final coordinate list
moveVectors :: Double -> Int -> [CartesianCoord]
-- exception: one dimension does not fit well with circular coordinates
--            (only yields positive radius)
moveVectors r 1 = [[-r],[r]]
moveVectors r l = map cartesian $ map (r:) $ angleCombinations (l - 1)

angleCombinations :: Int -> [[Angle]]
angleCombinations n = combinations n angles

angles :: [Angle]
angles = [2 * pi * x / movesPerCircle | x <- [1..movesPerCircle]]

-- n: length of each combination
-- xs: set of numbers items to choose from
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [(x:cs) | x <- xs, cs <- (combinations (n - 1) xs)]

-- r: radius
-- as: angles
cartesian :: PolarCoord -> CartesianCoord
cartesian (r:as) = zipWith mkComp
                           (tail $ inits $ (++ [(pi / 2)]) as)
                           (repeat r)
  where
    mkComp :: [Angle] -> Radius -> Double
    mkComp as r = r * (product $ map cos $ init as) * (sin $ last as)

-- ps: points
-- f: function to find cost of
defaultCostFunction :: [Point] -> Function -> Cost
defaultCostFunction ps f = sum $ map (\(x,y) -> (y - (f x))^2) ps

-- cf: cost function
-- mc: max cost of the final function
defaultCondition :: CostFunction -> Cost -> [Coefficient] -> Bool
defaultCondition cf mc = (< mc) . cf . ctof

-- alternative conditions:
-- - number of coefficients,
-- - significance of last coefficient as compared to the one preceding it
--   (has significant issues, eg slowly decending significance,
--    trignometric functions etc.)

defaultFit :: [Point] -> [Coefficient]
defaultFit ps = fit ps cf con
  where cf = defaultCostFunction ps
        con = defaultCondition cf finalCost

main :: IO ()
main = putStrLn $ show $ defaultFit [(x,(sin x)) | x <- [0,(pi/6)..(2*pi)]]
-- main = putStrLn $ show $ defaultFit testPoints
-- main = putStrLn $ show $ defaultFit [(x,(x**2)) | x <- [-5..5]]
