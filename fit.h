/* file: fit.h */

#ifndef FIT_H
#define FIT_H

/*
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
*/

/* startingDelta   = 10 */
#define STARTING_DELTA      10
/* finalDelta      = 0.0001 */
#define FINAL_DELTA         0.0001
/* reduceDelta     = (\x -> x / 10) */
#define REDUCE_DELTA        [0,0.1]
/* movesPerCircle  = 6 */
#define MOVES_PER_CIRCLE    6
/* finalCost       = 0.1 */
#define FINAL_COST          0.1

typedef struct
{
    double arrc;
    double *arrv;
} double_array;

typedef struct
{
    double x;
    double y;
} point;

typedef struct
{
    int arrc;
    point *arrv;
} point_array;

typedef struct
{
    int arrc;
    double_array *arrv;
} move_array;

/* main :: IO () */
int main(int argc, char **argv);
void memoryFail(void);
/* fit :: [Point] -> CostFunction -> Condition -> [Coefficient] */
double_array fit(double (*cost_function)(double_array),
                 int (*condition)(double_array));
/* refineFit :: [Point] -> CostFunction -> Condition -> [Coefficient] -> [Coefficient] */
double_array refineFit(double (*cost_function)(double_array),
                       int (*condition)(double_array), double_array coefs);
/* minimizeCost :: [Point] -> CostFunction -> [Coefficient] -> Double -> Double -> [Coefficient] */
double_array minimizeCost(double (*cost_function)(double_array),
                          double_array coefs,
                          double delta, double final_delta);
/* ctof :: [Coefficient] -> Function */
double evalCoefs(double_array coefs, double x);
/* moves :: Double -> [Coefficient] -> [[Coefficient]] */
move_array moves(double radius, double_array center);
/* moveVectors :: Double -> Int -> [CartesianCoord] */
move_array moveVectors(double radius, int dimensions);
/* angleCombinations :: Int -> [[Angle]] */
move_array angleCombinations(int num_angles);
/* angles :: [Angle] */
double_array angles(void);
/* combinations :: Int -> [a] -> [[a]] */
move_array combinations(int length, double_array set);
/* cartesian :: PolarCoord -> CartesianCoord */
double_array cartesian(double_array polar);
/* defaultCostFunction :: [Point] -> Function -> Cost */
double (*defaultCostFunction(point_array points))(double_array);
/* defaultCondition :: CostFunction -> Cost -> [Coefficient] -> Bool */
int (*defaultCondition(double (*cost_function)(double_array), double min_cost))
    (double_array);
/* defaultFit :: [Point] -> [Coefficient] */
double_array defaultFit(point_array points);

#endif
