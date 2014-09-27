/* file: fit.c */

#include <stdlib.h>
#include <stdio.h>
#include "hash_table.h"
#include "fit.h"

void memoryFail(void)
{
    printf("Insufficient memory, exiting");
    exit(EXIT_FAILURE);
}

/*
 * fit: fit a polynomial function to a given set of points until the given
 *      condition is met, using a given cost function to minimize the
 *      function's cost in fitting
 * cost_function: a function pointer returning a double representing the cost
 *                of a given list of double coefficients for the polynomial
 *                function of increasing power
 * condition: a function pointer returning a boolean int of whether or not
 *            the condition to end the fitting has been met from a given list
 *            of double coefficients for the polynomial function of increasing
 *            power
 * return value: a double_array representing the coefficients of the fitted
 *               polynomial function of increasing power
 */
double_array fit(double (*cost_function)(double_array),
                 int (*condition)(double_array))
{
    /* starts the recursive refineFit with a null starting coefficient list */
    double_array coefs;
    coefs.arrc = 0
    coefs.arrv = NULL;
    return refineFit(cost_function, condition, coefs);
}

/*
 * fit: fit a polynomial function to a given set of points until the given
 *      condition is met, using a given cost function to minimize the
 *      function's cost in fitting
 * cost_function: a function pointer returning a double representing the cost
 *                of a given list of double coefficients for the polynomial
 *                function of increasing power
 * condition: a function pointer returning a boolean int of whether or not
 *            the condition to end the fitting has been met from a given list
 *            of double coefficients for the polynomial function of increasing
 *            power
 * coefs: a list of coefficients to be used as the starting
 *                        point for the curve fitting
 * return value: a double_array representing the coefficients of the fitted
 *               polynomial function of increasing power
 */
double_array refineFit(double (*cost_function)(double_array),
                       int (*condition)(double_array), double_array coefs);
{
    /* base case: if the condition is met, return*/
    if ((*condition)(coefs)) return coefs;
    /* add a new term to the coefficients */
    coefs.arrc++;
    coefs.arrv = realloc(coefs.arrv, sizeof(double) * coefs.arrc)
    if (coefs.arrv = NULL) memoryFail();
    coefs.arrv[coefs.arrc - 1] = 0;
    /* minimize of the cost by adjusting the existing coefficients */
    minimizeCost(cost_function, coefs, STARTING_DELTA, FINAL_DELTA);
    /* recursively call the function to minimize the cost for this new set
     * of coefficients */
    refineFit(cost_function, condition, coefs);
}

/*
 * minimizeCost: minimize the cost of the function
 * cost_function: a function pointer that, given the coefficients of the
 *                current function will give its cost (greater cost --
 *                less optimal)
 * coefs: the coefficients of the current ascending-order polynomial function
 * delta: the initial radius of change to use in searching for possible
 *        coefficients
 * final_delta: the smallest radius to use in searching for better coefficients
 *              (the function will be optimized up to this radius of
 *              specificity)
 */
double_array minimizeCost(double (*cost_function)(double_array),
                          double_array coefs, double delta, double final_delta)
{
    int i;
    double_array min_cost_move;
    double current_move_cost, min_move_cost;
    /* get all possible moves from the coefs in the radius of delta */
    move_array movesarr = moves(delta, coefs);
    /* find the minimum cost move */
    min_cost_move = movesarr.arrv[0];
    min_move_cost = (*cost_function)(movesarr.arrv[0]);
    for (i = 1; i < movesarr.arrc; i++)
    {
        current_move_cost = (*cost_function)(movesarr.arrv[i]);
        if (current_move_cost < min_move_cost)
        {
            min_cost_move = movesarr.arrv[i];
            min_move_cost = current_move_cost;
        }
    }
    /* base case (absolute): current delta is less than the final delta */
    if (delta < final_delta) return coefs;
    /* base case (per delta): cost of current position is less than that of
     *                        all moves */
    if ((*cost_function)(coefs) < (*cost_function)(min_cost_move))
    {
        return minimizeCost(points, cost_function, coefs,
                            evalCoefs REDUCE_DELTA delta, final_delta);
    }
    /* otherwise, use the min cost move in the next iteration of the
     * minimization */
    return minimizeCost(points, cost_function, min_cost_move,
                        delta, final_delta);
}

/*
 * evalCoefs: return the value of the given function applied to the given input
 * coefs: the coefficients of the ascending-order polynomial function
 * x: the input number
 * return value: the result of the function applied to x
 */
double evalCoefs(double_array coefs, double x)
{
    int i;
    double sum = 0;
    for (i = 0; i < coefs.arrc; i++)
    {
        sum += coefs.arrv[i] * pow(x, (double) i);
    }
    return sum
}

/*
-- cs: cartesian coordinates representing coefficients of a polynomial function
-- r: radius of moves
moves :: Double -> [Coefficient] -> [[Coefficient]]
moves r cs = map (zipWith (+) cs) $ moveVectors r (length cs)
*/
move_array moves(double radius, double_array center)
{
    /* NEED TO COPY INNER ARRAYS */
    move_array *radius_move_vectors, *point_moves;
    radius_str = doubleToString(radius);
    moveVectorSet = (*move_array) get_value(move_table, radius_str);
    if (moveVectorSet == NULL)
    {
        radius_move_vectors = malloc(sizeof(move_array));
        radius_move_vectors->arrc = g_move_vectors->arrc
        memcpy(radius_move_vectors->arrv, &g_move_vectors->arrv,
               sizeof(double_array) * g_move_vectors->arrc);
        for (i = 0; i < radius_move_vectors->arrc; i++)
        {
            radius_move_vectors->arrv[i] *= radius;
        }
        set_value(move_table, radius_move_vectors)
    }
    memcpy(point_moves, &get_value(move_table, radius_str),
           sizeof(move_array));
}

char *doubleToString(double d)
{
    int decimals = 0;
    if (d < 1)
    {
        decimals = (int) (-1 * log(d)) + 1;
    }
    sprintf("%.*f", decimals, d)
}

/*
-- r: radius
-- l: length of final coordinate list
moveVectors :: Double -> Int -> [CartesianCoord]
-- exception: one dimension does not fit well with circular coordinates
--            (only yields positive radius)
moveVectors r 1 = [[-r],[r]]
moveVectors r l = map cartesian $ map (r:) $ angleCombinations (l - 1)
*/

/*
angleCombinations :: Int -> [[Angle]]
angleCombinations n = combinations n angles
*/

/*
angles :: [Angle]
angles = [2 * pi * x / movesPerCircle | x <- [1..movesPerCircle]]
*/

/*
-- n: length of each combination
-- xs: set of numbers items to choose from
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [(x:cs) | x <- xs, cs <- (combinations (n - 1) xs)]
*/

/*
-- r: radius
-- as: angles
cartesian :: PolarCoord -> CartesianCoord
cartesian (r:as) = zipWith mkComp
                           (tail $ inits $ (++ [(pi / 2)]) as)
                           (repeat r)
  where
    mkComp :: [Angle] -> Radius -> Double
    mkComp as r = r * (product $ map cos $ init as) * (sin $ last as)
*/
double_array cartesian(double_array polar)
{
}

/*
-- ps: points
-- f: function to find cost of
defaultCostFunction :: [Point] -> Function -> Cost
defaultCostFunction ps f = sum $ map (\(x,y) -> (y - (f x))^2) ps
*/

/*
-- cf: cost function
-- mc: max cost of the final function
defaultCondition :: CostFunction -> Cost -> [Coefficient] -> Bool
defaultCondition cf mc = (< mc) . cf . ctof

-- alternative conditions:
-- - number of coefficients,
-- - significance of last coefficient as compared to the one preceding it
--   (has significant issues, eg slowly decending significance,
--    trignometric functions etc.)
*/

/*
defaultFit :: [Point] -> [Coefficient]
defaultFit ps = fit ps cf con
  where cf = defaultCostFunction ps
        con = defaultCondition cf finalCost
*/

/*
main :: IO ()
main = putStrLn $ show $ defaultFit [(x,(sin x)) | x <- [0,(pi/6)..(2*pi)]]
*/
