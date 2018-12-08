/**
 * @title Munkres' Assignment Algorithm with RcppArmadillo
 * @author Lars Simon Zehnder
 * @license GPL (>= 2)
 * @tags armadillo
 * @summary Demonstrates the Minimal (or Maximal) Assignment Problem algorithm.
 */

/**
 * _Munkres' Assignment Algorithm_
 * ([Munkres (1957)](http://www.jstor.org/discover/10.2307/2098689?uid=3737864&uid=2&uid=4&sid=21102674250347),
 * also known as _hungarian algorithm_) is a well known algorithm
 * in Operations Research solving the problem to optimally
 * assign `N` jobs to `N` workers.
 *
 * I needed to solve the _Minimal Assignment Problem_ for a
 * relabeling algorithm in MCMC sampling for _finite mixture_
 * distributions, where I use a random permutation Gibbs
 * sampler. For each sample an optimal labeling must be found,
 * i.e. I have `N` parameter vectors and must assign each vector
 * to one of the `N` components of the finite mixture under the restriction
 * that each component gets a parameter vector. For the assignment
 * of parameters to components
 * [Stephens (1997b)] (http://www.isds.duke.edu/~scs/Courses/Stat376/Papers/Mixtures/LabelSwitchingStephensJRSSB.pdf)
 * suggests an algorithm relying on the Minimal Assignment in regard
 * to a _loss matrix_. The labeling with the smallest loss is
 * then considered as optimal.
 *
 * After an unsuccessful search for libraries implementing
 * the algorithm easily for C++ or C, I made the decision to
 * program it myself using `RcppArmadillo` for good performance.
 * I found some guidance by the websites of
 * [Bob Pilgrim](http://csclab.murraystate.edu/bob.pilgrim/445/munkres.html) and
 * [TopCoder] (http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=hungarianAlgorithm).
 * These websites offer excellent tutorials to understand
 * the algorithm and to convert it to code. The order of this
 * implementation of Munkres' algorithm is of an order `N` to the power of 4. There exists
 * also a version of order `N` to the power of 3, but an order of `N` to the power of 4 works
 * very good for me and coding time is as usual a critical factor
 * for me.
 *
 * In the following I walk through the different steps of
 * Munkres' algorithm and explain the main parts and their
 * functionality.
 *
 * Let's begin. The first step in Munkres' algorithm is to
 * subtract the minimal element of each row from each element
 * in this row.
 */

#include<RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

void step_one(unsigned int &step, arma::mat &cost,
        const unsigned int &N)
{
    for (unsigned int r = 0; r < N; ++r) {
        cost.row(r) -= arma::min(cost.row(r));
    }
    step = 2;
}

/**
 * Note, that we use references for all function arguments.
 * As we have to switch between the steps of the algorithm
 * continously, we always must be able to determine which
 * step should be chosen next. Therefore we give a mutable
 * unsigned integer `step` as an argument to each step
 * function of the algorithm.
 *
 * Inside the function we can easily access a whole row by
 * Armadillo's `row()` method for matrices.
 * In the second step, we then search for a zero in the
 * modified cost matrix of step one.
 */
void step_two (unsigned int &step, const arma::mat &cost,
        arma::umat &indM, arma::ivec &rcov,
        arma::ivec &ccov, const unsigned int &N)
{
    for (unsigned int r = 0; r < N; ++r) {
        for (unsigned int c = 0; c < N; ++c) {
            if (cost.at(r, c) == 0.0 && rcov.at(r) == 0 && ccov.at(c) == 0) {
                indM.at(r, c)  = 1;
                rcov.at(r)     = 1;
                ccov.at(c)     = 1;
                break;                                              // Only take the first
                                                                    // zero in a row and column
            }
        }
    }
    /* for later reuse */
    rcov.fill(0);
    ccov.fill(0);
    step = 3;
}

/**
 * Only the first zero in a row is taken. Then, the indicator
 * matrix `indM` indicates this zero by setting the corresponding
 * element at `(r, c)` to 1. A unique zero - the only or first one in
 * a column and row - is called _starred zero_. In `step 2` we find
 * such a _starred zero_.
 *
 * Note, that we use here Armadillo's element access via the
 * method `at()`, which makes no bound checks and improves performance.
 *
 * _Note Bene: This code is thoroughly debugged - never do this for fresh written
 * code!_
 *
 * In `step 3` we cover each column with a _starred zero_. If already
 * `N` columns are covered all _starred zeros_ describe a complete
 * assignment - so, go to `step 7` and finish. Otherwise go to
 * `step 4`.
 */
void step_three(unsigned int &step, const arma::umat &indM,
        arma::ivec &ccov, const unsigned int &N)
{
    unsigned int colcount = 0;
    for (unsigned int r = 0; r < N; ++r) {
        for (unsigned int c = 0; c < N; ++c) {
            if (indM.at(r, c) == 1) {
                ccov.at(c) = 1;
            }
        }
    }
    for (unsigned int c = 0; c < N; ++c) {
        if (ccov.at(c) == 1) {
            ++colcount;
        }
    }
    if (colcount == N) {
        step = 7;
    } else {
        step = 4;
    }
}
/**
 * We cover a column by looking for 1s in the indicator
 * matrix `indM` (See `step 2` for assuring that these are
 * indeed only _starred zeros_).
 *
 * `Step 4` finds _noncovered zeros_ and _primes_ them. If there
 * are zeros in a row and none of them is _starred_, _prime_
 * them. For this task we program a helper function to keep
 * the code more readable and reusable. The helper function
 * searches for _noncovered zeros_.
 */
void find_noncovered_zero(int &row, int &col,
        const arma::mat &cost, const arma::ivec &rcov,
        const arma::ivec &ccov, const unsigned int &N)
{
    unsigned int r = 0;
    unsigned int c;
    bool done = false;
    row = -1;
    col = -1;
    while (!done) {
        c = 0;
        while (true) {
            if (cost.at(r, c) == 0.0 && rcov.at(r) == 0 && ccov.at(c) == 0) {
                row = r;
                col = c;
                done = true;
            }
            ++c;
            if (c == N || done) {
                break;
            }
        }
        ++r;
        if (r == N) {
            done = true;
        }
    }
}
/**
 * We can detect _noncovered zeros_ by checking if the cost matrix
 * contains at row r and column c a zero and row and column
 * are not covered yet, i.e. `rcov(r) == 0`, `ccov(c) == 0`.
 * This loop breaks, if we have found our first _uncovered zero_  or
 * no _uncovered zero_ at all.
 *
 * In `step 4`, if no _uncovered zero_ is found we go to `step 6`. If
 * instead an _uncovered zero_ has been found, we set the indicator
 * matrix at its position to 2. We then have to search for a _starred
 * zero_ in the row with the _uncovered zero_, _uncover_ the column with
 * the _starred zero_ and _cover_ the row with the _starred zero_. To
 * indicate a _starred zero_ in a row and to find it we create again
 * two helper functions.
 */
bool star_in_row(int &row, const arma::umat &indM,
        const unsigned int &N)
{
    bool tmp = false;
    for (unsigned int c = 0; c < N; ++c) {
        if (indM.at(row, c) == 1) {
            tmp = true;
            break;
        }
    }
    return tmp;
}

void find_star_in_row (const int &row, int &col,
        const arma::umat &indM, const unsigned int &N)
{
    col = -1;
    for (unsigned int c = 0; c < N; ++c) {
        if (indM.at(row, c) == 1) {
            col = c;
        }
    }
}
/**
 * We know that _starred zeros_ are indicated by the indicator
 * matrix containing an element equal to 1.
 * Now, `step 4`.
 */
void step_four (unsigned int &step, const arma::mat &cost,
        arma::umat &indM, arma::ivec &rcov, arma::ivec &ccov,
        int &rpath_0, int &cpath_0, const unsigned int &N)
{
    int row = -1;
    int col = -1;
    bool done = false;
    while(!done) {
        find_noncovered_zero(row, col, cost, rcov,
                ccov, N);

        if (row == -1) {
            done = true;
            step = 6;
        } else {
            /* uncovered zero */
            indM(row, col) = 2;
            if (star_in_row(row, indM, N)) {
                find_star_in_row(row, col, indM, N);
                /* Cover the row with the starred zero
                 * and uncover the column with the starred
                 * zero.
                 */
                rcov.at(row) = 1;
                ccov.at(col) = 0;
            } else {
                /* No starred zero in row with
                 * uncovered zero
                 */
                done = true;
                step = 5;
                rpath_0 = row;
                cpath_0 = col;
            }
        }
    }
}
/**
 * Notice the `rpath_0` and `cpath_0` variables. These integer
 * variables store the first _vertex_ for an _augmenting path_ in `step 5`.
 * If zeros could be _primed_ we go further to `step 5`.
 *
 * `Step 5` constructs a path beginning at an _uncovered primed
 * zero_ (this is actually graph theory - alternating and augmenting
 * paths) and alternating between _starred_ and _primed zeros_.
 * This path is continued until a _primed zero_ with no _starred
 * zero_ in its column is found. Then, all _starred zeros_ in
 * this path are _unstarred_ and all _primed zeros_ are _starred_.
 * All _primes_ in the indicator matrix are erased and all rows
 * are _uncovered_. Then return to `step 3` to _cover_ again columns.
 *
 * `Step 5` needs several helper functions. First, we need
 * a function to find _starred zeros_ in columns.
 */
void find_star_in_col (const int &col, int &row,
        const arma::umat &indM, const unsigned int &N)
{
    row = -1;
    for (unsigned int r = 0; r < N; ++r) {
        if (indM.at(r, col) == 1) {
            row = r;
        }
    }
}
/**
 * Then we need a function to find a _primed zero_ in a row.
 * Note, that these tasks are easily performed by searching the
 * indicator matrix `indM`.
 */
void find_prime_in_row (const int &row, int &col,
        const arma::umat &indM, const unsigned int &N)
{
    for (unsigned int c = 0; c < N; ++c) {
        if (indM.at(row, c) == 2) {
            col = c;
        }
    }
}
/**
 * In addition we need a function to augment the path, one to
 * clear the _covers_ from rows and one to erase the _primed zeros_
 * from the indicator matrix `indM`.
 */
void augment_path (const int &path_count, arma::umat &indM,
        const arma::imat &path)
{
    for (unsigned int p = 0; p < path_count; ++p) {
        if (indM.at(path(p, 0), path(p, 1)) == 1) {
            indM.at(path(p, 0), path(p, 1)) = 0;
        } else {
            indM.at(path(p, 0), path(p, 1)) = 1;
        }
    }
}

void clear_covers (arma::ivec &rcov, arma::ivec &ccov)
{
    rcov.fill(0);
    ccov.fill(0);
}

void erase_primes(arma::umat &indM, const unsigned int &N)
{
    for (unsigned int r = 0; r < N; ++r) {
        for (unsigned int c = 0; c < N; ++c) {
            if (indM.at(r, c) == 2) {
                indM.at(r, c) = 0;
            }
        }
    }
}
/**
 * The function to augment the path gets an integer matrix `path`
 * of dimension 2 * N x 2. In it all vertices between rows and columns
 * are stored row-wise.
 * Now, we can set the complete `step 5`:
 */
void step_five (unsigned int &step,
        arma::umat &indM, arma::ivec &rcov,
        arma::ivec &ccov, arma::imat &path,
        int &rpath_0, int &cpath_0,
        const unsigned int &N)
{
    bool done = false;
    int row = -1;
    int col = -1;
    unsigned int path_count = 1;
    path.at(path_count - 1, 0) = rpath_0;
    path.at(path_count - 1, 1) = cpath_0;
    while (!done) {
        find_star_in_col(path.at(path_count - 1, 1), row,
                indM, N);
        if (row > -1) {
            /* Starred zero in row 'row' */
            ++path_count;
            path.at(path_count - 1, 0) = row;
            path.at(path_count - 1, 1) = path.at(path_count - 2, 1);
        } else {
            done = true;
        }
        if (!done) {
            /* If there is a starred zero find a primed
             * zero in this row; write index to 'col' */
            find_prime_in_row(path.at(path_count - 1, 0), col,
                    indM, N);
            ++path_count;
            path.at(path_count - 1, 0) = path.at(path_count - 2, 0);
            path.at(path_count - 1, 1) = col;
        }
    }
    augment_path(path_count, indM, path);
    clear_covers(rcov, ccov);
    erase_primes(indM, N);
    step = 3;
}
/**
 * Recall, if `step 4` was successfull in uncovering all columns
 * and covering all rows with a primed zero, it then calls
 * `step 6`.
 * `Step 6` takes the cover vectors `rcov` and `ccov` and looks
 * in the uncovered region of the cost matrix for the smallest
 * value. It then subtracts this value from each element in an
 * _uncovered column_ and adds it to each element in a _covered row_.
 * After this transformation, the algorithm starts again at `step 4`.
 * Our last helper function searches for the smallest value in
 * the uncovered region of the cost matrix.
 */
void find_smallest (double &minval, const arma::mat &cost,
        const arma::ivec &rcov, const arma::ivec &ccov,
        const unsigned int &N)
{
    for (unsigned int r = 0; r < N; ++r) {
        for (unsigned int c = 0; c < N; ++c) {
            if (rcov.at(r) == 0 && ccov.at(c) == 0) {
                if (minval > cost.at(r, c)) {
                    minval = cost.at(r, c);
                }
            }
        }
    }
}
/**
 * `Step 6` looks as follows:
 */
void step_six (unsigned int &step, arma::mat &cost,
        const arma::ivec &rcov, const arma::ivec &ccov,
        const unsigned int &N)
{
    double minval = DBL_MAX;
    find_smallest(minval, cost, rcov, ccov, N);
    for (unsigned int r = 0; r < N; ++r) {
        for (unsigned int c = 0; c < N; ++c) {
            if (rcov.at(r) == 1) {
                cost.at(r, c) += minval;
            }
            if (ccov.at(c) == 0) {
                cost.at(r, c) -= minval;
            }
        }
    }
    step = 4;
}
/**
 * At last, we must create a function that enables us to
 * jump around the different steps of the algorithm.
 * The following code shows the main function of
 * the algorithm. It defines also the important variables
 * to be passed to the different steps.
 */
arma::umat hungarian(const arma::mat &input_cost)
{
    const unsigned int N = input_cost.n_rows;
    unsigned int step = 1;
    int cpath_0 = 0;
    int rpath_0 = 0;
    arma::mat cost(input_cost);
    arma::umat indM(N, N, arma::fill::zeros);
    arma::ivec rcov(N, arma::fill::zeros);
    arma::ivec ccov(N, arma::fill::zeros);
    arma::imat path(2 * N, 2, arma::fill::zeros);

    indM = arma::zeros<arma::umat>(N, N);
    bool done = false;
    while (!done) {
        switch (step) {
            case 1:
                step_one(step, cost, N);
                break;
            case 2:
                step_two(step, cost, indM, rcov, ccov, N);
                break;
            case 3:
                step_three(step, indM, ccov, N);
                break;
            case 4:
                step_four(step, cost, indM, rcov, ccov,
                        rpath_0, cpath_0, N);
                break;
            case 5:
                step_five(step, indM, rcov, ccov,
                        path, rpath_0, cpath_0, N);
                break;
            case 6:
                step_six(step, cost, rcov, ccov, N);
                break;
            case 7:
                done = true;
                break;
        }
    }
    return indM;
}
/**
 * Note, this function takes the numeric cost matrix as
 * an argument and returns the integer indicator matrix
 * `indM`.
 *
 * I chose to set the argument `input_cost` constant and copy
 * it for reasons of reusability of the cost matrix in other
 * C++ code. When working with rather huge cost matrices, it
 * makes sense to make the argument mutable. Though, I used
 * _pass-by-reference_ for all the arguments in functions to
 * avoid useless copying inside the functions.
 *
 * To call the main function `hungarian` from R and to use
 * our algorithm we construct an `Rcpp Attribute`:
 */

//' hungarian_cc
//' @param cost cost matrix
// [[Rcpp::export]]
arma::imat hungarian_cc(Rcpp::NumericMatrix cost)
{
    // Reuse memory from R
    unsigned int N = cost.rows();
    arma::mat arma_cost(cost.begin(), N, N, false, true);
    // Call the C++-function 'hungarian'
    arma::umat indM = hungarian(arma_cost);
    //Convert the result to an Armadillo integer
    //matrix - R does not know unsigned integers.
    return arma::conv_to<arma::imat>::from(indM);
}

/**
 * If we want to provide this function also to other users
 * we should probably ensure, that the matrix is square (There
 * exists also an extension to rectangular matrices, see
 * [Burgeois and Lasalle (1971)](http://dl.acm.org/citation.cfm?id=362945)).
 * This can be done easily with the exceptions provided by
 * `Rcpp` passed over to R:
 */

//' hungariansafe_cc
//' @param cost cost matrix
// [[Rcpp::export]]
arma::imat hungariansafe_cc(Rcpp::NumericMatrix cost)
{
    unsigned int N = cost.rows();
    unsigned int K = cost.cols();
    if (N != K) {
        throw Rcpp::exception("Matrix is not square.");
    }
    // Reuse memory from R
    arma::mat arma_cost(cost.begin(), N, K, false, true);
    // Call the C++-function 'hungarian'
    arma::umat indM = hungarian(arma_cost);
    //convert the result to an Armadillo integer
    //matrix - R does not know unsigned integers.
    return arma::conv_to<arma::imat>::from(indM);
}

/**
 * Note, that it is also possible to use for the attribute
 * directly an `Armadillo` matrix, but following the [recent
 * discussion on the Rcpp-devel list](http://www.mail-archive.com/rcpp-devel@lists.r-forge.r-project.org/msg05784.html),
 * a _pass-by-reference_ of arguments is not yet possible. Romain
 * Francois' proposals seem promising, so maybe we can expect
 * in some of the next releases _shallow_ copies allowing
 * _pass-by-reference_ in `Rcpp Attributes`.
 */

/**
 * Let us begin now with a very easy example that makes clear
 * what is going on.
 */

/*** R
# Check exception:
cost <- matrix(c(1:6), nrow = 3, ncol = 2, byrow = TRUE)
tryCatch(indM <- hungariansafe_cc(cost), error = function(e) {print(e)})
cost <- matrix(c(1, 2, 2, 4), nrow = 2, ncol = 2, byrow = TRUE)
cost
indM <- hungarian_cc(cost)
indM
min.cost <- sum(indM * cost)
min.cost
*/

/**
 * We can also compute a maximal assignment over a revenue
 * matrix by simply considering the difference between
 * a big value and this matrix as a cost matrix.
 */

/*** R
revenues <- matrix(seq(1, 4)) %*% seq(1, 4)
revenues
cost <- 100 - revenues
indM <- hungarian_cc(cost)
indM
max.revenue <- sum(indM * revenues)
max.revenue
*/

/**
 * CoCost matrices containing negative values work as well:
 */
/*** R
cost <- matrix(rnorm(100), ncol = 10, nrow = 10)
cost
indM <- hungarian_cc(cost)
indM
min.cost <- sum(indM * cost)
min.cost
*/

/**
 * Finally let us make some benchmarking. We load the
 * rbenchmark package and take now a more _realistic_ cost
 * matrix.
 */
/*** R
library(rbenchmark)
cost <- matrix(rpois(10000, 312), ncol = 100, nrow = 100)
benchmark(indM <- hungarian_cc(cost), columns=c('test','replications','elapsed','user.self','sys.self'), replications=1000)
*/
/**
 * But we also see, where the limitations of this algorithm lie
 * - huge matrices:
 */

/*** R
cost <- matrix(rpois(250000, 312), ncol = 500, nrow = 500)
system.time(indM <- hungarian_cc(cost))
*/

/**
 * Some last notes on the structure of the code. I prefer to
 * put the code of the algorithm in an header file, e.g.
 * `hungarian.h`. And use an `attributes.cpp` (`attributes.cc`)
 * file to program the `Rcpp Attributes`. With this,
 * I can easily reuse the algorithm in C++ code by simple
 * inclusion (`#include "hungarian.h"`) and have a complete
 * overview on all the C++ functions I export to R.
 */
