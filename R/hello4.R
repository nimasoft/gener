# Header
# Filename:     optim.R
# Description:  This module is part of niramath package and contains functions for optimization (linear and non-linear programming)
# Author:       Nima Ramezani Taghiabadi
# Email :       nima.ramezani@gmail.com
# Start Date:   27 January 2017
# Last change:  21 February 2017
# Version:      1.1.0

# Version   Date               Action
# -----------------------------------
# 1.0.0     27 January 2017    Initial Issue: Function findNearestVect()
# 1.1.0     21 Febryary 2017   Function scalarQP() added.


# Finds the  vector nearest to the given vector x0 s.t. one linear equality constraint.
# minimize (x - x0)^2
# subject to sum(a*x) = b and x > lb & x < ub
# where:
# a, x are vectors of the same size
# b is a scalar
# lb and ub are vectors same size of x specifying lower and upper bounds of x
#
# Inputs: x0, a, b, lb, ub, mode
# Output: x
findNearestVect = function(x0, a, b, lb = NULL, ub = NULL, mode = 'subject to single linear equality constraint'){
  if (sum(a > 0) == 0){return(x0)} else if ((sum(a > 0) == 1)){
    x = x0
    x[a > 0] = b/sum(a)
    return(x)
  }

  # Closed form solution:
  x = x0 + a*(b - sum(a*x0))/sum(a^2)

  nlopt.needed = F

  if (!is.null(lb)){nlopt.needed = sum(x < lb) > 0}
  if (!is.null(ub)){nlopt.needed = nlopt.needed | (sum(x > ub) > 0)}

  if (nlopt.needed){
    assert(require(nloptr), "Package forecast is not installed!", err_src = match.call()[[1]])
    eval_f = function(x){
      value = sum((x - x0)^2)
      grad  = 2*(x - x0)
      return( list( "objective" = value,
                    "gradient"  = grad))
    }

    eval_g_eq <- function( x ) {
      constr <- sum(a*x) - b
      grad   <- a
      return( list( "constraints"=constr, "jacobian" = grad ) )
    }

    local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                        "xtol_rel" = 1.0e-7 )
    opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
                  "xtol_rel" = 1.0e-7,
                  "maxeval" = 1000,
                  "print_level" = 0,
                  "local_opts" = local_opts )

    res = nloptr( x0 = x0, eval_f = eval_f, eval_g_eq = eval_g_eq, lb = lb, ub = ub, opts = opts)
    x   = res$solution
  }
  names(x) <- names(x0)
  return(x)
}

# Maximize C^T*x
# s.t.: A*x <= b
# This function is written based on karmarkar's algorithm (Not Completed!)
fastLP = function(A, b, cc, x0, gama = 0.5){
  x   <- x0
  cnt <- 0
  while (cnt < 10){
    cnt = cnt + 1
    v   = b - A %*% x
    D   = diag(as.numeric(1.0/v^2))
    hx  = solve(t(A) %*% D %*% A) %*% cc
    hv  = -A %*% hx
    if (sum(hv < 0) == 0){return(x)}
    ix  = hv < 0
    a   = gama*min(- v[ix]/hv[ix])
    x   = x + a*hx
  }
  return(x)
}


# This function provides a closed-form (not iterative) scalar solution to the following quadratic programming problem:
# Maximize (or minimize) a*x^2 + b*x + C
# S.t.: A[i]*x^2 + B[i]*x + C[i] >= 0  (for i = 1,..,n)
scalarQP = function(a, b, A, B, C){
  assert(!equal(a, 0, tolerance = 0.00001), "Argument 'a' must be non-zero", match.call()[[1]])
  xstar = - b/(2*a)
  N     = length(A)
  assert((length(B) == N) & (length(C) == N), "Given vectors 'B' and 'C' must be same length as 'A'", match.call()[[1]])

  u   = c()
  I1  = rep(NA,N)
  I2  = I1
  cnt = 0
  for(i in sequence(N)){
    # service if A[i] == 0
    if (equal(A[i], 0)){
      assert(!equal(B[i], 0), "A and B can not be both zero!")
      r = - C[i]/B[i]
      w = which(u == r)
      if (length(w) > 0){
        assert(length(w) == 1, "This should not happen!")
        I1[i] <- w
      } else {
        cnt   = cnt + 1
        u     = c(u, r)
        I1[i] = cnt
      }
    }
    else {
      D = B[i]^2 - 4*A[i]*C[i]
      if (D > 0){
        r     = (- B[i] - sqrt(D))/(2*A[i])
        w     = which(u == r)
        if (length(w) > 0){
          assert(length(w) == 1, "This should not happen!")
          I1[i] <- w
        } else {
          cnt   = cnt + 1
          u     = c(u, r)
          I1[i] = cnt
        }

        r     = (- B[i] + sqrt(D))/(2*A[i])
        w     = which(u == r)
        if (length(w) > 0){
          assert(length(w) == 1, "This should not happen!")
          I2[i] <- w
        } else {
          cnt   = cnt + 1
          u     = c(u, r)
          I2[i] = cnt
        }
      }
    }
  }
  if (length(u) == 0){
    if (sum(A < 0) == 0){return(xstar)} else {return(NA)}
  }
  r   = rank(u) + 1
  S   = matrix(0, nrow = N, ncol = cnt + 1)
  colnames(S) <- c('  ', as.character(sort(u))) %+% '  '
  for (i in sequence(N)){
    if (!is.na(I1[i])){
      if (is.na(I2[i])){
        if (B[i] > 0) {S[i, 1:(r[I1[i]]-1)] <- 1} else {S[i, r[I1[i]]:(cnt+1)] <- 1}
      } else {
        if (A[i] > 0) {S[i,r[I1[i]]:(r[I2[i]]-1)] <- 1}
        else {
          S[i,1:(r[I2[i]]-1)]   <- 1
          S[i,r[I1[i]]:(cnt+1)] <- 1
        }
      }
    } else {
      if (A[i] < 0){S[i, ] <- 1}
    }
  }

  # chk shows which of the roots can be a solution (satisfy all the constraints)
  # then we should use v = v %U% chk to add those solutions to v
  chk = c()
  for (k in 1:cnt){if (sum(S[,k] & S[,k + 1]) == 0){chk = c(chk,k)}}
  u = sort(u)
  v = u[chk]

  s = colSums(S) == 0
  if (sum(s == 0) == 0){return(NA)}
  u = c(-Inf, u)
  if (s[rank(c(xstar,u))[1] - 1]){return(xstar)} else {
    w = which(s)
    v = u[c(w, w + 1)] %U% v
    return(v[order(abs(xstar - v))][1])
  }

}

# This function provides a solution to the following problem using CCD method associated with scalar quadratic programming:
# How to locate n circles on a plane so that:
# 1- Not any two circles overlap
# 2- The sum of squares of distances between the centers are minimized
# (The methodology can be extended for spheres as well)
bubblesCoordCCD = function(x0, y0, r){
  N    = length(r)
  i    = 0
  x    = x0
  y    = y0
  cnt  = 0
  while(cnt < 2*N){
    i = i + 1
    if (i > N) {
      i   = 1
      cnt = 0
    }

    p = 1:N %-% i
    if (i > N){i = 1}
    d = (r[i] + r[p])^2
    xs = scalarQP(a = N - 1, b = -2*sum(x[p]), A = rep(1,N-1), B = -2*x[p], C = x[p]^2 + (y[i] - y[p])^2 - d)
    cnt = cnt + (equal(xs,x[i]) | is.na(xs))
    if (!is.na(xs)){x[i] = xs}
    ys = scalarQP(a = N - 1, b = -2*sum(y[p]), A = rep(1,N-1), B = -2*y[p], C = y[p]^2 + (x[i] - x[p])^2 - d)
    cnt = cnt + (equal(ys,y[i]) | is.na(ys))
    if (!is.na(ys)){y[i] = ys}

    # plot.new()
    # plot.window(xlim = range(c(x+r,x-r)), ylim = range(c(y+r,y-r)))

    # for (j in 1:N){draw.ellipse(x[j],y[j],r[j], r[j])}

    # of = (x[1] - x[2])^2 + (y[1] - y[2])^2 + (x[1] - x[3])^2 + (y[1] - y[3])^2 + (x[2] - x[3])^2 + (y[2] - y[3])^2
  }
  return(list(x = x, y = y))
}



#' niramath: nirasoft R package for for math operations
#'
#' @docType package
#' @name niramath
#'
#' @include optim.R

# Current Version: 0.1.0
# Initial Issue: 27 January 2017
# Last Issue   : 24 July 2017

# Version     Date                 Action
# --------------------------------------------------------------------
# 0.0.1       27 January 2017      Initial Issue of prom.R
# 0.1.0       24 July 2017         Issued as package niramath

NULL
#> NULL
Package: niramath
Type: Package
Title: nirasoft R package for for math operations
Version: 0.1.0
Author: Nima Ramezani
Maintainer: Nima Ramezani <nima.ramezani@gmail.com>
Description: This package contains math tools including some optimization algorithms
License: MIT
Encoding: UTF-8
LazyData: true
Collate: 
    'optim.R'
    'niramath.R'
RoxygenNote: 6.0.1
