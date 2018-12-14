
# Header
# Filename:      linalg.R
# Version History:

# Version   Date                 Action
# ----------------------------------
# 1.0.0     11 September 2013    Initial Issue.
# 1.1.0     24 May 2016          Some function names changed.
# 1.2.0     28 October 2016      Function vect.shift.up() added
# 1.2.1     16 March 2017        Function mat.profile() added
# 1.2.3     05 July 2018         Function column.shift.up() column.shift.down() modified


#' @export
column.shift.up <- function(A, col, k=1, keep.rows=FALSE){
  N = nrow(A)
  if(N == 0){return(A)}
  if(N > k){
    A[1:(N-k), col] = A[(k + 1):N, col]
  }
  if (keep.rows){
    A[(N-k+1):N,col] = NA
    return(A)
  } else {
    return(A[(-N+k-1):(-N),])
  }
}

#' @export
column.shift.down <- function(A, col, k=1, keep.rows=FALSE){
  N = nrow(A)
  if(N == 0){return(A)}
  if(N > 1){
    A[(k + 1):N, col] = A[1:(N-k), col]
  }
  if (keep.rows){
    A[1:k, col] = NA
    return(A)
  } else {
    return(A[(-1):(-k),])
  }
}

#' @export
vect.shift.down <- function(v, k=1, keep.rows=FALSE){
  n = length(v)
  v[(k + 1):n] = v[1:(n-k)]
  if (keep.rows){
    v[1:k] = NA
    return(v)
  } else {
    return(v[(-1):(-k)])
  }
}

#' @export
vect.shift.up <- function(v, k=1, keep.rows=FALSE){
  n = length(v)
  v[1:(n-k)] = v[(k + 1):n]
  if (keep.rows){
    v[(n-k+1):n] = NA
    return(v)
  } else {
    return(v[(-n+k-1):(-n)])
  }
}


#' @export
column.average.down <- function(A, col=1:dim(A)[2], k=1, keep.rows=FALSE){
  # For column(s) given as "col", each row is replaced by the moving average among
  # that row and "K" rows before that.
  d=dim(A)
  n = d[1]
  m = d[2]
  V = A
  for (j in col){
    for (i in (k+1):n){
      V[i, j] = mean(A[(i-k):i, j])
    }
  }
  
  if (keep.rows){
    V[1:k, col] = NA
    return(V)
  } else {
    return(V[(-1):(-k),])
  }
}

#' @export
regslope <- function(y){
  # Fits a line over the vector data and returns the slope of the regression line
  N      = length(y)
  x      = sequence(N)
  x.bar  = (N+1)/2
  x2.bar = x.bar*(2*N+1)/3
  y.bar  = mean(y)
  xy.bar = mean(x*y)
  return ((xy.bar - x.bar*y.bar)/(x2.bar - x.bar^2))
}

#' @export
column.regslope.down <- function(A, col=1:dim(A)[2], k=1, keep.rows=FALSE){
  # For column(s) given as "col", each row is replaced by the slope of the lease squares (regression) line among
  # that row and "K" rows before that.
  d=dim(A)
  n = d[1]
  m = d[2]
  V = A
  for (j in col){
    for (i in (k+1):n){
      V[i, j] = regslope(A[(i-k):i, j])
    }
  }
  
  if (keep.rows){
    V[1:k, col] = NA
    return(V)
  } else {
    return(V[(-1):(-k),])
  }
}

#' @export
column.delta.up <- function(A, col=1:dim(A)[2], k=1, keep.rows=FALSE){
  d=dim(A)
  n = d[1]
  m = d[2]
  
  V=A[,col]
  if (length(col)==1){V[1:(n-k)] = A[(k + 1):n, col]}
  else{V[1:(n-k),] = A[(k + 1):n, col]}
  A[,col] = A[,col] - V
  if (keep.rows){
    A[(n-k+1):n,col]=NA
    return(A)
  } else {
    return(A[(-n+k-1):(-n),])
  }
}

#' @export
column.average.up <- function(A, col=1:dim(A)[2], k=1, keep.rows=FALSE){
  # For column(s) given as "col", each row is replaced by the moving average among
  # that row and "K" rows after that.
  
  d=dim(A)
  n = d[1]
  m = d[2]
  
  V <- A
  for (j in col){
    for (i in 1:(n-k)){
      V[i,j] = mean(A[i:(k + i), j])
    }
  }
  if (keep.rows){
    V[(n-k+1):n,col]=NA
    return(V)
  } else {
    return(V[(-n+k-1):(-n),])
  }
}

#' @export
mat.standardize <- function(M, byrow = FALSE){
  # Standardizes each column and returns a matrix of the same dimention as the given matrix where
  # all the columns are standardized (centralized and divided by standard deviation)
  # if byrow is TRUE, then each row is considered as a vector and will be standardized
  if (byrow){return(t(scale(t(M))))} else {return(scale(M))}
}

#' @export
vect.centralize <- function(v){
  # This function centralizes the given vector
  # Each element is subtracted by the mean of vector
  # x_i <- x_i - mean(x)
  v_bar=mean(v)
  return(v - v_bar)
}

#' @export
vect.standardize <- function(v, cent = TRUE){
  # This function returns the z factors of given vector v
  return(v %>% scale(center = cent) %>% as.numeric)
}

#' Normalizes the given vector by dividing each element by the vector magnitude
#'
#' @param v a vector of numerics to be normalized
#' @param cent A logical. Should the vector be also centralized?
#' @return A numeric vector containing the normalized values. Sum of squares of elements of the output vector will be 1.0.
#' @examples
#' vect.normalize(c(3, 4))
#' [1] 0.6 0.8
#'
#' @export
vect.normalize <- function(v, cent = FALSE){
  # This function normalizes the given vector
  # and returns a unit vector parallel to the given vector
  if (cent){v=vect.centralize(v)}
  l = sqrt(sum(v*v))
  if (l==0.0){return(v)}
  else {return(v/l)}
}

#' @export
mat.normalize <- function(M, dimension = 2){
  assert(dimension %in% c(1,2), 'Error: dimension must be 1 or 2')
  S = apply(M, dimension, vect.normalize)
  if (dimension == 1){S = t(S)}
  return (S)
  
}

#' @export
vect.normalise <- function(v){
  # This function normalises the given vector by dividing all its elements by sum of elements
  # and returns a vector parallel to the given vector so that sum of all its elements is one.
  l = sum(v)
  if (l==0.0){
    print("vect.normalise Error: Sum of elements of the given matrix is zero. Given argument returned")
    return(v)}
  else {return(v/l)}
}

#' @export
column.delta.down <- function(A, col=1:dim(A)[2], k=1, keep.rows=FALSE){
  # This function gets matrix "A" as input and finds the difference between its column(s) given
  # by argument "col" and its shifted down column(s)
  
  d=dim(A)
  n = d[1]
  m = d[2]
  
  V=A[,col]
  if (length(col)==1){V[(k + 1):n] = A[1:(n-k), col]}
  else{V[(k + 1):n, ] = A[1:(n-k), col]}
  
  A[,col] = A[,col] - V
  if (keep.rows){
    A[1:k, col] = NA
    return(A)
  } else {
    return(A[(-1):(-k),])
  }
}

# spherical.dist = cosine.dissimilarity (Reflects the angle between the vectors)
#' @export
spherical.dist <- function(u,v){
  # Finds the cosine of the angle between two vectors and Returns the dissimilarity
  # of two vectors as 1-cos(u,v). The output is between 0 and 2
  u.mod   = sqrt(sum(u^2))
  v.mod   = sqrt(sum(v^2))
  uv.prod = sum(u*v)
  
  cos.theta = uv.prod/(u.mod*v.mod)
  return(1 - cos.theta)
}

#' @export
difference <- function (v1, v2) {
  # Returns the euclidean distance between two given vectors v1 & v2
  d = v1 - v2
  return (sqrt(sum(d^2)))
}

#' @export
vect.extend <- function(v, n){
  m = length(v)
  if (m >= n){
    return(v[sequence(n)])
  }
  ve = v
  while (n > m + length(ve)){ve = c(ve, v)}
  return (c(ve, v[sequence(n - length(ve))]))
}

#' @export
mat.extend <- function(M, n, on_rows = T){
  if (on_rows){m = nrow(M)}else{m = ncol(M)}
  if (m >= n){
    if (on_rows){return(M[sequence(n),, drop = F])}else{return(M[,sequence(n), drop = F])}
  }
  Me = M
  if (on_rows){
    while (n > m + nrow(Me)){Me = rbind(Me, M)}
    return (rbind(Me, M[sequence(n - nrow(Me)),, drop = F]))
  } else{
    while (n > m + ncol(Me)){Me = cbind(Me, M)}
    return (cbind(Me, M[,sequence(n - ncol(Me)), drop = F]))
  }
}

#' @export
vect.dist <- function(v1, v2, metric = 'euclidean'){
  n1 = length(v1)
  n2 = length(v2)
  assert(n1 == n2, 'Error: Given vectors must have the same length')
  if      (metric == 'euclidean'){return(difference(v1, v2))}
  else if (metric == 'spherical'){return(spherical.dist(v1, v2))}
  else if (metric == 'binary'){return(mean(xor(v1, v2)))}
  else if (metric == 'manhattan'){return(sum(abs(v1 - v2)))}
  else if (metric == 'canberra'){return(sum(abs(v1 - v2)/abs(v1 + v2), na.rm = TRUE))}
  else if (metric == 'maximum'){return(max(abs(v1 - v2)))}
  else {print('Error: Given metric is not recognized!')}
}

# Test this function with a list of square matrices
#' @export
mat.profile = function(M, cn = NULL, func = sum, on_rows = T, remove_na = T){
  if (!inherits(M, 'list')){M = list(M = M)}
  if (!inherits(func, 'list')){func = list(func)}
  
  dfcn = chooseif(inherits(cn, 'character'), chooseif(on_rows, colnames(M[[1]]), rownames(M[[1]])), chooseif(on_rows, sequence(ncol(M[[1]])),sequence(nrow(M[[1]]))))
  cn %<>% verify(c('character', 'integer', 'factor'), domain = dfcn, default = dfcn, varname = 'cn')
  
  if ((length(func) == 1) & (length(M) > 1)) (func = rep(func, length(M)))
  
  if (is.empty(M)) {return(matrix())}
  res = NULL
  for (i in sequence(length(M))){
    verify(func[[i]], 'function', varname = 'func')
    if(inherits(M[[i]], 'data.frame')){M[[i]] = as.matrix(M[[i]])}
    verify(M[[i]], 'matrix', dims = dim(M[[1]]), varname = 'M[[' %++% i  %++% ']]')
    
    column = chooseif(on_rows,M[[i]][, cn, drop = F], M[[i]][cn, , drop = F]) %>% apply(2 - on_rows, func[[i]])
    
    res %<>% cbind(column)
  }
  
  if (is.null(names(M))){colnames(res) <- paste('M', sequence(ncol(res)), sep = '.')} else {colnames(res) <- names(M)}
  if (on_rows){rownames(res) <- rownames(M[[1]])} else {rownames(res) <- colnames(M[[1]])}
  if (remove_na)  {res = res[!is.na(rowSums(res)),, drop = F]}
  
  return(res)
}

# Returns the geometric mean of vector v
#' @export
geomean = function(v, na.rm = T){
  v %>% log(base = exp(1)) %>% mean(na.rm = na.rm) %>% exp
}

#' @export
magnitude = function(v, na.rm = T){
  v^2 %>% as.numeric %>% sum(na.rm = na.rm) %>% sqrt
}

