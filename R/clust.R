# Header
# Filename:     clust.R
# An empty vector of total within-group sum of squares. This vector will contain
# total WG-SS for each num.cluster (number of clusters)
# Version History:
# Version   Date                 Action
# ----------------------------------
# 1.0.0     16 September 2013    Initial Issue.
# 1.1.0     20 September 2017    function elbow.plot() renamed to elbow() and modified:
#                                returns wss, Argument doPlot added, if TRUE plots
# 1.2.0     21 September 2016    Function bnc() added: best number of clusters
# 1.2.1     21 September 2016    Function elbow() modified and exported: returns a list containing:
#                                wgss: within group sum of squares
#                                clst: clustering list: list of all kmenas or skmeans clusterings
#                                bnc: best number of clusters
# 1.2.2     21 September 2016    Argument bnc_threshold added to functions bnc() and elbow()


cluster.distances <- function(M, SK){
  # Returns a vector containing the distances of each point from the center of
  # its cluster.
  N = nrow(M)
  distances = c()
  for (i in 1:N){
    distances = c(distances, spherical.distance(M[i,],SK$prototype[SK$cluster[i], ]))
  }
  return(distances)
}

cluster.moments <- function(M, SK){
  # Returns the within group sum of dissimilarities (moments) of the given spherical clustering
  #   Input 1: M (A n X m Matrix of data, where n is the number of points (vectors or objects) and
  #               m is the number of dimensions of the vectors. )
  #               (points are arranged as rows)
  #   Input 2: SK (A spherical k-means object. Output of function skmeans())
  #   Output: A vector of length k of real numbers containing sum of dissimilarities of each cluster,
  #           where k is the number of clusters.
  k = dim(SK$prototypes)[1]
  n = dim(M)[1]
  wgss = rep(0, k)
  for (i in 1:n){  # For each vector
    cluster.number = SK$cluster[i]
    wgss[cluster.number] =  wgss[cluster.number] + spherical.distance(M[i,], SK$prototype[cluster.number,])
  }
  return(wgss)
}

#' Use this function to find the best number of clusters
#'
#' @param M A matrix containing vectors for clustering
#' @param max.num.clusters Maximum number of clusters
#' @param metric Either 'euclidean' or 'spherical' determining the metric used for clustering
#' @param bnc_threshold Specifies the threshold for reduction ratio in within group sum of squares
#' @param doPlot logical: Would you like to see the elbow plot to determine the number of clusters?
#' @return list: containing three elements: wgss (Within group sum of squares), clst (list of clustering objects), bnc(best number of clusters)
#' @examples
#' a = elbow(iris[,1:4], doPlot = T)
#'
#' a$wgss
#' [1] 681.37060 152.34795  78.85144  71.44525  46.44618  43.68323  37.39514  36.09792  31.79668  29.28663
#'
#' a$clst[[a$bnc]]$cluster %>% table
#' .
#' 1  2  3  4  5
#' 12 50 24 39 25
#' @export
elbow <- function(M, max.num.clusters = 10, metric = "euclidean", bnc_threshold = 0.2, doPlot = T) {
  if(!inherits(M, 'matrix')){M %<>% as.matrix}
  assert(nrow(M) >= max.num.clusters, 'Number of rows less than maximum number of clusters', err_src = 'elbow')
  assert(max.num.clusters > 1, 'Maximum number of clusters must be greater than 1', err_src = 'elbow')
  
  wgss = c()
  clst = list()
  
  if (metric == "euclidean"){
    for (num.clusters in 1:max.num.clusters) {
      K    = kmeans(M, num.clusters)
      wgss = c(wgss, K$tot.withinss)
      clst %<>% list.add(K)
    }
    if(doPlot){plot(1:max.num.clusters, wgss, type="b")}
    return(list(wgss = wgss, clst = clst, bnc = wgss %>% bnc(bnc_threshold = bnc_threshold)))
  }
  else if (metric == "spherical"){
    library("skmeans")
    for (num.clusters in 2:max.num.clusters) {
      SK    = skmeans(M, num.clusters)
      tot.withinss = sum(cluster.moments(M, SK))
      wgss = c(wgss, tot.withinss)
      clst %<>% list.add(K)
    }
    if(doPlot){plot(2:max.num.clusters, wgss, type="b")}
    return(list(wgss = wgss, clst = clst, bnc = wgss %>% bnc(bnc_threshold = bnc_threshold)))
  }
}

bnc <- function(wss, bnc_threshold = 0.2){
  N   = length(wss)
  names(wss) = N %>% sequence %>% as.character
  w   =  which((wss[-N] - wss[-1])/wss[-N] < bnc_threshold)
  while(!is.empty(w)){
    wss = wss[- w - 1]
    N   = length(wss)
    w   =  which((wss[-N] - wss[-1])/wss[-N] < bnc_threshold)
  }
  
  return(names(wss)[N] %>% as.integer)
}
