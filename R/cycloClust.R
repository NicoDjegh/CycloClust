#' Cyclical time-Constrained Clustering
#'
#' Function \code{cycloClust} performs cyclical-time-constrained agglomerative clustering from a multivariate dissimilarity matrix.
#' The function piggy-backs largely on function \code{\link{constr.hclust}} by P. Legendre and G. Guénard, from package \code{\link{adespatial}},
#' the user is strongly advised to check the corresponding documentation as well as documentation from \code{\link{hclust}}, the more general R function for hierarchical clustering.
#'
#' @details
#'
#' @return
#'
#' @author Nicolas Djeghri, UBO
#'
#' @references
#'
#' @encoding UTF-8
#' @name cycloClust
#'
#' @seealso \code{\link{constr.hclust}},\code{\link{hclust}}
#'
#' @examples
#'
#' @param d A class \code{\link{dist}} dissimilarity matrix.
#' @param method The agglomeration method to be used (parsed to \code{\link{constr.hclust}}).
#' @param time A vector indicating the linear time corresponding to objects in \code{d}.
#' @param cycle.duration A number indicating the duration of one cycle. Must be in the same units as \code{time}.
#' @param cyclic.link.tolerance a number indicating how far in cyclic time a link can reach (see details). Defaults to \code{cycle.duration/10}.
#' @export


cycloClust <- function(d,
                       method = "ward.D2",
                       time,
                       cycle.duration,
                       cyclic.link.tolerance = cycle.duration/10)
{
  if(!inherits(d, "dist")) stop("'d' should be of class `dist`")
  if(any(table(time)>1)) stop("Duplicate times are not supported (only one time-series treated)")
  if(length(cycle.duration)>1) stop("'cycle.duration' should be a single number")
  if(cyclic.link.tolerance>cycle.duration) stop("'cyclic.link.tolerance' should be smaller than 'cycle.duration'")

  reord <- order(time)
  time <- time[reord]
  d <- as.matrix(d)
  d <- d[reord,reord]
  d <- as.dist(d)

  origin <- 1:(length(time)-1)
  target <- 2:length(time)

  cyclic.time <- time%%cycle.duration
  d1 <- as.matrix(dist(cyclic.time))
  d2 <- cycle.duration - d1
  cyclic.d <- cbind(as.vector(d1),as.vector(d2))|>
    apply(1,min)|>
    matrix(length(cyclic.time),length(cyclic.time))

  linear.d <- as.matrix(dist(time))

  cyclic.d[upper.tri(cyclic.d,diag=T)] <- NA
  cyclic.d[linear.d > (cycle.duration + cyclic.link.tolerance)] <- NA
  cyclic.d[linear.d < (cycle.duration - cyclic.link.tolerance)] <- NA
  colnames(cyclic.d) <- 1:length(time)

  truc <- unlist(apply(cyclic.d,2,which.min))

  origin <- c(origin,as.numeric(names(truc)))
  target <- c(target,as.numeric(truc))
  links <- data.frame(origin,target)

  clust <- adespatial::constr.hclust(d,method=method,links=links)
  clust$times <- data.frame(times=time,cycles=time-cyclic.time,cyclic.time=cyclic.time)
  clust$cycle.duration <- cycle.duration

  class(clust) <- c("cycloClust",class(clust))
  return(clust)
}
