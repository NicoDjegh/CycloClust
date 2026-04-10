#' Clusters' cyclical start, end, mean and range
#'
#' Function \code{cluster.cyclo.pos} computes indices of cluster position
#' (start, end, mean and range) of cyclical-time-constrained clusters along
#' different cycles.
#'
#' @details
#' A valuable aspect of a cyclical-time constrained clustering is the ability to
#' compare clusters' position in several cycles. Displacement of clusters can be
#' interpreted as advancing or delaying of a particular state along the cycles.
#' The function \code{cluster.cyclo.pos} compute four indices allowing related to
#' cluster position within cycles (see Value).
#'
#' @return
#' An object of class \code{\link{list}}, containing:
#' \itemize{
#'  \item{\code{means}: A table containing the mean position of each cluster in each cycle.}
#'  \item{\code{starts}: A table containing the starting position of each cluster along each cycle.}
#'  \item{\code{ends}: A table containing the ending position of each cluster along each cycle.}
#'  \item{\code{ranges}: A table containing the ranges covered by each cluster within each cycle.}
#' }
#'
#' @author Nicolas Djeghri, UBO
#'
#'
#' @encoding UTF-8
#' @name cluster.cyclo.pos
#'
#' @seealso \code{\link{cyclo.hclust}}
#'
#' @examples
#' #WRITE SOME R CODE IN THERE!!!
#'
#' @param x A cyclical-time-constrained clustering object, as provided by function \code{\link{cyclo.hclust}}.
#' @param k an integer scalar with the desired number of clusters (as in \code{\link{cutree}}).
#' @export

cluster.cyclo.pos <- function(x,
                              k)
{
  if(!inherits(x, "cycloClust")) stop("'x' should be of class `cycloClust`")

  groups <- cutree (x,k)


  angles.cyclic.time <- (x$times$cyclic.time/x$cycle.duration)*2*pi

  cycleMeans <- matrix(NA,length(unique(x$times$cycles)),length(unique(groups)))
  rownames(cycleMeans) <- unique(x$times$cycles)
  colnames(cycleMeans) <- unique(groups)
  cycleStarts <- cycleMeans
  cycleEnds <- cycleMeans

  for (i in unique(x$times$cycles)){
    index <- x$times$cycles==i

    groupsi <- groups[index]
    anglesi <- angles.cyclic.time[index]

    if (length(unique(groupsi))>1){
      means <- ((tapply(anglesi,groupsi,cyclo.mean)%%(2*pi))/(2*pi))*x$cycle.duration
      cycleMeans[as.character(i),] <- means[as.character(1:length(unique(groups)))]

      for (j in 1:length(groupsi)){
        if (j == 1){
          if (groupsi[j]!=groupsi[length(groupsi)]){
            cycleStarts[as.character(i),as.character(groupsi[j])] <- cyclo.mean(anglesi[c(1,length(groupsi))])
          }
          if (groupsi[j]!=groupsi[j+1]){
            cycleEnds[as.character(i),as.character(groupsi[j])] <- cyclo.mean(anglesi[1:2])
          }
        }else if (j == length(groupsi)){
          if (groupsi[j]!=groupsi[j-1]){
            cycleStarts[as.character(i),as.character(groupsi[j])] <- cyclo.mean(anglesi[c(j,j-1)])
          }
          if (groupsi[j]!=groupsi[1]){
            cycleEnds[as.character(i),as.character(groupsi[j])] <- cyclo.mean(anglesi[c(j,1)])
          }
        }else{
          if (groupsi[j]!=groupsi[j-1]){
            cycleStarts[as.character(i),as.character(groupsi[j])] <- cyclo.mean(anglesi[c(j,j-1)])
          }
          if (groupsi[j]!=groupsi[j+1]){
            cycleEnds[as.character(i),as.character(groupsi[j])] <- cyclo.mean(anglesi[c(j,j+1)])
          }
        }
      }
    }else{
      cycleMeans[as.character(i),as.character(unique(groupsi))] <- NaN
      cycleStarts[as.character(i),as.character(unique(groupsi))] <- NaN
      cycleEnds[as.character(i),as.character(unique(groupsi))] <- NaN
    }
  }
  cycleStarts <- ((cycleStarts%%(2*pi))/(2*pi))*x$cycle.duration
  cycleEnds <- ((cycleEnds%%(2*pi))/(2*pi))*x$cycle.duration

  cycleRanges <- ((cycleEnds+x$cycle.duration)-cycleStarts)%%x$cycle.duration

  output <- list()
  output$means <- cycleMeans
  output$starts <- cycleStarts
  output$ends <- cycleEnds
  output$ranges <- cycleRanges

  class(output) <- c("list")
  return(output)
}


