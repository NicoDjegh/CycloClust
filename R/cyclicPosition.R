#' Clusters' cyclical start, end, mean and range
#'
#' Function \code{cyclicPosition} computes the start, end, mean and range of cyclical-time-constrained clusters along the time series cycles.
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
#' @name cyclicPosition
#'
#' @seealso \code{\link{cycloClust}},
#'
#' @examples
#'
#' @param x A cyclical-time-constrained clustering object, as provided by function \code{\link{cycloClust}}.
#' @param k an integer scalar with the desired number of clusters (as in \code{\link{cutree}}).
#' @export

cyclicPosition <- function(x,
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
      means <- ((tapply(anglesi,groupsi,circ.mean)%%(2*pi))/(2*pi))*x$cycle.duration
      cycleMeans[as.character(i),] <- means[as.character(1:length(unique(groups)))]

      for (j in 1:length(groupsi)){
        if (j == 1){
          if (groupsi[j]!=groupsi[length(groupsi)]){
            cycleStarts[as.character(i),as.character(groupsi[j])] <- circ.mean(anglesi[c(1,length(groupsi))])
          }
          if (groupsi[j]!=groupsi[j+1]){
            cycleEnds[as.character(i),as.character(groupsi[j])] <- circ.mean(anglesi[1:2])
          }
        }else if (j == length(groupsi)){
          if (groupsi[j]!=groupsi[j-1]){
            cycleStarts[as.character(i),as.character(groupsi[j])] <- circ.mean(anglesi[c(j,j-1)])
          }
          if (groupsi[j]!=groupsi[1]){
            cycleEnds[as.character(i),as.character(groupsi[j])] <- circ.mean(anglesi[c(j,1)])
          }
        }else{
          if (groupsi[j]!=groupsi[j-1]){
            cycleStarts[as.character(i),as.character(groupsi[j])] <- circ.mean(anglesi[c(j,j-1)])
          }
          if (groupsi[j]!=groupsi[j+1]){
            cycleEnds[as.character(i),as.character(groupsi[j])] <- circ.mean(anglesi[c(j,j+1)])
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

  return(output)
}


