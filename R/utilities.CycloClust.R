#' Utility functions for CycloClust
#'
#' Function \code{cyclo.mean} computes cyclical (i.e. circular) means while
#' function \code{lin.cyclo.ts} "linearizes" a cyclical time-series (see details).
#' This is particularly useful to test trends along time (OTHER FUNCTION???).
#'
#' @details
#' Function \code{cyclo.mean} computes a cyclical (i.e. circular) mean in radians.
#'
#' Function \code{lin.cyclo.ts} returns a "linearized" version of a cyclical
#' time-series (mostly used for time-series of dates along cycles). The function
#' works by computing distances along the cycles between all consecutive points.
#' Note that there is two ways to compute the distance between two point from a
#' cyclical variable: clockwise or counter-clockwise. \code{lin.cyclo.ts} always
#' takes the shortest path (most conservative option). The function then assigns
#' a sign to this distance as a function of its rotational direction (positive
#' when going in the direction of time, negative when going opposite). The
#' "linearized" time-series is obtained with the cumulative sum of those
#' distances.
#'
#'
#' @author Nicolas Djeghri, UBO
#'
#' @references
#' NONE for now!
#'
#' @encoding UTF-8
#' @name utilities.CycloClust
#' @aliases cyclo.mean lin.cyclo.ts
#'
#' @seealso \code{\link{cyclo.hclust}},\code{\link{cluster.cyclo.pos}}
#'
#' @examples
#' #WRITE SOME R CODE HERE!!!
#'
#' @rdname utilities.CycloClust
#' @param x A vector containing angles (in radians) to be averaged
#' @param na.rm Boolean. Should the \code{NA} be removed prior to circular mean computation?
#' @export
cyclo.mean <- function(x,na.rm=T){
  truc <- atan2(mean(sin(x),na.rm=na.rm),mean(cos(x),na.rm=na.rm))
  return(truc)
}

#' @rdname utilities.CycloClust
#' @param ts A circular time series that is to be linearized.
#' @param cycle.duration The duration of a cycle in ts
#' @param start.is.0 Boolean. Should the start of the linearized time series be set to 0? Else it is taken as the first value of \code{ts}.
#' @export
lin.cyclo.ts <- function(ts,cycle.duration,start.is.0=T){

  past <- ts[1:(length(ts)-1)]
  future <- ts[2:length(ts)]

  a1 <- abs(future-past)
  a2 <- cycle.duration-abs(future-past)
  mins <- as.numeric(apply(cbind(a1,a2),1,which.min))
  mins[is.na(mins)] <- "FYIWDWYTM"

  diffs <- rep(NA,length(past))
  for (j in 1:length(past)){
    if(mins[j] == 1){
      diffs[j] <- future[j]-past[j]
    }else if (mins[j] == 2){
      diffs[j] <- a2[j]*sign(past[j]-future[j])
    }
  }

  diffs <- diffs[is.na(diffs)==FALSE]

  changes <- cumsum(c(0,diffs))
  if (start.is.0==F){
    changes <- changes + ts[is.na(ts)==F][1]
  }
  names(changes) <- names(which(is.na(ts)==FALSE))

  return(changes)
}
