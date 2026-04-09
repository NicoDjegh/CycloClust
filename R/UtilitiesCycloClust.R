#' Utility function for CycloClust
#'
#' Function \code{circ.mean} computes circular means while function \code{linearize.circ.ts} "linearizes" a circular time series (see details).
#' This is particularly useful to test trends along time (see examples).
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
#' @name UtilitiesCycloClust
#' @aliases circ.mean linearize.circ.ts
#'
#' @seealso \code{\link{cycloClust}},\code{\link{cyclicPosition}}
#'
#' @examples


#' @rdname UtilitiesCycloClust
#' @param x A vector containing angles (in radians) to be averaged
#' @param na.rm Boolean. Should the \code{NA} be removed prior to circular mean computation?
#' @export
circ.mean <- function(x,na.rm=T){
  truc <- atan2(mean(sin(x),na.rm=na.rm),mean(cos(x),na.rm=na.rm))
  return(truc)
}

#' @rdname UtilitiesCycloClust
#' @param ts A circular time series that is to be linearized.
#' @param cycle.duration The duration of a cycle in ts
#' @param start.is.0 Boolean. Should the start of the linearized time series be set to 0? Else it is taken as the first value of \code{ts}.
#' @export
linearize.circ.ts <- function(ts,cycle.duration,start.is.0=T){

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
  names(changes) <- names(which(is.na(ts))==FALSE)

  return(changes)
}
