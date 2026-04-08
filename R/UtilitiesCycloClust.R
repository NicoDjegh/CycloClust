# Utility functions for CycloClust performing basic tasks related to cyclical (circular) time.

#a function for computing circular means
circ.mean <- function(x,na.rm=T){
  truc <- atan2(mean(sin(x),na.rm=na.rm),mean(cos(x),na.rm=na.rm))
  return(truc)
}


#a function to linearize time series define on circular variables (allows trend tests etc...)
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
