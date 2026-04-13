#' Clusters' cyclical start, end, mean and range
#'
#' Function \code{plot.cyclo.clust} provides options to visualize the cyclical-
#' time constrained clusters produced by \code{\link{cyclo.hclust}}.
#'
#' @author Nicolas Djeghri, UBO
#'
#'
#' @encoding UTF-8
#' @name plot.cyclo.clust
#'
#' @seealso \code{\link{cyclo.hclust}}, \code{\link{cluster.cyclo.pos}}
#'
#' @examples
#' #WRITE SOME R CODE IN THERE!!!
#'
#' @param x a cyclical-time-constrained clustering object, as provided by function \code{\link{cyclo.hclust}}.
#' @param k an integer scalar with the desired number of clusters (as in \code{\link{cutree}}).
#' @param clust.colors a vector of colors of the same length as \code{k}. Defaults to shades of grey if not provided.
#' @param cyclical.factor a factor allowing to convert from the unit used in linear time to the unit used in cyclical time used for display.
#' @param add.means boolean. Should the cluster mean positions be displayed?
#' @param lines.means boolean. Should the means be joined by lines?
#' @param add.points boolean. Should the raw data points be displayed?
#' @param ... other parameters parsed to function \code{\link{plot}}.
#' @export
plot.cyclo.clust <- function(x,
                             k,
                             clust.colors = NULL,
                             cyclical.factor = 1,
                             add.means = F,
                             lines.means =F,
                             add.points = F,
                             ...)
{
  if(!inherits(x, "cyclo.hclust")) stop("'x' should be of class `cyclo.hclust`")

  if(is.null(clust.colors)){
    ramp <- seq(0,1,1/(k-1))
    clust.colors <- rgb(ramp,ramp,ramp)
  }
  if(length(clust.colors)!=k) stop("the number of colors provided should correspond to 'k'")

  groups <- cutree (x,k)
  limits <- cluster.cyclo.pos(x,k)
  names(clust.colors) <- colnames(limits$means)

  plot(NA,NA,ylim=c(0,x$cycle.duration*cyclical.factor),
       xlim=c(min(x$times$cycles)-x$cycle.duration/2,max(x$times$cycles)+x$cycle.duration/2),
       xaxs="i",yaxs="i",...)
  starts <- apply(limits$starts,2,lin.cyclo.ts,cycle.duration=x$cycle.duration,start.is.0=F)

  for (i in names(starts)){
    startsi <- rep(starts[[i]],each=2)*cyclical.factor
    endsi <- startsi + limits$ranges[names(startsi),i]*cyclical.factor
    endsi <- endsi[length(endsi):1]
    ys <- c(startsi,endsi)

    xs <- as.numeric(names(startsi))+c(-x$cycle.duration/2,x$cycle.duration/2)
    xs <- c(xs,xs[length(xs):1])

    polygon(x=xs,y=ys,col=clust.colors[i],border=NA)
    polygon(x=xs,y=ys+x$cycle.duration*cyclical.factor,col=clust.colors[i],border=NA)
    polygon(x=xs,y=ys-x$cycle.duration*cyclical.factor,col=clust.colors[i],border=NA)
  }
  box()
  if (add.points == T){
    points(x=x$times$cycles,y=x$times$cyclic.time*cyclical.factor)
  }
  if (add.means == T){
    means <- apply(limits$means,2,lin.cyclo.ts,cycle.duration=x$cycle.duration,start.is.0=F)
    for (i in colnames(limits$means)){
      if (lines.means ==T){
        points(x=as.numeric(names(means[[i]])),
               y=means[[i]]*cyclical.factor,
               type="l")
        points(x=as.numeric(names(means[[i]])),
               y=means[[i]]*cyclical.factor+x$cycle.duration*cyclical.factor,
               type="l")
        points(x=as.numeric(names(means[[i]])),
               y=means[[i]]*cyclical.factor-x$cycle.duration*cyclical.factor,
               type="l")
      }
      points(x=as.numeric(names(limits$means[,i])),
             y=limits$means[,i]*cyclical.factor,
             xpd=NA,pch=16)
    }
  }
}
