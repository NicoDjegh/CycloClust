#' Trend tests for clusters' cyclical start, end, mean and range
#'
#' Function \code{trend.cyclo.pos} tests for linear trends in the clusters'
#' cyclical position indices obtained by \code{\link{cluster.cyclo.pos}}.
#'
#' @details
#' The function performs a Mann-Kendall (using function \code{\link{cor.test}})
#' trend test on the different clusters' cyclical position indices obtained by
#' function \code{\link{cluster.cyclo.pos}}.
#'
#' @return
#' An object of class \code{\link{list}}, with four tables giving the results of
#' the Mann-Kendall tests (tau statistic and p.value) for the different cyclical
#' position indices:
#' \itemize{
#'  \item{\code{means}}
#'  \item{\code{starts}}
#'  \item{\code{ends}}
#'  \item{\code{ranges}}
#' }
#' @seealso \code{\link{cluster.cyclo.pos}}, \code{\link{cor.test}}
#'
#' @author Nicolas Djeghri, UBO

trend.cyclo.pos <- function(x)
{
  if(!inherits(x, "cluster.cyclo.pos")) stop("'x' should be an output of function cluster.cyclo.pos()")

  output <- list()
  output$means <- matrix(NA,2,ncol(x$means))
  colnames(output$means) <- colnames(x$means)
  rownames(output$means) <- c("tau","p.value")

  output$starts <- output$means
  output$ends <- output$means
  output$ranges <- output$means

  for (i in c("means","starts","ends","ranges")){
    ts <- apply(x[[i]],2,lin.cyclo.ts,cycle.duration=x$cycle.duration)
    for (j in colnames(x[[i]])){
      test <- cor.test(y = ts[[j]],
                       x = as.numeric(names(ts[[j]])),
                       method="kendall")
      output[[i]]["tau",j] <- test$estimate
      output[[i]]["p.value",j] <- test$p.value
    }
  }
  class(output) <- c("list")
  return(output)
}
