#' CycloClust: Cyclical-time constrained clustering
#'
#' Constrained clustering for time series with cyclical dynamics
#' (seasonality, day-night cycles etc.).
#'
#' @name CycloClust-package
#' @aliases CycloClust CycloClust-package
#' @docType package
#' @author Nicolas Djeghri
#' \email{nicolas.djeghri@@gmail.com}
#' [ORCID](https://orcid.org/0000-0001-5740-3386)
#'
#' @details
#' The package revolves around function \code{\link{cyclo.hclust}}, derived from
#' \code{\link[adespatial]{constr.hclust}}, and allowing to perform cyclical-time
#' constrained hierarchical clustering.
#' Once a clustering is obtained, \code{CycloClust} provides functions to follow
#' changes in cluster position within cycles (see \code{\link{cluster.cyclo.pos}},
#' \code{\link{trend.cyclo.pos}}) and to visualize clusters
#' (\code{\link{plot.cyclo.clust}}).
#'
#'
#' @seealso \code{\link[adespatial]{constr.hclust}}, \code{\link{hclust}}
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats dist as.dist cutree cor.test
#' @importFrom adespatial constr.hclust
#' @importFrom grDevices rgb
#' @importFrom graphics polygon box points
## usethis namespace: end
NULL
