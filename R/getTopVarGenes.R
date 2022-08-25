# check arguments
checkArg_getTopVarGenes <- function(countsDat,topN){
  assert_that(is.matrix(countsDat))
  assert_that(is.numeric(topN))
}
#' Gives top highly variable genes
#'
#' @param countsDat a matrix
#' @param topN an integer
#'
#' @return a matrix
#' @export getTopVarGenes
#'
#' @examples
#'
#' @importFrom matrixStats rowVars
#'
getTopVarGenes <- function(countsDat, topN=1000){
  checkArg_getTopVarGenes(countsDat=countsDat, topN=topN)

  vars <- rowVars(countsDat)
  topVriableMat <- countsDat[head(order(vars, decreasing=TRUE), n=topN ), ]
  return(topVriableMat)
}
