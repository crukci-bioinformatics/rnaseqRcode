checkArg_correlationPlot <- function(countsMat){
  assert_that(is.matrix(countsMat))
}
#' Correlation plot from counts matrix
#'
#' @param countsMat a counts matrix
#'
#' @return
#' @export correlationPlot
#'
#' @examples
#'
#' @importFrom  corrplot corrplot
#' @importFrom stats cor
#'
correlationPlot <- function(countsMat){

  checkArg_correlationPlot(countsMat)

  corrMat <- cor(countsMat)
  corrplot(corr=corrMat,
           method='color',
           outline='grey',
           tl.col='blue',
           tl.srt=45,
           col = brewer.pal(n=8, name="YlOrBr"))

}
