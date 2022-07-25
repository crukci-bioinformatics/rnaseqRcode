checkArg_correlationPlot <- function(countsMat, s_sheet){
  assert_that(is.matrix(countsMat))
  is_validMetaData(s_sheet = s_sheet, columnsToCheck = 'SampleName')
  is_validMetaData(s_sheet = s_sheet, columnsToCheck = 'SampleGroup')
}
#' Correlation plot from counts matrix
#'
#' @param countsMat a counts matrix
#' @param s_sheet  a data frame; sample meta data sheet
#'
#' @return
#' @export correlationPlot
#'
#' @examples
#'
#' @importFrom pheatmap pheatmap
#' @importFrom stats cor
#'
correlationPlot <- function(countsMat, s_sheet){

  checkArg_correlationPlot(countsMat=countsMat, s_sheet = s_sheet)

  corrMat <- cor(countsMat)

  annotData <- s_sheet %>%
    column_to_rownames("SampleName") %>%
    select(SampleGroup)

  pheatmap(corrMat,
           color = colorRampPalette(brewer.pal(n = 9, name = "Oranges"))(50),
           annotation_col= annotData,
           annotation_row = annotData)
}
