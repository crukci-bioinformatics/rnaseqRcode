# Argument check function
checkArg_getPcaPlot <- function(countsDat, s_sheet, columnsToCheck) {
  assert_that(is.matrix(countsDat))
  assert_that(is_validMetaData(s_sheet = s_sheet, columnsToCheck = columnsToCheck))
}


#' get PCA plots given counts matrix and sample sheet
#'
#' @param countsDat a counts matrix
#' @param s_sheet a data frame; sample sheet used for plot aesthetics
#' @param pcaColFactor a character vector with one value; column name in a sample sheet for PCA color aesthetic
#' @param pcaShapeFac a character vector with one value; column name in a sample sheet for PCA shape aesthetic
#'
#' @return a plot
#' @export getPcaPlot
#'
#' @examples
#'
#' @import ggfortify
#' @importFrom ggplot2 theme_classic autoplot
#'
getPcaPlot <- function(countsDat, s_sheet, pcaColFactor="SampleGroup", pcaShapeFac=NULL) {
  checkArg_getPcaPlot(countsDat,s_sheet, columnsToCheck=c(pcaColFactor,pcaShapeFac))

  pcaData <- prcomp(t(countsDat), center = TRUE, scale. = TRUE )

  p <- autoplot(pcaData,
                data=s_sheet,
                colour=pcaColFactor,
                shape=pcaShapeFac
                ) +
    theme_classic()

  return(p)

}
