# Argument check function
checkArg_getPcaPlot <- function(countsDat, s_sheet, columnsToCheck, PCx, PCy) {
  assert_that(is.matrix(countsDat))
  assert_that(is_validMetaData(s_sheet = s_sheet, columnsToCheck = columnsToCheck))
  assert_that(is.numeric(PCx))
  assert_that(is.numeric(PCy))

}


#' get PCA plots given counts matrix and sample sheet
#'
#' @param countsDat a counts matrix
#' @param s_sheet a data frame; sample sheet used for plot aesthetics
#' @param pcaColFactor a character vector with one value; column name in a sample sheet for PCA color aesthetic
#' @param pcaShapeFac a character vector with one value; column name in a sample sheet for PCA shape aesthetic
#' @param PCx an integer, X-axis PC, default is 1, that is PC1
#' @param PCy an integer, Y-axis PC, default is 2, that is PC2
#'
#' @return a plot
#' @export getPcaPlot
#'
#' @examples
#'
#' @import ggfortify
#' @importFrom ggplot2 theme_classic autoplot
#'
getPcaPlot <- function(countsDat, s_sheet,
                       pcaColFactor="SampleGroup",
                       pcaShapeFac=NULL,
                       PCx=1,
                       PCy=2) {
  checkArg_getPcaPlot(countsDat,s_sheet,
                      columnsToCheck=c(pcaColFactor,pcaShapeFac),
                      PCx=PCx,
                      PCy=PCy
                      )

  pcaData <- prcomp(t(countsDat), center = TRUE, scale. = TRUE )

  p <- autoplot(pcaData,
                x=PCx,
                y=PCy,
                data=s_sheet,
                fill=pcaColFactor,
                shape = 21,
                size=5,
                alpha=0.6
                ) +
    theme_classic() +
    theme(
      legend.position = 'top'
    )

  return(p)

}
