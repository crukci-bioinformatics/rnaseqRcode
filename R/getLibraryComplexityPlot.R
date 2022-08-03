# check arguments
checkArg_getLibraryComplexityPlot <- function(s_sheet, countsData){
  is_validMetaData(s_sheet, columnsToCheck = 'SampleName')
  is_validMetaData(s_sheet, columnsToCheck = 'SampleGroup')
  assert_that(is.matrix(countsData))
}

#' Gives library complexity plot
#'
#' @param countsData a matrix; raw counts matrix
#' @param s_sheet a data frame; sample metadata sheet
#'
#' @return An object created by \code{ggplot}
#' @export getLibraryComplexityPlot
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#'
getLibraryComplexityPlot <- function(countsData, s_sheet){
  checkArg_getLibraryComplexityPlot(countsData=countsData, s_sheet=s_sheet )

  countsData <- countsData[rowSums(countsData) > 0, ]
  countFrac <- map_dfr(1:ncol(countsData), function(i) {
    cts <- sort(countsData[,i], decreasing=T)
    tibble(
      SampleName = colnames(countsData)[i],
      countsFraction = cumsum(cts)/sum(cts),
      genesFraction = (1:length(cts))/length(cts)
    )
  })

  countFrac <- left_join(countFrac, s_sheet, by='SampleName')

  p <- ggplot(countFrac,
              aes(x=genesFraction, y=countsFraction,
                  group=SampleName, color=SampleGroup)) +
    geom_line() +
    labs(
      x = 'Fraction of Genes',
      y = 'Fraction of Counts',
      color='Sample Group',
      title = 'Library complexity'
    ) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text(color='blue'),
      legend.position = 'bottom',
      plot.title = element_text(color='brown', hjust = 0.5, size=15, face='bold')
    )
  return(p)
}
