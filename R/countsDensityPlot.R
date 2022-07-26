# check arguments
checkArg_countsDensityPlot <- function(s_sheet, countsData){
  is_validMetaData(s_sheet, columnsToCheck = 'SampleName')
  is_validMetaData(s_sheet, columnsToCheck = 'SampleGroup')
  assert_that(is.matrix(countsData))
}

#' count density plot
#' @param s_sheet a data frame; sample metadata sheet
#' @param countsData a matrix; raw counts matrix
#'
#' @return An object created by \code{ggplot}
#' @export countsDensityPlot
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#'
countsDensityPlot <- function(s_sheet, countsData){

  checkArg_countsDensityPlot(s_sheet=s_sheet, countsData=countsData)

  tab <- countsData[rowSums(countsData) > 0, ] %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    pivot_longer(cols = -gene_id, names_to = 'SampleName', values_to = 'counts') %>%
    left_join(s_sheet, by='SampleName')

  p <- ggplot(tab, aes(x=log2(counts+1), group=SampleName, color=SampleGroup)) +
    geom_density( alpha=0.4, size=0.75) +
    labs(
      y='Density',
      x='Counts(log2 + 1)',
      title = 'Count Density'
    ) +
    theme(
      legend.position = 'bottom',
      panel.background = element_blank(),
      axis.text = element_text(color='blue', face='bold'),
      plot.title = element_text(size=20, face='bold', color='brown', hjust = 0.5)
    )
  return(p)
}
