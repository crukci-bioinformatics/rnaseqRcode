# check arguments
checkArg_readsPerSampleBarPlot <- function(readCounts, s_sheet){
  assert_that(is.data.frame(readCounts))
  assert_that( is.element('SampleName', names(readCounts)))
  assert_that(is.element('fragments', names(readCounts)))
  is_validMetaData(s_sheet, columnsToCheck = 'SampleName')
  is_validMetaData(s_sheet, columnsToCheck = 'SampleGroup')
}
#' barplot of read counts per sample.
#'
#' @param readCounts a data frame with at least two columns SampleName and fragments
#' @param s_sheet a data frame; sample metadata with at lest two columns SampleName and SampleGroup
#'
#' @return a ggplot object
#' @export readsPerSampleBarPlot
#'
#' @examples
#'
#' @importFrom dplyr mutate
#' @import ggplot2
#'
readsPerSampleBarPlot <- function(readCounts, s_sheet){

  checkArg_readsPerSampleBarPlot(readCounts, s_sheet)

  readCounts <- left_join(readCounts, s_sheet, by='SampleName') %>%
    arrange(SampleGroup) %>%
    mutate(SampleName = factor(SampleName, levels = SampleName))
  p <- ggplot(data=readCounts, mapping=aes(x=SampleName, y=fragments, fill=SampleGroup)) +
    geom_bar(stat = 'identity') +
    labs(
      title = 'reads/sample',
      x='',
      y='Fragments (million)'
    ) +
    geom_hline(yintercept = 20, color='black') +
    coord_flip() +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = 'bold', size=20, color = 'black'),
      legend.position = 'bottom'
    )
  return(p)
}
