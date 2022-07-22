# check arguments
checkArg_readsPerSampleBarPlot <- function(readCounts){
  assert_that(is.data.frame(readCounts))
  assert_that( is.element('SampleName', names(readCounts)))
  assert_that(is.element('fragments', names(readCounts)))
}
#' barplot of read counts per sample.
#'
#' @param readCounts a data frame with at least two columns SampleName and fragments
#'
#' @return a ggplot object
#' @export readsPerSampleBarPlot
#'
#' @examples
#'
#' @importFrom dplyr mutate
#' @import ggplot2
#'
readsPerSampleBarPlot <- function(readCounts){

  checkArg_readsPerSampleBarPlot(readCounts)
  readCounts <- readCounts %>%
    mutate( fragments = fragments / 1000000)

  p <- ggplot(data=readCounts, mapping=aes(x=SampleName, y=fragments)) +
    geom_bar(stat = 'identity', fill='black') +
    labs(
      title = 'reads/sample',
      x='',
      y='Fragments (million)'
    ) +
    geom_hline(yintercept = 20, color='yellow', size=1) +
    coord_flip() +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = 'bold', size=20, color = 'brown'),
      axis.text = element_text(face='bold', colour = "blue"   )
    )
  return(p)
}
