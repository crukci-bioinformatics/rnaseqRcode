checkArg_readsPerGroupBoxplot <- function(s_sheet, readCounts){

  assert_that(is_validMetaData(s_sheet, columnsToCheck = c('SampleName')))
  assert_that(is_validMetaData(s_sheet, columnsToCheck = c('SampleGroup')))

  assert_that(is.data.frame(readCounts))
  assert_that(is.element('SampleName', names(readCounts)))
  assert_that(is.element('fragments', names(readCounts)))

}

#' Sample group read counts distribution box plot
#'
#' @param s_sheet a data frame; sample metadata with at lest two columns SampleName and SampleGroup
#' @param readCounts a data frame with at least two columns SampleName and fragments
#'
#' @return An object created by \code{ggplot}
#' @export readsPerGroupBoxplot
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate inner_join
#'
readsPerGroupBoxplot <- function(s_sheet, readCounts){

  checkArg_readsPerGroupBoxplot(s_sheet=s_sheet, readCounts=readCounts)

  readCounts <- readCounts %>%
    mutate( fragments = fragments / 1000000)

  readCounts <- inner_join(readCounts, s_sheet, by='SampleName')

  len <- length(unique(readCounts$SampleGroup))

  p <- ggplot(data=readCounts, mapping = aes(x=SampleGroup, y=fragments, color=SampleGroup)) +
    geom_boxplot(fill='black', color='grey') +
    geom_point(position = position_jitterdodge()) +
    theme_classic() +

    labs(
      title = 'Sample group read distribution',
      y='Fragments (million)'
    ) +

    scale_color_manual(values = rep('orange',len) ) +

    theme(
      plot.title = element_text(hjust = 0.5, size=15, color='brown', face='bold'),
      axis.text = element_text(color='blue', face='bold'),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      legend.position = "none"
    )

  return(p)
}
