# check arguments
checkArg_getPvalDistriPlot <- function(res, numerator, denominator, pValCutoff){

  assert_that(is.data.frame(res))
  assert_that(is.string(numerator))
  assert_that(is.string(denominator))
  assert_that(is.numeric(pValCutoff))
}
#' Gives p value distribution plot
#'
#' @param res a data frame; DESeq2 results converted to data frame
#' @param numerator a string; numerator in a contrast
#' @param denominator a string; denominator in a contrast
#' @param pValCutoff numeric value; padj cut off value
#'
#' @return An object created by \code{ggplot}
#' @export getPvalDistriPlot
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom dplyr  mutate if_else arrange
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#' @importFrom assertthat is.string

getPvalDistriPlot <- function(res, numerator, denominator, pValCutoff){

  checkArg_getPvalDistriPlot( res=res, numerator=numerator,
                              denominator=denominator,
                              pValCutoff=pValCutoff )

  pTitle <- str_c( numerator, 'vs', denominator, sep=' ')

  p <- ggplot(res, aes(x=pvalue)) +
    geom_histogram(bins=20, fill='white', color='darkgrey', size=1, na.rm =TRUE ) +
    labs(x='p values',
         y='Frequency',
         title = pTitle
    ) +
    theme_classic() +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(color='black', size=15, hjust=0.5, face='bold'),
      legend.position = 'bottom'
    )
  return(p)
}
