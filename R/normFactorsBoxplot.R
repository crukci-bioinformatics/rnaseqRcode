#' gives normalization factors distribution box plot
#'
#' @param dds a DESeqDataSet object; dds after applying estimateSizeFactors
#'
#' @return An object created by \code{ggplot}
#' @export normFactorsBoxplot
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom DESeq2 normalizationFactors
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join

normFactorsBoxplot <- function(dds){
  assert_that(is(dds, 'DESeqDataSet'), msg = 'Object is not DESeqDataSet')
  s_sheet <- colData(dds) %>%
    as.data.frame()

  normFactors <- normalizationFactors(dds) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    pivot_longer(cols = -gene_id, names_to = 'SampleName', values_to = 'normFactors') %>%
    left_join(s_sheet, by='SampleName')

  p <- ggplot(normFactors, aes(x=SampleName, y=normFactors, color=SampleGroup)) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      x='Sample Name',
      y='Normalisation Factors',
      title = 'Normalisation Factor Distribution'
    ) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text(color='blue', face='bold'),
      axis.text.x = element_text(angle=90),
      axis.ticks.x = element_blank(),
      legend.position = 'bottom',
      plot.title = element_text(color='brown', face='bold', size=15, hjust = 0.5)
    )

  return(p)

}
