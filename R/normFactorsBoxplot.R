#' gives normalization factors, raw and normalized counts distribution box plots
#'
#' @param dds a DESeqDataSet object; dds after applying estimateSizeFactors
#'
#' @return An object created by \code{ggplot}
#' @export normFactorsBoxplot
#'
#' @examples
#'
#' @import ggplot2
#' @import patchwork
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

  rawCounts <- counts(dds, normalized = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    pivot_longer(cols = -gene_id, names_to = 'SampleName', values_to = 'rawCounts') %>%
    left_join(s_sheet, by='SampleName')

  normCounts <- counts(dds, normalized = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    pivot_longer(cols = -gene_id, names_to = 'SampleName', values_to = 'normCounts') %>%
    left_join(s_sheet, by='SampleName')

  plotTheme <- theme(
    panel.background = element_blank(),
    axis.text = element_text(color='blue'),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = 'none',
    plot.title = element_text(color='brown', face='bold', size=10, hjust = 0.5)
  )


  pNormFactors <- ggplot(normFactors, aes(x=SampleName, y=normFactors, color=SampleGroup)) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      x='',
      y='Factors',
      title = 'Normalisation Factor Distribution'
    ) +
    plotTheme

  pRawCounts <- ggplot(rawCounts, aes(x=SampleName, y=log2(rawCounts+1), color=SampleGroup)) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      x='',
      y='counts(log2)',
      title = 'Raw counts Distribution'
    ) +
    plotTheme

  pNormCounts <- ggplot(normCounts, aes(x=SampleName, y=log2(normCounts+1), color=SampleGroup)) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      x='Sample Name',
      y='counts(log2)',
      title = 'Normalised counts Distribution'
    ) +
    plotTheme +
    theme( legend.position = 'bottom',
           axis.text.x = element_text(color='blue', angle = 90)
           )

  pNormFactors / pRawCounts /  pNormCounts

}
