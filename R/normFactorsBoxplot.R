#' gives normalization factors, raw and normalized counts distribution box plots
#'
#' @param dds a DESeqDataSet object; dds after applying estimateSizeFactors
#'
#' @return A list that contains two plots created by \code{ggplot}
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
    as.data.frame() %>%
    arrange(SampleGroup)

  rawCounts <- counts(dds, normalized = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    pivot_longer(cols = -gene_id, names_to = 'SampleName', values_to = 'rawCounts') %>%
    left_join(s_sheet, by='SampleName') %>%
    mutate(SampleName = factor(SampleName, levels = s_sheet$SampleName))

  normCounts <- counts(dds, normalized = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    pivot_longer(cols = -gene_id, names_to = 'SampleName', values_to = 'normCounts') %>%
    left_join(s_sheet, by='SampleName') %>%
    mutate(SampleName = factor(SampleName, levels = s_sheet$SampleName))

  plotTheme <- theme_classic() +
    theme(
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = 'none',
    plot.title = element_text(color='black', face='bold', size=10, hjust = 0.5)
  )

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
           axis.text.x = element_text(angle = 90)
           )

  return(list( pRawCounts=pRawCounts,
               pNormCounts=pNormCounts
               )
         )

}
