# check arguments
checkArg_getGeneCountsPlot <- function(dds, factorName, numerator,
                                       denominator, gtf,
                                       pValCutoff, genesToShow, topN){

  assert_that(is(dds, 'DESeqDataSet'))
  assert_that(is.string(factorName))
  assert_that(is.string(numerator))
  assert_that(is.string(denominator))
  assert_that(is.string(genesToShow) | is.null(genesToShow))
  assert_that(is(gtf, 'GRanges'))
  assert_that(is.numeric(pValCutoff))
  assert_that(is.numeric(topN))

}

#' Gives read counts scater plot for topN genes
#'
#' @param dds dds DESeqDataSet object; dds after applying \code{estimateSizeFactors}, \code{estimateDispersions} and \code{nbinomWaldTest}
#' @param factorName s string
#' @param numerator a string; numerator in a contrast
#' @param denominator a string; denominator (reference) in a contrast
#' @param gtf a GRanges object; typical output from \code{import.gff}
#' @param pValCutoff numeric value; padj cut off value to classify as significant
#' @param genesToShow  a string; list genes to show
#' @param topN integer; number of top gene names to show on plot; default top 6
#'
#' @return An object created by \code{ggplot}
#' @export getGeneCountsPlot
#'
#' @examples
#'
#' @importFrom dplyr filter select mutate left_join arrange bind_rows
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_c str_replace_all str_split
#' @importFrom rlang is_empty
#' @importFrom DESeq2 results counts
#' @importFrom assertthat is.string
#' @importFrom purrr map_chr
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
getGeneCountsPlot <- function(dds, factorName, numerator,
                              denominator, gtf,
                              pValCutoff=0.05, genesToShow=NULL, topN=6){


  checkArg_getGeneCountsPlot(dds=dds, factorName=factorName, numerator=numerator,
                             denominator=denominator, gtf=gtf,
                             pValCutoff=pValCutoff, genesToShow=genesToShow, topN=topN)

  if(!is.null(genesToShow)){
    genesToShow <- genesToShow %>%
      str_split(.,',', simplify = T) %>%
      as.vector() %>%
      map_chr(str_remove_all, ' ')
  }

  contr <- c(factorName, numerator, denominator)

  res <- results(dds, contrast=contr, alpha=pValCutoff) %>%
    as.data.frame()

  res <- addGeneInfoFromGtfToResTab(res=res, gtf=gtf) %>%
    arrange(padj)

  topTab <- res %>%
    head(n=topN)

  selTab <- res %>%
    filter( gene_name %in% genesToShow)

  if( nrow(selTab) > 0){
    topTab <- bind_rows(topTab, selTab)
  }

  topTab <- topTab %>%
    select(gene_id, gene_name)


  normCounts <- counts(dds, normalized= TRUE) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id')

  s_sheet <- colData(dds) %>%
    as.data.frame()

  plotTab <- left_join(topTab, normCounts, by='gene_id') %>%
    pivot_longer(cols=-gene_id:-gene_name, names_to = "SampleName", values_to = 'counts') %>%
    left_join(s_sheet, by="SampleName") %>%
    mutate(counts = log2(counts+1))

  p <- ggplot(plotTab, aes(x=SampleGroup, y=counts)) +
    geom_jitter(width=0, shape=21, fill='brown', size=2, alpha=0.5) +
    labs(
      x='Sample Group',
      y='Counts (log2)'
    ) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text(color='blue'),
      axis.text.x = element_text(angle = 90)
    ) +
    facet_wrap(vars(gene_name))

  return(p)

}
