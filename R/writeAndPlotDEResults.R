

#' write DE results and create plots for each contrast
#'
#' @param dds dds DESeqDataSet object; dds after applying \code{estimateSizeFactors}, \code{estimateDispersions} and \code{nbinomWaldTest}
#' @param factorName s string
#' @param numerator a string; numerator in a contrast
#' @param denominator a string; denominator (reference) in a contrast
#' @param DeOutDir a string; directory name
#' @param gtf a GRanges object; typical output from \code{import.gff}
#'
#' @return
#' @export writeAndPlotDEResults
#'
#' @examples
#'
#' @importFrom dplyr filter select mutate left_join arrange
#' @importFrom tibble rownames_to_column
#' @importFrom readr write_csv
#' @importFrom stringr str_c str_replace_all
#' @importFrom rlang is_empty
#' @importFrom tidyselect everything
#' @importFrom DESeq2 results

writeAndPlotDEResults <- function(dds, factorName, numerator, denominator, DeOutDir, gtf){

  gtfTab <- gtf %>%
    as.data.frame() %>%
    filter(type=='gene') %>%
    dplyr::select(gene_id, gene_name, chr=seqnames, start, end, strand, gene_biotype) %>%
    mutate(gene_name=toupper(gene_name)) %>%
    mutate(gene_biotype = str_replace_all(gene_biotype, '_', ' '))

  res <- results(dds, contrast=c(factorName, numerator, denominator), alpha=0.5) %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    left_join(gtfTab, by='gene_id') %>%
    dplyr::select(gene_id,gene_name, everything()) %>%
    arrange(padj)

  deOutFile <- str_c(factorName,numerator, 'vs', denominator, sep='_') %>%
    str_c(.,'csv', sep='.') %>%
    str_c(DeOutDir, ., sep='/')

  write_csv(x=res, file=deOutFile)
}
