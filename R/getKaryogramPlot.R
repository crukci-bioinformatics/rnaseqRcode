# check arguments
checkArg_getKaryogramPlot <- function( dds, factorName,
                                       numerator, denominator,
                                       gtf, pValCutoff, genome){
  assert_that(is(dds, 'DESeqDataSet'))
  assert_that(is.string(factorName))
  assert_that(is.string(numerator))
  assert_that(is.string(denominator))
  assert_that(is(gtf, 'GRanges'))
  assert_that(is.numeric(pValCutoff))
  assert_that(is.string(genome) | is.null(genome))

}
#' Gives Karyogram Plot
#'
#' @param dds dds DESeqDataSet object; dds after applying \code{estimateSizeFactors}, \code{estimateDispersions} and \code{nbinomWaldTest}
#' @param factorName s string
#' @param numerator a string; numerator in a contrast
#' @param denominator a string; denominator (reference) in a contrast
#' @param gtf a GRanges object; typical output from \code{import.gff}
#' @param pValCutoff a numeric value
#' @param genome a string; species name like 'Homo_sapiens'
#'
#' @return An object created by \code{ggplot}
#' @export  getKaryogramPlot
#'
#' @examples
#' @importFrom dplyr filter select mutate left_join arrange if_else
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_c str_to_sentence str_replace
#' @importFrom DESeq2 results
#' @importFrom assertthat is.string
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom GenomicRanges GRanges
#' @importFrom GenomeInfoDb keepStandardChromosomes



getKaryogramPlot <- function(dds, factorName,
                             numerator, denominator,
                             gtf, pValCutoff=0.05, genome=NULL){

  checkArg_getKaryogramPlot(dds=dds,
                            factorName=factorName,
                            numerator=numerator,
                            denominator=denominator,
                            gtf=gtf,
                            pValCutoff=pValCutoff,
                            genome=genome)

  if(!is.null(genome)){
    genome <- genome %>%
      str_replace(.,' ', '_' ) %>%
      str_to_sentence()
  }

  contr <- c(factorName, numerator, denominator)
  res <- results(dds, contrast=contr, alpha=pValCutoff) %>%
    as.data.frame()
  res <- addGeneInfoFromGtfToResTab(res=res, gtf=gtf) %>%
    drop_na()

  resGR <- with(res, GRanges(seqnames=chr, ranges = IRanges(start=start, end=end, strand=strand),
                             log2FoldChange=log2FoldChange, padj=padj, gene_id=gene_id,
                             gene_name=gene_name ))
  resGR$Regulation <- if_else( resGR$log2FoldChange > 0 &
                                 resGR$padj < pValCutoff, 'Signif. Up',
                               if_else(resGR$log2FoldChange < 0 &
                                         resGR$padj < pValCutoff, 'Signif. Down', 'Not Signif.'))


  resGR <- keepStandardChromosomes(resGR, pruning.mode='tidy', species=genome )

  title <- str_c(numerator, 'vs', denominator, sep=' ')
  p <- ggbio::autoplot(resGR,
                       layout="karyogram",
                       aes(color=Regulation, fill=Regulation)) +
    scale_fill_manual(values = c('grey', 'blue', 'red'), aesthetics='fill') +
    scale_color_manual(values = c('grey', 'blue', 'red'), aesthetics='color') +
    labs(
      title=title
    ) +
    theme(
      plot.title = element_text( color='brown', size=15, face='bold', hjust=0.5)
    )

  return(p)
}
