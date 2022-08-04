checkArg_writeAndPlotDEResults <- function(dds, factorName,
                                           numerator, denominator,
                                           DeOutDir, gtf, pValCutoff){
  assert_that(is(dds, 'DESeqDataSet'))
  assert_that(is.string(factorName))
  assert_that(is.string(numerator))
  assert_that(is.string(denominator))
  assert_that(dir.exists(DeOutDir))
  assert_that(is(gtf, 'GRanges'))
  assert_that(is.numeric(pValCutoff))

}

#' write DE results and create plots for each contrast
#'
#' @param dds dds DESeqDataSet object; dds after applying \code{estimateSizeFactors}, \code{estimateDispersions} and \code{nbinomWaldTest}
#' @param factorName s string
#' @param numerator a string; numerator in a contrast
#' @param denominator a string; denominator (reference) in a contrast
#' @param DeOutDir a string; directory name
#' @param gtf a GRanges object; typical output from \code{import.gff}
#' @param pValCutoff a numeric value
#'
#' @return a data frame after \code{lfcShrink} applied
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
#' @importFrom DESeq2 results lfcShrink
#' @importFrom assertthat is.string

writeAndPlotDEResults <- function(dds, factorName, numerator, denominator, DeOutDir, gtf, pValCutoff=0.05){

  checkArg_writeAndPlotDEResults(dds=dds, factorName=factorName,
                                 numerator=numerator, denominator=denominator,
                                 DeOutDir=DeOutDir, gtf=gtf,
                                 pValCutoff=pValCutoff)



  contr <- c(factorName, numerator, denominator)
  res <- results(dds, contrast=contr, alpha=pValCutoff) %>%
    as.data.frame()
  res <- addGeneInfoFromGtfToResTab(res=res, gtf=gtf)

  shrinkRes <- lfcShrink(dds=dds, contrast=contr, type='ashr') %>%
    as.data.frame()
  shrinkRes <- addGeneInfoFromGtfToResTab(res=shrinkRes, gtf=gtf)


  deOutFile <- str_c(factorName,numerator, 'vs', denominator, sep='_') %>%
    str_c(.,'csv', sep='.') %>%
    str_c(DeOutDir, ., sep='/')

  write_csv(x=res, file=deOutFile)
  return(shrinkRes)
}
