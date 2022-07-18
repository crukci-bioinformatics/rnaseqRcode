#' Save combined txi object after reading sample level txi data
#'
#' @param s_sheet A data frame with at least one column named SampleName.
#' @param quantOut A character vector, salmon output path.
#' @param tx2gene A data frame with TxID (transcript ids) and GeneID columns.
#' @param quantTool A character vector, quantification tool, like salmon
#' @param save Boolean value, do you want to save combined txi object, default is true, saves 'txi.rds' in quantOut
#'
#' @return a txi object
#' @export
#'
#' @examples
#'
importSampleTxiAndSaveRds <- function(s_sheet, quantOut, tx2gene, quantTool="salmon", save=TRUE) {
  txiPaths <-file.path( quantOut, s_sheet$SampleName, "quant.sf")
  names(txiPaths) <- s_sheet$SampleName
  txi <- tximport(txiPaths, type = quantTool, tx2gene = tx2gene)

  if(save) {
    txiOutFile <- str_c(quantOut, "txi.rds", sep='/')
    saveRDS(txi, file = txiOutFile)
  }
  return(txi)
}
