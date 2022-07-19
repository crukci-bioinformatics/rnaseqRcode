# function for checking arguments
checkArg_importSampleTxiAndSaveRds <- function(s_sheet, quantOut, tx2gene, ...){
  assert_that(is_validMetaData(s_sheet, columnsToCheck = 'SampleGroup'))
  assert_that(is_validTx2gene(tx2gene))
  assert_that(dir.exists(quantOut))
}
#' Save combined txi object after reading sample level txi data
#'
#' @param s_sheet A data frame with at least one column named SampleName.
#' @param quantOut A character vector, salmon output path.
#' @param tx2gene A data frame with TxID (transcript ids) and GeneID columns.
#' @param quantTool A character vector, quantification tool, like salmon
#' @param save Boolean value, do you want to save combined txi object, default is true, saves 'txi.rds' in quantOut
#'
#' @return a txi object
#' @export importSampleTxiAndSaveRds
#'
#' @examples
#' @importFrom tximport tximport
#' @importFrom stringr str_c
#'
importSampleTxiAndSaveRds <- function(s_sheet, quantOut, tx2gene, quantTool="salmon", save=TRUE) {
  checkArg_importSampleTxiAndSaveRds(s_sheet, quantOut, tx2gene)

  if(is.data.frame(s_sheet) & dir.exists(quantOut) & is.data.frame(tx2gene)) {
    txiPaths <- file.path( quantOut, s_sheet$SampleName, "quant.sf")
    names(txiPaths) <- s_sheet$SampleName
    txi <- tximport(txiPaths, type = quantTool, tx2gene = tx2gene)

    if(save) {
      txiOutFile <- str_c(quantOut, "txi.rds", sep='/')
      saveRDS(txi, file = txiOutFile)
    }
    return(txi)
  } else {
    message("Check arguments for 'importSampleTxiAndSaveRds' function")

  }
}
