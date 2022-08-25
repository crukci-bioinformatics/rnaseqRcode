# check arguments
checkArg_createDdsAndDESeq <- function(txi, s_sheet, design){
  assert_that( object.size(txi) != 0 & is.list(txi))
  checkAndCreateCorrectDesignString( s_sheet = s_sheet, design = design )
}

#' Create DDS object and do basic DE
#'
#' @param txi a txi object
#' @param s_sheet a data frame; sample metadata sheet
#' @param design a string; design
#' @param fitType a string; fit Type for dispersion estimates
#'
#' @return a DESeqDataSet object
#' @export createDdsAndDESeq
#'
#' @examples
#'
#' @import DESeq2
#'
createDdsAndDESeq <- function(txi, s_sheet, design, fitType = 'parametric'){

  checkArg_createDdsAndDESeq(txi=txi, design=design, s_sheet=s_sheet)

  ddsRaw <- DESeqDataSetFromTximport(txi = txi,
                                         colData = s_sheet,
                                         design = as.formula(design))
  ddsFilt <- ddsRaw[rowSums(counts(ddsRaw)) > 5, ]

  dds <- estimateSizeFactors(ddsFilt)
  dds <- estimateDispersions(dds, fitType=fitType)
  dds <- nbinomWaldTest(dds)
  return(dds)
}
