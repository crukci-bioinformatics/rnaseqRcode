#' Filter and transform raw counts
#'
#' @param rawCounts a counts matrix
#' @param countsCutOff an integer, keep only rows with a total row count of at least countsCutOff
#' @param FUN a function name for transformation. vst is the default transformation function, but rlog, log2, etc., can also be selected.
#'
#' @return a transformed counts matrix
#' @export transformCounts
#'
#' @examples
#' @importFrom DESeq2 vst rlog
#'
transformCounts <- function(rawCounts, countsCutOff = 10, FUN=vst) {

  if(is.matrix(rawCounts) & is.function(FUN)){
    filtCounts <- rawCounts[rowSums(rawCounts) >= countsCutOff, ]
    trfCounts <- FUN(filtCounts)
    return(trfCounts)
  } else {
    message("Supply counts matrix and a valid function")
  }
}
