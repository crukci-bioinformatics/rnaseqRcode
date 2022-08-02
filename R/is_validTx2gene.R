#' tests if tx2gene format is correct
#'
#' @param tx2gene a data frame
#'
#' @return Boolean value
#' @export is_validTx2gene
#'
#' @examples
is_validTx2gene <- function(tx2gene){
  okay <- FALSE
  if(is.data.frame(tx2gene)){
    okay <- TRUE
  }else{
    message( 'tx2gene should be a data frame' )
    okay <- FALSE
  }
  return(okay)
}
