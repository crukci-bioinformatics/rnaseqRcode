##################################################################################
# collection of function argument checking helper functions
##################################################################################

#' @import assertthat
#' @importFrom  magrittr %>%
#' @export


##################################################################################
is_validMetaData <- function( s_sheet, columnsToCheck ){

  okay <- FALSE
  if(is.data.frame(s_sheet)){

    # given columns present in metadata columns?
    if(sum(colnames(s_sheet) %in% columnsToCheck) > 0 ){
      okay <- TRUE
    }else{
      message('Given columns not found in metadata columns!')
      okay <- FALSE
    }
  }else{
    message('Metadata should be a data frame')
    okay <- FALSE
  }
  return(okay)
}
##################################################################################
# check if tx2gene is valid
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
##################################################################################
