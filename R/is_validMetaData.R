#' check if the metadata is valid
#'
#' @param s_sheet a data frame
#' @param columnsToCheck string; column name
#'
#' @return Boolean value
#' @export is_validMetaData
#'
#' @examples
#'
is_validMetaData <- function( s_sheet, columnsToCheck ){

  okay <- FALSE
  if(is.data.frame(s_sheet)){

    # given columns present in metadata columns?
    if(is.element(columnsToCheck, names(s_sheet)) ){
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
