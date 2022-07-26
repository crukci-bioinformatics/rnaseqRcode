##################################################################################
# collection of function argument checking helper functions
##################################################################################

#' @import assertthat
#' @importFrom  magrittr %>%
#' @importFrom grDevices col2rgb
#' @import stringr
#' @importFrom purrr map_lgl
#' @export


##################################################################################
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

##################################################################################
# is color valid?
is_validColor <- function(color){
  tests <- map_lgl(color, ~tryCatch(is.matrix(col2rgb(.x)),
                                     error = function(e) FALSE))
  all(tests)
}
##################################################################################

##################################################################################
# check sample colors for plotting
is_validSampleColors <- function(s_sheet, sampleColors, colorByCol ){
  assert_that(is_validColor(sampleColors))
  colnm <- names(sampleColors)

  colbynm <- s_sheet[[colorByCol]] %>% unique()


  assert_that(!is.null(colnm),
              msg = str_c("sampleColors should be a named vector, where the ",
                          "names are values in the ", colorByCol, " column of ",
                          "the sample sheet"))
  assert_that(length(colbynm)==length(colnm),
              msg = str_c("sampleColors should provide a color for each ",
                          "value in the ", colorByCol, " column of the s_sheet ",
                          "object"))
  assert_that(all(colbynm %in% colnm) && all(colnm %in% colbynm),
              msg = str_c("The names of sampleColors do not match the values ",
                          "in the ", s_sheet, " column of the s_sheet object"))
}
##################################################################################


##################################################################################
# check if files exists
is_fileExists <- function(filesToCheck){

  filesToCheck %>%
    map_lgl( function(x){
      file.exists(x)
    }) %>%
    all()
}
##################################################################################

##################################################################################
checkAndCreateCorrectDesignString <- function(s_sheet, design){

  assert_that(!design == '', msg = 'Give factor(s) names for DESEq2 analysis')

  factors <- design %>%
    str_remove('~') %>%
    str_split('[+:,]', simplify = TRUE) %>%
    as.vector()

  assert_that( all(factors %in% names(s_sheet)),
               msg="the sample sheet columns don't match those in the design")

  corDesignStr <- str_c(factors, collapse ='+') %>%
    str_c('~', ., sep='')

  return(corDesignStr)
}


##################################################################################
