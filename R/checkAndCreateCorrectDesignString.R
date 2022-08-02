#' Tests given design is correct
#'
#' @param s_sheet a data frame
#' @param design a string
#'
#' @return a string
#' @export checkAndCreateCorrectDesignString
#'
#' @examples
#' @importFrom assertthat assert_that
#' @importFrom stringr str_remove str_split str_c
#'
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
