# check arguments
checkArg_assignColors <- function(s_sheet,colorByCol){
  assert_that( is_validMetaData(s_sheet, columnsToCheck = colorByCol))
}

#' Assign colors to samples / sample groups
#'
#' @param s_sheet, a data frame; metadata sheet
#' @param colorByCol character vector of length 1; column name of the metadata sheet for color assignment.
#'
#' @return a named vector of colors
#' @export assignColors
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @import RColorBrewer
#' @importFrom dplyr pull
#' @import grDevices
#' @importFrom stats setNames
#'
assignColors <- function(s_sheet, colorByCol="SampleGroup") {
  checkArg_assignColors(s_sheet, colorByCol)

  colorGroups <- s_sheet %>%
    pull(colorByCol) %>%
    unique()

  len <- length(colorGroups)
  maxColorsInSet <- 12

  if(len <= maxColorsInSet){
    clean <- c(len, 3) %>% max() %>% c(maxColorsInSet) %>% min()
    colorsF <- brewer.pal(clean, "Set3") %>% colorRampPalette()
    sampleColors <- setNames(colorsF(len), colorGroups)
    return(sampleColors)
  }else{
    message( 'More groups than avilable colors, assigning orange to every group' )
    defaultColor <- '#FFA500'
    sampleColors <- setNames(rep(defaultColor, len), colorGroups)
    return(sampleColors)
  }
}
