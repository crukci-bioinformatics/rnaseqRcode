# check arguments
checkArg_assignColors <- function(s_sheet,colorBy){
  assert_that( is_validMetaData(s_sheet, columnsToCheck = colorBy))
}

#' Assign colors to samples / sample groups
#'
#' @param s_sheet, a data frame; metadata sheet
#' @param colorBy character vector of length 1; column name of the metadata sheet for color assignment.
#'
#' @return a named vector of colors
#' @export assignColors
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @import RColorBrewer
#' @importFrom dplyr pull
assignColors <- function(s_sheet, colorBy="SampleGroup") {
  checkArg_assignColors(s_sheet, colorBy)

  colorGroups <- s_sheet %>%
    pull(colorByCol) %>%
    unique()

  len <- length(colorGroups)
  clean <- c(len, 3) %>% max() %>% c(12) %>% min()
  colorsF <- brewer.pal(clean, "Set3") %>% colorRampPalette()
  sampleColors <- setNames(colorsF(len), colorGroups)
  return(sampleColors)
}
