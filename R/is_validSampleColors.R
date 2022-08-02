#' Tests that given sample colors valid
#'
#' @param s_sheet data frame; sample metadata sheet
#' @param sampleColors colors vector
#' @param colorByCol column name
#'
#' @return Boolean value
#' @export is_validSampleColors
#'
#' @examples
#' @importFrom assertthat assert_that
#'
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
