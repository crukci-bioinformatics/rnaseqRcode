#' tests if file exists
#'
#' @param filesToCheck a vector of files to test
#'
#' @return Boolean value
#' @export is_fileExists
#'
#' @examples
#' @importFrom purrr map_lgl
#'
is_fileExists <- function(filesToCheck){

  filesToCheck %>%
    map_lgl( function(x){
      file.exists(x)
    }) %>%
    all()
}
