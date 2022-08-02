#' tests if color string is valid
#'
#' @param color a string
#'
#' @return Boolean value
#' @export is_validColor
#'
#' @examples
#' @importFrom purrr map_lgl
is_validColor <- function(color){
  tests <- map_lgl(color, ~tryCatch(is.matrix(col2rgb(.x)),
                                    error = function(e) FALSE))
  all(tests)
}
