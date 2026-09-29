#' Title
#'
#' @param gtfFile; a string
#'
#' @return a GRanges object
#' @export loadGTF
#'
#' @examples
#' @importFrom rtracklayer  import.gff
#' @importFrom assertthat assert_that
#'
loadGTF <- function(gtfFile){
  assert_that(file.exists(gtfFile))
  gtf <- tryCatch(import.gff( gtfFile, format='gtf'),
                  error = function(cond){
                    message(cond)
                    stop('Provide correct GTF file')
                  }
  )
  return(gtf)
}
