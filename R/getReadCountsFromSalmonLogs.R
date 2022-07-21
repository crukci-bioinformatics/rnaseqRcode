# check arguments
checkArg_getReadCountsFromSalmonLogs <- function(s_sheet, quantOut, logFiles){
  assert_that(is_validMetaData(s_sheet = s_sheet, columnsToCheck = 'SampleName'))
  assert_that(dir.exists(quantOut))
  is_fileExists(logFiles)
}
#' gets reads pre sample table from salmon log files
#'
#' @param s_sheet a data frame; metadata sheet
#' @param quantOut a vector of length one; salmon output folder
#'
#' @return a data frame; sample and number of fragments table.
#' @export getReadCountsFromSalmonLogs
#'
#' @examples
#'
#' @importFrom stringr str_c str_detect str_split
#' @importFrom magrittr %>%
#' @importFrom purrr map_df
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything
#' @importFrom readr write_csv
#'
getReadCountsFromSalmonLogs <- function(s_sheet, quantOut){

  logFiles <- str_c(quantOut, s_sheet$SampleName, 'logs', 'salmon_quant.log', sep='/')
  names(logFiles) <- s_sheet$SampleName

  checkArg_getReadCountsFromSalmonLogs(s_sheet = s_sheet,
                                       quantOut = quantOut,
                                       logFiles = logFiles)

  readCounts <- logFiles %>%
    map_df( function(x){
      readLines(x) %>%
        .[str_detect(.,'^Observed')] %>%
        str_split(., ' ', simplify=TRUE) %>%
        .[1,2] %>%
        as.integer()
    }) %>%
    pivot_longer(cols = everything(), names_to = 'SampleName', values_to = 'fragments')

  outFile <- str_c(x=quantOut, 'readsPerSample.csv', sep='/')
  write_csv(x=readCounts, file =outFile )
  return(readCounts)

}
