# check arguments
checkArg_getGeneDetctionPlot <- function(s_sheet, countsData, thresholds){
  is_validMetaData(s_sheet, columnsToCheck = 'SampleName')
  is_validMetaData(s_sheet, columnsToCheck = 'SampleGroup')
  assert_that(is.matrix(countsData))
  assert_that(is.vector(thresholds))
}


#' Gives gene detection plot
#'
#' @param countsData a matrix; raw counts matrix
#' @param s_sheet a data frame; sample metadata sheet
#' @param thresholds a vector of counts thresholds
#'
#' @return An object created by \code{ggplot}
#' @export getGeneDetctionPlot
#'
#' @examples
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr arrange group_by desc summarize filter mutate left_join

getGeneDetctionPlot <- function(countsData, s_sheet, thresholds=seq(0,50, by=5)){

  checkArg_getGeneDetctionPlot(countsData=countsData, s_sheet=s_sheet, thresholds=thresholds)

  plotDat <- countsData %>%
    .[rowSums(.) > 0, ] %>%
    as.data.frame() %>%
    rownames_to_column(var='gene_id') %>%
    pivot_longer(-gene_id, names_to = "SampleName", values_to = "counts") %>%
    arrange(desc(counts)) %>%
    group_by(SampleName) %>%
    summarize(
      detection_threshold = counts,
      number_of_genes = row_number(),
      .groups = "drop"
    ) %>%
    group_by(SampleName, detection_threshold) %>%
    summarize(number_of_genes = max(number_of_genes), .groups = "drop") %>%
    filter(detection_threshold %in% thresholds) %>%
    mutate(detection_threshold=as.factor(detection_threshold))

  plotDat <- left_join(plotDat, s_sheet, by='SampleName')


  p <- ggplot(plotDat,
              aes(x=detection_threshold,
                  y=number_of_genes, group = SampleName, color=SampleGroup) ) +
    geom_line() +
    labs(
      x = 'Counts',
      y = 'Number of Genes',
      color='Sample Group',
      title = 'Gene detection'
    ) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text(color='blue'),
      legend.position = 'bottom',
      plot.title = element_text(color='brown', hjust = 0.5, size=15, face='bold')
    )

  return(p)
}
