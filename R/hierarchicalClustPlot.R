# check arguments
checkArg_hierarchicalClustPlot <- function(countsDat, s_sheet, colorByCol='SampleGroup',
                                           horizontal=TRUE, sampleColors=NULL, title=""){
  assert_that(is.matrix(countsDat))
  assert_that(is.string(colorByCol))
  assert_that(is_validMetaData(s_sheet, columnsToCheck = colorByCol))
  assert_that(is.flag(horizontal))
  assert_that(is_validSampleColors(s_sheet=s_sheet, sampleColors=sampleColors, colorByCol=colorByCol))
  assert_that(is.string(title))
}
#' Hierarchical clustering plot
#' Computes and displays hierarchical clustering plot for samples in a given counts data
#'
#' @param countsDat a matrix;  vst or rlog transformed counts matrix
#' @param s_sheet a data frame; sample metadata
#' @param colorByCol a character vector; name of sample metadata column to color the samples

#' @param horizontal a boolean value; orientation of the dendrogram. Default is TRUE.
#' @param sampleColors a vector of colors; if NULL, colors are automatically assigned.
#' @param title a character vector; the main title for the dendrogram.
#'
#' @return An object created by \code{ggplot}
#' @export hierarchicalClustPlot
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom dplyr across left_join mutate
#' @importFrom ggdendro dendro_data label
#' @importFrom magrittr %>%
#' @importfrom rlang sym
#' @importFrom stats as.dendrogram dist hclust
#' @importFrom utils head
#'
hierarchicalClustPlot <- function(countsDat, s_sheet, colorByCol='SampleGroup',
                                  horizontal=TRUE, sampleColors=NULL, title=""){

  if(is.null(sampleColors)){
    sampleColors <- assignColors(s_sheet=s_sheet, colorByCol=colorByCol)
  }

  checkArg_hierarchicalClustPlot(countsDat=countsDat,
                                 s_sheet=s_sheet,
                                 sampleColors = sampleColors ,
                                 colorByCol=colorByCol,
                                 horizontal=horizontal,
                                 title=title)

  dendroData <- t(countsDat) %>%
    dist(method = "euclidean") %>%
    hclust() %>%
    as.dendrogram() %>%
    dendro_data()

  colorByCol <- sym(colorByCol)

  labelDat <- dendroData$labels %>%
    mutate(SampleName = as.character(label)) %>%
    left_join(s_sheet, "SampleName") %>%
    mutate(across(!!colorByCol, as.factor))

  axisBreaks <- pretty(dendroData$segments$yend)[-1] %>% head(-1)

  if (horizontal) {
    hj <- 0
    ny <- 1
    ang <- 0
  }
  if (!horizontal) {
    hj <- 1
    ny <- -1
    ang <- 90
  }

  hcPlot <- ggplot(dendroData$segment) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(
      data = labelDat,
      aes(x = x, y = y, label = SampleName, colour = !!colorByCol),
      hjust = hj, nudge_y = ny, angle = ang, fontface='bold'
    ) +
    guides(colour = "none") +
    scale_colour_manual(values = sampleColors) +
    labs(x = NULL, y = "Distance", title = title)
  if (horizontal) {
    hcPlot <- hcPlot +
      scale_y_reverse(expand = c(0.2, 0), breaks = axisBreaks) +
      coord_flip() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank()
      )
  } else {
    hcPlot <- hcPlot +
      scale_y_continuous(expand = c(0.2, 0), breaks = axisBreaks) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank()
      )
  }
  return(hcPlot)
}
