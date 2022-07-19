hierarchicalClustPlot <- function(countsDat, s_sheet, colorByCol='SampleGroup', horizontal=TRUE, title=""){

  maxColors <- s_sheet %>%
    pull(colorByCol) %>%
    unique() %>%
    length()

}
