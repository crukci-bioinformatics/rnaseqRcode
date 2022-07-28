getPvalDistriPlot <- function(res, numerator, denominator, pValCutoff){

  res <- res %>%
    mutate(Truth = if_else( pvalue < pValCutoff, 'Alternate', 'Null')) %>%
    arrange(padj)

  ggplot(res, aes(x=pvalue, color=Truth)) +
    geom_histogram(bins=20, fill='white', size=1, na.rm =TRUE ) +
    scale_color_manual(values = c('black', 'orange')) +
    theme(
      panel.background = element_blank()
    )


}
