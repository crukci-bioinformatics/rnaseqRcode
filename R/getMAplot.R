getMAplot <- function(res, numerator, denominator, selGenes=NULL, topN = 20 ){

  res <- res %>%
    mutate(is_significant = if_else( padj < 0.05, 'Significant', 'Not significant', missing = 'NA'))

  ggplot(res, aes(x=log2(baseMean), y=log2FoldChange, fill=is_significant)) +
    geom_point( shape=21, alpha=0.5, size=2) +
    geom_hline(yintercept = 0) +
    labs(fill='Is significant?') +
    scale_fill_manual(values = c("#999999", "#F0E442", "#D55E00")) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text(color='blue', face='bold'),
      legend.position = 'bottom'
    )

}
