getVolcanoPlot <- function( res, numerator,
                            denominator,
                            topN=20, genesToShow = NULL){

  res <- res %>%
    mutate(is_significant = if_else( padj < pValCutoff, 'Significant', 'Not significant', missing = 'NA')) %>%
    mutate(modPval = -log10(pvalue)) %>%
    arrange(padj)

  topGenes <- res %>%
    filter(padj < pValCutoff) %>%
    head(n=topN)

  selGenes <- res %>%
    filter( gene_name %in% genesToShow)

  pTitle <- str_c(numerator , 'vs', denominator, sep=' ')

  ggplot(res, aes(x=log2FoldChange, y=modPval, fill=is_significant)) +
    geom_point( shape=21, alpha=0.5, size=2) +
    geom_hline(yintercept = 0) +
    geom_text(data=topGenes, mapping = aes(x=log2FoldChange, y=modPval, label=gene_name),
              inherit.aes=FALSE,
              check_overlap=TRUE,
              show.legend=FALSE,
              size=3.5,
              fontface='bold') +

    geom_text(data=selGenes, mapping = aes(x=log2FoldChange, y=modPval, label=gene_name),
              inherit.aes=FALSE,
              check_overlap=TRUE,
              show.legend=FALSE,
              size=3.5,
              fontface='bold', color='#A3F7BF') +

    labs(fill='Is significant?',
         y='-log10(pval)',
         x = 'log2 fold change',
         title = pTitle) +
    scale_fill_manual(values = c("#999999", "#F0E442", "#D55E00")) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text(color='blue', face='bold'),
      legend.position = 'bottom',
      plot.title = element_text(size=15, color='brown', hjust=0.5, face='bold')
    )



}
