# check arguments

checkArg_getVolcanoPlot <- function(res, numerator,
                                    denominator,
                                    topN, genesToShow,
                                    pValCutoff){
  assert_that(is.data.frame(res))
  assert_that(is.string(numerator))
  assert_that(is.string(denominator))
  assert_that(is.character(genesToShow) | is.null(genesToShow))
  assert_that(is.numeric(topN))
  assert_that(is.numeric(pValCutoff))

}
#' gives volcano plot
#'
#' @param res a data frame; DESeq2 results converted to data frame
#' @param numerator numerator  a string; numerator in a contrast
#' @param denominator denominator a string; denominator in a contrast
#' @param topN integer; number of top gene names to show on plot; default top 50
#' @param genesToShow a vector; list genes to show
#' @param pValCutoff numeric value; padj cut off value to classify as significant
#'
#' @return An object created by \code{ggplot}
#' @export getVolcanoPlot
#'
#' @examples
#' @import ggplot2
#' @importFrom dplyr filter mutate if_else
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#'
getVolcanoPlot <- function( res, numerator,
                            denominator,
                            topN=50, genesToShow = NULL,
                            pValCutoff=0.05){

  checkArg_getVolcanoPlot(res=res, numerator = numerator,
                          denominator = denominator, topN=topN,
                          genesToShow=genesToShow, pValCutoff=pValCutoff)


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

  p <- ggplot(res, aes(x=log2FoldChange, y=modPval, fill=is_significant)) +
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

  return(p)

}
