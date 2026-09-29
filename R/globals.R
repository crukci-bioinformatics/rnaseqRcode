#' @importFrom methods is
#' @importFrom stats as.formula prcomp
#' @importFrom utils object.size
#' @importFrom stringr str_remove_all
#' @importFrom SummarizedExperiment colData
#' @importFrom tibble column_to_rownames
NULL

utils::globalVariables(c(
  ".", "PC1", "PC_rank", "Regulation", "SampleGroup", "SampleName", "Source",
  "baseMean", "chr", "countsFraction", "countsPecent", "detection_threshold",
  "end", "fragments", "gene_biotype", "gene_id", "gene_name", "genesFraction",
  "is_significant", "log2FoldChange", "modPval", "number_of_genes",
  "padj", "percentCounts", "pvalue", "seqnames", "start", "strand",
  "totalCounts", "type", "x", "xend", "y", "yend"
))
