#' PBMC 3K example dataset
#'
#' A dataset containing expression informations of 39 genes in 8 celltypes. Original dataset from seurat-data package.
#'
#'
#' \itemize{
#'   \item genes.plot. Gene symbol
#'   \item id. Cell type
#'   \item pct.exp. Percentage of cells expressing the gene in the cell type
#'   \item pct.exp2. pct.exp with 2 decimal places
#'   \item pct.exp100. pct.exp.2 x 100
#'   \item avg.exp. Average expression of the gene in the cell type
#'   \item avg.exp.scale. Scaled average expression
#'   \item abs_avg_exp_scale Absolute value of avg.exp.scale
#'   \item avg_logFC. log fold-chage of the average expression of the gene between the cell type and the others
#'   \item avg_logFC2. avg_logFC with 2 decimal places
#'   \item p_val_adj. Adjusted p-value based on bonferroni correction
#'   \item p_val_adj2. p_val_adj with 2 decimal places
#'   \item pval_symb. Adjusted p-value classification ("<1e-100" or "<1e-50" or  "<1e-10" or  "<0.01" or   ">0.01")
#' }
#'
#' @docType data
#' @keywords datasets
#' @name PBMC3K_example_data
#' @usage data(PBMC3K_example_data)
#' @format A data frame with 312 rows and 13 variables
#' @references \url{https://github.com/satijalab/seurat-data}
"PBMC3K_example_data"



#' CBMC 8K example dataset
#'
#' A dataset containing expression informations of 12 genes in 11 celltypes. Original dataset from seurat-data package.
#'
#'
#' \itemize{
#'   \item features.plot. Gene symbol
#'   \item id. Cell type
#'   \item RNA.pct.exp. RNA level : Percentage of cells expressing the gene in the cell type
#'   \item RNA.avg.exp. RNA level : Average expression of the gene in the cell type
#'   \item RNA.avg.exp.log2p1. RNA.avg.exp with log2(x+1) tranformation
#'   \item RNA.avg.exp.scaled. RNA level : Scaled average expression
#'   \item ADT.pct.exp.sup0. ADT level : Percentage of cells expressing the gene in the cell type (with expression >0)
#'   \item ADT.pct.exp.sup.cutoff. ADT level : Percentage of cells expressing the gene in the cell type (with expression > background)
#'   \item ADT.avg.exp. ADT level : Average expression of the gene in the cell type
#'   \item ADT.avg.exp.log2p1. ADT.avg.exp with log2(x+1) tranformation
#'   \item ADT.avg.exp.scaled. ADT level : Scaled average expression
#'   \item canonical_marker. If the gene is a canonical marker of the cell type (yes or no)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name CBMC8K_example_data
#' @usage data(CBMC8K_example_data)
#' @format A data frame with 132 rows and 12 variables
#' @references \url{https://github.com/satijalab/seurat-data}
"CBMC8K_example_data"
