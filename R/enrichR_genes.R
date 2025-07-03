#' EnrichR Analysis of Gene List in Multiple Databases
#'
#' This function queries a list of human gene symbols against multiple enrichment databases using the EnrichR API and returns a nested dataframe with the results for each database.
#'
#' @param gene_list A character vector of human gene symbols to be queried.
#' @param databases Optional; a character vector of database names to query. The default includes GO, KEGG, Reactome, MSigDB.
#' @param quiet Logical; whether to suppress enrichR output messages.
#'
#' @return A nested tibble where each row corresponds to a database, and the results for each database are nested within.
#' @export
#'
#' @examples
#' genes <- c("TP53", "BRCA1", "BRCA2", "EGFR", "PTEN")
#' results_default <- enrichR_genes(genes)
#' custom_dbs <- c("Panther_2016", "WikiPathways_2016")
#' results_custom <- enrichR_genes(genes, custom_dbs)
enrichR_genes <- function(gene_list,
                          databases = c(
                            "GO_Molecular_Function_2021",
                            "GO_Biological_Process_2021",
                            "GO_Cellular_Component_2021",
                            "KEGG_2021_Human",
                            "Reactome_2022",
                            "MSigDB_Hallmark_2020",
                            "MSigDB_Oncogenic_Signatures"
                          ),
                          quiet = FALSE) {
  options(enrichR.quiet = quiet)
  suppressMessages(enrichR::setEnrichrSite("Enrichr"))

  enrichR::enrichr(gene_list, databases) |>
    dplyr::bind_rows(.id = "database") |>
    tibble::as_tibble() |>
    tidyr::nest(data = -database)
}
