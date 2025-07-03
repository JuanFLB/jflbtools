#' Plot Enrichment Results as a Bar Chart
#'
#' This function takes a dataframe of enrichment analysis results and plots a bar chart for a specified term.
#' It shows the top terms based on adjusted p-value, highlighting their odds ratio and significance level.
#'
#' @param df Data frame containing enrichment analysis results.
#' @param term The specific term or database to filter results by (default is "KEGG_2021_Human").
#' @param max_terms The maximum number of terms to display in the plot (default is 10).
#'
#' @return A ggplot object representing the bar chart of enrichment results.
#' @export
#'
#' @examples
#' # Assuming `enrichment_results` is your dataframe with enrichment analysis results
#' enrichR_plot_bar(enrichment_results, term = "KEGG_2021_Human", max_terms = 10)
enrichR_plot_bar <- function(df, term = "KEGG_2021_Human", max_terms = 10) {
  df |>
    dplyr::filter(database == term) |>
    tidyr::unnest(data) |>
    dplyr::mutate(
      overlap_percent =
        base::as.numeric(stringr::str_split(Overlap, "/", simplify = TRUE)[, 1]) /
        base::as.numeric(stringr::str_split(Overlap, "/", simplify = TRUE)[, 2]) * 100
    ) |>
    dplyr::arrange(Adjusted.P.value, dplyr::desc(Odds.Ratio)) |>
    dplyr::mutate(label = dplyr::if_else(dplyr::row_number() <= 5, Term, "")) |>
    dplyr::mutate(Term = forcats::fct_reorder(Term, dplyr::desc(Adjusted.P.value))) |>
    dplyr::slice_min(Adjusted.P.value, n = max_terms) |>
    ggplot2::ggplot(ggplot2::aes(x = -base::log10(Adjusted.P.value), y = Term, fill = Odds.Ratio)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = -base::log10(0.05), linetype = "dotted") +
    MetBrewer::scale_fill_met_c("Hokusai2") +
    ggplot2::labs(
      title = paste0(term, ". Top ", max_terms, " terms"),
      y = "",
      x = "-log10(FDR)",
      fill = "Odds ratio"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title.position = "plot")
}
