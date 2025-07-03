#' Check Normality and Homogeneity Assumptions for Paired Comparisons
#'
#' Performs Shapiro-Wilk and Levene's tests within each group for paired data.
#' Returns both a raw results tibble and an optional formatted `gt` table.
#'
#' @param data A data frame.
#' @param group_col Name of the grouping column (e.g., cluster, patient group).
#' @param sample_col Name of the sample ID column.
#' @param label_col Name of the paired condition column (e.g., before/after).
#' @param value_col Name of the numeric value column to test.
#' @param label_ref Optional: reference level for the `label_col`. If NULL, uses alphabetical order.
#' @param verbose Logical; print outlier and QQ plots (default = FALSE).
#' @param return_table Logical; include a formatted `gt` table in the returned object (default = TRUE).
#'
#' @return A list with:
#' \item{df}{tibble with normality and homogeneity test results}
#' \item{table}{(optional) gt table summary}
#'
#' @examples
#' df <- tibble::tibble(
#'   group = rep(c("A", "B"), each = 6),
#'   sample = rep(paste0("S", 1:3), each = 2, times = 2),
#'   label = rep(c("T1", "T2"), times = 6),
#'   score = stats::rnorm(12)
#' )
#' result <- check_assumptions(df, group_col = "group", sample_col = "sample",
#'                             label_col = "label", value_col = "score")
#' result$df
#' result$table
#'
#' @export
check_assumptions <- function(data,
                              group_col = "group",
                              sample_col = "sample",
                              label_col = "label",
                              value_col = "score",
                              label_ref = NULL,
                              verbose = FALSE,
                              return_table = TRUE) {

  # Convert column names to symbols
  group_sym <- rlang::sym(group_col)
  sample_sym <- rlang::sym(sample_col)
  label_sym <- rlang::sym(label_col)
  value_sym <- rlang::sym(value_col)

  # Check that each group has exactly 2 labels
  label_counts <- data |>
    dplyr::count(!!group_sym, !!label_sym) |>
    dplyr::count(!!group_sym)

  if (any(label_counts$n != 2)) {
    stop("Each group must have exactly 2 labels in '", label_col, "' for paired comparison.")
  }

  # Create wide format difference dataframe
  diff <- data |>
    tidyr::pivot_wider(names_from = !!label_sym, values_from = !!value_sym) |>
    (\(df) dplyr::reframe(
      .data = df,
      .by = !!group_sym,
      !!rlang::as_name(sample_sym),
      difference = df[[3]] - df[[4]]
    ))() # subtract columns by position (more general)

  if (verbose) {
    base::print(diff |> rstatix::identify_outliers(difference))
    base::print(diff |> ggpubr::ggqqplot("difference") + ggplot2::facet_wrap(as.formula(paste("~", group_col))))
  }

  # Shapiro test
  shapiro <- diff |>
    tidyr::nest(data = -!!group_sym) |>
    dplyr::mutate(
      shapiro_test = purrr::map(data, ~ {
        if (nrow(.x) >= 3) {
          stats::shapiro.test(.x$difference) |>
            broom::tidy() |>
            dplyr::mutate(note = "OK")
        } else {
          tibble::tibble(
            statistic = NA_real_,
            p.value = NA_real_,
            method = "Shapiro-Wilk",
            alternative = NA_character_,
            note = "too few samples"
          )
        }
      }),
      shapiro_pvalue = purrr::map_dbl(shapiro_test, "p.value"),
      shapiro_note = purrr::map_chr(shapiro_test, "note"),
      is_normal = shapiro_pvalue > 0.05
    ) |>
    dplyr::select(!!group_sym, shapiro_pvalue, is_normal, shapiro_note)

  # Levene test
  lavene <- data |>
    dplyr::mutate(!!label_sym := if (!is.null(label_ref)) {
      base::factor(!!label_sym, levels = c(label_ref, setdiff(base::levels(base::factor(!!label_sym)), label_ref)))
    } else {
      base::factor(!!label_sym)  # default: alphabetic
    }) |>
    tidyr::nest(data = -!!group_sym) |>
    dplyr::mutate(
      lavene_test = purrr::map(data, ~ car::leveneTest(stats::as.formula(paste(rlang::as_name(value_sym), "~", label_col)), data = .x) |>
                                 broom::tidy() |>
                                 rstatix::add_significance()),
      lavene_pvalue = purrr::map_dbl(lavene_test, "p.value"),
      is_homog = lavene_pvalue > 0.05
    ) |>
    dplyr::select(!!group_sym, lavene_pvalue, is_homog)

  results_df <- dplyr::left_join(shapiro, lavene, by = group_col)

  result <- list(df = results_df)

  if (return_table) {
    result$table <- results_df |>
      gt::gt() |>
      gt::fmt_number(columns = c("shapiro_pvalue", "lavene_pvalue"), decimals = 3) |>
      gt::tab_header(
        title = "Tests for statistical assumptions",
        subtitle = glue::glue("Paired comparison within each {group_col}")
      )
  }

  class(result) <- "assumption_result"
  return(result)
}
