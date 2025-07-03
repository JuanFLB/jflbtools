#' Perform Paired Statistical Tests by Group with Annotated Plots
#'
#' Applies paired t-test or Wilcoxon test depending on normality and variance assumptions.
#' Returns test statistics and annotated plots per group.
#'
#' @param data A data frame with columns: group, sample, label, value.
#' @param method_pass Output from `check_assumptions()`.
#' @param group_col, label_col Names of the group and label columns (character).
#' @param value_col Name of the value column (character).
#' @param label_ref Optional: set the reference level in the label factor.
#' @param palette A named vector of colors (label -> color).
#' @param y_label Y-axis label for the plots.
#'
#' @return A tibble with one row per group, including test results and ggplot objects.
#'
#' @export
perform_stat_tests <- function(data,
                               method_pass,
                               group_col = "group",
                               label_col = "label",
                               value_col = "value",
                               label_ref = NULL,
                               palette,
                               y_label = "Score") {
  group_sym <- rlang::sym(group_col)
  label_sym <- rlang::sym(label_col)
  value_sym <- rlang::sym(value_col)

  pass <- method_pass$df |>
    dplyr::summarize(pass = sum(is_normal & is_homog)) |>
    dplyr::pull(pass)

  data <- data |>
    dplyr::mutate(
      !!label_sym := if (!is.null(label_ref)) {
        base::factor(!!label_sym,
                     levels = c(label_ref,
                                setdiff(base::levels(base::factor(!!label_sym)), label_ref)))
      } else {
        base::factor(!!label_sym)
      }
    )

  stat_test <- data |>
    tidyr::nest(data = -!!group_sym)

  if (pass == 2) {
    stat_test <- stat_test |>
      dplyr::mutate(
        stat = purrr::map(data, ~ rstatix::t_test(.x, formula = stats::as.formula(paste(value_col, "~", label_col)), paired = TRUE) |>
                            rstatix::add_significance() |>
                            rstatix::add_xy_position(x = label_col)),
        stat_label = purrr::map(stat, ~ ggpubr::stat_pvalue_manual(.x))
      )
  } else {
    stat_test <- stat_test |>
      dplyr::mutate(
        stat = purrr::map(data, ~ rstatix::wilcox_test(.x, formula = stats::as.formula(paste(value_col, "~", label_col)), paired = TRUE) |>
                            rstatix::add_significance() |>
                            rstatix::add_xy_position(x = label_col)),
        effsize = purrr::map(data, ~ rstatix::wilcox_effsize(.x, formula = stats::as.formula(paste(value_col, "~", label_col)))),
        stat_label = purrr::map(stat, ~ ggpubr::stat_pvalue_manual(.x))
      )
  }

  stat_test <- stat_test |>
    dplyr::mutate(
      plot = purrr::pmap(
        list(!!group_sym, data, stat, stat_label),
        function(group, df, stat, stat_label) {
          .plot_stat_result(group, df, stat, stat_label, palette, y_label)
        }
      )
    )

  return(stat_test)
}
