#' Compare numeric scores between two groups by feature or annotation
#'
#' General-purpose comparison of numeric values between two groups.
#'
#' @param data A numeric matrix or data.frame with rows as features (e.g. metabolites, genes).
#' @param metadata A data.frame with sample annotations (e.g. group assignments).
#' @param sample_col Name of the column in `metadata` containing sample IDs (string).
#' @param group_col Name of the column in `metadata` containing group labels (string).
#' @param group_by Optional: If provided, a data.frame with feature annotations (e.g. pathway, subsystem).
#' Must contain columns: `feature` and `group`.
#' @param feature_col Optional: Column name in `group_by` that contains feature IDs.
#' @param annotation_col Optional: Column name in `group_by` with groupings (e.g. subsystems).
#'
#' @return A tibble with summary statistics and p-values per feature or group.
#' @export
#'
#' @examples
#' # Example with synthetic data
#' set.seed(123)
#' data_mat <- matrix(rnorm(100), nrow = 10,
#'                    dimnames = list(paste0("F", 1:10), paste0("S", 1:10)))
#' metadata <- data.frame(sample = paste0("S", 1:10),
#'                        group = rep(c("A", "B"), each = 5))
#'
#' compare_groups(data = data_mat,
#'                        metadata = metadata,
#'                        sample_col = "sample",
#'                        group_col = "group")

compare_groups <- function(data,
                           metadata,
                           sample_col,
                           group_col,
                           group_by = NULL,
                           feature_col = NULL,
                           annotation_col = NULL) {

  data <- base::as.data.frame(data) |>
    tibble::rownames_to_column("feature")

  if (!is.null(group_by)) {
    if (is.null(feature_col) || is.null(annotation_col)) {
      stop("If `group_by` is used, you must provide `feature_col` and `annotation_col`.")
    }

    data <- dplyr::left_join(
      data,
      group_by |>
        dplyr::select(feature = !!rlang::sym(feature_col),
                      group = !!rlang::sym(annotation_col)),
      by = "feature"
    ) |>
      dplyr::filter(!is.na(group)) |>
      dplyr::select(-feature) |>
      dplyr::group_by(group) |>
      dplyr::summarise(dplyr::across(where(is.numeric), mean), .groups = "drop") |>
      dplyr::rename(feature = group)
  }

  long_df <- tidyr::pivot_longer(data, -feature,
                                 names_to = "sample",
                                 values_to = "value") |>
    dplyr::left_join(metadata, by = stats::setNames("sample", sample_col)) |>
    dplyr::rename(group = !!rlang::sym(group_col)) |>
    tidyr::drop_na(group, value)

  groups <- unique(long_df$group)
  if (length(groups) != 2) stop("Function supports exactly two groups.")

  results <- long_df |>
    dplyr::group_by(feature) |>
    dplyr::summarise(
      mean_1 = mean(value[group == groups[1]]),
      mean_2 = mean(value[group == groups[2]]),
      median_1 = stats::median(value[group == groups[1]]),
      median_2 = stats::median(value[group == groups[2]]),
      log2FC = base::log2(mean_2 / mean_1),
      t_pval = base::tryCatch(stats::t.test(value ~ group)$p.value, error = function(e) NA),
      wilcox_pval = base::tryCatch(stats::wilcox.test(value ~ group)$p.value, error = function(e) NA),
      norm_pval_1 = base::tryCatch(stats::shapiro.test(value[group == groups[1]])$p.value, error = function(e) NA),
      norm_pval_2 = base::tryCatch(stats::shapiro.test(value[group == groups[2]])$p.value, error = function(e) NA),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      t_fdr = stats::p.adjust(t_pval, method = "fdr"),
      wilcox_fdr = stats::p.adjust(wilcox_pval, method = "fdr"),
      normality = dplyr::case_when(
        norm_pval_1 > 0.05 & norm_pval_2 > 0.05 ~ "t-test",
        TRUE ~ "wilcoxon"
      )
    ) |>
    dplyr::rename_with(~ paste0("mean_", groups[1]), "mean_1") |>
    dplyr::rename_with(~ paste0("mean_", groups[2]), "mean_2") |>
    dplyr::rename_with(~ paste0("median_", groups[1]), "median_1") |>
    dplyr::rename_with(~ paste0("median_", groups[2]), "median_2")

  return(results)
}
