#' Activate main and current boards for a given study_run
#'
#' This function reads the board index and validates the structure of the study.
#' It ensures that a given `study_id` has exactly one main, one first, and at least one rerun.
#' It also ensures that the requested `study_run` is valid.
#'
#' @param study_run A character string indicating the specific study run to activate.
#'
#' @param pins_path Character. Path to the base directory where boards will be
#' created. Default is `here::here("data/pins")`.
#'
#' @return A list with two elements: \code{main} (main board object) and \code{current} (requested board object).
#' @export
#'
#' @examples
#' boards <- activate_boards("01A_rerun2")
#' pins::pin_write(board = boards$main, x = df, name = "my_data", type = "qs")
activate_boards <- function(study_run, pins_path = here::here("data/pins") ) {

  # Load the index file
  index_path <- here::here(pins_path, "board_index.tsv")
  if (!fs::file_exists(index_path)) {
    stop(glue::glue("Index file not found at: {index_path}"))
  }

  index <- readr::read_tsv(index_path, show_col_types = FALSE)

  # Extract study_id from input (e.g., 01, 01A, etc.)
  study_id <- stringr::str_extract(study_run, "^[0-9]+[a-z]*")

  if (is.na(study_id)) {
    stop(glue::glue("Could not extract study_id from input: '{study_run}'"))
  }

  # Filter index to entries of the given study_id
  id_rows <- index[index$study_id == study_id,]

  if (nrow(id_rows) == 0) {
    stop(glue::glue("No match for study_id '{study_id}' in the index.\nDid you create boards for the new study yet?"))
  }

  # Count expected roles
  counts <- table(id_rows$board_tag)
  n_main  <- counts[["main"]]  %||% 0
  n_first <- counts[["first"]] %||% 0
  n_rerun <- if ("rerun" %in% names(counts)) counts[["rerun"]] else 0

  if (n_main != 1 || n_first != 1) {
    stop(glue::glue(
      "Expected at least 1 main, 1 first for study_id '{study_id}', but got:\n",
      "main: {n_main}, first: {n_first}, rerun: {n_rerun}"
    ))
  }

  # Check that study_run exists in this study_id group
  if (!study_run %in% id_rows$study_run) {
    stop(glue::glue(
      "study_run '{study_run}' not found among runs for study_id '{study_id}'.\nAvailable: {paste(id_rows$study_run, collapse = ', ')}"
    ))
  }



  # Error if study_run equals study_name but not labeled as main or first
  study_name <- unique(id_rows$study_name)

  if (study_run == study_name) {
    run_role <- id_rows[id_rows$study_run == study_run, ]$board_tag
    if (!all(run_role %in% c("main", "first"))) {
      stop(glue::glue(
        "study_run '{study_run}' matches study_name '{study_name}', but is not labeled as main or first (got '{id_rows$board_tag}')."
      ))
    }
  }

  # Get main board info
  main_row <- id_rows[id_rows$board_tag == "main",]
  main_path <- here::here(main_row$board_path)

  # Get current board info
  current_row <- id_rows[id_rows$study_run == study_run & id_rows$board_tag != "main",]
  current_path <- here::here(current_row$board_path)

  # Check paths
  if (!fs::dir_exists(main_path)) {
    stop(glue::glue("Main board directory not found at: {main_path}"))
  }
  if (!fs::dir_exists(current_path)) {
    stop(glue::glue("Current board directory not found at: {current_path}"))
  }

  # Return boards
  return(list(
    main = pins::board_folder(main_path, versioned = TRUE),
    current = pins::board_folder(current_path, versioned = TRUE)
  ))
}
