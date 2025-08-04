#' Update the board index with a new study run entry
#'
#' This function registers a new analysis run (main or rerun) by adding a row
#' to the board index TSV. It will treat the first time the study_id is registered as main
#' It prevents duplication and assigns a consistent board ID based on the study ID.
#'
#' @param study_run Character. Unique name for this analysis run (e.g. "06b_ProjectX" or "06b_ProjectX_rerun1").
#' @param path Character. Path to the `board_index.tsv` file. Default: "data/pins/board_index.tsv".
#'
#' @return Invisibly returns the updated tibble. A message is shown if updated.
#' @export

update_board_index <- function(study_run,
                               path = here::here("data/pins/board_index.tsv")) {

  # Extract short study ID (e.g. "06b") from run name
  study_id <- stringr::str_extract(study_run, "^[0-9]+[a-z]*")

  if (is.na(study_id)) {
    stop("study_run must start with a valid numeric ID (e.g., '06b').")
  }

  # Generate new board name
  board_prefix <- paste0("board_", study_id)

  # Load or initialize index
  if (fs::file_exists(path)) {
    index <- readr::read_tsv(path, show_col_types = FALSE)
  } else {
    fs::dir_create(fs::path_dir(path))
    index <- tibble::tibble(
      study_id = character(),
      study_name = character(),
      study_run = character(),
      board_tag = character(),
      board_name = character())
  }

  # Check if study_run already exists
  if (study_run %in% index$study_run) {
    response <- readline(glue::glue(
      "Run '{study_run}' is already registered. Overwrite? [y/N]: "
    ))
    if (!tolower(response) %in% c("y", "yes")) {
      stop("Aborted: Run registration cancelled by user.")
    }
    # remove existing entry before overwrite
    index <- dplyr::filter(index, study_run != !!study_run)
  }

  # Determine how many runs already exist for this study ID
  prev_runs <-
    index |>
    dplyr::filter(stringr::str_starts(board_name, board_prefix))



  # Generate metadata accordingly to first entry or re-runs
  new_entry <-
    if (nrow(prev_runs) < 2) {
      # First run → 2 folders needed: main folder with no suffix, current run "_01"
      tibble::tibble(
        study_id = rep(study_id, 2),
        study_name = rep(study_run, 2),
        study_run = rep(study_run, 2),
        board_tag = c("main", "first"),
        board_name = paste0(board_prefix, c("", "_01"))
      )
    } else {

      if (nrow(prev_runs[prev_runs$board_tag != "rerun", ]) != 2) {
        stop(glue::glue("Error with folder structure for study_id '{study_id}'\n",
        "If you are trying to rerun 'main' after other reruns, remove those first"))
      }

    # create new row
      tibble::tibble(
        study_id = study_id,
        study_name = prev_runs[prev_runs$board_tag == "main",]$study_name,
        study_run = study_run,
        board_tag = "rerun",
        board_name = paste0(board_prefix, "_", sprintf("%02d", nrow(prev_runs)))
      )
    }

  # Add new entry to index
  updated <-
    dplyr::bind_rows(index, new_entry) |>
    dplyr::arrange(study_id, board_name)

  board_name <- unique(new_entry$board_name)

  # Save
  readr::write_tsv(updated, path)
  message(glue::glue("Board index updated: {paste0(board_name, collapse = ' & ')} for {study_run}"))
}
