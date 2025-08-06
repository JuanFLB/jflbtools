#' Update Study Board Index
#'
#' Registers a new `study_run` into the study board index, managing the logic
#' for main, first, and rerun board creation. It ensures consistent structure,
#' prevents overwrites, and optionally creates corresponding board folders.
#'
#' @param study_run Character. Name of the current study run, e.g., `"06b_rerun"`.
#' Must begin with a numeric `study_id` (e.g., `"06b"`). This ID is used to
#' group related runs.
#'
#' @param pins_path Character. Path to the base directory where boards will be
#' created. Default is `here::here("data/pins")`.
#'
#' @param index_file Character. Name of the TSV file storing the index table.
#' Default is `"board_index.tsv"`.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Extracts `study_id` from `study_run` and validates it.
#' 2. Loads the index file or initializes an empty index if not present.
#' 3. Checks if `study_run` is already indexed and stops if so.
#' 4. Determines the correct registration scenario:
#'   - If no previous runs exist, creates two entries: `"main"` and `"first"` boards.
#'   - If two or more entries exist and both `"main"` and `"first"` are present,
#'     registers a new `"rerun"` board.
#'   - If a malformed or unexpected structure is detected (e.g., only one prior entry),
#'     an error is raised to prevent incorrect registration.
#' 5. Creates the corresponding board folders if not already present.
#' 6. Appends the new entry to the index and saves the updated table.
#'
#' @return
#' Invisibly returns the new entry (`tibble`) created. Side effects include:
#' - Folder creation (`fs::dir_create()`),
#' - Index update (`readr::write_tsv()`),
#' - Message logging to guide user verification.
#'
#' @section Index Structure:
#' The index file is a TSV with the following columns:
#' - `study_id`: extracted numeric prefix from the run name (e.g., `"06b"`),
#' - `study_name`: the canonical name for the study (taken from first `study_run`),
#' - `study_run`: the name of the individual run (e.g., `"06b_rerun2"`),
#' - `board_tag`: one of `"main"`, `"first"`, `"rerun"`,
#' - `board_name`: the name of the board (e.g., `"board_06b"`, `"board_06b_02"`),
#' - `board_path`: full path to the board directory.
#'
#' @examples
#' \dontrun{
#' update_board_index("06b_rerun")
#' }
#'
#' @export
update_board_index <- function(study_run,
                               pins_path = here::here("data/pins"),
                               index_file = "board_index.tsv") {

  # 1. Extract short study ID (e.g. "01b") from run name
  study_id <- stringr::str_extract(study_run, "^[0-9]+[a-z]*")
  # 1a. Validate study_id or stop
  if (is.na(study_id)) {
    stop("study_run must start with a valid numeric ID (e.g., '06b').")
  }

  # 2. Initialize index
  # 2a. Created full path to index file
  index_path <- here::here(pins_path, index_file)
  # 2b. Load or create index
  if (fs::file_exists(index_path)) {
    index <- readr::read_tsv(index_path, show_col_types = FALSE)
  } else {
    fs::dir_create(fs::path_dir(index_path))
    index <- tibble::tibble(
      study_id = character(),
      study_name = character(),
      study_run = character(),
      board_tag = character(),
      board_name = character(),
      board_path = character())
  }

  # 3. Verify entry information
  # 3a. Determine if study_run is already present. stop if yes
  if (study_run %in% index$study_run) {
    stop(glue::glue("study_run {study_run} is already indexed.\n
                    No boards/folders were created. Check correct setup.\n
                    Objects will be saved as new versions on corresponding board"))
  }
  # 3b. Gather all runs for study_id
  prev_runs <- index[index$study_id == study_id,]
  # 3c. Determine if current study_run is main
  run_is_main <- isTRUE(unique(prev_runs$study_name) == study_run)

  # 4. Create new board entry
  # 4a. Generate new board name
  board_prefix <- paste0("board_", study_id)
  # 4b. Generate entry accordingly for main/first or re-runs
  new_entry <-
    # If there are no entries is main/first
    if (nrow(prev_runs) == 0) {
      # First run → 2 folders needed: main folder with no suffix, current run "_01"
      tibble::tibble(
        study_id = rep(study_id, 2),
        study_name = rep(study_run, 2),
        study_run = rep(study_run, 2),
        board_tag = c("main", "first"),
        board_name = paste0(board_prefix, c("", "_01"))
      ) |>
        # conditional creation of board_paths
        dplyr::mutate(board_path = ifelse(board_tag == "main",
                                          here::here(pins_path, study_name),
                                          here::here(pins_path, study_name, board_name)
        ))
      # If >= 2 entries and main and first are present (correct setup), this is a rerun
    } else if (nrow(prev_runs) >= 2 &&
               all(c("main", "first") %in% prev_runs$board_tag)) {
      # create new row for rerun board
      tibble::tibble(
        study_id = study_id,
        study_name = prev_runs[prev_runs$board_tag == "main",]$study_name,
        study_run = study_run,
        board_tag = "rerun",
        board_name = paste0(board_prefix, "_", sprintf("%02d", nrow(prev_runs)))
      ) |>
        dplyr::mutate(board_path = here::here(pins_path, study_name, board_name))
      # Partial or malformed existing entries → stop
    } else if (nrow(prev_runs) == 1 ||
               any(!prev_runs$board_tag %in% c("main", "first", "rerun"))) {
      stop(glue::glue("Error. Unexpected board index structure for study_id {study_id}."))
      # Not considered scenario → stop
    } else {
      stop(glue::glue("Error. Unexpected behaviour. Check section 4 of the function"))
    }

  # 5. Create corresponding folders for the new entry
  if (all(fs::dir_exists(new_entry$board_path))) {
    message("Board folders already present. Check correct setup before proceeding")
  } else {
    fs::dir_create(new_entry$board_path)
    message("Board folders created. Confirm correct setup before proceeding")
  }

  # 6. Add new entry to index
  updated <-
    dplyr::bind_rows(index, new_entry) |>
    dplyr::arrange(study_id, board_name)
  # 6a. Save
  readr::write_tsv(updated, index_path)

  # 7. Final message
  board_name <- unique(new_entry$board_name)
  message(glue::glue("Board index updated: {paste0(board_name, collapse = ' & ')} for {study_run}"))
}
