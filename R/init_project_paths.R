init_project_paths <- function(doc_name = NULL, 
                               subfolders = c("rds", "figs", "tables")) {
  
  base_dir <- here::here("data/results")
  
  if (is.null(doc_name)) {
    doc_name <- fs::path_ext_remove(fs::path_file(knitr::current_input()))
  }
  
  out_paths <- fs::path(base_dir, doc_name, subfolders)
  purrr::walk(out_paths, fs::dir_create)
  
  paths <- purrr::set_names(out_paths, subfolders)
  assign("RESULT_PATHS", paths, envir = .GlobalEnv)
  
  message(glue::glue("Initialized output folders under: data/results/{doc_name}/"))
  return(invisible(paths))
}

# Use it as

#| label: setup-folders
#| eval: true
#| include: false
init_project_paths()

# It will generate

results/
  └── analysis1/
    ├── rds/
    ├── figs/
    └── tables/

# save as

readr::write_rds(my_data, file = fs::path(RESULT_PATHS["rds"], "processed_data.rds"))
ggsave("plot1.png", path = RESULT_PATHS["figs"], plot = p)