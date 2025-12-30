# render-workbooks.R
# 
# Render student worksheets (answers hidden) and instructor solutions (answers shown)
# from a single source. Run from the project root directory.
#
# Usage:
#   source("render-workbooks.R")
#   render_all()                    # Renders both versions
#   render_chapter("ch03", "both")  # Renders single chapter, both versions

library(quarto)
library(fs)

#' Render all workbook chapters in both student and instructor versions
#' 
#' @param chapters Character vector of chapter files (without path/extension),
#'   or NULL to render all chapters in chapters/ directory
#' @return Invisible NULL; side effect is rendered documents
render_all <- function(chapters = NULL) {
  
 if (is.null(chapters)) {
    chapters <- fs::dir_ls("chapters", glob = "*.qmd") |>
      fs::path_file() |>
      fs::path_ext_remove()
  }
  
  # Create output directories
  fs::dir_create("_output/student")
  fs::dir_create("_output/instructor")
  
  for (ch in chapters) {
    render_chapter(ch, version = "both")
  }
  
  message("\n✓ All chapters rendered successfully")
  message("  Student versions:    _output/student/")
  message("  Instructor versions: _output/instructor/")
  
  invisible(NULL)
}

#' Render a single chapter in student, instructor, or both versions
#' 
#' @param chapter Chapter name without path or extension (e.g., "ch03-workbook")
#' @param version One of "student", "instructor", or "both"
#' @return Invisible NULL
render_chapter <- function(chapter, version = "both") {
  
  input_file <- fs::path("chapters", chapter, ext = "qmd")
  
  if (!fs::file_exists(input_file)) {
    stop("Chapter file not found: ", input_file)
  }
  
  if (version %in% c("student", "both")) {
    message("Rendering student version: ", chapter)
    quarto::quarto_render(
      input = input_file,
      output_file = fs::path(chapter, ext = "html"),
      execute_params = list(hide_answers = TRUE),
      output_format = "html"
    )
    # Move to student directory
    fs::file_move(
      fs::path("chapters", chapter, ext = "html"),
      fs::path("_output/student", chapter, ext = "html")
    )
  }
  
  if (version %in% c("instructor", "both")) {
    message("Rendering instructor version: ", chapter)
    quarto::quarto_render(
      input = input_file,
      output_file = fs::path(paste0(chapter, "-solutions"), ext = "html"),
      execute_params = list(hide_answers = FALSE),
      output_format = "html"
    )
    # Move to instructor directory
    fs::file_move(
      fs::path("chapters", paste0(chapter, "-solutions"), ext = "html"),
      fs::path("_output/instructor", paste0(chapter, "-solutions"), ext = "html")
    )
  }
  
  invisible(NULL)
}

#' Quick render of a single chapter for preview during development
#' 
#' @param chapter Chapter name
#' @param show_answers Logical; if TRUE shows solutions
#' @return Opens rendered HTML in viewer
preview_chapter <- function(chapter, show_answers = FALSE) {
  
  input_file <- fs::path("chapters", chapter, ext = "qmd")
  
  quarto::quarto_render(
    input = input_file,
    execute_params = list(hide_answers = !show_answers)
  )
  
  # The file renders in place; open in viewer
  output_file <- fs::path("chapters", chapter, ext = "html")
  if (interactive()) {
    utils::browseURL(output_file)
  }
  
  invisible(output_file)
}
