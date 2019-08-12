#'
#' Wrapper function to generate complete HTML report: entity-level  plus variable-level reports for one entity.
#'
#' @param entity_list (list) A list containing information on a single data entity in metajam output format.
#' @param output_path (character) Path to save complete HTML report to. If "ask", a Rstudio pop-up window appears allowing choice of save directory (requires RStudio >= 1.1.287).
#'
#' @return A HTML file with complete report on the chosen data entity.
#' 
#' @export

static_report_complete <- function(entity_list, output_path, shiny = F) {
  
  if ("summary_metadata" %in% names(entity_list)) {
  # append "File_Name" in summary metadata to report name
  report_filename <-
    paste0("report_", entity_list[["summary_metadata"]][1, 2], ".html")
  } else report_filename <- paste0("report_data.html")
  
  # make entity-level report
  entity_df <- entity_list[["data"]]
  entity_report <- static_report_entity(entity_df)
  
  # detect spatial information
  space_cols <- space_detective(entity_list)
  
  
  # loop over columns in entity data and make variable-level reports
  var_report <- lapply(colnames(entity_df), static_report_variable, entity_df = entity_df, space_cols = space_cols)
  names(var_report) <- colnames(entity_df)
  

  # create environment for RMarkdown
  envir <-
    list(
      entity_list = entity_list,
      report_title = paste("Summary report for", entity_list[["summary_metadata"]][1, 2]),
      df = entity_df,
      space_cols = space_cols,
      entity_report = entity_report,
      var_report = var_report
      )
  
  # set template path
  
  if (shiny == F) {
  template_path <- system.file("rmd", "static_report_template.Rmd", package = "datapie")
  # template_path <- "./inst/rmd/static_report_template.Rmd"
  } else if (shiny == T) {
   template_path <- system.file("rmd", "static_report_template_shiny.Rmd", package = "datapie")
    # template_path <- "./inst/rmd/static_report_template_shiny.Rmd"
  }
  
  # set report output path.
  if (output_path == "ask") {
    output_path <- rstudioapi::selectDirectory(caption = "Select directory to save report to.")
  }
  
  try(rmarkdown::render(
    input = template_path,
    #output_format = "html_document",
    output_dir = output_path,
    output_file = report_filename,
    envir = envir
  ))
  
  # only return the file name if used in the shiny GUI environment
  # the report is already rendered to the output path
  
  if (shiny == T) {
    return(report_filename)
  }
  
}