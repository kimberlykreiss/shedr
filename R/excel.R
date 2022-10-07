# Write table to Excel file --------------------------------------
# If it's the first time writing to the Excel file,
# set append = FALSE
# this function expects data.frame
#' write_excel
#' @name write_excel
#' @description  Writes a data.frame to an xlsx file.
#' @usage
#' write_excel(df, wb, excel, units = "Percent", title = "Table", reference = "sheet", footnote = "")
#'
#' @param df A data.frame or tibble object.
#' @param wb openxlsx::createWorkbook()
#' @param excel Excel file path. Requires a string.
#' @param units Requires a string.
#' @param title Title of table. Requires a string.
#' @param reference Reference type and number. Requires a string. Used for title and for sheetname.
#' @param footnote Table footnote. Requires a string.
#' @param title_ref Reference in the title if different from sheet reference. Requires a string.
#'
#' @return A workbook object
#'
#' @examples
#' # Load in SHED 2019 data
#' SHED_2019<-data("shed2019")
#' # Create one-way table
#' rent_table <- multi1way(SHED_2019, c(R1_f, R1_e, R1_a, R1_c, R1_b, R1_d), weight = TRUE, missing = FALSE, labelvarnames = TRUE,
#'                  rowvalall = 1, breaks =c(1,3), breaklabels=c("Mortgage Access", "Preference"))
#' # Write table to excel
#' wb <- openxlsx::createWorkbook()
#' write_excel(rent_reason_table, wb, excel = "housing.xlsx", units = "Percent", title = "Reasons for Renting", reference = "Table 6.3", footnote = "Among renters. Respondents could select multiple answers.")
#' @export
write_excel <- function(df, wb, excel, units= "Percent", title = "Table", reference = "sheet", footnote = "", title_ref = "",
                        chart_type = NA) {

  # adding to footnote based on chart type
  if(footnote == ""){
    footnote = "Among all adults."
  }

  if(!stringr::str_detect(footnote, "Key identifies") & !is.na(chart_type)){
  if(stringr::str_detect(tolower(chart_type), "group")){
    footnote = paste0(footnote, " Key identifies bars in order from top to bottom.")
  }
  if(stringr::str_detect(tolower(chart_type), "dumbbell")){
    footnote = paste0(footnote, " Key identifies circles in order from left to right.")
  }
  if(stringr::str_detect(tolower(chart_type), "connected|line")){
    footnote = paste0(footnote, " Key identifies curves in order from top to bottom.")
  }
  if(stringr::str_detect(tolower(chart_type), "stack")){
    footnote = paste0(footnote, " Key identifies bars in order from left to right.")
  }
    }

  # Get parameters from table attributes
  sheet_name <- reference
  Title <- dplyr::if_else(title_ref == "", paste0(sheet_name,". ",title), paste0(title_ref,". ",title))
  Footnote <- dplyr::if_else(footnote == "", "", paste0("Note: ",footnote))
  bold_st <-openxlsx::createStyle(textDecoration="Bold")
  units_flag <-  dplyr::if_else(units == "Percent", 1, 0)
  bold_cells <- attr(df,"breaks") + units_flag + 2
  line_st <- openxlsx::createStyle(border = "bottom",
                                   borderColour = getOption("openxlsx.borderColour", "black"),
                                   borderStyle = getOption("openxlsx.borderStyle", "thin"))
  overall_st <- openxlsx::createStyle(textDecoration = "Bold",
                                      border = "bottom",
                                      borderColour = getOption("openxlsx.borderColour", "black"),
                                      borderStyle = getOption("openxlsx.borderStyle", "thin"))
  # write.xslx needs a data.frame, not a tibble
  outDF <- data.frame(df)

  # Converting to data frame replaces " " with ".",
  # so set colnames back to original
  colnames(outDF) <- colnames(df)

  # Add Title and Footnote as additional rows
  # Copy the first row
  footnoteDF <- outDF[1,]
  # Replace first column with the footnote
  footnoteDF[,1] <- Footnote
  # Replace remaining columns with NA
  footnoteDF[1,2:ncol(footnoteDF)] <- NA

  # Add the Title and Footnote rows
  outDF <- rbind(outDF, footnoteDF)

  try(openxlsx::removeWorksheet(wb, sheet_name), silent = TRUE)
  ws <- openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb,
                      sheet_name,
                      data.frame(Title),
                      startRow = 1,
                      colNames = FALSE)
  if(units == "Percent"){
    openxlsx::writeData(wb,
                      sheet_name,
                      data.frame(units),
                      startRow = 2,
                      colNames = FALSE)
  }
  openxlsx::writeData(wb, sheet_name, outDF, startRow = units_flag + 2)
  openxlsx::addStyle(wb,sheet_name, bold_st, rows=c(1, bold_cells), col =1)
  openxlsx::addStyle(wb,sheet_name, line_st, rows=c(units_flag + 2), col = c(1:ncol(outDF)))

  # Last line style
  if(length(attr(df, "breaks"))== 0){
     max_break <- -Inf
  } else {
    max_break <- max(attr(df, "breaks"))
  }

  if(max_break == nrow(df)| length(attr(df, "overall")) != 0) {
    openxlsx::addStyle(wb,sheet_name, overall_st, rows=c(nrow(outDF) + units_flag + 1), col = c(1:ncol(outDF)))
  } else{
    openxlsx::addStyle(wb,sheet_name, line_st, rows=c(nrow(outDF) + units_flag + 1), col = c(1:ncol(outDF)))
  }

  # Footnote format
  foot_style <- openxlsx::createStyle(wrapText = T)
  openxlsx::mergeCells(wb, sheet_name, cols = c(1:ncol(outDF)), rows = c(nrow(outDF) + units_flag + 2))
  openxlsx::addStyle(wb,sheet_name, foot_style, cols = c(1:ncol(outDF)), rows = c(nrow(outDF) + units_flag + 2))

  # cell size formats
  max_width <- lapply(df[,1], function(x) max(nchar(as.character(x)), na.rm = TRUE)) %>% unlist() %>% max()
  cell_width <- dplyr::if_else(max_width >= 30, 30,  dplyr::if_else(max_width >8 , 18, 8.43))
  openxlsx::setColWidths(wb, sheet_name, col = 1, widths = cell_width)

  df_width <- cell_width + 8.43*(ncol(df)- 1)
  wdt <- graphics::strwidth(Footnote, unit = "in") * 20 / 1.43 # 20 px = 1.43 in
  if (length(wdt) == 0) {
    rowh <- 15
  } else {
    rowh <- 15 + 12 * as.integer(max(wdt / df_width))
  }
  openxlsx::setRowHeights(wb, sheet_name, rows = c(nrow(outDF) + units_flag + 2), heights = rowh )

  # Save workbook
  openxlsx::saveWorkbook(wb, excel, overwrite = TRUE)
}

# Write plot to excel file  --------------------------------------
# If it's the first time writing to the Excel file,
# set append = FALSE
# This function expects a plot and a excel filepath

#' write_excel_plot
#' @name write_excel_plot
#'
#' @description  Writes a SHED plot/chart or ggplot object to an xlsx file.
#' @param g A SHED plot/chart, or similar object.
#' @param wb openxlsx::createWorkbook()
#' @param excel Excel file path. Requires a string.
#' @param reference Reference type and number. Requires a string. Used for title and for sheetname.
#'
#' @return A workbook object
#'
#' @examples
#' # Load in the SHED 2019 data
#' SHED_2019 <- data("shed2019")
#' # Creates a one-way table using multi1way()
#' rent_table <- multi1way(SHED_2019, c(R1_f, R1_e, R1_a, R1_c, R1_b, R1_d), weight = TRUE, missing = FALSE, labelvarnames = TRUE,
#'                  rowvalall = 1, breaks =c(1,3), breaklabels=c("Mortgage Access", "Preference"))
#' # Create bar chart from the one-way table
#' rent_chart <- oneway_chart(one_way_table, plot_title = "Reasons for Renting", plot_caption = "Among renters. Respondents could select multiple answers.")
#' # Write chart to excel
#' wb <- openxlsx::createWorkbook()
#' write_excel_plot(rent_chart, wb, excel = "housing.xlsx", reference = "Figure 23")
#' @export
write_excel_plot <- function(g, wb, excel, reference, width = 6.5, height = 4) {
  # Get parameters from table attributes
  sheet_name <- paste0(reference, " plot")

  print(g)

  try(openxlsx::removeWorksheet(wb, sheet_name), silent = TRUE)
  ws <- openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::insertPlot(wb,
                       sheet_name,
                       #file = excel,
                       width = width,
                       height = height,
                       fileType = 'png')
  openxlsx::saveWorkbook(wb, excel, overwrite = TRUE)
}



