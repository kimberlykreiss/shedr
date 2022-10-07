# oneway_chart ----------------------------------------------------------------
#
# Takes in a table created by the multi1way or one_var_tab function
# Outputs a horizontal bar chart with bars ordered by the rows on the table
# Create a bar charts for a variable with
# multiple options that are mutually exclusive
#' Creates a simple bar chart
#' @name oneway_chart
#'
#' @description  Create a simple bar charts from a one-way percent distribution table
#'
#' @usage
#' oneway_chart(df, plot_title = "Title", plot_caption = "")
#'
#' @param df One-way percent distribution table
#' @param plot_title Plot title. Requires a string.
#' @param plot_caption Plot footnote. Requries a string.
#' @param horizontal if True, returns a horizontal bar chart, if false returns a vertical bar chart
#' @param bold_rows Adds bold labelling to each row number in list.
#'
#' @return Bar chart
#'
#' @examples
#' # Load in the SHED 2019 data
#' data("shed2019")
#' # Creates a one-way table using multi1way()
#' one_way_table <- multi1way(shed2019, c(R1_f, R1_e, R1_a, R1_c, R1_b, R1_d), weight = TRUE, missing = FALSE, labelvarnames = TRUE,
#'                  rowvalall = 1, breaks =c(1,3), breaklabels=c("Mortgage Access", "Preference"))
#' # Create bar chart from the one-way table
#' oneway_chart(one_way_table, plot_title = "Reasons for Renting", plot_caption = "Among renters. Respondents could select multiple answers.")
#' @export
#' @importFrom dplyr "%>%"
oneway_chart <- function(df, plot_title = "Title", plot_caption = "", horizontal = TRUE, bold_rows = c(), color_list = c(),
                         wb = NA, excel = NA, reference = NA, width = 6.5, height = 4){

  if(!is.null(attr(df, "title")) & plot_title == "Title"){
    plot_title = attr(df, "title")
  }
  if(!is.null(attr(df, "footnote")) & plot_caption == ""){
    plot_caption = attr(df, "footnote")
  }
  if(!is.null(attr(df, "reference")) & is.na(reference)){
    reference = attr(df, "reference")
  }

  names(df)[1] <- "Characteristic"
  names(df)[2] <- "Percent"

  # reordering factor levels to have the bars ordered by the rows on the table
  table_reorder<- df %>%
    dplyr::mutate(Characteristic = factor(Characteristic, levels = unique(df$Characteristic))) %>%
    dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic))

  # adding footnote to figure
  footnote <- dplyr::if_else(plot_caption == "none", "",
                             dplyr::if_else(plot_caption == "", "Note: Among all adults.", paste0("Note: ", plot_caption)))
#
#   bold_labels <- dplyr::if_else(levels(table_reorder$Characteristic) %in% df$Characteristic[attr(df, "breaks")],
#                                 "bold", "plain")
  if(!is.na(reference) & !stringr::str_detect(plot_title, "Figure")){
    plot_title <- paste0(reference, ". ", plot_title)
  }

  # bold labels
  if(length(bold_rows) > 0){
    if( min(bold_rows) < 1 | max(bold_rows) > nrow(df)){
      stop("Input for bold_rows out of bounds. Please update.")
    }
  }

  bold_labels <- rep("plain", nrow(df))
  bold_labels[bold_rows] <- "bold"
  bold_labels <- rlist::list.reverse(bold_labels)

  if(length(color_list) == 0){
    color_list = rep("steelblue", nrow(df))
  } else{
    color_list = rev(color_list)
  }

  # bar labels
  table_reorder$barlabels <- if_else(table_reorder$Percent == 0, "*",
                                     as.character(table_reorder$Percent))

  # chart output with horizontal bars with numbers at the end of each bar
  # graph limits based on percent
  if(horizontal == TRUE){
    chart_output <- table_reorder %>%
    ggplot2::ggplot(ggplot2::aes(x = Characteristic, y = Percent, fill = Characteristic)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = stringr::str_wrap(plot_title, 60), caption = stringr::str_wrap(footnote,60)) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(hjust = 1),
                   axis.line = ggplot2::element_line(colour = "black"),
                   panel.background = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(face = bold_labels),
                   legend.position = "none") +
    ggplot2::geom_text(ggplot2::aes(label = barlabels), nudge_y = (max(table_reorder$Percent, na.rm = T)/50),
                       size = 3.5, color = "black", na.rm= FALSE) +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, max(table_reorder$Percent, na.rm = T) + 3 )) +
    ggplot2::scale_fill_manual(values = color_list) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x , width = 18))
  } else{
    chart_output <- table_reorder %>%
      dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic)) %>%
      ggplot2::ggplot(ggplot2::aes(x = Characteristic, y = Percent, fill = Characteristic)) +
      ggplot2::geom_col(width = .7) +
      ggplot2::labs(title = stringr::str_wrap(plot_title, 60), caption = stringr::str_wrap(footnote,100)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     panel.background = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 0),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::geom_text(ggplot2::aes(label = barlabels), nudge_y = (max(table_reorder$Percent, na.rm = T)/50),
                         size = 3.5, color = "black", na.rm= FALSE)  +
      ggplot2::scale_fill_manual(values = color_list)+
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x , width = 18))
  }

  if(!is.na(excel)){
    write_excel_plot(chart_output, wb, excel, reference, width, height)
  }
  return(chart_output)
}


# pie_chart ----------------------------------------------------------------
#
# Takes in a table created by the one_var_tab or multi1way function
# Outputs a pie chart.
#
#' Creates a simple pie chart
#' @name pie_chart
#'
#' @description  Create a simple pie chart from a one-way percent distribution table
#'
#' @usage
#' pie_chart(df, plot_title = "Title", plot_caption = "")
#'
#' @param df One-way percent distribution table
#' @param plot_title Plot title. Requires a string.
#' @param plot_caption Plot footnote. Requries a string.
#'
#' @return Pie chart
#'
#' @examples
#' # Load in the SHED 2019 data
#' shed_2019 <- read_sheddata(2019)
#' bank_status_table <- one_var_tab(shed_2019, bank_status)
#' # Create bar chart from the one-way table
#' pie_chart(bank_status_table, plot_title = "Figure 18. Banking Status", plot_caption = "Fully banked individuals had a bank or credit union account \n and had not used an alternative financial service in the past year.")
#' @export
pie_chart <- function(df, plot_title = "Title", plot_caption = "",
                      wb = NA, excel = NA, reference = NA, width = 6.5, height = 4){

  names(df)[1] <- "Characteristic"

  # adding footnote to figure
  footnote <- dplyr::if_else(plot_caption == "none", "",
                             dplyr::if_else(plot_caption == "", "Note: Among all adults.", paste0("Note: ", plot_caption)))

  if(!is.na(reference) & !stringr::str_detect(plot_title, "Figure")){
    plot_title <- paste0(reference, ". ", plot_title)
  }

  # pie chart
  df <- df %>%
    dplyr::arrange(Percent) %>%
    dplyr::mutate(label = paste0(Characteristic, "\n", Percent, "%"))


  chart_output <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = '', y = Percent, fill = Characteristic)) +
    ggplot2::geom_bar(stat = 'identity', color = 'white') +
    ggplot2::coord_polar('y', start = 0, direction = -1) +
    ggplot2::labs(title = plot_title, caption = footnote) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0),
                   legend.position = 'none')+
    ggplot2::scale_y_continuous(breaks = sum(df$Percent) - cumsum(df$Percent) + df$Percent /2, labels = df$label) +
    ggplot2::scale_fill_manual(values =  c("slategray1", "lightsteelblue","steelblue4", "dodgerblue4"))

  if(!is.na(excel)){
    write_excel_plot(chart_output, wb, excel, reference, width, height)
  }

  return(chart_output)
}

# stacked_bar ----------------------------------------------------------------
#
# Takes in a table created by tabcol or tabrow function
# Outputs a stacked bar chart
#
#' Creates a stacked bar chart
#' @name stacked_bar
#'
#' @description  Create a stacked chart from a two-way percent distribution table
#'
#' @usage
#' stacked_chart(df, plot_title = "Title", plot_caption = "", row_col_subrow)
#'
#' @param df Two-way percent distribution table
#' @param plot_title Plot title. Requires a string.
#' @param plot_caption Plot footnote. Requries a string.
#' @param row_col_subrow if row, uses the row as the group; if col uses the column as the group; if subrow uses subrow as group (should typically use col if df from tabcol)
#' @param bold_rows Adds bold labelling to each row number in list.
#'
#' @return Stacked bar chart
#'
#' @examples
#' # Load in the SHED 2019 data
#' shed_2019 <- read_sheddata(2019)
#' # Figure 26 in SHED 2019 report
#' fig6_6_data <- tabcol(shed_2019, row_vars = c(GH1), col_vars = ppmsacat, rowvalall = c(2, 1, 3, 4), colvalall = c(1,2), labelrowvars = T, labelcolvars = F, overall  = F)
#' fig6_6_chart <- stacked_bar(fig6_6_data, "Housing tenure (by urban/rural status)", "Key identifies bars in order from left to right.", row_col_subrow = "col")
#'
#' @export
stacked_bar <- function(df, plot_title = "Title", plot_caption = "", row_col_subrow, bold_rows = c(),
                        wb = NA, excel = NA, reference = NA, width = 6.5, height = 4, bar_sum = F){

  if(!(row_col_subrow %in% c("row", "col", "subrow"))){
    stop("Value for row_or_col must be 'row' for tabrow, 'col' for tabcol, or 'subrow' for tabsubrow. Please update")
  }

  # extracting table info from df
  if(!is.null(attr(df, "title")) & plot_title == "Title"){
    plot_title = attr(df, "title")
  }
  if(!is.null(attr(df, "footnote")) & plot_caption == ""){
    plot_caption = attr(df, "footnote")
  }
  if(!is.null(attr(df, "reference")) & is.na(reference)){
    reference = attr(df, "reference")
  }

  # transform data
  names(df)[1] <- "Characteristic"

  cols <- names(df)[-1]

  # bold labels
  if(length(bold_rows) > 0){
    if( min(bold_rows) < 1 | max(bold_rows) > nrow(df)){
      stop("Input for bold_rows out of bounds. Please update.")
    }
  }

  bold_labels <- rep("plain", nrow(df))
  bold_labels[bold_rows] <- "bold"
  bold_labels <- rlist::list.reverse(bold_labels)

  if(row_col_subrow=="subrow") {
    num_rowgroups <- df %>% dplyr::filter(is.na(df[,2]))
    subrows <- df %>%
      dplyr::filter(!is.na(df[,2])) %>%
      dplyr::filter(!grepl("Overall", Characteristic))

    new_char <- c()
    for(var in num_rowgroups$Characteristic){
      new_char <- c(new_char, rep(paste0(var), length(unique(subrows$Characteristic))))
    }
    wide_data <- data.frame(Characteristic = new_char, Cross = subrows$Characteristic, Percent = subrows[,2])

    df2 <- num_rowgroups$Characteristic
  } else if(row_col_subrow == "row"){
      df2 <- df$Characteristic
      df <- df %>%
        dplyr::mutate(Characteristic = paste0(Characteristic, dplyr::row_number()))

      wide_data <- df %>%
        tidyr::pivot_longer(cols, names_to = "Cross", values_to = "Percent")
  } else{
    df2 <- names(df)[-1]

    wide_data <- df %>%
      tidyr::pivot_longer(cols, names_to = "Cross", values_to = "Percent") %>%
      dplyr::rename(Cross2 = Cross,
                    Cross = Characteristic) %>%
      dplyr::rename(Characteristic = Cross2)
  }

  # reorder labels
  df2 <- rlist::list.reverse(df2)

  # reordering factor levels to have the bars ordered by the rows on the table
  table_reorder<- wide_data %>%
    dplyr::mutate(Characteristic = factor(Characteristic, levels = unique(wide_data$Characteristic))) %>%
    dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic)) %>%
    dplyr::mutate(Cross = factor(Cross, levels = unique(wide_data$Cross))) %>%
    dplyr::mutate(Cross = forcats::fct_rev(Cross))

  num_groups = length(unique(wide_data$Cross))

  bar_colors <- c("navyblue","slategray1","steelblue4", "lightsteelblue", "dodgerblue4")
  text_list <- c("black", "white", "black", "white")

  color_list <- tail(bar_colors, num_groups)
  text_color <- tail(text_list, num_groups)

  # adding footnote to figure
  footnote <- dplyr::if_else(plot_caption == "none", "",
                             dplyr::if_else(plot_caption == "", "Note: Among all adults. Key identifies bars in order from left to right.",
                             paste0("Note: ", plot_caption, " Key identifies bars in order from left to right.")))

  if(!is.na(reference) & !stringr::str_detect(plot_title, "Figure")){
    plot_title <- paste0(reference, ". ", plot_title)
  }
  # stacked bar chart output with horizontal bars with numbers at the end of each bar
  chart_output <- table_reorder %>%
    ggplot2::ggplot(ggplot2::aes(x = Characteristic, y = Percent, fill = Cross)) +
    ggplot2::geom_col(color = "white", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = stringr::str_wrap(plot_title,60), caption = stringr::str_wrap(footnote, 95)) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(hjust = 1),
                   axis.line = ggplot2::element_line(colour = "black"),
                   panel.background = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   axis.text.y = suppressWarnings(ggplot2::element_text(face = bold_labels))) +
    ggplot2::geom_text(ggplot2::aes(label = Percent, color = Cross), position = ggplot2::position_stack(vjust = 0.5), show.legend = F, na.rm = F) +
    ggplot2::scale_color_manual(values = text_color, guide = ggplot2::guide_none())+
    ggplot2::scale_fill_manual(values = color_list) +
    ggplot2::guides(colour = ggplot2::guide_legend(reverse = T)) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_discrete(labels = lapply(df2, function(x) stringr::str_wrap(x , width = 18)))

  if(bar_sum == T){
      chart_output = chart_output +
        ggplot2::geom_text(ggplot2::aes(label = stat(y), group = Characteristic), stat = 'summary', fun = sum, hjust = -0.5, fontface = 'bold')

  }

  # write plot to excel if path provided
  if(!is.na(excel)){
    write_excel_plot(chart_output, wb, excel, reference, width, height)
  }

  return(chart_output)
}

# Outputs a grouped bar chart
#
#' Creates a grouped bar chart
#' @name grouped_bar
#'
#' @description  Create a grouped chart from a two-way percent distribution table
#'
#' @usage
#' grouped_bar(df, plot_title = "Title", plot_caption = "", row_or_col)
#'
#' @param df Two-way percent distribution table
#' @param plot_title Plot title. Requires a string.
#' @param plot_caption Plot footnote. Requries a string.
#' @param row_col_subrow if row, uses the row as the group; if col uses the column as the group (should typically use row if df from tabrow), if subrow recognizes format from tabsubrow
#' @param rev if True, colors go from light to dark if horizontal, if false colors go from dark to light if horiztonal, opposite if vertical
#' @param horizontal if True, returns a horizontal grouped bar chart; if False, returns a vertical grouped bar chart
#' @param bold_rows Adds bold labelling to each row number in list.
#'
#'  @return Grouped bar chart
#'
#' @examples
#' # Load in the SHED 2019 data
#' shed_2019 <- read_sheddata(2019)
#' # Figure 11 in SHED 2019 Report
#' fig3_4_data <- tabcol(shed_2019, row_vars = c(D34), col_vars = educ_3cat, rowvalall = c(2,3), labelrowvars = T, labelcolvars = F)
#' fig3_4_chart <- grouped_bar(fig3_4_data, row_col_subrow = "row", plot_title = "Figure 11. Usual place of work (by education)", plot_caption = "Among adults who worked for someone else")
#' @export
grouped_bar <- function(df, plot_title = "Title", plot_caption = "", row_col_subrow, rev = F, horizontal = T, bold_rows =c(),
                        wb = NA, excel = NA, reference = NA, width = 6.5, height = 4){

  if(!(row_col_subrow %in% c("row", "col", "subrow"))){
    stop("Value for row_col_subrow must be 'row' for tabrow, 'col' for tabcol, or 'subrow' for tabsubrow. Please update")
  }

  # extracting table info from df
  if(!is.null(attr(df, "title")) & plot_title == "Title"){
    plot_title = attr(df, "title")
  }
  if(!is.null(attr(df, "footnote")) & plot_caption == ""){
    plot_caption = attr(df, "footnote")
  }
  if(!is.null(attr(df, "reference")) & is.na(reference)){
    reference = attr(df, "reference")
  }

  # transform data
  names(df)[1] <- "Characteristic"

  cols <- names(df)[-1]

  # bold labels
  if(length(bold_rows) > 0){
    if( min(bold_rows) < 1 | max(bold_rows) > nrow(df)){
      stop("Input for bold_rows out of bounds. Please update.")
    }
  }

  bold_labels <- rep("plain", nrow(df))
  bold_labels[bold_rows] <- "bold"
  bold_labels <- rlist::list.reverse(bold_labels)

  if(row_col_subrow=="subrow") {
    num_rowgroups <- df %>% dplyr::filter(is.na(df[,2]))
    subrows <- df %>%
      dplyr::filter(!is.na(df[,2])) %>%
      dplyr::filter(!grepl("Overall", Characteristic))

    new_char <- c()
    for(var in num_rowgroups$Characteristic){
      new_char <- c(new_char, rep(paste0(var), length(unique(subrows$Characteristic))))
    }
    wide_data <- data.frame(Characteristic = new_char, Cross = subrows$Characteristic, Percent = subrows[,2])
  } else if (row_col_subrow== "row"){
    wide_data <- df %>%
      tidyr::pivot_longer(cols, names_to = "Cross", values_to = "Percent")  %>%
      dplyr::rename(Cross2 = Cross,
                    Cross = Characteristic) %>%
      dplyr::rename(Characteristic = Cross2)
  }
    else{
    wide_data <- df %>%
      tidyr::pivot_longer(cols, names_to = "Cross", values_to = "Percent")
  }

  # reordering factor levels to have the bars ordered by the rows on the table
  table_reorder<- wide_data %>%
    dplyr::mutate(Characteristic = factor(Characteristic, levels = unique(wide_data$Characteristic))) %>%
    dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic)) %>%
    dplyr::mutate(Cross = factor(Cross, levels = unique(wide_data$Cross))) %>%
    dplyr::mutate(Cross = forcats::fct_rev(Cross))

  if(horizontal == F){
    table_reorder <- table_reorder %>%
      dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic)) %>%
      dplyr::mutate(Cross = forcats::fct_rev(Cross))
  }

    num_groups = length(unique(wide_data$Cross))


  if(num_groups == 5){
    color_list <- c("slategray1", "lightsteelblue","steelblue4", "dodgerblue4", "navyblue")
  }

  if(num_groups == 4){
      color_list <- c("slategray1", "lightsteelblue","steelblue4", "dodgerblue4")
  }

  if(num_groups == 3){
      color_list <- c("lightsteelblue","steelblue4", "dodgerblue4")
  }

  if(num_groups == 2){
      color_list <- c("lightsteelblue", "dodgerblue4")
  }

  if(rev == T){
    color_list = rev(color_list)
  }

  # bar labels
  table_reorder$barlabels <- dplyr::if_else(table_reorder$Percent == 0, "*",
                                     as.character(table_reorder$Percent))

  # adding footnote to figure
  footnote <- dplyr::if_else(plot_caption == "none", "",
                             dplyr::if_else(plot_caption == "", "Note: Among all adults. Key identifies bars in order from top to bottom.",
                             paste0("Note: ", plot_caption, " Key identifies bars in order from top to bottom.")))

  if(!is.na(reference) & !stringr::str_detect(plot_title, "Figure")){
    plot_title <- paste0(reference, ". ", plot_title)
  }

  # grouped bar chart output with horizontal bars with numbers at the end of each bar
  if(horizontal == T){
    chart_output <- table_reorder %>%
      ggplot2::ggplot(ggplot2::aes(x = Characteristic, y = Percent, fill = Cross)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width=0.8), width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = stringr::str_wrap(plot_title,60), caption = stringr::str_wrap(footnote,95)) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(hjust = 1),
                     axis.line = ggplot2::element_line(colour = "black"),
                     panel.background = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 0),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.title = ggplot2::element_blank(),
                     axis.text.y = suppressWarnings(ggplot2::element_text(face = bold_labels))) +
      ggplot2::geom_text(ggplot2::aes(label = barlabels), position = ggplot2::position_dodge(0.9), hjust = -0.5)+
      ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, max(table_reorder$Percent, na.rm = T) + 5 )) +
      ggplot2::scale_fill_manual(values = color_list) +
      ggplot2::guides(colour = ggplot2::guide_legend(reverse = T)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x , width = 18))

    } else{
      chart_output <- table_reorder %>%
        ggplot2::ggplot(ggplot2::aes(x = Characteristic, y = Percent, fill = Cross)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width=0.8), width = 0.7) +
        ggplot2::labs(title = stringr::str_wrap(plot_title,60), caption = stringr::str_wrap(footnote,95)) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black"),
                       panel.background = ggplot2::element_blank(),
                       plot.caption = ggplot2::element_text(hjust = 0),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       legend.position = "bottom",
                       legend.title = ggplot2::element_blank(),
                       axis.text.x = suppressWarnings(ggplot2::element_text(face = bold_labels))) +
        ggplot2::geom_text(ggplot2::aes(label = barlabels), position = ggplot2::position_dodge(0.9), vjust = -0.5)+
        ggplot2::scale_fill_manual(values = color_list) +
        ggplot2::guides(colour = ggplot2::guide_legend(reverse = F)) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = F)) +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x , width = 18))
    }

  # write plot to excel if path provided
  if(!is.na(excel)){
    write_excel_plot(chart_output, wb, excel, reference, width, height)
  }

  return(chart_output)
}



# dumbbell ----------------------------------------------------------------
#
# Takes in a table created by tabcol or tabrow function
# Outputs a dumbbell plot
#
#' Creates a dumbbell plot
#' @name dumbbell
#'
#' @description  Create a dumbbell plot from a two-way percent distribution table for up to three groupings of variables.
#'
#' @usage
#' dumbbell(df, plot_title = "Title", plot_caption = "", row_or_col)
#'
#' @param df Two-way percent distribution table
#' @param plot_title Plot title. Requires a string.
#' @param plot_caption Plot footnote. Requries a string.
#' @param row_col_subrow if "row", uses the row as the group; if "col" or "subrow" uses the column as the group (should typically use row if df from tabrow)
#' @param bold_rows Adds bold labelling to each row number in list.
#'
#' @return Dumbell bar chart
#'
#' @examples
#' # Load in the SHED 2019 data
#' shed_2019 <- read_sheddata(2019)
#' # Figure 25 in SHED 2019 Report
#' fig6_5_data <- tabcol(shed_2019, row_vars = c(GH3_e, GH3_a, GH3_b, GH3_c, GH3_d), col_vars = GH1_3cat, rowvalall = 1, colvalall = c(1,2), labelrowvars = T, labelcolvars = F, overall = F)
#' fig6_5_chart <- dumbbell(fig6_5_data, plot_title = "Figure 25. Satisfied with local neighborhood and housing (by housing tenure)", plot_caption = "Among adults who owned or rented their home.", refused = F, row_or_col = 'row')
#' @export
dumbbell <- function(df, plot_title = "Title", plot_caption = "", row_col_subrow, bold_rows = c(), rev = F,
                     wb = NA, excel = NA, reference = NA, width = 6.5, height = 4){

  if(!(row_col_subrow %in% c("row", "col", "subrow"))){
    stop("Value for row_col_subrow must be 'row' for tabrow, 'col' for tabcol, or 'subrow' for tabsubrow. Please update")
  }

  # extracting table info from df
  if(!is.null(attr(df, "title")) & plot_title == "Title"){
    plot_title = attr(df, "title")
  }
  if(!is.null(attr(df, "footnote")) & plot_caption == ""){
    plot_caption = attr(df, "footnote")
  }
  if(!is.null(attr(df, "reference")) & is.na(reference)){
    reference = attr(df, "reference")
  }

  if(length(bold_rows) > 0){
    if( min(bold_rows) < 1 | max(bold_rows) > nrow(df)){
    stop("Input for bold_rows out of bounds. Please update.")
    }
  }

  # transform data
  names(df)[1] <- "Characteristic"

  cols <- names(df)[-1]

  bold_labels <- rep("plain", nrow(df))
  bold_labels[bold_rows] <- "bold"
  bold_labels <- rlist::list.reverse(bold_labels)

  if(row_col_subrow=="subrow") {
    num_rowgroups <- df %>% dplyr::filter(is.na(df[,2]))
    subrows <- df %>%
      dplyr::filter(!is.na(df[,2])) %>%
      dplyr::filter(!grepl("Overall", Characteristic))

    new_char <- c()
    for(var in num_rowgroups$Characteristic){
      new_char <- c(new_char, rep(paste0(var), length(unique(subrows$Characteristic))))
    }
    wide_data <- data.frame(Characteristic = new_char,
                            Cross = subrows$Characteristic,
                            Percent = subrows[,2]) %>%
      dplyr::group_by(Cross) %>%
      dplyr::mutate(max = max(Percent), min = min(Percent),
                    Order = dplyr::case_when(Percent == max ~ "Greatest",
                                             Percent == min ~ "Least",
                                             TRUE ~ "Middle")) %>%
      dplyr::select(-c(max,min)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(Cross2 = Cross,
                    Cross = Characteristic) %>%
      dplyr::rename(Characteristic = Cross2)
    }
  else if(row_col_subrow == "col"){
    wide_data <- df %>%
    tidyr::pivot_longer(cols, names_to = "Cross", values_to = "Percent") %>%
    dplyr::group_by(Characteristic) %>%
    dplyr::mutate(max = max(Percent), min = min(Percent),
                  Order = dplyr::case_when(Percent == max ~ "Greatest",
                                              Percent == min ~ "Least",
                                              TRUE ~ "Middle")) %>%
    dplyr::select(-c(max,min)) %>%
    dplyr::ungroup()
    }
  else {
    wide_data <- df %>%
      tidyr::pivot_longer(cols, names_to = "Cross", values_to = "Percent") %>%
      dplyr::group_by(Cross) %>%
      dplyr::mutate(max = max(Percent), min = min(Percent),
                    Order = dplyr::case_when(Percent == max ~ "Greatest",
                                             Percent == min ~ "Least",
                                             TRUE ~ "Middle")) %>%
      dplyr::select(-c(max,min)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(Cross2 = Cross,
                    Cross = Characteristic) %>%
      dplyr::rename(Characteristic = Cross2)
  }

  # reordering factor levels to have the bars ordered by the rows on the table
  table_reorder<- wide_data %>%
    dplyr::mutate(Characteristic = factor(Characteristic, levels = unique(wide_data$Characteristic))) %>%
    dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic))  %>%
    dplyr::mutate(Cross = factor(Cross, levels = unique(wide_data$Cross))) %>%
    dplyr::mutate(Cross = forcats::fct_rev(Cross))

  num_groups = length(unique(wide_data$Cross))

  if(num_groups == 6){
    color_list <- c("lightsteelblue", "dodgerblue4","steelblue4", "navyblue", "deepskyblue", "slategray1" )
  }

  if(num_groups == 4){
    color_list <- c("slategray1", "lightsteelblue","steelblue4", "dodgerblue4")
  }

  if(num_groups == 3){
    color_list <- c( "steelblue4","lightsteelblue","dodgerblue4")
  }

  if(num_groups == 2){
    color_list <- c("dodgerblue4", "lightsteelblue")
  }

  if(rev == T){
    color_list = rev(color_list)
  }

  # adding footnote to figure
  footnote <- dplyr::if_else(plot_caption == "none", "",
                             dplyr::if_else(plot_caption == "", "Note: Among all adults. Key identifies circles in order from left to right.",
                             paste0("Note: ", plot_caption, " Key identifies circles in order from left to right.")))

  if(!is.na(reference) & !stringr::str_detect(plot_title, "Figure")){
    plot_title <- paste0(reference, ". ", plot_title)
  }

  # grouped bar chart output with horizontal bars with numbers at the end of each bar
  chart_output <- table_reorder %>%
    ggplot2::ggplot(ggplot2::aes(x = Characteristic, y = Percent, group = Characteristic)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(color = Cross, size = 5), show.legend = T) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = plot_title, caption = footnote) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(hjust = 1),
                   axis.line = ggplot2::element_line(colour = "black"),
                   panel.background = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(colour = 'transparent', fill = 'transparent'),
                   axis.text.y = suppressWarnings(ggplot2::element_text(face = bold_labels))) +
    ggplot2::geom_text(data = dplyr::filter(table_reorder, Order == "Least"),ggplot2::aes(label = Percent), hjust = 2)+
    ggplot2::geom_text(data = dplyr::filter(table_reorder, Order == "Greatest"),ggplot2::aes(label = Percent), hjust = -1)+
    ggplot2::geom_text(data = dplyr::filter(table_reorder, Order == "Middle"),ggplot2::aes(label = Percent), vjust = -1.5)+
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(min(table_reorder$Percent, na.rm = T) -5, max(table_reorder$Percent, na.rm = T) + 5 )) +
    ggplot2::scale_color_manual(values = color_list) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE), size = F) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x , width = 18))

  if(!is.na(excel)){
    write_excel_plot(chart_output, wb, excel, reference, width, height)
  }

  return(chart_output)
}

# connected_scatter ----------------------------------------------------------------
#
# Takes in a table created by tabcol or tabrow function
# Outputs a connected_scatter plot
#
#' Creates a connected_scatter plot
#' @name connected_scatter
#'
#' @description  Create a connected_scatter plot from a two-way percent distribution table for up to three groupings of variables.
#'
#' @usage
#' connected_scatter(df, plot_title = "Title", plot_caption = "", row_col, color_list = c())
#'
#' @param df Two-way percent distribution table
#' @param plot_title Plot title. Requires a string.
#' @param plot_caption Plot footnote. Requries a string.
#' @param row_col if "row", uses the rows as the group (each row will be a line); if "col" uses the column as the group (each column will correspond to a line)
#' @param color_list list of colors of lines
#'
#' @return connected_scatter bar chart
#'
#' @examples
#' # Load in the historical panel data
#' panel <- read_sheddata(historicalpanel)
#'
#' #wellbeing by year
#' wellbeing <- tabrow(shed_data = panel, row_vars = c(year), col_vars = c(atleast_okay), colval1 = c(1),
#' labelcolvars = T, overall=F)
#'
#' connected_scatter(wellbeing, plot_title = "Doing okay financially by year", row_col = "col")
#'
#' # Figure 2 in SHED 2017 Report
#' tab <- c()
#' for(y in 2013:2017){
#'  tab_year <- tabrow(panel %>% filter(year == y),
#'                      row_var = educ_3cat,
#'                      col_var = atleast_okay,
#'                      rowval1 = c(1:3),
#'                      colval1 = 1,
#'                      overall = F)
#'   names(tab_year)[2] <- y
#'
#'   if(y != 2013){
#'     tab_year <- tab_year[,2]
#'   }
#'   tab <- bind_cols(tab, tab_year)
#'   }
#' fig2 <- connected_scatter(tab,
#'                           plot_title = "Figure 2. At least doing okay financially (by survey year and education)",
#'                           row_col = "col")
#'
#'
#' @export
connected_scatter <- function(df, plot_title = "Title", plot_caption = "", row_col, color_list = c(),
                              wb = NA, excel = NA, reference = NA, width = 6.5, height = 4){

  if(!(row_col %in% c("row", "col"))){
    stop("Value for row_col must be 'row' for row groups, 'col' for col groups. Please update")
  }

  # extracting table info from df
  if(!is.null(attr(df, "title")) & plot_title == "Title"){
    plot_title = attr(df, "title")
  }
  if(!is.null(attr(df, "footnote")) & plot_caption == ""){
    plot_caption = attr(df, "footnote")
  }
  if(!is.null(attr(df, "reference")) & is.na(reference)){
    reference = attr(df, "reference")
  }

  # transform data
  names(df)[1] <- "Characteristic"

  cols <- names(df)[-1]

  wide_data <- df %>%
    tidyr::pivot_longer(cols, names_to = "Cross", values_to = "Percent") %>%
    mutate(Percent = as.numeric(as.character(Percent)))

  table_reorder <- wide_data %>%
    dplyr::mutate(Characteristic = factor(Characteristic, levels = unique(wide_data$Characteristic))) %>%
    dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic))  %>%
    dplyr::mutate(Cross = factor(Cross, levels = unique(wide_data$Cross)))

  if(row_col == "col"){
    table_reorder<- table_reorder%>%
      dplyr::mutate(Characteristic = forcats::fct_rev(Characteristic))  %>%
      dplyr::rename(Cross2 = Cross,
                    Cross = Characteristic) %>%
      dplyr::rename(Characteristic = Cross2)
  }

  num_groups = length(unique(table_reorder$Characteristic))

  if(length(color_list) == 0){
  if(num_groups == 5){
    color_list <- c("black","slategray1", "lightsteelblue","steelblue4", "dodgerblue4")
  }
  if(num_groups == 4){
    color_list <- c("slategray1", "lightsteelblue","steelblue4", "dodgerblue4")
  }

  if(num_groups == 3){
    color_list <- c( "steelblue4","lightsteelblue","dodgerblue4")
  }

  if(num_groups == 2){
    color_list <- c("dodgerblue4", "lightsteelblue")
  }
  if(num_groups == 1){
    color_list <- c("dodgerblue4")
  }
  }
  # adding footnote to figure
  footnote <- dplyr::if_else(plot_caption == "none", "",
                             dplyr::if_else(plot_caption == "", "Note: Among all adults. Key identifies curves in order from top to bottom.",
                             paste0("Note: ", plot_caption, " Key identifies curves in order from top to bottom.")))

  if(!is.na(reference) & !stringr::str_detect(plot_title, "Figure")){
    plot_title <- paste0(reference, ". ", plot_title)
  }

  # connected scatter output
  chart_output <- table_reorder %>%
      ggplot2::ggplot(ggplot2::aes(x = Cross, y = Percent, group = Characteristic)) +
      ggplot2::geom_line(ggplot2::aes(color = Characteristic)) +
      ggplot2::geom_point(ggplot2::aes(color = Characteristic, size = 5), show.legend = T) +
      ggplot2::labs(title = plot_title, caption = footnote) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = 1),
                     axis.title.x = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     panel.background = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 0),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.title = ggplot2::element_blank(),
                     legend.key = ggplot2::element_rect(colour = 'transparent', fill = 'transparent')) +
      ggplot2::geom_text(ggplot2::aes(label = Percent), vjust = -1.5)+
      ggplot2::scale_y_continuous(expand = c(0,0), limits = c(min(table_reorder$Percent, na.rm = T) - 5, max(table_reorder$Percent, na.rm = T) + 5 )) +
      ggplot2::scale_color_manual(values = color_list) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE), size = F)

  if(!is.na(excel)){
    write_excel_plot(chart_output, wb, excel, reference, width, height)
  }

  return(chart_output)

}

