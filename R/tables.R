# Initializing recode_list
recode_list <- c("-1", "-2","Refused", "Don't know", "No", "Not Sure", "Not In Universe (not asked)", "refused",
                 "Not Asked", "$0", "Unknown", "Refused creating hardship", "Refused varying income", "REFUSED", "Not asked",
                 "Working for someone else", "None", "No, do not expect to return to the same employer", "Unknown or Refused")

#' @export
show_recodes <- function(){
  print(recode_list)
}

# one_var_tab --------------------------------------
#
# Replica of the stata command tab var 1 [weight]
#' Create one way table for SHED variable
#' @name one_var_tab
#' @description one_var_tab() creates percent distribution table for a single variable
#' @usage #default option
#' one_var_tab <- function(shed_data, shed_var, weight = final_weight, missing = FALSE, roundn = 0)
#' @param shed_data SHED dataset
#' @param shed_var a SHED variable
#' @param weight weight variable used to create percent distribution, default is population weight (final_weight)
#' @param missing if TRUE, includes missing values in the denominator when calculating frequenices.
#' FALSE: excludes missing values in the denominator when calculating frequencies. If missing values are found in the row variables,
#' this option must be specified
#' @param roundn integer indicating the number of decimal places (round).
#'
#'
#' @return the output will be a dataframe that is a percent distribution table
#'
#' @examples
#' data("shed2019")
#' one_var_tab(shed2019, B2, weight=TRUE, missing=TRUE)
#' @export
#' @importFrom dplyr "%>%"
one_var_tab <- function(shed_data, shed_var, weight = final_weight, missing = FALSE, roundn = 0,
                        wb = NA, excel= NA, units= "Percent", title = "Table", reference = "sheet", footnote = "", title_ref = "",
                        chart_type = NA){

  shed_var_name <- rlang::quo_name(rlang::enquo(shed_var))
  var_label <- attr(shed_data[[shed_var_name]], "label")
  weight_var <- rlang::quo_name(rlang::enquo(weight))

  # Recode hardcoded missings in 2017 data
  if("Not In Universe (not asked)" %in% levels(shed_data[[shed_var_name]])){
    shed_data <- missing_hardcode(shed_data)
  }

  if(weight_var %in% c(F, "none", "no", "unweighted", "false")) {
    weight_var = as.character(weight_var)
    shed_data <- shed_data %>%
      dplyr::mutate(!! weight_var := 1)
  }

  if(missing == FALSE){
    shed_data <- shed_data %>%
      tidyr::drop_na({{ shed_var }})
  }

  if(missing == TRUE){
    shed_data[[shed_var_name]] <-forcats::fct_explicit_na(shed_data[[shed_var_name]])
  }


  table_output <- shed_data %>%
    dplyr::group_by({{ shed_var }}) %>%
    dplyr::tally(wt = {{weight}}) %>%
    dplyr::mutate(Percent = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::select({{ shed_var }}, Percent) %>%
    dplyr::mutate(Percent = round(100*Percent, roundn))

  table_fin <- table_output %>% dplyr::arrange({{ shed_var }}) %>%
    dplyr::rename(!!var_label := shed_var_name)

  # write table to excel if path provided
  if(!is.na(excel)){
    write_excel(table_fin, wb, excel, units, title, reference, footnote, title_ref,chart_type)
    attr(table_fin, "title") <- title
    attr(table_fin, "reference") <- reference
    attr(table_fin, "footnote") <- footnote
  }

  return(table_fin)
}

# multi1way --------------------------------------
#
# Similarly to the stata command tab var 1 [weight] but with flexibility to include multiple variables to the table
# Option to add breaks and to add labels to the break

#' Configure table and charts for SHED annual report
#' @name multi1way
#' @description multi1way () is similar to the stata tab command but with flexibility to include multiple variables to the table
#' option to add breaks and to add labels to the breaks. It allows for additional options such as including all adults or to just
#' respondents the question was asked to.
#' @usage #method for default
#' multi1way(shed_data, var_list, rowval1 = NA, rowval2 = NA, rowval3 = NA, rowval4 = NA, rowval5 = NA,rowval6 = NA, rowval7 = NA, rowval8 = NA,
#' rowval9 = NA, rowval10 = NA, rowval11 = NA, rowvalall=NA, breaks=NA,labelbreaks= FALSE, labelvarnames= TRUE,
#' colname1 = "Characteristic", weight = final_weight, missing = FALSE, breaklabels = NA, roundn = 0)
#' @param shed_data SHED dataset
#' @param var_list a list of SHED variables for percent distribution table
#' @param rowval for rowval[n], only display rows where the nth row variable has a value in numlist. If not specified, the default is all values.
#' @param rowvalall only displays rows where each SHED variable in var_list has a value in rowvalall
#' @param breaks Inserts a row break before each specified row variable.
#' @param auto_labelbreaks if TRUE, displays respective row variable label in row break lines. if FALSE, breaks are left as empty rows, NA.
#' @param labelvarnames if TRUE, displays row variable in row break lines. if FALSE, displays value labels in rows. Default is to show value labels in rows. Usually only specified
#' when using one value per variable (i.e. share saying yes) so the variable name is more meaningful than
#' the value label.
#' @param colname1 specifies the first column name
#' @param weight weight variable used to create percent distribution, default is population weight (final_weight)
#' @param missing if TRUE, includes missing values in the denominator when calculating frequencies
#' if FALSE, excludes missing values in the denominator when calculating frequencies. If missing values are found in row
#' variables, this options must be specified.
#' @param breaklabels specifies a list of strings for labeling the breaks, where the first object in the string corresponds to the first break
#' @param roundn integer indicating the number of decimal places (round).
#'
#' @return the output will be a dataframe of a percent distribution table.
#'dev
#' @examples
#' # Creates a dataframe using the SHED 2019 dataset with two break labels and label breaks before the first and third specified variables.
#' data("shed2019")
#' multi1way(shed2019, c(R1_f, R1_e, R1_a, R1_c, R1_b, R1_d), weight = final_weight, missing = FALSE, labelvarnames = TRUE,
#'                  rowvalall = 1, breaks =c(1,3), breaklabels=c("Mortgage Access", "Preference"))
#'
#' # Creates family income distribution table
#' fig2.4_data <- multi1way(shed_2019, I40, rowvalall = c(0:11), colname1 = "Income Category")
#' @export
multi1way <- function(shed_data, var_list, rowval1 = NA, rowval2 = NA, rowval3 = NA, rowval4 = NA, rowval5 = NA,
                      rowval6 = NA, rowval7 = NA, rowval8 = NA, rowval9 = NA, rowval10 = NA, rowval11 = NA, rowval12 = NA, rowval13 = NA, rowvalall=NA, breaks=c(),
                      auto_labelbreaks= FALSE, labelvarnames= TRUE, colname1 = "Characteristic", colname2 = NA, weight = final_weight, missing = FALSE, breaklabels =c(), roundn = 0,
                      wb = NA, excel = NA, units= "Percent", title = "Table", reference = "sheet", footnote = "", title_ref = "",
                      chart_type = NA){


  # Check to see that the number of breaks is the same as the number of labels
  if (length(breaklabels)!=length(breaks) & length(breaks) >0 & auto_labelbreaks != F & length(breaklabels) >0) {
    stop("The number of breaks is not the same as the number of labels. They must be equal.")
  }

  # Recode hardcoded missings in 2017 data
  if(attr(shed_data[[names(shed_data)[1]]], "year") == 2017){
    shed_data <- missing_hardcode(shed_data)
  }

  vars_exprs <- rlang::quo_text(rlang::enquo(var_list))
  shed_vars_list <- vars_exprs %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()
  weight_var <- rlang::quo_name(rlang::enquo(weight))

  # Check that the correct row values are given
  rowval_used <- c()
  for(i in 1:13){
    rowval <- get(paste0("rowval", i))
    rowval_used <- c(rowval_used, anyNA(rowval))
  }

  rowval_used <- ifelse(FALSE %in% rowval_used, which(rowval_used == F), 0)

  suppressWarnings(if (is.na(rowvalall) == F & rowval_used != 0) {
    stop("If rowvallall is specified, then rowval 1-13 should not be.")
  })

  # # Default is all values and set rowvalall if necessary
  # suppressWarnings(if(is.na(rowvalall)){
  #   if(rowval_used == 0){
  #     for(i in 1:length(shed_vars_list)){
  #       max_vals = length(unique(shed_data[[ shed_vars_list[i] ]]))
  #       assign(paste0("rowval",i), c(-2:max_vals))
  #     }
  #
  #   } else{
  #       for(i in 1:length(rowval_used)){
  #         if(is.na(get(paste0("rowval",i)))){
  #           max_vals = length(unique(shed_data[[ shed_vars_list[i] ]]))
  #           assign(paste0("rowval",i), c(-2:max_vals))
  #       }
  #     }
  #   }
  # } else{
  #   for(i in 1:length(rowval_used)){
  #       assign(paste0("rowval",i), rowvalall)
  #     }
  #})


  # function for tabs
  tabs <- function(shed_var){
    tab_out <- one_var_tab(shed_data = shed_data, !!rlang::sym(shed_var), missing = missing, weight = weight, roundn = roundn)

    names(tab_out)[1] <- "Characteristic"

    index <- which(shed_vars_list == shed_var)

    shed_label <- attr(shed_data[[ shed_var ]], "label")

    # get rowvalues
    #rowval <- get(paste0("rowval", index))
    suppressWarnings(if (is.na(rowvalall)){
      rowval <- get(paste0("rowval", index))
    } else{
      rowval <- rowvalall
    })

    # stata values less than 1 could not be translated to level values less than 1
    num_recode <- length(intersect(recode_list, levels(shed_data[[ shed_var ]])))

    new_rowval <- rowval + num_recode

    # Add a values out of bound error message
    # suppressWarnings(if (max(new_rowval) > max(as.integer(shed_data[[shed_var]]))) {
    #   stop("Value out of bounds. Check row value inputs.")
    # })

    # filter based on row values and changing to string
    tab_out <- tab_out %>%
      dplyr::filter(as.integer(Characteristic) %in% new_rowval) %>%
      dplyr::slice(match(new_rowval, as.integer(Characteristic))) %>%
      dplyr::mutate(Characteristic = as.character(Characteristic))

    # variable labels
    if (labelvarnames == TRUE & "Yes" %in% tab_out$Characteristic){
      suppressWarnings(if(is.na(rowvalall)){
        tab_out <- tab_out %>%
          dplyr::mutate(Characteristic = paste0(shed_label, " - " , Characteristic))
      } else {
        tab_out <- tab_out %>%
          dplyr::mutate(Characteristic = paste0(shed_label))
      })
    }

    # label breaks
    suppressWarnings(if(index %in% breaks) {
      if(auto_labelbreaks == TRUE & length(breaklabels) == 0){
        new_row <- data.frame(Characteristic = shed_label, Percent = NA)
      } else {
        if(length(breaklabels) == 0){
          new_row <- data.frame(Characteristic = NA, Percent = NA)
        } else {
          break_index <- which(breaks == index)
          new_row <- data.frame(Characteristic = breaklabels[break_index], Percent = NA)
        }
      }
      tab_out <-suppressWarnings(dplyr::bind_rows(new_row, tab_out))
    })

    return(tab_out)
  }

  # tabing each variable and binding to a single table
  percents <- lapply(shed_vars_list, function(x) tabs(x))
  multi_table <- NULL

  for(i in percents){
    multi_table <- dplyr::bind_rows(multi_table, i)
  }

  multi_table <- multi_table %>%
    dplyr::mutate(!! colname1 := Characteristic) %>%
    dplyr::select(colname1, Percent)

  if(!is.na(colname2)){
    names(multi_table)[2] <- colname2
  }

  attr(multi_table, "breaks") <-which(is.na(multi_table$Percent))

  # write table to excel if path provided
  if(!is.na(excel)){
    write_excel(multi_table, wb, excel, units, title, reference, footnote, title_ref,chart_type)
    attr(multi_table, "title") <- title
    attr(multi_table, "reference") <- reference
    attr(multi_table, "footnote") <- footnote
  }

  return(multi_table)
}



# tabrow
#
# This function takes in a dataset and two lists of variables.
# It reports the relative frequency within its row of each cell
# The rows are from row_vars and columns are from col_vars.
#' Title
#'@name tabrow
#'@description This function takes in a dataset and two lists of variables. It then reports the relative frequency
#'within its row of each cell. The rows are from row_vars and columns are from col_vars.
#'@usage #method for default
#'tabrow <- function(shed_data, row_vars, col_vars, rowval1 = NA, rowval2 = NA, rowval3 = NA, rowval4 = NA, rowval5 = NA,
#'rowval6 = NA, rowval7 = NA, rowval8 = NA, rowval9 = NA, rowval10 = NA, rowval11 = NA, rowval12 = NA, rowval13 = NA, rowvalall=NA,
#'colval1 = NA, colval2 = NA, colval3 = NA, colval4 = NA, colval5 = NA, colval6 = NA, colval7 = NA, colval8 = NA, colval9 = NA,
#'colval10 = NA, colval11 = NA, colval12 = NA, colval13 = NA, colvalall=NA, breaks=c(), auto_labelbreaks= FALSE, labelcolvars= FALSE, labelrowvars = FALSE,
#'colname1 = "Characteristic", weight = final_weight, missing = FALSE, breaklabels =c(), overall = TRUE, roundn = 0)
#' @param shed_data SHED dataset
#' @param row_vars list of SHED variables used for the rows of the table
#' @param col_vars list of SHED variables used for the columns of the table
#' @param rowval for rowval [n], only display rows where the nth row has a value in numlist.
#' @param rowvalall for all row varibles, only displays rows where each SHED variable in var_list has a value in rowvalall
#' @param colval for colval [n], only displays columns where the nth column variable has a value in numlist
#' @param colvalall for all column variables, only displays columns where column variables have a value in colvalall
#' @param breaks inserts a row break before each new row
#' @param auto_labelbreaks if TRUE, displays row variable in row break lines. if FALSE, displays value labels in rows
#' @param labelcolvars if TRUE, uses the question label to label the column, if FALSE, displays the value labels
#' @param labelrowvars if TRUE, uses the question label to label the row, if FALSE, displays the value labels
#' @param colname specifies the name of column [n], up to 5 columns
#' @param weight weight variable used to create percent distribution, default is population weight (final_weight)
#' @param missing if TRUE, includes missing values in the denominator when calculating frequencies
#' @param breaklabels specifies a list of strings for labeling the breaks, where the first object in the string corresponds to the first break
#' @param overall if TRUE, includes overall row
#' @param roundn integer indicating the number of decimal places (round).
#'
#' @return the output will be a frequency table.
#' @export
#'
#' @examples
#' shed_2019 <- read_sheddata(2019)
#' tabrow(shed_2019, row_vars = c(ppethm, educ_3cat), col_vars = c(EF1, EF2), rowvalall = c(1,2,3), colvalall = 1, labelcolvars = T)


tabrow <- function(shed_data, row_vars, col_vars, rowval1 = NA, rowval2 = NA, rowval3 = NA, rowval4 = NA, rowval5 = NA,
                   rowval6 = NA, rowval7 = NA, rowval8 = NA, rowval9 = NA, rowval10 = NA, rowval11 = NA, rowval12 = NA, rowval13 = NA, rowvalall=NA,
                   colval1 = NA, colval2 = NA, colval3 = NA, colval4 = NA, colval5 = NA, colval6 = NA, colval7 = NA, colval8 = NA, colval9 = NA,
                   colval10 = NA, colval11 = NA, colval12 = NA, colval13 = NA, colvalall=NA, breaks=c(), auto_labelbreaks= FALSE, labelcolvars= FALSE, labelrowvars = FALSE,
                   weight = final_weight, missing = FALSE, breaklabels =c(), overall = TRUE, roundn = 0,
                   colname1 = "Characteristic", colname2 = NA, colname3 = NA, colname4 = NA, colname5 = NA,
                   wb = NA, excel = NA, units= "Percent", title = "Table", reference = "sheet", footnote = "", title_ref = "",
                   chart_type = NA) {

  # Recode hardcoded missings in 2017 data
  if(attr(shed_data[[names(shed_data)[1]]], "year") == 2017){
    shed_data <- missing_hardcode(shed_data)
  }

  #creating lists of relevant variables and labels
  row_vars_list <- rlang::quo_text(rlang::enquo(row_vars)) %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()

  col_vars_list <- rlang::quo_text(rlang::enquo(col_vars)) %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()

  weight_var <- rlang::quo_name(rlang::enquo(weight))

  if(weight_var %in% c(F, "none", "no", "unweighted", "false")) {
    weight_var = as.character(weight_var)
    shed_data <- shed_data %>%
      dplyr::mutate(!! weight_var := 1)
  }

  # cross tab of one row var and one col var based on value inputs
  cross_tab <- function(temp_data, row_var, col_var){

    # drop missing values
    if(missing == FALSE){
      temp_data <- temp_data %>%
        tidyr::drop_na({{ row_var }}, {{col_var}})
    } else{
      temp_data[[row_var]] <-forcats::fct_explicit_na(temp_data[[row_var]])
      temp_data[[col_var]] <-forcats::fct_explicit_na(temp_data[[col_var]])
    }

    # get rowval[n] and colval[n]
    row_index <- which(row_vars_list == row_var)
    col_index <- which(col_vars_list == col_var)
    row_label <- attr(shed_data[[ row_var ]], "label")
    col_label <- attr(shed_data[[ col_var ]], "label")

    suppressWarnings(if (is.na(rowvalall)){
      rowval <- get(paste0("rowval", row_index))
    } else{
      rowval <- rowvalall
    })

    suppressWarnings(if (is.na(colvalall)){
      colval <- get(paste0("colval", col_index))
    } else{
      colval <- colvalall
    })

    # stata values less than 1 could not be translated to level values less than 1
    # stata values less than 1 could not be translated to level values less than 1
    # if rowval[n] is NA but there is an nth row_var, then the default is all values
    # if colval[n] is NA but there is an nth col_var, then the default is all values
    suppressWarnings(if(is.na(rowval) == F){
      rowval_recode <- rowval + length(intersect(recode_list, levels(shed_data[[ row_var ]])))
    } else {
      rowval_recode <- c(1:length(levels(shed_data[[ row_var ]])))
    })

    suppressWarnings(if(is.na(colval) == F){
      colval_recode <- colval + length(intersect(recode_list, levels(shed_data[[ col_var ]]))) + 1
    } else {
      colval_recode <- c(1:length(levels(shed_data[[ col_var ]])) + 1)
    })

    # Create table
    temp_table <- temp_data %>%
      dplyr::group_by(.data[[row_var]], .data[[col_var]] ) %>%
      dplyr::tally(wt = {{weight}}) %>%
      dplyr::mutate(Percent = n / sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data[[col_var]], .data[[row_var]], Percent) %>%
      dplyr::mutate(Percent = round(100*Percent, roundn)) %>%
      tidyr::pivot_wider(names_from = .data[[col_var]], values_from = Percent)

    # include overall
    overall <- temp_data %>%
      dplyr::group_by(.data[[col_var ]]) %>%
      dplyr::tally(wt = {{weight}}) %>%
      dplyr::mutate(Percent = n / sum(n)) %>%
      dplyr::select(.data[[col_var ]], Percent) %>%
      dplyr::mutate(Percent = round(100*Percent, roundn)) %>%
      tidyr::pivot_wider(names_from = .data[[ col_var ]], values_from = Percent)

     # This is a temporary fix for the
    class(temp_table[[row_var]]) <- 'factor'

    table_output <- dplyr::bind_rows(temp_table, overall) %>%
      dplyr::mutate(!!row_var := forcats::fct_explicit_na(.data[[ row_var ]], na_level = "Overall"))


    names(table_output)[1] <- "Characteristic"

    missing_col_index <- which(!levels(shed_data[[col_var]]) %in% names(temp_table))
    missing_col <- levels(shed_data[[col_var]])[missing_col_index]
    if(length(missing_col_index)!=0){
      for(i in missing_col){
        table_output[[i]] <- 0
      }
    }


    missing_row_index <- which(!levels(shed_data[[row_var]]) %in% temp_table[[row_var]])
    missing_row <- levels(shed_data[[row_var]])[missing_row_index]
    if(length(missing_row_index)!=0){
      for(i in 1:length(missing_row_index)){

        new_row <- table_output[1,]
        new_row[1,1] <- missing_row[i]
        new_row[c(2:ncol(new_row))] <- 0
        table_output <- DataCombine::InsertRow(table_output, new_row, missing_row_index[i])

      }
    }

    table_output <- table_output[c("Characteristic",levels(shed_data[[col_var]]))]

    table_output<-table_output
    # filter based on row values and col values and changing to string
    row_diff <- ifelse(min(rowval_recode) - 1 == 0, 0, (min(rowval_recode) -1))

    tab_out <- table_output %>%
      dplyr::filter(as.integer(Characteristic) %in% rowval_recode | Characteristic == "Overall") %>%
      dplyr::select(1, colval_recode) %>%
      dplyr::arrange(c(rowval_recode - row_diff, nrow(table_output))) %>%
      dplyr::mutate(Characteristic = as.character(Characteristic))

    # label variables
    if (labelrowvars == TRUE & ( "Yes" %in% temp_table[[ row_var ]] | "Satisfied" %in% temp_table[[ row_var ]])){
      if (length(rowval_recode) > 1){
        tab_out <- tab_out %>%
          dplyr::mutate(Characteristic = dplyr::if_else(
            Characteristic != "Overall", paste0(row_label, " - ", Characteristic), Characteristic))
      } else{
        tab_out <- tab_out %>%
          dplyr::mutate(Characteristic = dplyr::if_else(
            Characteristic != "Overall", paste0(row_label), Characteristic))

      }
    }

    if (labelcolvars == TRUE & ("Yes" %in% names(temp_table) | "Satisfied" %in% names(temp_table))){
      if(length(colval_recode) >1){
        for (i in 1:length(colval_recode)){
          names(tab_out)[1 + i] <- paste0(col_label, " - ", names(tab_out)[1+i])
        }
      } else{
        names(tab_out)[-1] <- paste0(col_label)

      }
    }

    if (labelcolvars == FALSE & ("Yes" %in% names(temp_table) | "Satisfied" %in% names(temp_table))){
      for (i in 1:length(colval_recode)){
        names(tab_out)[1 + i] <- paste0(col_var, " - ", names(tab_out)[1+i])
      }
    }

    # label breaks
    suppressWarnings(if(row_index %in% breaks) {
      if(auto_labelbreaks == TRUE & length(breaklabels) == 0){
        new_row <- data.frame(Characteristic = row_label)
      } else {
        if(length(breaklabels) == 0){
          new_row <- data.frame(Characteristic = NA)
        } else {
          break_index <- which(breaks == row_index)
          new_row <- data.frame(Characteristic = breaklabels[break_index])
        }
      }
      tab_out <-suppressWarnings(dplyr::bind_rows(new_row, tab_out))
    })


    return(tab_out)
  }


  full_temp <- NULL
  #for each row variable
  for(x in row_vars_list){

    temp <- NULL
    #for each col variable
    for(y in col_vars_list){
      cross_output <- cross_tab(shed_data, x, y)

      # merging columns
      if(is.null(temp) == TRUE){
        temp<- cross_output
      }
      else{
        temp <- suppressMessages(dplyr::left_join(temp,cross_output, by = "Characteristic"))
      }
    }

    #merging tables
    if(is.null(full_temp) == TRUE){
      full_temp <- temp
    }
    else{
      full_temp <- dplyr::bind_rows(full_temp, temp)
    }
  }

  # overall row option
  if (overall == TRUE){
    overall_last<- full_temp %>%
      dplyr::filter(grepl("Overall", Characteristic)) %>%
      dplyr::slice(dplyr::n())

    table_out <- suppressMessages(full_temp %>%
                                    dplyr::filter(Characteristic != "Overall") %>%
                                    dplyr::full_join(overall_last))

  }
  else(
    table_out <- full_temp %>%
      dplyr::filter(Characteristic != "Overall")
  )

  # rename first col
  names(table_out)[1] <- colname1

  attr(table_out, "breaks") <-which(is.na(table_out[1:nrow(table_out),2]))
  attr(table_out, "overall") <- which(table_out[,1] == "Overall")
  #table_out <- suppressMessages(sticky::sticky_all(table_out))

  # rename up to the first five columns
  for(i in 1:5){
    if(!is.na(get(paste0("colname", i)))){
      names(table_out)[i] <- get(paste0("colname", i))
    }
  }

  # write table to excel if path provided
  if(!is.na(excel)){
    write_excel(table_out, wb, excel, units, title, reference, footnote, title_ref, chart_type)
    attr(table_out, "title") <- title
    attr(table_out, "reference") <- reference
    attr(table_out, "footnote") <- footnote
  }

  return(table_out)

}


# tabcol --------------------------------------
# Similarly to the stata command tab var1 var2 [weight], col nofreq but with flexibility to include multiple variables to the table
# Option to add breaks and to add labels to the break.

#' Tabcol
#'@name tabcol
#'@description This function takes in a SHED dataset and two lists of variables. It then reports the relative frequency
#'within its columns of each cell. It is equivalent to the stata command tab var1 var2, col while allowing
#'for multiple rows and columns. The columns are from col_vars and rows are from row_vars.
#'@usage
#'tabcol(shed_data, row_vars, col_vars, colval1 = NA, colval2 = NA, colval3 = NA, colval4 = NA, colval5 = NA,
#'colval6 = NA, colval7 = NA, colval8 = NA, colval9 = NA, colval10 = NA, colval11 = NA, colval12 = NA, colval13 = NA, colvalall=NA,
#'rowval1 = NA, rowval2 = NA, rowval3 = NA, rowval4 = NA, rowval5 = NA,
#'rowval6 = NA, rowval7 = NA, rowval8 = NA, rowval9 = NA, rowval10 = NA, rowval11 = NA, rowval12 = NA, rowval13 = NA, rowvalall=NA,
#'breaks=c(), auto_labelbreaks= FALSE, labelcolvars= FALSE, labelrowvars = FALSE,
#'colname1 = "Characteristic", weight = final_weight, missing = FALSE, breaklabels =c(), overall = TRUE, roundn = 0)
#' @param shed_data SHED dataset
#' @param row_vars list of SHED variables used for the rows of the table
#' @param col_vars list of SHED variables used for the columns of the table
#' @param rowval for rowval [n], only display rows where the nth row has a value in numlist; default is all values.
#' @param rowvalall for all row variables, only displays rows where each SHED variable in var_list has a value in rowvalall; default is all values.
#' @param colval for colval [n], only display column where the nth column variable has a value in numlist; default is all values
#' @param colvalall for all column variables, only displays columns where column variables have a value in colvalall; default is all values.
#' @param breaks inserts a row break before each new row
#' @param auto_labelbreaks if TRUE, displays row variable in row break lines. if FALSE, displays value labels in rows
#' @param labelcolvars if TRUE, uses the question label to label the column, if FALSE, displays the value labels
#' @param labelrowvars if TRUE, uses the question label to label the row, if FALSE, displays the value labels
#' @param colname specifies the name of column [n], up to 5 columns
#' @param weight weight variable used to create percent distribution, default is population weight (final_weight)
#' @param missing if TRUE, includes missing values in the denominator when calculating frequencies
#' @param breaklabels specifies a list of strings for labeling the breaks, where the first object in the string corresponds to the first break
#' @param overall if TRUE, includes overall row
#' @param roundn integer indicating the number of decimal places (round).
#'
#' @return the output will be a frequency table.
#' @export
#'
#' @examples
#' # Creates a dataframe using the SHED 2019 dataset with two break labels and label breaks before the first and fourth specified variables.
#' # Table 11: Forms of alternative financial services used
#' shed_2019 <- read_sheddata(2019)
#' table11 <- tabcol(shed_2019, row_vars = c(BK2_a, BK2_b, afs_trans, BK2_c, BK2_d, BK2_e, afs_credit), col_vars = afs,
#'                colvalall = 1, rowvalall = 1, labelcolvars = T, labelrowvars = T, breaks = c(1,4), breaklabels = c("Transaction services", "Borrowing Services"))
tabcol <- function(shed_data, row_vars, col_vars, rowval1 = NA, rowval2 = NA, rowval3 = NA, rowval4 = NA, rowval5 = NA,
                   rowval6 = NA, rowval7 = NA, rowval8 = NA, rowval9 = NA, rowval10 = NA, rowval11 = NA, rowval12 = NA, rowval13 = NA, rowvalall=NA,
                   colval1 = NA, colval2 = NA, colval3 = NA, colval4 = NA, colval5 = NA, colval6 = NA, colval7 = NA, colval8 = NA, colval9 = NA,
                   colval10 = NA, colval11 = NA, colval12 = NA, colval13 = NA, colvalall=NA, breaks=c(), auto_labelbreaks= FALSE, labelcolvars= FALSE, labelrowvars = FALSE,
                   weight = final_weight, missing = FALSE, breaklabels =c(), overall = FALSE, roundn = 0,
                   colname1 = "Characteristic", colname2 = NA, colname3 = NA, colname4 = NA, colname5 = NA,
                   wb = NA, excel = NA, units= "Percent", title = "Table", reference = "sheet", footnote = "", title_ref = "",
                   chart_type = NA){

  # Recode hardcoded missings in 2017 data
  if(attr(shed_data[[names(shed_data)[1]]], "year") == 2017){
    shed_data <- missing_hardcode(shed_data)
  }

  #creating lists of relevant variables and labels
  row_vars_list <- rlang::quo_text(rlang::enquo(row_vars)) %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()

  col_vars_list <- rlang::quo_text(rlang::enquo(col_vars)) %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()

  weight_var <- rlang::quo_name(rlang::enquo(weight))

  if(weight_var %in% c(F, "none", "no", "unweighted", "false")) {
    weight_var = as.character(weight_var)
    shed_data <- shed_data %>%
      dplyr::mutate(!! weight_var := 1)
  }

  # cross tab of one row var and one col var based on value inputs
  cross_tab <- function(temp_data, row_var, col_var){

    # drop missing values
    if(missing == FALSE){
      temp_data <- temp_data %>%
        tidyr::drop_na({{ row_var }}, {{col_var}})
    } else{
      temp_data[[row_var]] <-forcats::fct_explicit_na(temp_data[[row_var]])
      temp_data[[col_var]] <-forcats::fct_explicit_na(temp_data[[col_var]])
    }

    # get rowval[n] and colval[n]
    row_index <- which(row_vars_list == row_var)
    col_index <- which(col_vars_list == col_var)
    row_label <- attr(shed_data[[ row_var ]], "label")
    col_label <- attr(shed_data[[ col_var ]], "label")

    suppressWarnings(if (is.na(rowvalall)){
      rowval <- get(paste0("rowval", row_index))
    } else{
      rowval <- rowvalall
    })

    suppressWarnings(if (is.na(colvalall)){
      colval <- get(paste0("colval", col_index))
    } else{
      colval <- colvalall
    })

    # stata values less than 1 could not be translated to level values less than 1
    # if rowval[n] is NA but there is an nth row_var, then the default is all values
    # if colval[n] is NA but there is an nth col_var, then the default is all values
    suppressWarnings(if(is.na(rowval) == F){
      rowval_recode <- rowval + length(intersect(recode_list, levels(shed_data[[ row_var ]])))
    } else {
      rowval_recode <- c(1:length(levels(shed_data[[ row_var ]])))
    })

    suppressWarnings(if(is.na(colval) == F){
      colval_recode <- colval + length(intersect(recode_list, levels(shed_data[[ col_var ]]))) +1
    } else {
      colval_recode <- c(1:length(levels(shed_data[[ col_var ]])) + 1)
    })

    # Create table
    temp_table <- temp_data %>%
      dplyr::group_by(.data[[col_var]], .data[[row_var]] ) %>%
      dplyr::tally(wt = {{weight}}) %>%
      dplyr::mutate(Percent = n / sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data[[col_var]], .data[[row_var]], Percent) %>%
      dplyr::mutate(Percent = round(100*Percent, roundn)) %>%
      tidyr::pivot_wider(names_from = .data[[col_var]], values_from = Percent)

    # include overall
    overall <- temp_data %>%
      dplyr::group_by(.data[[row_var ]]) %>%
      dplyr::tally(wt = {{weight}}) %>%
      dplyr::mutate(Percent = n / sum(n)) %>%
      dplyr::select(.data[[row_var ]], Percent) %>%
      dplyr::mutate(Percent = round(100*Percent, roundn)) %>%
      tidyr::pivot_wider(names_from = .data[[ row_var ]], values_from = Percent) %>%
      t() %>% data.frame(Overall = ., row_var = row.names(.)) %>%
      dplyr::mutate(!!row_var := row_var) %>%
      dplyr::select(Overall, .data[[ row_var]])

    # This is a temporary fix for the can't combine factor issue
    class(temp_table[[row_var]]) <- 'factor'

    overall_order  <- suppressWarnings(dplyr::left_join(temp_table, overall, by = row_var)) %>%
      dplyr::select(Overall)

    table_output <- dplyr::bind_cols(temp_table, overall_order)

    names(table_output)[1] <- "Characteristic"

    missing_col_index <- which(!levels(shed_data[[col_var]]) %in% names(temp_table))
    missing_col <- levels(shed_data[[col_var]])[missing_col_index]
    if(length(missing_col_index)!=0){
      for(i in missing_col){
        table_output[[i]] <- 0
      }
    }

    missing_row_index <- which(!levels(shed_data[[row_var]]) %in% temp_table[[row_var]])
    missing_row <- levels(shed_data[[row_var]])[missing_row_index]
    if(length(missing_row_index)!=0){
      for(i in 1:length(missing_row_index)){

        new_row <- table_output[1,]
        new_row[1,1] <- missing_row[i]
        new_row[c(2:ncol(new_row))] <- 0
        table_output <- DataCombine::InsertRow(table_output, new_row, missing_row_index[i])

      }
    }

    table_output <- table_output[c("Characteristic",levels(shed_data[[col_var]]), "Overall")]
    # filter based on row values and col values and changing to string
    row_diff <- ifelse(min(rowval_recode) - 1 == 0, 0, (min(rowval_recode) -1))

    tab_out <- table_output %>%
      dplyr::filter(as.integer(Characteristic) %in% rowval_recode ) %>%
      dplyr::select(c(1, colval_recode), Overall) %>%
      dplyr::arrange(rowval_recode - row_diff) %>%
      dplyr::mutate(Characteristic = as.character(Characteristic))

    # label variables
    if (labelrowvars == TRUE & ( "Yes" %in% temp_table[[ row_var ]] | "Satisfied" %in% temp_table[[ row_var ]])){
      if (length(rowval_recode) > 1){
        tab_out <- tab_out %>%
          dplyr::mutate(Characteristic = paste0(row_label, " - ", Characteristic))
      } else{
        tab_out <- tab_out %>%
          dplyr::mutate(Characteristic = paste0(row_label))
      }
    }

    if (labelcolvars == TRUE& ( "Yes" %in% names(temp_table) | "Satisfied" %in% names(temp_table)) ){
      if(length(colval_recode) >1){
        for (i in 1:length(colval_recode)){
          names(tab_out)[1 + i] <- paste0(col_label, " - ", names(tab_out)[1+i])
        }
      }
      else{
        names(tab_out)[c(-1, -length(tab_out))] <- paste0(col_label)
      }
    }

    if (labelcolvars == FALSE & ( "Yes" %in% names(temp_table) | "Satisfied" %in% names(temp_table))){
      for (i in 1:length(colval_recode)){
        names(tab_out)[1 + i] <- paste0(col_var, " - ", names(tab_out)[1+i])
      }
    }

    # label breaks
    suppressWarnings(if(row_index %in% breaks) {
      if(auto_labelbreaks == TRUE & length(breaklabels) == 0){
        new_row <- data.frame(Characteristic = row_label)
      } else {
        if(length(breaklabels) == 0){
          new_row <- data.frame(Characteristic = NA)
        } else {
          break_index <- which(breaks == row_index)
          new_row <- data.frame(Characteristic = breaklabels[break_index])
        }
      }
      tab_out <-suppressWarnings(dplyr::bind_rows(new_row, tab_out))
    })

    return(tab_out)
  }

  full_temp <- NULL
  #for each col variable
  for(x in row_vars_list){

    temp <- NULL
    #for each row variable
    for(y in col_vars_list){
      cross_output <- cross_tab(shed_data, x, y)

      # merging columns
      if(is.null(temp) == TRUE){
        temp<- cross_output
      }
      else{
        temp <- suppressMessages(dplyr::left_join(temp,cross_output, by = "Characteristic"))
      }

    }

    #merging tables
    if(is.null(full_temp) == TRUE){
      full_temp <- temp
    }
    else{
      full_temp <- dplyr::bind_rows(full_temp, temp)
    }
  }

  attr(full_temp, "breaks") <-which(is.na(full_temp[,ncol(full_temp)]))

  # overall row option
  overall_col <- full_temp %>%
    dplyr::select(dplyr::starts_with("Overall"), dplyr::ends_with("Overall")) %>%
    dplyr::select(1)
  names(overall_col)[1] <- "Overall"

  full_temp <- full_temp %>%
    dplyr::select(-c(dplyr::starts_with("Overall"), dplyr::ends_with("Overall")))

  if(overall == TRUE){
    full_temp <- dplyr::bind_cols(full_temp, overall_col)
  }

  #full_temp <- suppressMessages(sticky::sticky_all(full_temp))

  # rename columns
  for(i in 1:5){
    if(!is.na(get(paste0("colname", i)))){
      names(full_temp)[i] <- get(paste0("colname", i))
    }
  }

  # write table to excel if path provided
  if(!is.na(excel)){
    write_excel(full_temp, wb, excel, units, title, reference, footnote, title_ref,chart_type)
    attr(full_temp, "title") <- title
    attr(full_temp, "reference") <- reference
    attr(full_temp, "footnote") <- footnote
  }

  return(full_temp)

}


#' Tabsubrow
#' @name tabsubrow
#' @description This function takes in a SHED dataset and two lists of variables. It then reports the relative frequency
#' @usage #method for defualt
#' @param shed_data SHED dataset.
#' @param subrow_var identifies SHED variable used for the subrow of the table
#' @param subrow_val only displays rows where the row variable has a value in numlist. Also sorts row display in the order of numbers in the numlist. default is to show all non-missing values.
#' @param subrow_labels optional argument to provide custom labels for the subrow breaks. Default is subrow_var labels.
#' @param overall if TRUE, includes overall row within each group.
#' @param tabsub_overall if TRUE includes overall row for the entire table
#' @param row_vars  list of SHED variables used for the rows of the table
#' @param col_vars list of SHED variables used fro the columns of the table
#' @param rowval[n] for rowval [n], only display rows where the nth row has a value in numlist; default is all values.
#' @param rowvalall for all row variables, only displays rows where each SHED variable in var_list has a value in rowvalall; deafult is all values.
#' @param colval[n] for colval [n], only display column where the nth column variable has a value in numlist; default is all values.
#' @param colvalall for all column variables, only displays columns where column variables have a value in colvalall;default is all values.
#' @param breaks inserts a row break before each new row
#' @param auto_labelbreaks if TRUE, displays row variables in row break lines. if FALSE, displays the value labels.
#' @param labelcolvars if TRUE, uses the question label to label the column, if FALSE, displays the value labels.
#' @param labelrowvars if TRUE, uses the question label to label the row, if FALSE, displays the value labels.
#' @param colname specifies the name of column [n], up to 5 columns
#' @param weight weight variable used to create percent distribution, default is population weight (final_weight)
#' @param missing if TRUE, includes missing values in the denominator when calculating frequencies
#' @param breaklabels specifies a list of strings for labeling the breaks for rows, where the first object in the string corresponds to the first break
#'
#' @return the output will be a table
#' @export
#'
#' @examples
#' Replicates Table 3 from Chapter 5- Credit applicants in demographic groups by adverse credit outcome
#' table5.3<-tabsubrow(shed_2019, row_vars=inc_3cat, subrow_var = race_4cat, col_vars=c(A1_a,denied_offered_less), missing=FALSE,rowvalall=c(1,2,3),
#' subrow_val=c(1,2,3), colvalall=1, subtotals=TRUE)
#'
tabsubrow <- function(shed_data, subrow_var, subrow_val, subrow_labels = c(), tabsub_overall = F, overall = F, weight = final_weight,
                      wb = NA, excel = NA, units= "Percent", title = "Table", reference = "sheet", footnote = "", title_ref = "",
                      chart_type = NA, ...){

  subrow_vars_list <- rlang::quo_text(rlang::enquo(subrow_var)) %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()

  weight_var <- rlang::quo_name(rlang::enquo(weight))

  subrow_val_recode <- subrow_val + length(intersect(recode_list, levels(shed_data[[subrow_vars_list[1]]])))

  #create a dataframe that maps labels to values
  df <- with(shed_data, data.frame(val=as.numeric(unique(shed_data[[subrow_vars_list[1]]])), level = unique(shed_data[[subrow_vars_list[1]]]))) %>%
    dplyr::mutate(level=as.character(level)) %>%
    dplyr::filter(val %in% subrow_val_recode)


  df <- df[match(subrow_val_recode, df$val),]


  if(!is.null(subrow_labels) & length(subrow_labels) != nrow(df)){
    stop("subrow_labels must be the same length as subrow_val")
  }

  if(!is.null(subrow_labels)){
    df[,2] <- subrow_labels
  }


  # index <- length(intersect(recode_list, levels(shed_data[[subrow_vars_list[1]]]))) %>%
  #   as.numeric()
  # df <- df %>%
  #   dplyr::filter(val > index)

  shed_data[['subrow_var']] <- shed_data[[subrow_vars_list[1]]]

  tabsub_output <- NULL
  for(val in 1:nrow(df)){
    temp <- tabrow(shed_data = shed_data %>%
                     dplyr::filter(subrow_var==df[val,2]), overall=overall,...)

    df_break <- data.frame("Characteristic" = df[val,2])

    temp <- suppressWarnings(dplyr::bind_rows(df_break, temp))
    tabsub_output <- suppressWarnings(dplyr::bind_rows(tabsub_output, temp))

  }



  if(tabsub_overall == T){
    overall_final <-tabrow(shed_data=shed_data, overall=tabsub_overall,...) %>%
      dplyr::filter(Characteristic == "Overall")
    tabsub_output <- dplyr::bind_rows(tabsub_output, overall_final)
  }

  attr(tabsub_output, "tabsubrow") <- T
  tabsub_output <- suppressMessages(sticky::sticky_all(tabsub_output))

  # write table to excel if path provided
  if(!is.na(excel)){
    write_excel(tabsub_output, wb, excel, units, title, reference, footnote, title_ref,chart_type)
    attr(tabsub_output, "title") <- title
    attr(tabsub_output, "reference") <- reference
    attr(tabsub_output, "footnote") <- footnote
  }

  attr(tabsub_output, "subrows") <- levels(shed_data[[subrow_vars_list]])[subrow_val_recode]

  return(tabsub_output)
}

#' freqtable
#' @name freqtable
#' @description This function takes in a SHED dataset and two lists of variables. It then reports the relative frequency
#' @usage #method for defualt
#' @param shed_data SHED dataset.
#' @param freq_var the variable to create a tab of across other variables
#' @param freq_val the value of the frequency variable
#' @param row_var the row variable
#' @param col_var the column variable
#' @param overall if TRUE, includes overall row within each group.
#' @param colname specifies the name of column [n], up to 5 columns
#' @param weight weight variable used to create percent distribution, default is population weight (final_weight)
#' @param missing if TRUE, includes missing values in the denominator when calculating frequencies
#'
#' @return the output will be a table
#' @export
#'
#' @examples
#' fig28 <- freqtable(shed_2019,
#'                  col_var = degree_enrollment,
#'                   col_val = c(1,2,3),
#'                   row_var = ppethm,
#'                   rowvalall = c(1,2,4),
#'                   freq_var = benefits,
#'                   freq_val = c(1))
#'
freqtable <- function(shed_data, freq_var, freq_val, col_var, col_val, overall = F,
                      colname1 = "Characteristic", colname2 = NA, colname3 = NA, colname4 = NA, colname5 = NA, weight = weight_final,
                      wb = NA, excel = NA, units= "Percent", title = "Table", reference = "sheet", footnote = "", title_ref = "",
                      chart_type = NA, ...){

  freq_vars_list <- rlang::quo_text(rlang::enquo(freq_var)) %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()

  col_var_list <- rlang::quo_text(rlang::enquo(col_var)) %>%
    strsplit(",") %>%
    lapply(function(x) gsub("c\\(|\\)| |\n", "", x)) %>%
    unlist()

  weight_var <- rlang::quo_name(rlang::enquo(weight))

  suppressWarnings(if(is.na(freq_val) == F){
    freq_val_recode <- freq_val + length(intersect(recode_list, levels(shed_data[[ freq_vars_list[1] ]])))
  } else {
    freq_val_recode <- c(1:length(levels(shed_data[[ freq_vars_list[1] ]])))
  })

  suppressWarnings(if(is.na(col_val) == F){
    colval_recode <- col_val + length(intersect(recode_list, levels(shed_data[[ col_var_list[1] ]])))
  } else {
    colval_recode <- c(1:length(levels(shed_data[[ col_var_list[1] ]])))
  })


  #create a dataframe that maps labels to values
  df_col <- with(shed_data, data.frame(val=as.numeric(unique(shed_data[[col_var_list[1]]])), level = unique(shed_data[[col_var_list[1]]]))) %>%
    dplyr::mutate(level=as.character(level)) %>%
    dplyr::filter(val %in% colval_recode)

  df_col <- df_col[match(colval_recode, df_col$val),] %>%
    dplyr::filter(!is.na(val))

  #freqvar
  df_freq <- with(shed_data, data.frame(val=as.numeric(unique(shed_data[[freq_vars_list[1]]])), level = unique(shed_data[[freq_vars_list[1]]]))) %>%
    dplyr::mutate(level=as.character(level)) %>%
    dplyr::filter((val) %in% freq_val_recode)

  df_freq <- df_freq[match(freq_val_recode, df_freq$val),]


  # if(!is.null(subrow_labels) & length(subrow_labels) != nrow(df_col)){
  #   stop("subrow_labels must be the same length as col_val")
  # }
  #
  # if(!is.null(subrow_labels)){
  #   df_col[,2] <- subrow_labels
  # }

  shed_data[['col_var']] <- sjlabelled::as_numeric(shed_data[[col_var_list[1]]])
  # shed_data[['col_var']] <- sjlabelled::as_numeric(shed_data[[col_vars_list[1]]])

  freq_output <- NULL
  for(val in 1:nrow(df_col)){
    temp <- tabrow(shed_data = shed_data %>%
                      dplyr::filter(col_var==df_col[val,1]), overall=overall, col_vars = {{freq_var}}, colvalall = {{freq_val}},...) #%>%
      #dplyr::select(Characteristic, df_freq[1,2])

    names(temp) <- c("Characteristic", df_col[val,2])

   if(val == 1){
      freq_output <- temp
   }
    else{
   freq_output <- dplyr::left_join(freq_output, temp, by = 'Characteristic')
    }
  }

  # rename columns
  for(i in 1:5){
    if(!is.na(get(paste0("colname", i)))){
      names(freq_output)[i] <- get(paste0("colname", i))
    }
  }

  # write table to excel if path provided
  if(!is.na(excel)){
    write_excel(freq_output, wb, excel, units, title, reference, footnote, title_ref,chart_type)
    attr(freq_output, "title") <- title
    attr(freq_output, "reference") <- reference
    attr(freq_output, "footnote") <- footnote
  }

  return(freq_output)
}



# pivot_subrows --------------------------------------
#
#' Pivot tables with subrows
#' @name pivot_subrows
#' @description pivot_subrows() pivots a long table with subrows to a wide table
#' @usage #default option
#' pivot_table <- function(df, pivot_var = "subrow", colname1 = "")
#' @param df Long data frame with subrows
#' @param pivot_var if pivot_var = "subrow" then subrows become the columns, if "break" then the breaks become the columns, if "row", then rows become the columns.
#' @param colname renames the name of column [n]
#'
#'
#' @return the output will be a wider dataframe that uses either the subrows or rows as the columns
#'
#' @examples
#' data("historicalpanel")
#' fig1.2 <- tabsubrow(panel, row_vars = educ_4cat, col_var = atleast_okay, subrow_var = year, rowval1 = c(1:4), colval1 = 1, subrow_val = 1:5)
#' pivot_1.2 <- pivot_subrows(fig1.2, "subrow", colname1 = "Education")
#' @export
#' @importFrom dplyr "%>%"
pivot_subrows <- function(df, pivot_var, colname1 = NA, colname2 = NA, colname3 = NA, colname4 = NA, colname5 = NA){
  if(is.na(colname1)){
    colname1 = names(df)[1]
  }
  names(df)[1:2] <- c("Characteristic", "Percent")

  # extract the subrows or breakrows
  if(stringr::str_detect(pivot_var, "break") | is.null(attr(df, "subrows"))){
    subrow_list <- df$Characteristic[attr(df, "breaks")]
  } else{
    subrow_list <- attr(df, "subrows")
  }

  subrows <- df %>%
      dplyr::filter(Characteristic %in% subrow_list)

  test_rows <- which(df$Characteristic %in% subrow_list)

  # pivot table
  pivot_names <- c()
  for(i in 2:length(test_rows)){
    pivot_names <- c(pivot_names, rep(subrows[i-1,1], test_rows[i] - test_rows[i-1]))
  }
  pivot_names <- c(pivot_names, rep(subrows[length(test_rows),1], test_rows[length(test_rows)] - test_rows[length(test_rows)-1]))

  df$cross <- pivot_names

  if(pivot_var %in% c("subrows", "sub", "subrow") | stringr::str_detect(pivot_var, "break")){
    df <- df %>%
      dplyr::filter(!(Characteristic %in% subrow_list)) %>%
      tidyr::pivot_wider(values_from = Percent, names_from = cross)
  } else if(pivot_var %in% c("row", "rows")){
    df <- df %>%
      dplyr::filter(!(Characteristic %in% subrow_list)) %>%
      tidyr::pivot_wider(values_from = Percent, names_from = Characteristic)
  }

  # rename columns
  for(i in 1:5){
    if(!is.na(get(paste0("colname", i)))){
      names(df)[i] <- get(paste0("colname", i))
    }
  }

  return(df)

}
