##########################################################
#
# read_data.R
#
# It reads in SHED data set in .dta format.
#
# Lisa Chen (2/18/2019)
# Updated by Kayla Jones and Sara Canilang (7/28/2020)
##########################################################


#' Read internal SHED data
#' @description
#' This function reads in Federal Reserve SHED data for each available year. The function also serves to read in historical and public use SHED datasets
#' This dataset is intended only for the internal use of the Consumer and Community Research team.
#' To load in the historical dataset, use year = "historicalpanel". To load in the COVID supplement data,
#' use year = "april2020" or year = "july2020"
#'
#' @param Typically the input for this function is the year (ranging from 2013-2019). However for historical and shed supplement datasets, the input will be the historical_panel, shedapril2020 or shedjuly2020.
#'
#' @return The output will be a tibble.
#' @export
#'
#' @examples
#' # Create a dataframe for SHED 2017.
#' df <- read_sheddata(2017)
#' # Create a dataframe for historical SHED dataset
#' df <- read_sheddata(historical_panel)
#'@importFrom dplyr '%>%'




read_sheddata <- function(year){

#Location of final SHED R files
SHED_path <- get_shed_dir()
temp <- rlang::quo_name(rlang::enquo(year))
if (temp == 'historicalpanel') {
  year <- temp
} else {
  year <- as.character(year)
}

#check if you have access to this file path
if(identical(dir(path = SHED_path), character(0))) {
  stop("You do not have access to the DCCA/CCR file directory. Please use the public use datasets.")
}

#Check: does this R.dta file exist?
if(!(paste0("shed", year, ".dta") %in% list.files(paste0(SHED_path)))){
  stop("There is no shed .dta file in this folder. Check that you entered the correct year.")
}



# reading in SHED data, without the text fields
data_input <- suppressMessages(sjlabelled::read_stata(paste0(SHED_path, "/shed", year, ".dta"),
                         atomic.to.fac = TRUE)) %>%
              dplyr::as_tibble()

# Read in all the variable labels
labels <- sjlabelled::get_labels(data_input)
levels <- sjlabelled::get_values(data_input)

# Varibales with missing value labels
relab_list = c()
for ( i in names(data_input)){
  suppressWarnings(if(!is.null(attr(data_input[[i]], "labels")) & is.null(attr(data_input[[i]], "levels")) & !grepl("pp", i)  & !grepl("PP", i)  ){
    if(!grepl("scale|age|time|year|words|how much|Number of Loans", attr(data_input[[i]], "label")) & length(unique(data_input[[i]])) < 17){
      relab_list = c(relab_list, i)
    }
  })
}

if("FL0" %in% names(data_input) & year != 2021){
  relab_list = c(relab_list,"FL0")
}


# & !grepl("scale", attr(data_input[[i]], "label"))
for(var in c(names(data_input)[sapply(data_input, class) == "factor"], relab_list)) {
  # Character data that is already a factor will fail here
  # We can just silently ignore those errors

  label <- sjlabelled::get_label(data_input[[var]])

  #removes the appended questions to answer choices and replaces \x92 with apostraphes
  if (is.null(label) == F & var!= "B7_a" & var != "B7_b"){
    label <- gsub("\x92","'", label)
    if (year>=2019) {
     if(grepl("L0_", var) & year >= 2020){
       label <- stringr::str_split(label, " - ", simplify = TRUE)[,2]
      } else{
     label <- stringr::str_split(label, " - ", simplify = TRUE)[,1]
      }}
    else {
     label <- sub("\\["," ", label)
      label <- stringi::stri_split_fixed(label, "] ", n = 2)[[1]][1]}
  }


  # replaces \x92 with apostraphes
  var_labels <- labels[[var]]

  var_labels <- list(labs = sapply(var_labels, function(v) gsub("\x92","'", v)))

  var_labels <- paste0(var_labels$labs)

  labels[[var]] <- var_labels

  if(var %in% relab_list){

    suppressWarnings(if(setdiff(unique(data_input[[var]])[!is.na(unique(data_input[[var]]))], attr(data_input[[var]], "labels")) == -1){
      err <- try(
        data_input[[var]] <- factor(data_input[[var]],
                                    levels = c("-1", attr(data_input[[var]], "labels") %>% as.character()),
                                    labels = c("Refused", attr(attr(data_input[[var]], "labels"), "names"))),
        silent = TRUE
      )
    } else if(setdiff(unique(data_input[[var]])[!is.na(unique(data_input[[var]]))], attr(data_input[[var]], "labels")) == -9){
      err <- try(
        data_input[[var]] <- factor(data_input[[var]],
                                    levels = c("-9", attr(data_input[[var]], "labels") %>% as.character()),
                                    labels = c("Not In Universe (not asked)", attr(attr(data_input[[var]], "labels"), "names"))),
        silent = TRUE
      )
    } else if((c(-9, -1) %in% setdiff(unique(data_input[[var]]), attr(data_input[[var]], "labels"))) == c(TRUE, TRUE)){
      err <- try(
        data_input[[var]] <- factor(data_input[[var]],
                                    levels = c("-9", "-1", attr(data_input[[var]], "labels") %>% as.character()),
                                    labels = c("Not In Universe (not asked)", "Refused", attr(attr(data_input[[var]], "labels"), "names"))),
        silent = TRUE
      )
    } else if(setdiff(unique(data_input[[var]])[!is.na(unique(data_input[[var]]))], attr(data_input[[var]], "labels")) == 0){
      err <- try(
        data_input[[var]] <- factor(data_input[[var]],
                                    levels = c("-1", "0", attr(data_input[[var]], "labels")[2:length(attr(data_input[[var]], "labels"))] %>% as.character()),
                                    labels = c( "Refused", "None", attr(attr(data_input[[var]], "labels")[2:length(attr(data_input[[var]], "labels"))], "names"))),
        silent = TRUE
      )
    } else if(setdiff(unique(data_input[[var]])[!is.na(unique(data_input[[var]]))], attr(data_input[[var]], "labels")) == 3){
      err <- try(
        data_input[[var]] <- factor(data_input[[var]],
                                    levels = c(attr(data_input[[var]], "labels")%>% as.character(), "3"),
                                    labels = c(attr(attr(data_input[[var]], "labels"), "names"), "Don't know")),
        silent = TRUE
      )
    } else if((c(-2, -1) %in% setdiff(unique(data_input[[var]]), attr(data_input[[var]], "labels"))) == c(TRUE, TRUE)){
      err <- try(
        data_input[[var]] <- factor(data_input[[var]],
                                    levels = c("-2", "-1", attr(data_input[[var]], "labels") %>% as.character()),
                                    labels = c("Don't know", "Refused", attr(attr(data_input[[var]], "labels"), "names"))),
        silent = TRUE
      )
    } else if(var == "FL0"){
    err <- try(data_input[[var]] <- factor(data_input[[var]],
                                levels = c("-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                labels = c("Refused", "Not at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very willing")),
                                silent = TRUE)
  } else{
      err <- try(
        data_input[[var]] <- factor(data_input[[var]],
                                    levels = c(attr(data_input[[var]], "labels") %>% as.character()),
                                    labels = c(attr(attr(data_input[[var]], "labels"), "names"))),
        silent = TRUE
      )
    })

  } else{
    err <- try(
      data_input[[var]] <- factor(data_input[[var]],
                                  levels = levels[[var]],
                                  labels = labels[[var]]),
      silent = TRUE
    )

  }


  if(!is.null(label)){
    attributes(data_input[[var]])$label <- label
  } else {
    attributes(data_input[[var]])$label <- var
  }

}

# Make attributes sticky
data_input <- sticky::sticky_all(data_input)

#Assigning weight variable
#SHED 2013 and SHED 2014 do not have population weights
if (year==2019 | year == 2020 | year == 2021) {
  data_input$final_weight <- data_input$weight_pop
}

if (year==2018) {
  data_input$final_weight <- data_input$weight2b
}

if (year==2017 | year==2016 | year==2015) {
  data_input$final_weight <- data_input$weight3b
}

if (year==2014) {
  data_input$final_weight <- data_input$weight3
}
if (year==2013) {
  data_input$final_weight <- data_input$weight
}
if(year == "historicalpanel"){
  data_input$final_weight <- data_input$weight
}

# Reading in SHED text fields
if((paste0("shed", year, "_text.csv") %in% list.files(paste0(SHED_path)))){
  shed_text <- suppressWarnings(utils::read.csv(paste0(SHED_path, "/shed", year, "_text.csv"), stringsAsFactors = F))
  shed_text <- suppressWarnings(dplyr::mutate(shed_text,CaseID = as.numeric(CaseID)))
  data_input <- data_input %>% dplyr::mutate(CaseID = as.numeric(CaseID))
  data_input <- dplyr::left_join(data_input, shed_text, by = "CaseID")
}

# Add year attribute to CaseID so we can find when we are working with 2017, which is important because of the hardcoded missings
attr(data_input$CaseID, "year") <- year

# Relabel nongrid questions that are cut off
data_input <- geofacet::nongrid_labels(data_input)


return(data_input)
}

# This function is specifically for SHED 2017
# Changes hard coded missings -9 to NAs
missing_hardcode <- function(shed_data){

  for(var in names(shed_data)){
    if("Not In Universe (not asked)" %in% levels(shed_data[[var]])){
      levels(shed_data[[var]])[levels(shed_data[[var]]) == "Not In Universe (not asked)"] <- NA

    }
  }

  return(shed_data)
}


# Function to allow users to work on Windows
get_shed_dir <- function() {
  # Determine if the session is being run on the PC
  # or Linux
  my_info <- Sys.info()
  if (my_info['sysname'] == 'Windows') {
    prefix <- ''
  } else if (my_info['sysname'] == 'Linux') {
    prefix <- ''
  } else stop('Unable to determine if session is being run on PC or ADAP.')

  out_dir <- file.path(prefix, 'shed_finaldata/shedr')

  if (!dir.exists(out_dir))
    stop('Could not locate SHED directory.')

  out_dir
}

