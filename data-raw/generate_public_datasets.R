###################################################
#
#  This file loads in the public file shed datasets
#  for each year and does basic cleaning on them to
#  maintain the stata labels for inside the R environment
#  Note that the function reads in public datasets from
#  the public_sheddata folder in the SHED_Surveys file.
#  These will have to be updated every year with each year's
#  new final dataset.
#
#  Note that the gitlab repo will have to be
#  specified based on who is writing the script
###################################################

library(sjlabelled)
library(tidyverse)

# Update this for where the .RData sets should be dropped (this should be the updater's data folder on the git repo)
data_folder <-"/shedr/data/"

source("./R/labels.R")
shed_process <- function(shed_data){
data_input <- sjlabelled::read_stata(paste0("/shed_publicdata/", shed_data),
                                     atomic.to.fac = TRUE) %>%
  dplyr::as_tibble()

# Read in all the variable labels
labels <- sjlabelled::get_labels(data_input)
levels <- sjlabelled::get_values(data_input)
for (var in names(data_input)[sapply(data_input, class) == "factor"]) {
  # Character data that is already a factor will fail here
  # We can just silently ignore those errors
  label <- sjlabelled::get_label(data_input[[var]])

  # removes the appended questions to answer choices and replaces \x92 with apostraphes
  if (is.null(label) == F & var!= "B7_a" & var != "B7_b"){
    label <- gsub("\x92","'", label)
    if (shed_data=="public2019.dta") {
      label <- stringr::str_split(label, " - ", simplify = TRUE)[,1]}
    else {
      label <- sub("\\["," ", label)
      label <- stringi::stri_split_fixed(label, "] ", n = 2)[[1]][1]}
  }


  # replaces \x92 with apostraphes
  var_labels <- labels[[var]]

  var_labels <- list(labs = sapply(var_labels, function(v) gsub("\x92","'", v)))

  var_labels <- paste0(var_labels$labs)

  labels[[var]] <- var_labels

  err <- try(
    data_input[[var]] <- factor(data_input[[var]],
                                levels = levels[[var]],
                                labels = labels[[var]]),
    silent = TRUE
  )
  attributes(data_input[[var]])$label <- label

}

# Make FL0 into a factor
if("FL0" %in% names(data_input)){
  err <- try(data_input$FL0 <- factor(data_input$FL0,
                                         levels = c("-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                         labels = c("Refused", "Not at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very willing")),
             silent = TRUE)
}

# Add year attribute to CaseID so we can find when we are working with 2017, which is important because of the hardcoded missings
attributes(data_input[[names(data_input)[1]]])$"year"  <- dplyr::if_else(shed_data != "historicalpanel", paste0(substr(shed_data, 7, nchar(shed_data)- 4)), "historical")

# Make attributes sticky
data_input <- sticky::sticky_all(data_input)

# Relabel nongrid questions that are cut off
data_input <- nongrid_labels(data_input)

return(data_input)
}


shed2013 <- shed_process("public2013.dta") %>%
  mutate(final_weight = weight)
save(shed2013, file = paste(data_folder, "shed2013.RData", sep=""))

shed2014 <- shed_process("public2014.dta") %>%
  mutate(final_weight = weight3)
save(shed2014, file = paste(data_folder, "shed2014.RData", sep=""))

shed2015 <- shed_process("public2015.dta") %>%
  mutate(final_weight = weight3)
save(shed2015, file = paste(data_folder, "shed2015.RData", sep=""))

shed2016 <- shed_process("public2016.dta") %>%
  mutate(final_weight = weight3b)
save(shed2016, file = paste(data_folder, "shed2016.RData", sep=""))

shed2017 <- shed_process("public2017.dta") %>%
  mutate(final_weight = weight3b)
save(shed2017, file = paste(data_folder, "shed2017.RData", sep=""))

shed2018 <- shed_process("public2018.dta") %>%
  mutate(final_weight = weight2b)
save(shed2018, file = paste(data_folder, "shed2018.RData", sep=""))

shed2019 <- shed_process("public2019.dta") %>%
  dplyr::mutate(final_weight = weight_pop)
save(shed2019, file = paste(data_folder, "shed2019.RData", sep=""))

shedapril2020 <- shed_process("publicApril2020.dta") %>%
  mutate(final_weight=weight)
save(shedapril2020, file = paste(data_folder, "shedapril2020.RData", sep=""))

shedjuly2020 <- shed_process("publicJuly2020.dta") %>%
  mutate(final_weight=weight)
save(shedjuly2020, file = paste(data_folder, "shedjuly2020.RData", sep=""))

shed2020 <- shed_process("public2020.dta") %>%
  dplyr::mutate(final_weight = weight_pop)
save(shed2020, file = paste(data_folder, "shed2020.RData", sep=""))

historicalpanel <- shed_process("historicalpanel.dta") %>%
  mutate(final_weight = weight)
save(historicalpanel, file = paste(data_folder, "historicalpanel.RData", sep=""))


