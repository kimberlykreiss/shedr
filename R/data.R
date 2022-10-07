#' Survey of Household Economics and Decisionmaking (SHED) 2013 Data
#'
#' Data from the 2013 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#' * Note that for 2013 data only, if you are replicating numbers in the 2013 report some may be slightly off.
#' This is due to adding income stratification and re-weighting the 2013 weight variable.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2013codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2013,
#'final_weight is equal to weight, which is scaled to match the number of respondents in the survey (sample weight). Note that population weights
#'are not provided in surveys before 2016.
#' @name shed2013
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2013)
#'
#' @format A tibble with 4134 observations and 321 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2013)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2013$B2, weights = shed2013$final_weight)
#'
"shed2013"


#' Survey of Household Economics and Decisionmaking (SHED) 2014 Data
#'
#' Data from the 2014 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2014codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2014,
#'final_weight is equal to weight3, which is scaled to match the number of respondents in the survey (sample weight). Note that population weights
#'are not provided in surveys before 2016.
#' @name shed2014
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2014)
#'
#' @format A tibble with 5896 observations and 320 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2014)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2014$B2, weights = shed2014$final_weight)
#'
"shed2014"

#' Survey of Household Economics and Decisionmaking (SHED) 2015 Data
#'
#' Data from the 2015 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2015codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2015,
#'final_weight is equal to weight3, which is scaled to match the number of respondents in the survey (sample weight). Note that population weights
#'are not provided in surveys before 2016.
#' @name shed2015
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2015)
#'
#' @format A tibble with 5642 observations and 396 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2015)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2015$B2, weights = shed2015$final_weight)
#'
"shed2015"

#' Survey of Household Economics and Decisionmaking (SHED) 2016 Data
#'
#' Data from the 2016 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2016codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2016,
#'final_weight is equal to weight3b, which is scaled to match the number of adults in the United States (population weight). Note that
#'population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
#' @name shed2016
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2016)
#'
#' @format A tibble with 6610 observations and 467 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2016)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2016$B2, weights = shed2016$final_weight)
#'
"shed2016"

#' Survey of Household Economics and Decisionmaking (SHED) 2017 Data
#'
#' Data from the 2017 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2017codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2017,
#'final_weight is equal to weight3b, which is scaled to match the number of adults in the United States (population weight). Note that
#'population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
#' @name shed2017
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2017)
#'
#' @format A tibble with 12447 observations and 441 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2017)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2017$B2, weights = shed2017$final_weight)
#'
"shed2017"

#' Survey of Household Economics and Decisionmaking (SHED) 2018 Data
#'
#' Data from the 2018 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2018codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2018,
#'final_weight is equal to weight2b, which is scaled to match the number of adults in the United States (population weight). Note that
#'population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
#' @name shed2018
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2018)
#'
#' @format A tibble with 11316 observations and 380 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2018)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2018$B2, weights = shed2018$final_weight)
#'
"shed2018"

#' Survey of Household Economics and Decisionmaking (SHED) 2019 Data
#'
#' Data from the 2019 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2019codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2019,
#'final_weight is equal to weight_pop, which is scaled to match the number of adults in the United States (population weight). Note that
#'population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
#' @name shed2019
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2019)
#'
#' @format A tibble with 12173 observations and 324 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2019)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2019$B2, weights = shed2019$final_weight)
#'
"shed2019"

#' Survey of Household Economics and Decisionmaking (SHED) April 2020 Supplement Data
#'
#' Data from the April 2020 Federal Reserve's Survey of Household Economic Decision Making (SHED) Supplement.
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances
#' through the COVID-19 crisis.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/SHED-2019-codebook-supplemental-survey-april-2020.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2013,
#'final_weight is equal to weight, which is scaled to match the number of respondents in the survey (sample weight). Note that population weights
#'are not provided in surveys before 2016.
#' @name shedapril2020
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed_april2020)
#'
#' @format A tibble with 1030 observations and 58 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shedapril2020)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shedapril2020$B2, weights = shedapril2020$final_weight)
#'
"shedapril2020"

#' Survey of Household Economics and Decisionmaking (SHED) July 2020 Supplement Data
#'
#' Data from the July 2020 Federal Reserve's Survey of Household Economic Decision Making (SHED) Supplement.
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances
#' through the COVID-19 crisis.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/SHED-2019-codebook-supplemental-survey-july-2020.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2013,
#'final_weight is equal to weight, which is scaled to match the number of respondents in the survey (sample weight). Note that population weights
#'are not provided in surveys before 2016.
#'
#' @name shedjuly2020
#' @keywords datasets
#' @docType data
#'
#' @usage data(shedjuly2020)
#'
#' @format A tibble with 4147 observations and 74 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/files/SHED-2019-codebook-supplemental-survey-july-2020.pdf>
#' @examples
#'  data(shed_july2020)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shedjuly2020$B2, weights = shedjuly2020$final_weight)
#'
"shedjuly2020"

#' Survey of Household Economics and Decisionmaking (SHED) 2020 Data
#'
#' Data from the 2020 Federal Reserve's Survey of Household Economic Decision Making (SHED).
#' This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
#' The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
#' savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
#' information about the respondent such as income, race/ethnicity, gender, education and more.
#'
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2020codebook.pdf>.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2019,
#'final_weight is equal to weight_pop, which is scaled to match the number of adults in the United States (population weight). Note that
#'population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
#' @name shed2020
#' @keywords datasets
#' @docType data
#'
#' @usage data(shed2020)
#'
#' @format A tibble with 11,648 observations and 372 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(shed2020)
#'  # load package for analyzing survey data and
#'  # calculate weighted tabulation of financial wellbeing
#'  library(questionr)
#'  wtd.table(x = shed2020$B2, weights = shed2020$final_weight)
#'
"shed2020"

#' Survey of Household Economics and Decisionmaking Historical Panel Data
#'
#' This dataset is a custom dataset that includes several standardized variables present in each SHED dataset for every available year since 2013.
#' Its purpose is to provide an easy way to tabulate common variables each year.
#'
#' Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2019codebook.pdf>.
#' Note that in 2013, the income categories are different from following years.
#'
#'The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For this historical panel dataset,
#'final_weight is equal to weight, which is scaled to match the number of adults in the United States (population weight). Note that
#'population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
#' @name historicalpanel
#' @keywords datasets
#' @docType data
#'
#' @usage data(historicalpanel)
#'
#' @format A tibble with 58218 observations and 20 variables
#'
#' @source <https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
#' @examples
#'  data(historical_panel)
#'  # load in dataset
#'  # displays a list of all variables included in the dataset
#'  ls(historicalpanel)
#'
"historicalpanel"

