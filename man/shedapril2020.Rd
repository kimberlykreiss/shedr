% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{shedapril2020}
\alias{shedapril2020}
\title{Survey of Household Economics and Decisionmaking (SHED) April 2020 Supplement Data}
\format{A tibble with 1030 observations and 58 variables}
\source{
<https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
}
\usage{
data(shed_april2020)
}
\description{
Data from the April 2020 Federal Reserve's Survey of Household Economic Decision Making (SHED) Supplement.
This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances
through the COVID-19 crisis.
}
\details{
Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/SHED-2019-codebook-supplemental-survey-april-2020.pdf>.

The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2013,
final_weight is equal to weight, which is scaled to match the number of respondents in the survey (sample weight). Note that population weights
are not provided in surveys before 2016.
}
\examples{
 data(shedapril2020)
 # load package for analyzing survey data and
 # calculate weighted tabulation of financial wellbeing
 library(questionr)
 wtd.table(x = shedapril2020$B2, weights = shedapril2020$final_weight)

}
\keyword{datasets}
