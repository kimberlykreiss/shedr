% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{shed2019}
\alias{shed2019}
\title{Survey of Household Economics and Decisionmaking (SHED) 2019 Data}
\format{A tibble with 12173 observations and 324 variables}
\source{
<https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
}
\usage{
data(shed2019)
}
\description{
Data from the 2019 Federal Reserve's Survey of Household Economic Decision Making (SHED).
This dataset measures the economic well-being of U.S. households and identifies potential risks to their finances.
The survey includes modules on a range of topics relevant to financial well-being including credit access and behaviors
savings, retirement, economic fragility, and education and student loans. In addition, the survey includes demographic
information about the respondent such as income, race/ethnicity, gender, education and more.
}
\details{
Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2019codebook.pdf>.

The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For 2019,
final_weight is equal to weight_pop, which is scaled to match the number of adults in the United States (population weight). Note that
population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
}
\examples{
 data(shed2019)
 # load package for analyzing survey data and
 # calculate weighted tabulation of financial wellbeing
 library(questionr)
 wtd.table(x = shed2019$B2, weights = shed2019$final_weight)

}
\keyword{datasets}
