% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{historicalpanel}
\alias{historicalpanel}
\title{Survey of Household Economics and Decisionmaking Historical Panel Data}
\format{A tibble with 58218 observations and 20 variables}
\source{
<https://www.federalreserve.gov/consumerscommunities/shed_data.htm>
}
\usage{
data(historicalpanel)
}
\description{
This dataset is a custom dataset that includes several standardized variables present in each SHED dataset for every available year since 2013.
Its purpose is to provide an easy way to tabulate common variables each year.
}
\details{
Description of each variable can be found in the codebook online, at <https://www.federalreserve.gov/consumerscommunities/files/shed_2019codebook.pdf>.
Note that in 2013, the income categories are different from following years.

The variable final_weight corresponds to the final weight variable used to match characteristics of the U.S. adult population. For this historical panel dataset,
final_weight is equal to weight, which is scaled to match the number of adults in the United States (population weight). Note that
population weights are available and preferred for the survey starting in 2016, though this will not make a difference for estimates of shares across survey years.
}
\examples{
 data(historical_panel)
 # load in dataset
 # displays a list of all variables included in the dataset
 ls(historicalpanel)

}
\keyword{datasets}
