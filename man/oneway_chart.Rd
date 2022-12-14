% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bar_charts.R
\name{oneway_chart}
\alias{oneway_chart}
\title{Creates a simple bar chart}
\usage{
oneway_chart(df, plot_title = "Title", plot_caption = "")
}
\arguments{
\item{df}{One-way percent distribution table}

\item{plot_title}{Plot title. Requires a string.}

\item{plot_caption}{Plot footnote. Requries a string.}

\item{horizontal}{if True, returns a horizontal bar chart, if false returns a vertical bar chart}

\item{bold_rows}{Adds bold labelling to each row number in list.}
}
\value{
Bar chart
}
\description{
Create a simple bar charts from a one-way percent distribution table
}
\examples{
# Load in the SHED 2019 data
data("shed2019")
# Creates a one-way table using multi1way()
one_way_table <- multi1way(shed2019, c(R1_f, R1_e, R1_a, R1_c, R1_b, R1_d), weight = TRUE, missing = FALSE, labelvarnames = TRUE,
                 rowvalall = 1, breaks =c(1,3), breaklabels=c("Mortgage Access", "Preference"))
# Create bar chart from the one-way table
oneway_chart(one_way_table, plot_title = "Reasons for Renting", plot_caption = "Among renters. Respondents could select multiple answers.")
}
