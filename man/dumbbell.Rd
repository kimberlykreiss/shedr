% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bar_charts.R
\name{dumbbell}
\alias{dumbbell}
\title{Creates a dumbbell plot}
\usage{
dumbbell(df, plot_title = "Title", plot_caption = "", row_or_col)
}
\arguments{
\item{df}{Two-way percent distribution table}

\item{plot_title}{Plot title. Requires a string.}

\item{plot_caption}{Plot footnote. Requries a string.}

\item{row_col_subrow}{if "row", uses the row as the group; if "col" or "subrow" uses the column as the group (should typically use row if df from tabrow)}

\item{bold_rows}{Adds bold labelling to each row number in list.}
}
\value{
Dumbell bar chart
}
\description{
Create a dumbbell plot from a two-way percent distribution table for up to three groupings of variables.
}
\examples{
# Load in the SHED 2019 data
shed_2019 <- read_sheddata(2019)
# Figure 25 in SHED 2019 Report
fig6_5_data <- tabcol(shed_2019, row_vars = c(GH3_e, GH3_a, GH3_b, GH3_c, GH3_d), col_vars = GH1_3cat, rowvalall = 1, colvalall = c(1,2), labelrowvars = T, labelcolvars = F, overall = F)
fig6_5_chart <- dumbbell(fig6_5_data, plot_title = "Figure 25. Satisfied with local neighborhood and housing (by housing tenure)", plot_caption = "Among adults who owned or rented their home.", refused = F, row_or_col = 'row')
}
