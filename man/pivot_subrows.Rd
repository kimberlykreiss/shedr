% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{pivot_subrows}
\alias{pivot_subrows}
\title{Pivot tables with subrows}
\usage{
#default option
pivot_table <- function(df, pivot_var = "subrow", colname1 = "")
}
\arguments{
\item{df}{Long data frame with subrows}

\item{pivot_var}{if pivot_var = "subrow" then subrows become the columns, if "break" then the breaks become the columns, if "row", then rows become the columns.}

\item{colname}{renames the name of column [n]}
}
\value{
the output will be a wider dataframe that uses either the subrows or rows as the columns
}
\description{
pivot_subrows() pivots a long table with subrows to a wide table
}
\examples{
data("historicalpanel")
fig1.2 <- tabsubrow(panel, row_vars = educ_4cat, col_var = atleast_okay, subrow_var = year, rowval1 = c(1:4), colval1 = 1, subrow_val = 1:5)
pivot_1.2 <- pivot_subrows(fig1.2, "subrow", colname1 = "Education")
}
