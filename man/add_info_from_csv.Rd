% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_info_from_csv.R
\name{add_info_from_csv}
\alias{add_info_from_csv}
\title{Adds all info from imported csv file to observations data}
\usage{
add_info_from_csv(
  x,
  observations.set,
  dataset,
  id.column = "id",
  data.columns,
  order.by = "name",
  character2factor = T
)
}
\arguments{
\item{x}{imported info file}

\item{observations.set}{observations.set}

\item{dataset}{dataset}

\item{id.column}{column containing id of observations}

\item{data.columns}{(optional) specific data columns to add}

\item{order.by}{column to order observations based on}

\item{character2factor}{transform character data to factors}
}
\description{
Adds all info from imported csv file to observations data
}
