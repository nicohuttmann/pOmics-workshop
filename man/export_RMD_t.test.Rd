% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_tables.R
\name{export_RMD_t.test}
\alias{export_RMD_t.test}
\title{Template for functions that accept either a data frame or a list}
\usage{
export_RMD_t.test(
  data_,
  order.by = "p.adjust",
  descending = F,
  buttons,
  dom = "lBfrtip",
  add.annotations = F,
  dataset,
  view = F,
  input = "data_t.test",
  output = "data_DT_t.test"
)
}
\arguments{
\item{data_}{list or tibble}

\item{order.by}{order rows by given column}

\item{descending}{arrange in descending order}

\item{buttons}{(optional) add buttons ("copy", "csv", "excel", "pdf",
"print")}

\item{dom}{order of table elements (default: "lBfrtip", see
https://rstudio.github.io/DT/)}

\item{add.annotations}{add annotations to proteins}

\item{dataset}{dataset}

\item{view}{view table}

\item{input}{name of input data}

\item{output}{name of output data}
}
\description{
Template for functions that accept either a data frame or a list
}
