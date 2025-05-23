% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-ingestion_ext.R
\name{pa_transformations_get}
\alias{pa_transformations_get}
\title{get all pipeline definitions}
\usage{
pa_transformations_get()
}
\value{
A \code{data.table} containing the merged pipeline definitions, with one row per
combination of (\code{OHDEST}, \code{POSIT}).
}
\description{
Reads two CSV files (\emph{B4_PIPELINE_ORG.csv} and \emph{B4_PIPELINE_MOD.csv}) from a predefined
directory structure, merges them, and sorts the resulting data.
}
\details{
The function:
\enumerate{
  \item Reads original pipeline definitions from \emph{B4_PIPELINE_ORG.csv}.
  \item Reads modified pipeline definitions from \emph{B4_PIPELINE_MOD.csv}.
  \item Binds them into a single \code{data.table}.
  \item Adds columns like \code{SRC} and \code{WHERE_CLAUSE} as needed.
  \item Sorts the final table by \code{SRC}, \code{OHDEST}, and \code{POSIT}.
  \item Returns the first row per (\code{OHDEST}, \code{POSIT}) group.
}
}
\examples{
\dontrun{
  # Fetch pipeline data
  dt_pipe <- pa_transformations_get()
  head(dt_pipe)
}

}
