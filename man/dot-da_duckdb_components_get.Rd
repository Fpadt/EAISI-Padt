% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-access_int.R
\name{.da_duckdb_components_get}
\alias{.da_duckdb_components_get}
\title{Retrieve DuckDB Config Parts}
\usage{
.da_duckdb_components_get(
  .vtype = NULL,
  .ftype = NULL,
  .material = NULL,
  .salesorg = NULL,
  .scope_matl = NULL,
  .scope_sorg = FALSE,
  .cm_min = NULL,
  .cm_max = NULL,
  .step_min = NULL,
  .step_max = NULL,
  .lagg_min = NULL,
  .lagg_max = NULL
)
}
\arguments{
\item{.material}{A character vector of materials to filter on. If \code{NULL},
no material-based filter is applied (beyond scope constraints).}

\item{.salesorg}{A character vector of sales organizations to filter on. If \code{NULL},
no salesorg-based filter is applied (beyond scope constraints).}

\item{.scope_matl}{Logical. If \code{TRUE}, the returned \code{cte_scope_materials}
(and any relevant WHERE clauses) will restrict to materials in the scope definition.}

\item{.scope_sorg}{Logical. If \code{TRUE}, where clauses will restrict to
the provided sales organization scope (if \code{.salesorg} is not \code{NULL}).}

\item{.cm_min}{Character. Minimum YYYYMM to apply in date-based filtering. Defaults to
\code{NULL}.}

\item{.cm_max}{Character. Maximum YYYYMM to apply in date-based filtering. Defaults to
\code{NULL}.}

\item{.step_min}{Numeric. Minimum step filter. Defaults to \code{NULL}.}

\item{.step_max}{Numeric. Maximum step filter. Defaults to \code{NULL}.}

\item{.lagg_min}{Numeric. Minimum lag filter. Defaults to \code{NULL}.}

\item{.lagg_max}{Numeric. Maximum lag filter. Defaults to \code{NULL}.}
}
\value{
A named \code{list} with three elements:
  \itemize{
    \item \code{duckdb_con} - The active DuckDB connection.
    \item \code{cte_scope_materials} - SQL snippet for scope materials (may be an
           empty string if \code{.scope_matl=FALSE}).
    \item \code{where_clause} - A character vector of WHERE clauses.
  }
}
\description{
This internal helper function returns a list of three elements:
\enumerate{
  \item \code{duckdb_con} - The DuckDB connection.
  \item \code{cte_scope_materials} - The CTE (common table expression) snippet
        for scope materials, constructed if needed.
  \item \code{where_clause} - A character vector of one or more WHERE clauses,
        built according to the provided parameters.
}
}
\details{
This function centralizes the logic of creating a DuckDB connection,
constructing the \code{cte_scope_materials} snippet, and building the \code{where_clause}.
Other functions can call this to avoid repeating code.
}
\keyword{internal}
