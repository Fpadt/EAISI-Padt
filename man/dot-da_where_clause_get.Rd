% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-access_int.R
\name{.da_where_clause_get}
\alias{.da_where_clause_get}
\title{Build a SQL WHERE Clause with Scope Materials}
\usage{
.da_where_clause_get(
  .clauses = list(),
  .vtype = NULL,
  .ftype = NULL,
  .material = NULL,
  .salesorg = NULL,
  .scope_matl = FALSE,
  .scope_sorg = FALSE,
  .cm_min = NULL,
  .cm_max = NULL,
  .step_min = NULL,
  .step_max = NULL,
  .lagg_min = NULL,
  .lagg_max = NULL,
  .con = NULL
)
}
\arguments{
\item{.clauses}{A list of existing WHERE clauses (character strings). If empty, "TRUE" will be added as the initial condition.}

\item{.material}{Character vector. A list of materials to filter on. If \code{NULL}, no material-based filter is applied.}

\item{.salesorg}{Character vector. A list of sales organizations to filter on. If \code{NULL}, no sales organization-based filter is applied.}

\item{.scope_matl}{Logical. If \code{TRUE}, restricts the query to materials within the scope definition using a CTE.}

\item{.scope_sorg}{Logical. If \code{TRUE}, restricts the query to sales organizations within the scope definition.}

\item{.cm_min}{Character. Minimum calendar month (format: \code{YYYYMM}). If \code{NULL}, no lower bound is applied.}

\item{.cm_max}{Character. Maximum calendar month (format: \code{YYYYMM}). If \code{NULL}, no upper bound is applied.}

\item{.step_min}{Numeric. Minimum step value to filter on. If \code{NULL}, uses a default package-defined constant \code{STEP_MIN}.}

\item{.step_max}{Numeric. Maximum step value to filter on. If \code{NULL}, uses a default package-defined constant \code{STEP_MAX}.}

\item{.lagg_min}{Numeric. Minimum lag value to filter on. If \code{NULL}, uses a default package-defined constant \code{LAGG_MIN}.}

\item{.lagg_max}{Numeric. Maximum lag value to filter on. If \code{NULL}, uses a default package-defined constant \code{LAGG_MAX}.}

\item{.con}{A database connection object. Required for safely parameterizing SQL queries using \code{glue::glue_sql}.}
}
\value{
A character string representing the SQL WHERE clause.
}
\description{
Constructs a SQL WHERE clause by appending conditions based on the provided parameters. Additionally, it can include a Common Table Expression (CTE) for scope materials if required.
}
\details{
This function constructs a SQL WHERE clause by:
\enumerate{
  \item Ensuring that the initial condition is "TRUE" if no clauses exist in \code{.clauses}.
  \item Appending conditions for \code{.material}, \code{.salesorg}, and scope definitions (\code{.scope_matl}, \code{.scope_sorg}).
  \item Adding conditions for calendar month (\code{.cm_min}, \code{.cm_max}), step values (\code{.step_min}, \code{.step_max}), and lag values (\code{.lagg_min}, \code{.lagg_max}).
  \item Optionally constructing a Common Table Expression (CTE) for scope materials if \code{.scope_matl = TRUE}.
}

Scope-based filters are applied using pre-defined Common Table Expressions (CTEs).
}
\examples{
\dontrun{
# Build a basic WHERE clause
where_clause <- .da_where_clause_get(
  .material = c("MAT001", "MAT002"),
  .salesorg = c("SORG001"),
  .scope_matl = TRUE,
  .cm_min = "202101",
  .cm_max = "202312",
  .step_min = -100,
  .step_max = 100
)

print(where_clause)
}

}
\keyword{internal}
