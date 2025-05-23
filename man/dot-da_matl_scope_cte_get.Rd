% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-access_int.R
\name{.da_matl_scope_cte_get}
\alias{.da_matl_scope_cte_get}
\title{Construct a SQL CTE Clause for Scope Materials}
\usage{
.da_matl_scope_cte_get(.scope_matl, .con)
}
\arguments{
\item{.scope_matl}{Logical. Determines whether the scope materials CTE should
be included in the query. If \code{TRUE}, the function constructs the SQL snippet.}

\item{.con}{A database connection object. Required for safely constructing
the SQL snippet using \code{glue::glue_sql}.}
}
\value{
A character string containing the SQL snippet for the scope materials CTE
  if \code{.scope_matl} is \code{TRUE}. Returns an empty string otherwise.
}
\description{
Internal helper function that generates a Common Table Expression (CTE) SQL snippet
for scope materials. If \code{.scope_matl} is \code{TRUE}, the function constructs
the CTE SQL snippet using the \code{.da_matl_scope_get} function. If \code{.scope_matl}
is \code{FALSE}, an empty string is returned.
}
\details{
This function relies on the \code{.da_matl_scope_get} function to dynamically generate
the SQL snippet for the scope materials CTE. The snippet is constructed using
\code{glue::glue_sql} for safe parameterization of inputs. If the \code{.scope_matl}
parameter is \code{FALSE}, the function avoids constructing the CTE and instead
returns an empty string.
}
\examples{
\dontrun{
# Example usage
cte_sql <- .da_matl_scope_cte_get(
  .scope_matl = TRUE,
  .con = my_db_connection
)
print(cte_sql)
}

}
\keyword{internal}
