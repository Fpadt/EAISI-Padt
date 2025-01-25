# SCOPE_PLNT <-
#   "WITH SCOPE_PLNT AS (
#      SELECT DISTINCT
#        PLANT
#      FROM
#        read_parquet([{`FN_FRPR1`}, {`FN_FRPR3`}])
#      WHERE
#        SALESORG IN ({SCOPE_SORG*})
#      )
#   "

#' Display Verbose Output for Function Execution
#'
#' Helper function to print details about a function's name, arguments, and return value if verbose mode is enabled.
#'
#' @param function_name Character. The name of the function being executed.
#' @param function_args List. The arguments passed to the function. Defaults to \code{NULL}.
#' @param function_return Any. The return value of the function. Defaults to \code{NULL}.
#' @param verbose Logical. Whether to print the verbose output. Defaults to \code{FALSE}.
#'
#' @return This function does not return anything but prints details to the console if \code{verbose = TRUE}.
#'
#' @examples
#' # Example for internal testing
#' @keywords internal
.verbose <- function(
    function_name = NULL,
    function_args = NULL,
    function_return = NULL,
    verbose = FALSE
) {
  if (isTRUE(verbose)) {
    cat("Function name:\n")
    cat("  ", function_name, "\n\n")

    if (!is.null(function_args)) {
      cat("Function arguments:\n")
      print(function_args)
      cat("\n")
    }

    if (!is.null(function_return)) {
      cat("Function return value:\n")
      print(function_return)
      cat("\n")
    }
  }
}




#' Construct a SQL CTE Clause for Scope Materials
#'
#' Internal helper function that generates a Common Table Expression (CTE) SQL snippet
#' for scope materials. If \code{.scope_matl} is \code{TRUE}, the function constructs
#' the CTE SQL snippet using the \code{.get_scope_matl} function. If \code{.scope_matl}
#' is \code{FALSE}, an empty string is returned.
#'
#' @param .scope_matl Logical. Determines whether the scope materials CTE should
#'   be included in the query. If \code{TRUE}, the function constructs the SQL snippet.
#' @param .con A database connection object. Required for safely constructing
#'   the SQL snippet using \code{glue::glue_sql}.
#'
#' @return A character string containing the SQL snippet for the scope materials CTE
#'   if \code{.scope_matl} is \code{TRUE}. Returns an empty string otherwise.
#'
#' @details
#' This function relies on the \code{.get_scope_matl} function to dynamically generate
#' the SQL snippet for the scope materials CTE. The snippet is constructed using
#' \code{glue::glue_sql} for safe parameterization of inputs. If the \code{.scope_matl}
#' parameter is \code{FALSE}, the function avoids constructing the CTE and instead
#' returns an empty string.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' cte_sql <- .get_cte_scope_materials(
#'   .scope_matl = TRUE,
#'   .con = my_db_connection
#' )
#' print(cte_sql)
#' }
#'
#' @import glue
#' @importFrom DBI SQL
#' @keywords internal
.get_cte_scope_materials <- function(.scope_matl, .con) {

  # Retrieve the scope materials CTE SQL
  matl_scope <- .get_scope_matl(.con = .con)

  # Initialize the CTE SQL as empty
  cte_scope_materials <- ""

  # Generate the CTE SQL if .scope_matl is TRUE
  if (isTRUE(.scope_matl)) {
    cte_scope_materials <- glue::glue_sql(matl_scope, .con = .con)
  }

  return(cte_scope_materials)
}








#' Generate SQL for Scope Materials CTE
#'
#' Constructs a Common Table Expression (CTE) SQL string for scope materials.
#' This CTE selects distinct materials from a parquet file filtered by a condition
#' on the \code{PRDH1} column.
#'
#' @param .scope_prdh Character vector. The values to filter the \code{PRDH1} column.
#'   Defaults to the global constant \code{SCOPE_PRDH}.
#' @param .con A database connection object. Used to safely parameterize SQL queries
#'   via \code{glue::glue_sql}.
#'
#' @return A character string containing the SQL statement for the scope materials CTE.
#'
#' @details
#' This function dynamically constructs a SQL query using \code{glue::glue_sql},
#' ensuring that inputs are safely parameterized. The parquet file path for material
#' data is determined internally via \code{.data_full_file_names_get}.
#'
#' The returned SQL defines a Common Table Expression (CTE) named \code{SCOPE_MATL},
#' which filters materials from a parquet file based on the specified \code{PRDH1} values.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' sql <- .get_scope_matl(
#'   .scope_prdh = c("PRDH001", "PRDH002"),
#'   .con = my_db_connection
#' )
#' print(sql)
#' }
#'
#' @import glue
#' @importFrom DBI SQL
#' @keywords internal
.get_scope_matl <- function(.scope_prdh = SCOPE_PRDH, .con) {

  # Validate input parameters
  if (missing(.con)) {
    stop("DuckDB connection .con must be provided.")
  }

  # Get the list of files to query for the first file type in .ftype
  FN_MATL <- .data_full_file_names_get(
    .bsgp  = 2,
    .area  = 4,
    .vtype = '010',
    .ftype = 1,
    .etype = "parquet"
  )

  # Construct the SQL string using glue_sql
  sql_query <- glue::glue_sql("
    WITH SCOPE_MATL AS (
      SELECT DISTINCT MATERIAL
      FROM read_parquet([{DBI::SQL(FN_MATL)}])
      WHERE PRDH1 IN ({.scope_prdh*})
    )
  ", .scope_prdh = .scope_prdh, .con = .con)

  return(sql_query)
}



#' Build a SQL WHERE Clause with Scope Materials
#'
#' Constructs a SQL WHERE clause by appending conditions based on the provided parameters. Additionally, it can include a Common Table Expression (CTE) for scope materials if required.
#'
#' @param .clauses A list of existing WHERE clauses (character strings). If empty, "TRUE" will be added as the initial condition.
#' @param .material Character vector. A list of materials to filter on. If \code{NULL}, no material-based filter is applied.
#' @param .salesorg Character vector. A list of sales organizations to filter on. If \code{NULL}, no sales organization-based filter is applied.
#' @param .scope_matl Logical. If \code{TRUE}, restricts the query to materials within the scope definition using a CTE.
#' @param .scope_sorg Logical. If \code{TRUE}, restricts the query to sales organizations within the scope definition.
#' @param .cm_min Character. Minimum calendar month (format: \code{YYYYMM}). If \code{NULL}, no lower bound is applied.
#' @param .cm_max Character. Maximum calendar month (format: \code{YYYYMM}). If \code{NULL}, no upper bound is applied.
#' @param .step_min Numeric. Minimum step value to filter on. If \code{NULL}, uses a default package-defined constant \code{STEP_MIN}.
#' @param .step_max Numeric. Maximum step value to filter on. If \code{NULL}, uses a default package-defined constant \code{STEP_MAX}.
#' @param .lagg_min Numeric. Minimum lag value to filter on. If \code{NULL}, uses a default package-defined constant \code{LAGG_MIN}.
#' @param .lagg_max Numeric. Maximum lag value to filter on. If \code{NULL}, uses a default package-defined constant \code{LAGG_MAX}.
#' @param .con A database connection object. Required for safely parameterizing SQL queries using \code{glue::glue_sql}.
#'
#' @details
#' This function constructs a SQL WHERE clause by:
#' \enumerate{
#'   \item Ensuring that the initial condition is "TRUE" if no clauses exist in \code{.clauses}.
#'   \item Appending conditions for \code{.material}, \code{.salesorg}, and scope definitions (\code{.scope_matl}, \code{.scope_sorg}).
#'   \item Adding conditions for calendar month (\code{.cm_min}, \code{.cm_max}), step values (\code{.step_min}, \code{.step_max}), and lag values (\code{.lagg_min}, \code{.lagg_max}).
#'   \item Optionally constructing a Common Table Expression (CTE) for scope materials if \code{.scope_matl = TRUE}.
#' }
#'
#' Scope-based filters are applied using pre-defined Common Table Expressions (CTEs).
#'
#' @return A character string representing the SQL WHERE clause.
#'
#' @examples
#' \dontrun{
#' # Build a basic WHERE clause
#' where_clause <- .get_where_clause(
#'   .material = c("MAT001", "MAT002"),
#'   .salesorg = c("SORG001"),
#'   .scope_matl = TRUE,
#'   .cm_min = "202101",
#'   .cm_max = "202312",
#'   .step_min = -100,
#'   .step_max = 100
#' )
#'
#' print(where_clause)
#' }
#'
#' @import glue
#' @importFrom DBI SQL
#' @importFrom data.table setDT
#' @keywords internal
.get_where_clause <- function(
    .clauses     = list(),  # Existing WHERE clauses
    .vtype       = NULL,    # VTYPE to filter on
    .ftype       = NULL,    # FTYPE to filter on
    .material    = NULL,    # Materials to filter on
    .salesorg    = NULL,    # Sales organizations to filter on
    .scope_matl  = FALSE,   # Scope-based material filter
    .scope_sorg  = FALSE,   # Scope-based salesorg filter
    .cm_min      = NULL,    # Minimum calendar month filter
    .cm_max      = NULL,    # Maximum calendar month filter
    .step_min    = NULL,    # Minimum step filter
    .step_max    = NULL,    # Maximum step filter
    .lagg_min    = NULL,    # Minimum lag filter
    .lagg_max    = NULL,    # Maximum lag filter
    .con         = NULL     # Database connection
) {

  # matl_scope <- .get_scope_matl(.con = .con)

  # Ensure "TRUE" is the initial condition
  if (!any(vapply(.clauses, function(x) identical(x, "TRUE"), logical(1)))) {
    .clauses <- c(.clauses, "TRUE")
  }

  # Add MATERIAL filter
  if (!is.null(.material) && length(.material) > 0) {
    .clauses <- c(
      .clauses,
      glue_sql("MATERIAL IN ({vals*})", vals = pa_matn1_input(.material), .con = .con)
    )
  }

  # Add SALESORG filter
  if (!is.null(.salesorg) && length(.salesorg) > 0) {
    .clauses <- c(
      .clauses,
      glue_sql("SALESORG IN ({vals*})", vals = .salesorg, .con = .con)
    )
  }

  # Add scope-based filters
  if (isTRUE(.scope_matl)) {
    .clauses <- c(
      .clauses,
      glue_sql("MATERIAL IN (SELECT MATERIAL FROM SCOPE_MATL)", .con = .con)
    )
  }

  if (isTRUE(.scope_sorg)) {
    .clauses <- c(
      .clauses,
      glue_sql("SALESORG IN ({vals*})", vals = SCOPE_SORG, .con = .con)
    )
  }

  # Add CALMONTH filter
  if (!is.null(.cm_min) || !is.null(.cm_max)) {
    .cm_min <- .cm_min %||% CM_MIN
    .cm_max <- .cm_max %||% CM_MAX
    .clauses <- c(
      .clauses,
      glue_sql("CALMONTH BETWEEN {.cm_min} AND {.cm_max}", .con = .con)
    )
  }

  # Initialize STEP and LAG clauses
  .step_cls <- NULL
  .lagg_cls <- NULL

  # Construct STEP filter clause for Forecasts
  if(!is.null(.vtype) && any(.vtype %in% '060')){
    .step_min <- .step_min %||% STEP_MIN
    .step_max <- .step_max %||% STEP_MAX
    .step_cls <- glue_sql(
      "VTYPE = '060' AND STEP BETWEEN {.step_min} AND {.step_max}",
      .con = .con
    )
  }

  # Construct LAG filter clause for Actuals
  if(!is.null(.vtype) && any(.vtype %in% '010')){
    .lagg_min <- .lagg_min %||% LAGG_MIN
    .lagg_max <- .lagg_max %||% LAGG_MAX
    .lagg_cls <- glue_sql(
      "VTYPE = '010' AND STEP BETWEEN {.lagg_min} AND {.lagg_max}",
      .con = .con
    )
  }

  # Construct FTYPE filter pre[=1] and post [=2] DR
  # Actuals for Accuracy = [1,2], latest to forecast is [3,4]
  if(!is.null(.ftype) ){
    .clauses <- c(
      .clauses,
      glue_sql("FTYPE IN ({vals*})", vals = .ftype, .con = .con)
    )
  }

  # Combine STEP and LAG clauses with OR
  .comb_cls <- NULL
  if (!is.null(.step_cls) && !is.null(.lagg_cls)) {
    .comb_cls <- glue_sql("({.lagg_cls} OR {.step_cls})", .con = .con)
  } else if (!is.null(.step_cls)) {
    .comb_cls <- .step_cls
  } else if (!is.null(.lagg_cls)) {
    .comb_cls <- .lagg_cls
  }

  # Add combined clause to the existing clauses
  if (!is.null(.comb_cls)) {
    .clauses <- c(.clauses, .comb_cls)
  }

  # Combine clauses into a single WHERE clause
  paste(.clauses, collapse = " AND ")
}

