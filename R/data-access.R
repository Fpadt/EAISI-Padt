#' Retrieve Material Master Data
#'
#' Fetches global material master data, material master data per sales organization, or material master data per plant from parquet files using a DuckDB query.
#'
#' @param .material Character vector. An optional list of materials to filter on. If \code{NULL}, no material-based filtering is applied beyond scope constraints.
#' @param .salesorg Character vector. An optional list of sales organizations to filter on. If \code{NULL}, no sales organization filtering is applied beyond scope constraints.
#' @param .scope_matl Logical. If \code{TRUE}, restricts the query to materials within the Pythia scope. Defaults to \code{TRUE}.
#' @param .scope_sorg Logical. If \code{TRUE}, restricts the query to sales organizations within the Pythia scope. Defaults to \code{TRUE}.
#' @param .ftype Integer vector. Specifies the file types to query:
#'   \itemize{
#'     \item \code{1}: Global material master data (MATL)
#'     \item \code{2}: Material master data per sales organization (MATS)
#'     \item \code{3}: Material master data per plant (MATP)
#'   }
#'   Defaults to \code{c(1, 2, 3)}.
#' @param .n Numeric or \code{Inf}. The maximum number of rows to return. Defaults to \code{Inf}.
#'
#' @return A \code{data.table} containing the requested material master data. Each row represents a material, with columns reflecting the dataset schema.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Retrieves configuration details, such as Common Table Expression (CTE) for scope materials and WHERE clause filters, using \code{.get_duckdb_parts()}.
#'   \item Dynamically determines the list of parquet files to query based on the specified file type (\code{.ftype}) using \code{.get_data_full_file_names()}.
#'   \item Constructs and executes an SQL query in DuckDB to fetch the requested material master data, applying the provided filters.
#' }
#'
#' It assumes a properly configured DuckDB connection and accessible parquet files based on the user's environment configuration.
#'
#' @examples
#' \dontrun{
#' # Fetch all material master data
#' materials <- fGet_MAT()
#'
#' # Fetch material data filtered by material and sales organization
#' materials <- fGet_MAT(
#'   .material = c("MAT001", "MAT002"),
#'   .salesorg = c("SORG001", "SORG002"),
#'   .n = 100
#' )
#'
#' # Fetch only material master data per plant
#' materials <- fGet_MAT(.ftype = 3)
#' }
#'
#' @import data.table
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
#' @export
fGet_MAT <- function(
    .material    = NULL,     # Optional user-supplied material
    .salesorg    = NULL,     # Optional user-supplied salesorg
    .scope_matl  = TRUE,     # restrict to Pythia Scope
    .scope_sorg  = NULL,     # restrict to Pythia Scope
    .ftype       = c(1, 2, 3), # File type: 1 = MATL, 2 = MATS, 3 = MATP
    .n           = Inf       # number of rows to return
) {

  # Validate .ftype input
  if (!all(.ftype %in% c(1, 2, 3))) {
    stop("Invalid value for .ftype. Must be one of: 1 (MATL), 2 (MATS), 3 (MATP).")
  }

  # Retrieve configuration for the query
  config <- .get_duckdb_parts(
    .material    = .material,
    .salesorg    = .salesorg,
    .scope_matl  = .scope_matl,
    .scope_sorg  = .scope_sorg
  )

  # Get the list of files to query for the first file type in .ftype
  file_list <- .get_data_full_file_names(
    .bsgp  = 2,
    .area  = 4,
    .vtype = '010',
    .ftype = .ftype[1], # Use the first file type in .ftype
    .etype = "parquet"
  )

  # Construct the SQL query using the glue package
  query <- glue_sql("
          {DBI::SQL(config$cte_scope_materials)}
          SELECT
            *
          FROM
            read_parquet([{DBI::SQL(file_list)}])
          WHERE
            {DBI::SQL(config$where_clause)}
        ", .con = config$duckdb_con)

  # Execute the query and return the results as a data.table
  dbGetQuery(config$duckdb_con, query, n = .n) %>%
    setDT()
}


#' Retrieve DYN Data from Dynasys
#'
#' Fetches DYN (Dynamic) data from Dynasys using a DuckDB query. This function allows filtering based on various parameters such as version types, forecast types, materials, sales organizations, calendar months, forecast steps, and more.
#'
#' @param .vtype Character vector. Optional list of version types (e.g., actuals, forecasts). If \code{NULL}, all version types are retrieved.
#' @param .ftype Character vector. Optional list of forecast types. If \code{NULL}, all forecast types are retrieved.
#' @param .material Character vector. Optional list of materials to filter on. If \code{NULL}, no material-based filtering is applied beyond scope constraints.
#' @param .salesorg Character vector. Optional list of sales organizations to filter on. If \code{NULL}, no sales organization filtering is applied beyond scope constraints.
#' @param .scope_matl Logical. If \code{TRUE}, restricts the query to materials within the Pythia scope. Defaults to \code{TRUE}.
#' @param .scope_sorg Logical. If \code{TRUE}, restricts the query to sales organizations within the Pythia scope. Defaults to \code{TRUE}.
#' @param .cm_min Character or \code{NULL}. Minimal calendar month to filter data (e.g., "202301"). If \code{NULL}, no lower limit is applied.
#' @param .cm_max Character or \code{NULL}. Maximal calendar month to filter data (e.g., "202312"). If \code{NULL}, no upper limit is applied.
#' @param .step_min Numeric or \code{NULL}. Minimal forecast step ahead to filter data. If \code{NULL}, no lower limit is applied.
#' @param .step_max Numeric or \code{NULL}. Maximal forecast step ahead to filter data. If \code{NULL}, no upper limit is applied.
#' @param .lagg_min Numeric or \code{NULL}. Minimal difference between \code{VERSMON} and \code{MONTH}. If \code{NULL}, no lower limit is applied.
#' @param .lagg_max Numeric or \code{NULL}. Maximal difference between \code{VERSMON} and \code{MONTH}. If \code{NULL}, no upper limit is applied.
#' @param .n Numeric or \code{Inf}. Maximum number of rows to return. Defaults to \code{Inf}.
#'
#' @return A \code{data.table} containing the requested DYN data, with each row representing a combination of material, sales organization, and other attributes based on the query.
#'
#' @details
#' This function constructs an SQL query using the provided parameters and executes it via DuckDB. It retrieves the specified data and returns it as a \code{data.table}.
#'
#' The function assumes:
#' \itemize{
#'   \item A properly configured DuckDB connection.
#'   \item Accessible data files and configurations based on the user's environment.
#' }
#'
#' @examples
#' \dontrun{
#' # Fetch all DYN data
#' dyn_data <- fGet_DYN()
#'
#' # Fetch data filtered by material and sales organization
#' dyn_data <- fGet_DYN(
#'   .material = c("MAT001", "MAT002"),
#'   .salesorg = c("SORG001", "SORG002")
#' )
#'
#' # Fetch data for a specific version type and calendar month range
#' dyn_data <- fGet_DYN(
#'   .vtype = "FORECAST",
#'   .cm_min = "202301",
#'   .cm_max = "202312"
#' )
#' }
#'
#' @import data.table
#' @importFrom DBI dbGetQuery
#' @export
fGet_DYN <-
  function(
    .vtype       = NULL    , # NULL will get all vtypes
    .ftype       = NULL    , # NULL will get all ftypes
    .material    = NULL    , # Optional user-supplied material
    .salesorg    = NULL    , # Optional user-supplied salesorg
    .scope_matl  = TRUE    , # restrict to Pythia Scope
    .scope_sorg  = TRUE    , # restrict to Pythia Scope
    .cm_min      = NULL    , # minimal Cal Month
    .cm_max      = NULL    , # maximal Cal Month
    .step_min    = NULL    , # minimal forecast step ahead
    .step_max    = NULL    , # maximal forecast step ahead
    .lagg_min    = NULL    , # minimal diff. between VERSMON & MONTH
    .lagg_max    = NULL    , # maximal diff. between VERSMON & MONTH
    .n           = Inf       # number of materials to return
  ){

    # construct Query
    query <- .make_sql_query_dyn(
      .vtype       = .vtype,
      .ftype       = .ftype,
      .material    = .material,
      .salesorg    = .salesorg,
      .scope_matl  = .scope_matl,
      .scope_sorg  = .scope_sorg,
      .cm_min      = .cm_min,
      .cm_max      = .cm_max,
      .step_min    = .step_min,
      .step_max    = .step_max,
      .lagg_min    = .lagg_min,
      .lagg_max    = .lagg_max
    )

    # fetch .n results and return as data.table
    dbGetQuery(.get_duckdb_conn(), query, n = .n) %>%
      setDT()

  }


fGet_RTP <-
  function(
    .material    = NULL    , # Optional user-supplied material
    .salesorg    = NULL    , # Optional user-supplied salesorg
    .scope_matl  = TRUE   , # restrict to Pythia Scope
    .scope_sorg  = TRUE    , # restrict to Pythia Scope
    .cm_min      = '202101', # no data available before this date
    .cm_max      = '202506', # no data available after this date
    .n           = Inf       # number of materials to return
  ){

    # Get Centralized config
    config <- .get_duckdb_parts(
      .material   = .material,
      .salesorg   = .salesorg,
      .scope_matl = .scope_matl,
      .scope_sorg = .scope_sorg,
      .cm_min     = .cm_min,
      .cm_max     = .cm_max
    )

    # construct Query
    query <-
      glue_sql("
        {DBI::SQL(config$cte_scope_materials)}

        SELECT
          SALESORG,
          PLANT,
          MATERIAL,
       -- SOLDTO,
          -1                          AS STEP,
          CALMONTH,
          '4'                         AS FTYPE,
          '010'                       AS VTYPE,
          sum(SLS_QT_SO + SLS_QT_FOC) AS Q,
          sum(SLS_QT_SO)              AS SLS,
          sum(SLS_QT_RET)             AS RET,
          sum(SLS_QT_FOC)             AS FOC,
          sum(SLS_QT_DIR)             AS DIR,
       -- sum(SLS_QT_PRO)             AS PRO,
          sum(SLS_QT_IC)              AS ICS,
          sum(MSQTBUO)                AS MSL
        FROM
          read_parquet([{`FN_IRTP`}])
        WHERE
          {DBI::SQL(config$where_clause)}
        GROUP BY
          ALL
        ORDER BY
          ALL
      ", .con = config$duckdb_con)

    # fetch .n results and return as data.table
    dbGetQuery(config$duckdb_con, query, n = .n) %>%
      setDT()

  }

fGet_IPM <-
  function(
    .material    = NULL    , # Optional user-supplied material
    .salesorg    = NULL    , # Optional user-supplied salesorg
    .scope_matl  = TRUE   , # restrict to Pythia Scope
    .scope_sorg  = TRUE    , # restrict to Pythia Scope
    .cm_min      = '202101', # no data available before this date
    .cm_max      = '202506', # no data available after this date
    .n           = Inf       # number of materials to return
  ){

    # Get Centralized config
    config <- .get_duckdb_parts(
      .material   = .material,
      .salesorg   = .salesorg,
      .scope_matl = .scope_matl,
      .scope_sorg = .scope_sorg,
      .cm_min     = .cm_min,
      .cm_max     = .cm_max
    )

    # construct Query
    query <-
      glue_sql("
        {DBI::SQL(config$cte_scope_materials)}

        SELECT
          SALESORG,
          PLANT,
          MATERIAL,
       -- SOLDTO,
          -1                          AS STEP,
          CALMONTH,
          '3'                         AS FTYPE,
          '010'                       AS VTYPE,
          sum(SLS_QT_SO + SLS_QT_FOC) AS Q,
          sum(SLS_QT_SO)              AS SLS,
          sum(SLS_QT_RET)             AS RET,
          sum(SLS_QT_FOC)             AS FOC,
          sum(SLS_QT_DIR)             AS DIR,
       -- sum(SLS_QT_PRO)             AS PRO,
          sum(SLS_QT_IC)              AS ICS,
          sum(MSQTBUO)                AS MSL
        FROM
          read_parquet([{`FN_IIPM`}])
        WHERE
          {DBI::SQL(config$where_clause)}
        GROUP BY
          ALL
        ORDER BY
          ALL
      ", .con = config$duckdb_con)

    # fetch .n results and return as data.table
    dbGetQuery(config$duckdb_con, query, n = .n) %>%
      setDT()

  }
