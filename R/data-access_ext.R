#' Retrieve Material Master Data
#'
#' Fetches global material master data, material master data per sales organization, or material master data per plant from parquet files using a DuckDB query.
#'
#' @param .material Character vector. An optional list of materials to filter on. If \code{NULL}, no material-based filtering is applied beyond scope constraints.
#' @param .salesorg Character vector. An optional list of sales organizations to filter on. If \code{NULL}, no sales organization filtering is applied beyond scope constraints.
#' @param .scope_matl Logical. If \code{TRUE}, restricts the query to materials within the Pythia scope. Defaults to \code{TRUE}.
#' @param .scope_sorg Logical. If \code{TRUE}, restricts the query to sales organizations within the Pythia scope. Defaults to \code{TRUE}.
#' @param .dataset_name character, one of teh entries in master data in teh config file, e.g.
#' "material", "mat_sales", "mat_plant"....
#' @param .n Numeric or \code{Inf}. The maximum number of rows to return. Defaults to \code{Inf}.
#'
#' @return A \code{data.table} containing the requested material master data. Each row represents a material, with columns reflecting the dataset schema.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Retrieves configuration details, such as Common Table Expression (CTE) for scope materials and WHERE clause filters, using \code{.da_duckdb_components_get()}.
#'   \item Dynamically determines the list of parquet files to query based on the specified file type (\code{.ftype}).
#'   \item Constructs and executes an SQL query in DuckDB to fetch the requested material master data, applying the provided filters.
#' }
#'
#' It assumes a properly configured DuckDB connection and accessible parquet files based on the user's environment configuration.
#'
#' @examples
#' \dontrun{
#' # Fetch all material master data
#' materials <- pa_md_mat_get()
#'
#' # Fetch material data filtered by material and sales organization
#' materials <- pa_md_mat_get(
#'   .material = c("MAT001", "MAT002"),
#'   .salesorg = c("SORG001", "SORG002"),
#'   .n = 100
#' )
#'
#' # Fetch only material master data per plant
#' materials <- pa_md_mat_get(.ftype = 3)
#' }
#'
#' @import data.table
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
#' @export
pa_md_mat_get <- function(
    .material     = NULL,       # Optional user-supplied material
    .salesorg     = NULL,       # Optional user-supplied salesorg
    .scope_matl   = TRUE,       # restrict to Pythia Scope
    .scope_sorg   = NULL,       # restrict to Pythia Scope
    .dataset_name = .fh_dataset_field_get(
      .functional_area = "master_data", .field = "name"
      ),
    .n           = Inf         # number of rows to return
) {

  # match argument
  dataset_name <- match.arg(.dataset_name)

  # retrieve file pattern string
  file_list <- .fh_dataset_paths_get(
    .environment     = .hl_config_get()$project$active_environment,
    .staging         = "silver"  ,
    .functional_area = "master_data",
    .dataset_names   = dataset_name
  )

  # Retrieve configuration for the query
  ddb_comp <- .da_duckdb_components_get(
    .material    = .material,
    .salesorg    = .salesorg,
    .scope_matl  = .scope_matl,
    .scope_sorg  = .scope_sorg
  )

  # Construct the SQL query using the glue package
  query <- glue_sql("
          {DBI::SQL(ddb_comp$cte_scope_materials)}
          SELECT
            *
          FROM
            read_parquet([{DBI::SQL(file_list)}])
          WHERE
            {DBI::SQL(ddb_comp$where_clause)}
        ", .con = config$duckdb_con)

  # Execute the query and return the results as a data.table
  dbGetQuery(ddb_comp$duckdb_con, query, n = .n) %>%
    setDT()
}


#' Retrieve DYN Sales Data from Dynasys
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
#' dyn_data <- pa_td_dyn_get()
#'
#' # Fetch data filtered by material and sales organization
#' dyn_data <- pa_td_dyn_get(
#'   .material = c("MAT001", "MAT002"),
#'   .salesorg = c("SORG001", "SORG002")
#' )
#'
#' # Fetch data for a specific version type and calendar month range
#' dyn_data <- pa_td_dyn_get(
#'   .vtype = "FORECAST",
#'   .cm_min = "202301",
#'   .cm_max = "202312"
#' )
#' }
#'
#' @import data.table
#' @importFrom DBI dbGetQuery
#' @export
pa_td_dyn_get <-
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
    query <- .da_sql_sales_get(
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
    dbGetQuery(.dd_duckdb_open_conn(), query, n = .n) %>%
      setDT()

  }

#' Retrieve outbound Sales Data from SAP BW to Dynasys
#'
#' This function retrieves RTP (Real-Time Planning) data based on specified filters
#' such as material, sales organization, and calendar month range. It fetches data
#' from a DuckDB connection and returns it as a `data.table` object.
#'
#' @param .ftype Character scalar, mandatory rtp = 5, ipm = 6
#' @param .material [character] (default: `NULL`)
#'   Optional user-supplied material filter. If not specified, all materials are included.
#' @param .salesorg [character] (default: `NULL`)
#'   Optional user-supplied sales organization filter. If not specified, all sales organizations are included.
#' @param .scope_matl [logical] (default: `TRUE`)
#'   If `TRUE`, restricts data to materials within the Pythia scope.
#' @param .scope_sorg [logical] (default: `TRUE`)
#'   If `TRUE`, restricts data to sales organizations within the Pythia scope.
#' @param .cm_min [character] (default: `'202101'`)
#'   Specifies the earliest calendar month for which data is available, formatted as `YYYYMM`.
#' @param .cm_max [character] (default: `'202506'`)
#'   Specifies the latest calendar month for which data is available, formatted as `YYYYMM`.
#' @param .n [numeric] (default: `Inf`)
#'   The maximum number of materials to return. Use `Inf` to return all matching records.
#'
#' @return A `data.table` object containing RTP data with the following columns:
#'   - `SALESORG`: Sales organization code.
#'   - `PLANT`: Plant code.
#'   - `MATERIAL`: Material code.
#'   - `STEP`: Processing step (fixed as `-1`).
#'   - `CALMONTH`: Calendar month in `YYYYMM` format.
#'   - `FTYPE`: Fixed as `'4'`.
#'   - `VTYPE`: Fixed as `'010'`.
#'   - `Q`: Sum of sales and free-of-charge quantities.
#'   - `SLS`: Sum of sales quantities.
#'   - `RET`: Sum of return quantities.
#'   - `FOC`: Sum of free-of-charge quantities.
#'   - `DIR`: Sum of direct sales quantities.
#'   - `ICS`: Sum of intercompany sales quantities.
#'   - `MSL`: Sum of MSL quantities.
#'
#' @examples
#' # Fetch RTP data for all materials and sales organizations in the Pythia scope
#' @export
pa_td_sap_get <-
  function(
    .ftype        = c(5 ,6)        , # ipm: .hl_convert_type(6,'010')
    .material     = NULL           , # Optional user-supplied material
    .salesorg     = NULL           , # Optional user-supplied salesorg
    .scope_matl   = TRUE           , # restrict to Pythia Scope
    .scope_sorg   = TRUE           , # restrict to Pythia Scope
    .cm_min       = CM_MIN         , # no data available before this date
    .cm_max       = CM_MAX         , # no data available after this date
    .n            = Inf              # number of materials to return
  ){

    # Get DuckDB components, conn, whereclasue and CTE
    ddb_comp <- .da_duckdb_components_get(
      .material   = .material,
      .salesorg   = .salesorg,
      .scope_matl = .scope_matl,
      .scope_sorg = .scope_sorg,
      .cm_min     = .cm_min,
      .cm_max     = .cm_max
    )

    # TODO: replace by transformation rtp -> ftype = 5, ipm ftype = 6
    # TODO: remove {ftype} from sql

    ftype         <- .ftype[1]
    dataset_name_ <-  .hl_convert_type(.ftype = ftype, .vtype = '010')

    # retrieve file pattern string
    file_list <- .fh_dataset_paths_get(
      .environment     = .hl_config_get()$project$active_environment,
      .staging         = "silver"  ,
      .functional_area = "sales",
      .dataset_names   = dataset_name_
    )

    # construct Query
    query <-
      glue_sql("
        {DBI::SQL(ddb_comp$cte_scope_materials)}

        SELECT
          SALESORG,
          PLANT,
          MATERIAL,
       -- SOLDTO,
          STEP,
          CALMONTH,
          VERSMON,
          FTYPE,
          VTYPE,
          sum(SLS_QT_SO + SLS_QT_FOC) AS Q,
          sum(SLS_QT_SO)              AS SLS,
          sum(SLS_QT_RET)             AS RET,
          sum(SLS_QT_FOC)             AS FOC,
          sum(SLS_QT_DIR)             AS DIR,
       -- sum(SLS_QT_PRO)             AS PRO,
          sum(SLS_QT_IC)              AS ICS,
          sum(MSQTBUO)                AS MSL
        FROM
      read_parquet([{DBI::SQL(file_list)}])
        WHERE
          {DBI::SQL(ddb_comp$where_clause)}
        GROUP BY
          ALL
        ORDER BY
          ALL
      ", .con = ddb_comp$duckdb_con)

    # fetch .n results and return as data.table
    dbGetQuery(.dd_duckdb_open_conn(), query, n = .n) %>%
      setDT()

  }

