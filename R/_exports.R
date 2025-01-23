#' Left Pad Material Numbers
#'
#' Pads material numbers with leading zeros to match the standard 18-character SAP material number format.
#'
#' @param x A character vector of material numbers. Only numeric-like strings will be padded.
#'
#' @return A character vector where numeric strings are left-padded with zeros to a width of 18. Non-numeric strings remain unchanged.
#'
#' @details
#' This function mimics the behavior of SAP's \code{CONVERSION_EXIT_MATN1_INPUT}, which ensures material numbers are standardized to 18 characters by adding leading zeros.
#'
#' @examples
#' MATN1(c("123", "456789", "A123"))
#' # Returns: c("000000000000000123", "000000000000456789", "A123")
#'
#' @export
MATN1 <- function(x) {
  .LP0(x, 18)
}

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
    .scope_sorg  = TRUE,     # restrict to Pythia Scope
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
