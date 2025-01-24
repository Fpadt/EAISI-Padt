.make_sql_query_dyn <-
  function(
    .vtype       = NULL    , # NULL will get both 010 and 060
    .ftype       = NULL    , # NULL will get all ftypes
    .material    = NULL    , # NULL wont apply any filter
    .salesorg    = NULL    , # NULL wont apply any filter
    .scope_matl  = FALSE   , # FALSE wont apply any filter
    .scope_sorg  = FALSE   , # FALSE wont apply any filter
    .cm_min      = '202101', # minimal Cal Month
    .cm_max      = '202506', # maximal Cal Month
    .step_min    = NULL    , # minimal forecast step ahead
    .step_max    = NULL    , # maximal forecast step ahead
    .lagg_min    = NULL    , # minimal diff. between VERSMON & MONTH
    .lagg_max    = NULL      # maximal diff. between VERSMON & MONTH
  ) {

    # Get Centralized config
    config <- .get_duckdb_parts(
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

    # Determine Files to read
    file_list <- .get_data_full_file_names(
      .bsgp  = 2, .area = 1, .vtype = .vtype, .ftype = .ftype,
      .etype = "parquet"
    )

    # Construct the query using glue_sql()
    query <- glue::glue_sql("
    {DBI::SQL(config$cte_scope_materials)}

    SELECT
      SALESORG,
      PLANT,
      MATERIAL,
      STEP,
      CALMONTH,
      VERSMON,
      FTYPE,
      VTYPE,
      BASE_UOM,
      SUM(DEMND_QTY) AS Q
    FROM
      read_parquet([{DBI::SQL(file_list)}])
    WHERE
      {DBI::SQL(config$where_clause)}
    GROUP BY
      ALL
    ORDER BY
      ALL
  ", .con = config$duckdb_con)

    return(query)
  }
