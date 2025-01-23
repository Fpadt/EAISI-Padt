# Transaction Data Functions ####

## DYN from Dynasys ####
# fGet_DYN_Actuals <-
#   function(
    #     .material    = NULL    , # Optional user-supplied material
#     .salesorg    = NULL    , # Optional user-supplied salesorg
#     .scope_matl  = TRUE    , # restrict to Pythia Scope
#     .scope_sorg  = TRUE    , # restrict to Pythia Scope
#     .cm_min      = '202101', # no data available before this date
#     .cm_max      = '202512', # no data available after this date
#     .step_min    = -999    , # no data available before this step
#     .step_max   = 999     , # no data available before this step
#     .n           = Inf       # number of materials to return
#   ){
#
#     # construct Query
#     query <- .make_sql_query_dyn(
#       .vtype       = '010',
#       .material    = .material,
#       .salesorg    = .salesorg,
#       .scope_matl  = .scope_matl,
#       .scope_sorg  = .scope_sorg
#     )
#
#     # fetch .n results and return as data.table
#     dbGetQuery(.get_duckdb_conn(), query, n = .n) %>%
#       setDT()
#
#   }

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

# fGet_DYN_Forecast <-
#   function(
    #     .material    = NULL    , # Optional user-supplied material
#     .salesorg    = NULL    , # Optional user-supplied salesorg
#     .scope_matl  = TRUE    , # restrict to Pythia Scope
#     .scope_sorg  = TRUE    , # restrict to Pythia Scope
#     .cm_min      = '202101', # no data available before this date
#     .cm_max      = '202506', # no data available after this date
#     .step_min    = -999    , # no data available before this step
#     .step_max   = 999     , # no data available before this step
#     .n           = Inf       # number of materials to return
#   ){
#
#     # construct Query
#     query <- .make_sql_query_dyn(
#       .vtype       = '060',
#       .material    = .material,
#       .salesorg    = .salesorg,
#       .scope_matl  = .scope_matl,
#       .scope_sorg  = .scope_sorg
#     )
#
#     # fetch .n results and return as data.table
#     dbGetQuery(.get_duckdb_conn(), query, n = .n) %>%
#       setDT()
#
#   }

## RTP to Dynasys  ####
fGet_RTP_Actuals <-
  function(
    .material    = NULL    , # Optional user-supplied material
    .salesorg    = NULL    , # Optional user-supplied salesorg
    .scope_matl  = TRUE    , # restrict to Pythia Scope
    .scope_sorg  = TRUE    , # restrict to Pythia Scope
    .cm_min      = '202101', # no data available before this date
    .cm_max      = '202506', # no data available after this date
    .step_min    = -999    , # no data available before this step
    .step_max   = 999     , # no data available before this step
    .n           = Inf       # number of materials to return
  ){

    # Get Centralized config
    config <- .get_duckdb_parts(
      .material   = .material,
      .scope_matl = .scope_matl
    )

    # construct Query
    query <-
      glue_sql("
        {DBI::SQL(config$cte_scope_materials)}

        SELECT
          SALESORG,
          PLANT,
          MATERIAL,
          0 AS STEP,
          CALMONTH,
          sum(SLS_QT_SO + SLS_QT_FOC) as Q
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
