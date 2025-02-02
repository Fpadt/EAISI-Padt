#' calculate forecast accuracy metrics
#'
#' This function calculates forecast accuracy metrics by merging actual and forecast data,
#' deriving error-based metrics such as Absolute Percentage Error (APE), and returning a detailed
#' `data.table` with computed values.
#'
#' @param .tab A `data.table` containing the input data with at least the columns:
#'   `VTYPE`, `FTYPE`, `SALESORG`, `PLANT`, `MATERIAL`, `CALMONTH`, `Q`, and optionally `STEP`.
#' @param .act_ftype An integer specifying the `FTYPE` value for actual data. Defaults to `1`.
#' @param .fct_ftype An integer specifying the `FTYPE` value for forecast data. Defaults to `2`.
#'
#' @return A `data.table` containing the merged actual (`ACT`) and forecast (`FCT`) data along with
#' the following derived columns:
#' - `E`: Error (`ACT - FCT`)
#' - `E2`: Squared Error (`E^2`)
#' - `AE`: Absolute Error (`abs(E)`)
#' - `APE`: Absolute Percentage Error (`100 * AE / ACT`)
#' - `APA`: Absolute Percentage Accuracy (`100 - APE`)
#' - `EPE`: Error Percentage Error (`100 * AE / FCT`)
#' - `EPA`: Error Percentage Accuracy (`100 - EPE`)
#'
#' @details
#' The function performs the following steps:
#' 1. Subsets the input `data.table` for actual (`ACT`) and forecast (`FCT`) data based on
#'    `VTYPE` and `FTYPE` values.
#' 2. Merges the subsets using a full outer join on the keys: `SALESORG`, `PLANT`, `MATERIAL`,
#'    and `CALMONTH`.
#' 3. Replaces `NA` values in the `ACT` and `FCT` columns with `0`.
#' 4. Computes the derived columns (e.g., `E`, `AE`, `APE`) as described in the `@return` section.
#'
#' @examples
#' library(data.table)
#'
#' # Sample data
#' .tab <- data.table(
#'   VTYPE = c("010", "060", "010", "060"),
#'   FTYPE = c(1, 2, 1, 2),
#'   SALESORG = c("A", "A", "B", "B"),
#'   PLANT = c("P1", "P1", "P2", "P2"),
#'   MATERIAL = c("M1", "M1", "M2", "M2"),
#'   CALMONTH = c("202301", "202301", "202302", "202302"),
#'   Q = c(100, 120, 200, 190),
#'   STEP = c(1, 1, 1, 1)
#' )
#'
#' # Calculate accuracy metrics
#' result <- pa_model_accuracy(.tab)
#' print(result)
#'
#' @export
pa_model_accuracy <- function(
    .tab,
    .act_ftype = 1,
    .fct_ftype = 2) {

  # Ensure `.tab` is a data.table
  stopifnot(data.table::is.data.table(.tab))

  # --- 1) Subset for ACT (where VTYPE = "010") ---
  ACT <- .tab[
    VTYPE == "010" & FTYPE == .act_ftype,
    .(SALESORG, PLANT, MATERIAL, CALMONTH, FTYPE, ACT = Q)
  ]

  # --- 2) Subset for FCT (where VTYPE = "060") ---
  FCT <- .tab[
    VTYPE == "060" & FTYPE == .fct_ftype,
    .(SALESORG, PLANT, MATERIAL, CALMONTH, FTYPE, STEP, FCT = Q)
  ]

  # --- 3) Merge both (full outer join) ---
  out <- merge(
    x   = ACT,
    y   = FCT,
    by  = c("SALESORG", "PLANT", "MATERIAL", "CALMONTH"), # , "FTYPE"
    all = TRUE  # full outer join
  )

  # --- 4) Replace NA with 0 for ACT & FCT ---
  out[, `:=`(
    ACT = data.table::fifelse(is.na(ACT), 0, ACT),
    FCT = data.table::fifelse(is.na(FCT), 0, FCT)
  )]

  # --- 5) Compute derived columns (E, E2, AE, APE, EPE) ---
  out <- out[, {
    E   = ACT - FCT
    E2  = E ^ 2
    AE  = abs(E)
    APE = 100 * (AE / ifelse(ACT == 0, NA, ACT)) # Avoid division by zero
    APA = 100 - APE
    EPE = 100 * (AE / ifelse(FCT == 0, NA, FCT))
    EPA = 100 - EPE
    .(SALESORG, PLANT, MATERIAL, CALMONTH, STEP, # FTYPE,
      ACT, FCT, E, E2, AE, APE, APA, EPE, EPA)
  }]

  # MAE  = mean(abs(ACT - FCT), na.rm = TRUE),
  # RMSE = sqrt(mean((ACT - FCT)^2, na.rm = TRUE))
  # MAPE = mean(abs(ACT - FCT) / ifelse(ACT == 0, NA, ACT), na.rm = TRUE) * 100

  # Return the final data.table
  out
}
