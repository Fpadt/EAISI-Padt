#' Initialize Global Constants
#'
#' Creates global constants for colors, functional areas, stages, and environments by calling internal functions.
#'
#' @return NULL. This function creates constants dynamically in the global environment.
#' @export
pa_initialize_constants <- function() {

  # Check if internal functions exist
  if (!exists(".generate_color_variables") ||
      !exists(".generate_functional_area_variables") ||
      !exists(".generate_stage_variables") ||
      !exists(".generate_environment_variables")) {
    stop("One or more internal functions are missing.")
  }

  # Predefined data for colors, functional areas, stages, and environments
  pa_ET_COLS <- c(
    WT = "#FFFFFF", CG = "#0f5e3c",
    FG = "#089b35", LG = "#38e56d",
    YL = "#fff200", BL = "#000000"
  )

  pa_AREA <- c(
    SLS = "sales"     , STK = "stock",
    PRM = "promotions", MD  = "master_data"
  )

  pa_BSGP <- c(
    B = "Bronze", S = "Silver",
    G = "Gold"  , P = "Platinum"
  )

  pa_DTAP <- c(
    D = "Development", T = "Test",
    A = "Acceptance" , P = "Production",
    X = "Demo"
  )

  # Call internal functions to generate constants
  .generate_color_variables(pa_ET_COLS)
  .generate_functional_area_variables(pa_AREA)
  .generate_stage_variables(pa_BSGP)
  .generate_environment_variables(pa_DTAP)

  message("Global constants have been initialized successfully.")
}

