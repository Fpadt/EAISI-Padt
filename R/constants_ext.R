#' initialize global constants
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

  # Call internal functions to generate constants
  .generate_color_variables(.ET_COLS)
  .generate_functional_area_variables(.AREA)
  .generate_stage_variables(.BSGP)
  .generate_environment_variables(.DTAP)

  message(green("Global constants have been initialized successfully."))
}

