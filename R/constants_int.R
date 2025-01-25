.PACKAGE_NAME <- "padt"

fn_TRFN_ORG <- "transformation_rules_org.csv"
fn_TRFN_MOD <- "transformation_rules_mod.csv"

CONFIG_YAML <- "_pa_config.yaml"
CONFIG_FLDR <- "_pa_config"
PADEMO_FLDR <- "_pa_demo"


CM_MIN   <- '2021.01'
CM_MAX   <- '2025.06'
STEP_MIN <- 1
STEP_MAX <- 24
LAGG_MIN <- -999
LAGG_MAX <- 999

# duckdb environment
.duckdb_env <- new.env(parent = emptyenv())

SCOPE_SORG <- c('FR30', 'NL10')
SCOPE_PRDH <- c(
  '07',  # ALTER ECO
  '08',  # BJORG
  '10',  # CLIPPER (CUPPER)
  '15',  # ZONNATURA
  '53',  # TANOSHI
  '65'   # NATURELA
)

# Predefined data for colors, functional areas, stages, and environments
.ET_COLS <- c(
  WT = "#FFFFFF"    , CG = "#0f5e3c",
  FG = "#089b35"    , LG = "#38e56d",
  YL = "#fff200"    , BL = "#000000"
)

.AREA <- c(
  SLS = "sales"     , STK = "stock",
  PRM = "promotions", MD  = "master_data"
)

.BSGP <- c(
  B = "bronze"     , S = "silver",
  G = "gold"       , P = "platinum"
)

.DTAP <- c(
  D = "development", T = "test",
  A = "acceptance" , P = "production"
)

#' Generate Variables for Ecotone Brand Colors
#'
#' Dynamically creates variables named "col_<name>" in the specified environment.
#'
#' @param env The environment where the variables should be created. Defaults to the global environment.
#' @return NULL. The function creates variables dynamically.
#'
#' @details authorized colours for the logo.
#' Castle Green will only be used for the housing of the logo.
#' The colours for the leaves are Forest Green, Light Green and Yellow.
#' The ecotone wordmark colour is White.
#' For the tagline only Jet Black or White can be used.
#' [](https://coolors.co/3e074a-0f431c-fde8e9-f0f600)
#'
#' @keywords internal
.generate_color_variables <- function(colors, env = globalenv()) {

  for (name in names(colors)) {
    # Construct the variable name
    var_name <- paste0("col_", name)
    # Assign the color value to the variable in the specified environment
    assign(var_name, colors[[name]], envir = env)
  }

  message(
    paste0(green("Color variables generated: ",
                 paste(paste0("col_", names(colors)), collapse = ", "))))
}


# R/constants.R

#' Generate Variables for DTAP Environments
#'
#' Dynamically creates variables named "env_<name>" based on the given list of DTAP environments.
#'
#' @param env The environment where the variables should be created. Defaults to the global environment.
#' @return NULL. The function creates variables dynamically.
#' @keywords internal
.generate_environment_variables <- function(environments, env = globalenv()) {

  for (name in names(environments)) {
    # Construct the variable name
    var_name <- paste0("env_", name)
    # Assign the environment value to the variable in the specified environment
    assign(var_name, environments[[name]], envir = env)
  }

  message(
    paste0(green("Environment variables generated: ",
                 paste(paste0("env_", names(environments)), collapse = ", "))))
}

#' Generate Variables for Stages
#'
#' Dynamically creates variables named "stg_<name>" based on the given list of stages.
#'
#' @param env The environment where the variables should be created. Defaults to the global environment.
#' @return NULL. The function creates variables dynamically.
#' @keywords internal
.generate_stage_variables <- function(stages, env = globalenv()) {

  for (name in names(stages)) {
    # Construct the variable name
    var_name <- paste0("stg_", name)
    # Assign the stage value to the variable in the specified environment
    assign(var_name, stages[[name]], envir = env)
  }

  message(
    paste0(green("Stage variables generated: ",
                 paste(paste0("stg_", names(stages)), collapse = ", "))))
}

#' Generate Variables for Functional Areas
#'
#' Dynamically creates variables named "fun_<name>" based on the given list of functional areas.
#'
#' @param env The environment where the variables should be created. Defaults to the global environment.
#' @return NULL. The function creates variables dynamically.
#' @keywords internal
.generate_functional_area_variables <- function(areas, env = globalenv()) {

  for (name in names(areas)) {
    # Construct the variable name
    var_name <- paste0("fun_", name)
    # Assign the area value to the variable in the specified environment
    assign(var_name, areas[[name]], envir = env)
  }

  message(
    paste0(green("Functional area variables generated: ",
                 paste(paste0("fun_", names(areas)), collapse = ", "))))
}


#' initialize global constants
#'
#' Creates global constants for colors, functional areas, stages, and environments by calling internal functions.
#'
#' @return NULL. This function creates constants dynamically in the global environment.
#' @keywords internal
.initialize_constants <- function() {

  # Check if internal functions exist
  if (!exists(".generate_color_variables")
      # || !exists(".generate_functional_area_variables")
      # || !exists(".generate_stage_variables")
      # || !exists(".generate_environment_variables")
  ) {
    stop("One or more internal functions are missing.")
  }

  # Call internal functions to generate constants
  .generate_color_variables(.ET_COLS)
  # .generate_functional_area_variables(.AREA)
  # .generate_stage_variables(.BSGP)
  # .generate_environment_variables(.DTAP)

  message(green("Global constants have been initialized successfully."))
}

