.PACKAGE_NAME <- "padt"

fn_TRFN_ORG <- "transformation_rules_org.csv"
fn_TRFN_MOD <- "transformation_rules_mod.csv"

CONFIG_YAML <- "_config.yaml"
CONFIG_FLDR <- "_config"
PADEMO_FLDR <- "sample_data"

CM_MIN   <- '2021.01'
CM_MAX   <- '2025.06'
STEP_MIN <- 1
STEP_MAX <- 24
LAGG_MIN <- -999
LAGG_MAX <- 999

SCOPE_SORG <- c('FR30', 'NL10')
SCOPE_PRDH <- c(
  '07',  # ALTER ECO
  '08',  # BJORG
  '10',  # CLIPPER (CUPPER)
  '15',  # ZONNATURA
  '53',  # TANOSHI
  '65'   # NATURELA
)

#' [](https://coolors.co/3e074a-0f431c-fde8e9-f0f600)
# Predefined data for colors, functional areas, stages, and environments
# .ET_COLS <- c(
#   WT = "#FFFFFF"    , CG = "#0f5e3c",
#   FG = "#089b35"    , LG = "#38e56d",
#   YL = "#fff200"    , BL = "#000000"
# )

# .AREA <- c(
#   SLS = "sales"     , STK = "stock",
#   PRM = "promotions", MD  = "master_data"
# )

.BSGP <- c(
  B = "bronze"     , S = "silver",
  G = "gold"       , P = "platinum"
)

.DTAP <- c(
  D = "development", T = "test",
  A = "acceptance" , P = "production"
)

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


