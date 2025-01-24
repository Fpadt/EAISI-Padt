#' Ecotone Brand Colors
#'
#' @details authorized colours for the logo.
#' Castle Green will only be used for the housing of the logo.
#' The colours for the leaves are Forest Green, Light Green and Yellow.
#' The ecotone wordmark colour is White.
#' For the tagline only Jet Black or White can be used.
#' [](https://coolors.co/3e074a-0f431c-fde8e9-f0f600)
#' @export
pa_ET_COLS <- c(
  WT = "#FFFFFF", CG = "#0f5e3c",
  FG = "#089b35", LG = "#38e56d",
  YL = "#fff200", BL = "#000000"
)

# R/constants.R
#' pa_DTAP Environments
#'
#' These are the default pa_DTAP environments used in the package.
#'
#' @details Demo environment is to showcase the package functionality
#'
#' @export
pa_pa_DTAP <- c("Development", "Test", "Acceptance", "Production", "Demo")

#' pa_BSGP Stages
#'
#' The four data stages commonly used in data processing.
#'
#' @details Platinum is used for Forecast Exports
#' @export
pa_pa_BSGP <- c("Bronze", "Silver", "Gold", "Platinum")

#' Functional pa_AREAs
#'
#' Common functional pa_AREAs for demonstration purposes.
#' @export
pa_pa_AREA <- c("sales", "stock", "promotions", "master_data")


