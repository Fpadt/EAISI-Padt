
.computer_name <-
  Sys.getenv(
    ifelse(.Platform$OS.type == "windows", "COMPUTERNAME", "HOSTNAME")
    )

# folder locations
RW <-
  switch(
    .computer_name,
    "TREX-TOAD" = file.path("U:", "floris", "GH"),
                  file.path("C:", "RW")
  )
PRJ  <- file.path(RW , "EAISI-Pythia")
MOD  <- file.path(PRJ, "library")
RAW  <- file.path(PRJ, "10_RawData")
DAT  <- file.path(PRJ, "11_PrepData")
ANA  <- file.path(PRJ, "30_Analysis")
RES  <- file.path(PRJ, "60_Results")
FIG  <- file.path(PRJ, "70_Figures")
SAP  <- DAT

DWL <- file.path("c:", "Users", "Floris.padt", "Downloads")

ONE <- file.path("C:", "Users", "floris.padt", "OneDrive - Wessanen")
GEN <- file.path(ONE, "General")
OPS <- file.path(ONE, "OPS - Operations")
DEV <- file.path(ONE, "DEV - Development")
EIM <- file.path(ONE, "Business Intelligence & Analytics")

FCT <- file.path(ONE, "Forecasting")
IMM <- file.path(ONE, "Inventory Management")
SCA <- file.path(ONE, "General - EU EIM SCA")

# SharePoint
EBQ <- file.path(ONE, "BI_Documents - Ecotone BI QA")
EPB <- file.path(ONE, "BI_Documents")

# Personal
PET <- file.path("C:", "PW", "OneDrive", "ET")

# Pythia
PPET <- normalizePath(
  file.path(Sys.getenv("OneDriveConsumer"), "ET"), winslash = "/")
PDAT <- file.path(PPET, "pythia", "dat")

PS01 <- file.path(PDAT, "S1B")    # Staging - Bronze
PS02 <- file.path(PDAT, "S2S")    # Staging - Silver
PS03 <- file.path(PDAT, "S3G")    # Staging - Gold
PS04 <- file.path(PDAT, "S4P")    # Staging - Platinum

PPRM <- file.path(PS01, "PRM")    # Pythia - Promotions

# PSYS <- file.path(PS01,  SYS)          # BW SYSTEM

# PRTP <- file.path(PSYS, "RTP")         # Actuals deliverd to RTP project
# PIPM <- file.path(PSYS, "IPM")         # Actuals for Ambient for DSCP 2018
# PDYN <- file.path(PSYS, "DYN")         # Data From Dynasys Actuals & Forecast
# PEDA <- file.path(PSYS, "EDA")         # Data From EDA Actuals & Forecast


# Systems
SID <- "WPB500"
SYS <- substr(SID, 1, 3)

#' @import duckdb
#' @import lubridate
#' @import DBI
#' @import magrittr
#' @import glue
#' @import crayon
#' @import fs
#' @import data.table

#' @importFrom purrr walk

.datatable.aware <- TRUE

# https://coolors.co/3e074a-0f431c-fde8e9-f0f600
# Colors

ET_CG = "#0f5e3c"
ET_FG = "#089b35"
ET_LG = "#38e56d"
ET_YL = "#fff200"
ET_JB = "#333333"


COL_FACTR <- "#9CEB91"
COL___RED <- "#ff7f7f"
