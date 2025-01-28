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

.BSGP <- c(
  B = "bronze"     ,
  S = "silver"     ,
  G = "gold"       ,
  P = "platinum"
)

.DTAP <- c(
  D = "development",
  T = "test"       ,
  A = "acceptance" ,
  P = "production"
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
