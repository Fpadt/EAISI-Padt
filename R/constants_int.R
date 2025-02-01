.PACKAGE_NAME <- "padt"

fn_TRFN_ORG <- "transformation_rules_org.csv"
fn_TRFN_MOD <- "transformation_rules_mod.csv"

CONFIG_YAML <- "_config.yaml"
CONFIG_FLDR <- "padt_cfg"
PADEMO_FLDR <- "padt_dat"

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

# TODO: Fix this later
# FIXME: Debug this
# NOTE: Consider refactoring
# IDEA: Maybe use another approach
