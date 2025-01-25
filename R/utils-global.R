
# Suppress warnings about undefined global variables
# handling global variables (e.g., for data.table NSE)

utils::globalVariables(
  c(
    "colors"    , "environments", "stages" , "areas",
    ".ET_COLS"  , ".AREA"       , ".BSGP"  , ".DTAP",
    "BFN_src"   , "BFN_tgt"     , "MOD_src", "MOD_tgt",
    ".",
    "SRC", "OHDEST", "POSIT",
    "WHERE_CLAUSE", "SCOPE_MATL",
    "vtype", "ftype", "fname",
    "VTYPE", "FTYPE", "SALESORG", "PLANT", "MATERIAL", "CALMONTH", "Q", "STEP"
    )
  )

