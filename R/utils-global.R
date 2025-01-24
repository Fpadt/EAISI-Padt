
# Suppress warnings about undefined global variables
# handling global variables (e.g., for data.table NSE)

utils::globalVariables(
  c(
    "colors"    , "environments", "stages" , "areas",
    "pa_ET_COLS", "pa_AREA"     , "pa_BSGP", "pa_DTAP",
    ".",
    "SRC", "OHDEST", "POSIT",
    "WHERE_CLAUSE", "SCOPE_MATL",
    "vtype", "ftype", "fname",
    "VTYPE", "FTYPE", "SALESORG", "PLANT", "MATERIAL", "CALMONTH", "Q", "STEP"
    )
  )

