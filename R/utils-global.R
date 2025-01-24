
# Suppress warnings about undefined global variables
# handling global variables (e.g., for data.table NSE)

utils::globalVariables(
  c(
    ".",
    "SRC", "OHDEST", "POSIT",
    "WHERE_CLAUSE", "SCOPE_MATL",
    "pa_DTAP", "pa_BSGP", "pa_AREA",
    "vtype", "ftype", "fname",
    "VTYPE", "FTYPE", "SALESORG", "PLANT", "MATERIAL", "CALMONTH", "Q", "STEP"
    )
  )

