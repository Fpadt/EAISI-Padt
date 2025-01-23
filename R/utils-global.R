
# Suppress warnings about undefined global variables
# handling global variables (e.g., for data.table NSE)

utils::globalVariables(
  c(
    ".",
    "SRC", "OHDEST", "POSIT",
    "WHERE_CLAUSE", "SCOPE_MATL",
    "area", "vtype", "ftype", "fname",
    "VTYPE", "FTYPE", "SALESORG", "PLANT", "MATERIAL", "CALMONTH", "Q", "STEP"
    )
  )

