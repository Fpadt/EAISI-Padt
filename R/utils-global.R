
# Suppress warnings about undefined global variables
# handling global variables (e.g., for data.table NSE)

utils::globalVariables(
  c("SRC", "OHDEST", "POSIT", "WHERE_CLAUSE", ".", "vtype", "ftype"))
