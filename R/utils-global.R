
# Suppress warnings about undefined global variables
# handling global variables (e.g., for data.table NSE)

utils::globalVariables(
  c(
    "colors"    , "environments", "stages" , "areas",
    "BFN_src"   , "BFN_tgt"     , "MOD_src", "MOD_tgt",
    "width", "verbose", "create_platinum", "output", ".AREA", ".config", "config",
    ".", "MODEL", "%+replace%",
    "SRC", "OHDEST", "POSIT", "AREA",
    "WHERE_CLAUSE", "SCOPE_MATL",
    "vtype", "ftype", "fname",
    "VTYPE", "FTYPE", "SALESORG", "PLANT", "MATERIAL", "CALMONTH", "Q", "STEP",
    "aes", "coord_cartesian", "geom_col", "ggplot", "labs", "y"
    )
  )

