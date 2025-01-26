.onLoad <- function(libname, pkgname) {

  # Silent initialization of the config file
  tryCatch(
    {
      # copy the default config folder to the user's project directory
      .su_package_folder_copy(CONFIG_FLDR, ".", FALSE)

      # copy demo data folder to the user's project directory
      .su_package_folder_copy(PADEMO_FLDR, ".", FALSE)

      # add Ecotone Brand colors
      .cn_constants_color_generate(.ET_COLS)

    },
    error = function(e) {
      warning("Failed to initialize the config file: ", conditionMessage(e))
    }
  )
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    green(paste0(
      "\n\nWelcome to `Pythia's Advice - Data science Tooling` (a.k.a PADT)\n",
      "This package is designed during the data science program of EAISI@tue.nl\n"
      ))
  )
}

