# Package-specific environment
.padt_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  tryCatch(
    {
      # Initialize the padt environment
      .su_padt_initialize()
    },
    error = function(e) {
      warning("Failed to initialize the padt environment: ", conditionMessage(e))
    }
  )
}

.onAttach <- function(libname, pkgname) {

  if (interactive()) { # Only display the message in interactive sessions
    packageStartupMessage(
      green(paste0(
        "\n\nWelcome to `Pythia's Advice - Decision Tooling`\n",
        "This package results from the final module of the MD&AI program of EAISI at www.tue.nl\n"
      ))
    )
  }
}
