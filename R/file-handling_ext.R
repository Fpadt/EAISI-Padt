#' Prompt User for Transformation Decision with Options
#'
#' Asks the user how to handle the files found (transform all, transform new, transform modified, or do nothing).
#'
#' @param pa_SYNC A list with two elements:
#'   \describe{
#'     \item{NEW}{Character vector of paths for new files.}
#'     \item{MOD}{Character vector of paths for modified files.}
#'   }
#' @return Character. One of the options: ".all", ".new", ".mod", or ".none".
#' @export
pa_ask_user_transform_files <- function(pa_SYNC) {

  # Calculate file counts
  .new <- length(pa_SYNC[["NEW"]])
  .mod <- length(pa_SYNC[["MOD"]])
  .all <- .new + .mod

  # Define options
  options <- c(
    paste0("Transform All (", .all, " files)"),
    paste0("Transform New (", .new, " files)"),
    paste0("Transform Modified (", .mod, " files)"),
    "Do Nothing"
  )

  # Display the question and options
  cat("How would you like to handle the files?\n")
  for (i in seq_along(options)) {
    cat(i, ":", options[i], "\n")
  }

  # Get user input and validate it
  repeat {
    choice <- readline(prompt = "Please choose 1, 2, 3, or 4: ")

    # Validate the input
    if (choice %in% c("1", "2", "3", "4")) {
      # Map user choice to return value
      return(switch(
        choice,
        ".all", # Option 1
        ".new", # Option 2
        ".mod", # Option 3
        ".none" # Option 4
      ))
    } else {
      cat("Invalid input. Please choose 1, 2, 3, or 4.\n")
    }
  }
}
