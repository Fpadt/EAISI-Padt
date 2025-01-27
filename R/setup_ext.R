#' Run installation process for the `padt`-package
#'
#' Guides the user through the package installation process.
#' user can choose to create the 'config' folder, copy the demo environment
#' and activate the demo environment.
#'
#' @return Message indicating the installation process is completed.
#' @examples
#' \dontrun{
#' # Run the installation process
#' pa_su_config_reset()
#' }
#' @export
pa_su_config_reset <- function() {

  # Helper function to ask a question and process the user's choice
  ask_question <- function(question, options) {
    message(silver(question))
    for (i in seq_along(options)) {
      message(silver(i, ": ", options[[i]]))
    }
    repeat {
      choice <- as.integer(readline("Enter the number of your choice: "))
      if (!is.na(choice) && choice >= 1 && choice <= length(options)) {
        return(choice)
      }
      message(
        red(paste("Invalid choice. Please enter a number between 1 and",
                  length(options), ".")))
    }
  }

  # Question 1: create config folder
  choice1 <- ask_question(
    "create '_config' in project root directory?",
    options = c(
      "create, with overwrite",
      "create, don't overwrite",
      "skip, do nothing!",
      "stop initialization process!"
    )
  )
  if (choice1 == 1) {
    # copy the default config folder to the user's project directory
    .su_package_folder_copy(CONFIG_FLDR, ".", TRUE)
  } else if (choice1 == 2) {
    # copy the default config folder to the user's project directory
    .su_package_folder_copy(CONFIG_FLDR, ".", FALSE)
  } else if (choice1 == 4) {
    message(crayon::red("Initialization process stopped by user."))
  }

  # Question 2: copy demo environment with data
  choice2 <- ask_question(
    "create demo environment in project root directory?",
    options = c(
      "create, with overwrite",
      "create, don't overwrite",
      "skip, do nothing",
      "stop initialization process!"
    )
  )
  if (choice2 == 1) {
    # copy demo data folder to the user's project directory
    .su_package_folder_copy(PADEMO_FLDR, ".", TRUE)
  } else if (choice2 == 2) {
    # copy demo data folder to the user's project directory
    .su_package_folder_copy(PADEMO_FLDR, ".", FALSE)
  } else if (choice1 == 4) {
    message(crayon::red("Initialization process stopped by user."))
  }

  # Question 3: activate demo environment
  choice3 <- ask_question(
    "Activate the demo environment?",
    options = c(
      "Yes",
      "No"
    )
  )
  if (choice3 == 1) {
    pa_config_value_set("environment", "demo")
    pa_config_value_set("data_dir"   , ".")
  }

  message(green("Installation process completed."))
}
