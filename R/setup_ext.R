#' Run installation process for the `padt`-package
#'
#' Guides the user through the package installation process.
#' user can choose to create the 'config' folder, copy the demo environment
#' and activate the demo environment.
#'
#' @return Message indicating the installation process is completed.
#' @examples
#' # Run the installation process
#' run_installation()
#' @export
pa_initialize <- function() {

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
    "create 'config' in project root directory?",
    options = c(
      "create with overwrite",
      "create don't overwrite",
      "do nothing"
    )
  )
  if (choice1 == 1) {
    .copy_package_folder(
      folder_name = "config",
      target_dir = ".",
      .overwrite = TRUE)
  } else if (choice1 == 2) {
    .copy_package_folder(
      folder_name = "config",
      target_dir = ".",
      .overwrite = FALSE)
  }

  # Question 2: copy demo environment with data
  choice2 <- ask_question(
    "create demo environment in project root directory?",
    options = c(
      "create with overwrite",
      "create don't overwrite",
      "do nothing"
    )
  )
  if (choice2 == 1) {
    .copy_package_folder(
      folder_name = "demo",
      target_dir = ".",
      .overwrite = TRUE)
  } else if (choice2 == 2) {
    .copy_package_folder(
      folder_name = "demo",
      target_dir = ".",
      .overwrite = FALSE)
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
    pa_config_set_value("environment", "demo")
    pa_config_set_value("data_dir"   , ".")
  }

  # Question 4: Ecotone Brand colors
  choice3 <- ask_question(
    "Add Ecotone Brand Colors?",
    options = c(
      "Yes",
      "No"
    )
  )
  if (choice4 == 1) {
    .generate_color_variables(.ET_COLS)
  }


  message(green("Installation process completed."))
}
