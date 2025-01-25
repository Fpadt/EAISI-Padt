#' left pad material numbers
#'
#' Pads material numbers with leading zeros to match the standard
#' 18-character SAP material number format.
#'
#' @param x A character vector of material numbers.
#' Only numeric-like strings will be padded.
#'
#' @return A character vector where numeric strings are left-padded with
#' zeros to a width of 18. Non-numeric strings remain unchanged.
#'
#' @details
#' This function mimics the behavior of SAP's \code{CONVERSION_EXIT_MATN1_INPUT},
#' which ensures material numbers are standardized to 18 characters by
#' adding leading zeros.
#'
#' @examples
#' pa_matn1_input(c("123", "456789", "A123"))
#' # Returns: c("000000000000000123", "000000000000456789", "A123")
#'
#' @export
pa_matn1_input <- function(x) {
  # like SAP-CONVERSION_EXIT_MATN1_INPUT
  # only leftpad leading zero's in case it is a number
  is_num <- grepl("^[0-9]+$", x)
  ifelse(
    is_num,
    stringr::str_pad(string = x, width = width, side = "left", pad = "0"),
    x
  )
}

#' remove leading zeros
#'
#' This function removes leading zeros from a string, but only if the string is purely numeric.
#' If the string contains non-numeric characters, it will be returned unchanged.
#'
#' @param x A character vector. Each element will be checked to see if it is purely numeric.
#'
#' @return A character vector with leading zeros removed for numeric strings.
#'         Non-numeric strings will be returned unchanged.
#'
#' @details
#' This function mimics the behavior of SAP's \code{CONVERSION_EXIT_MATN1_OUTPUT}
#' which removes leading zeros from material numbers if these only contain numbers.
#'
#' @examples
#' pa_matn1_output("00123")     # Returns "123"
#' pa_matn1_output("00000A123") # Returns "00000A123"
#' pa_matn1_output(c("00123", "00000A123", "A000045")) # Returns c("123", "abc123", "A000045")
#'
#' @export
pa_matn1_output <-
  function(x){
    # like SAP-CONVERSION_EXIT_MATN1_OUTPUT
    # only remove leading zero's in case it is a number
    is_num <- grepl("^[0-9]+$", x)
    ifelse(
      is_num,
      sub("^0*", "", x, perl = TRUE),
      x
    )
  }
