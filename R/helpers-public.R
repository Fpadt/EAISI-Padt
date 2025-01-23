#' Left Pad Material Numbers
#'
#' Pads material numbers with leading zeros to match the standard 18-character SAP material number format.
#'
#' @param x A character vector of material numbers. Only numeric-like strings will be padded.
#'
#' @return A character vector where numeric strings are left-padded with zeros to a width of 18. Non-numeric strings remain unchanged.
#'
#' @details
#' This function mimics the behavior of SAP's \code{CONVERSION_EXIT_MATN1_INPUT}, which ensures material numbers are standardized to 18 characters by adding leading zeros.
#'
#' @examples
#' MATN1(c("123", "456789", "A123"))
#' # Returns: c("000000000000000123", "000000000000456789", "A123")
#'
#' @export
MATN1 <- function(x) {
  .LP0(x, 18)
}
