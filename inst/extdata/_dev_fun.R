
#' Get All Objects in the Package Namespace (Internal)
#'
#' Returns a data.table listing all objects in the package namespace, including
#' whether they are internal or external and whether they are functions.
#'
#' @return A data.table with columns:
#' \describe{
#'   \item{OBJ}{Character. The name of the object.}
#'   \item{ACC}{Character. Access level: "E" for exported (external) or "I" for internal.}
#'   \item{FUN}{Logical. TRUE if the object is a function, FALSE otherwise.}
#' }
#' @examples
#' .get_functions()
#' @keywords internal
.get_functions <- function() {
  # Ensure required packages are loaded
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but not installed. Please install it using install.packages('data.table').")
  }

  # Load data.table
  library(data.table)
  devtools::load_all()

  # Package name (change "padt" to your actual package name)
  package_name <- "padt"

  # Get all objects in the package namespace, including those starting with a dot
  all_objects <- ls(getNamespace(package_name), all.names = TRUE)

  # Create the data.table
  DT <- data.table(
    OBJ = all_objects,
    ACC = ifelse(all_objects %in% getNamespaceExports(package_name), "E", "I"),
    FUN = sapply(all_objects, function(obj) is.function(get(obj, envir = asNamespace(package_name))))
  )

  .open_as_xlsx(DT)
}

.open_as_xlsx <-
  function(pDT, pPath = tempdir(), pFN, pasTable = TRUE){

    library(lubridate)
    library(openxlsx)

    if (!dir.exists(pPath)) {
      dir.create(pPath)
    }

    if (missing(pFN) == TRUE) {
      pFN <- paste0("~", format(now(), "%Y%m%d-%H%M%S"), ".xlsx")
    }

    FFN <- file.path(pPath, pFN)
    write.xlsx(x = pDT, file = FFN, asTable = pasTable,
               tableStyle = "TableStyleMedium4")
    openXL(FFN)

  }
