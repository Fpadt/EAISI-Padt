test_that("Test .di_csv_file_spec_get functionality", {

    # Ensure the environment is initialized
    .su_padt_initialize()

    # Test the functionality
    result <- .di_csv_file_spec_get()
    expect_true(!is.null(result))

})
