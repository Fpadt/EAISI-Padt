test_that("Test .di_csv_file_spec_get functionality", {

    # Ensure the environment is initialized
    .padt_env_initialize()

    # Test the functionality
    result <- .di_csv_file_spec_get()
    expect_true(!is.null(result))

})
