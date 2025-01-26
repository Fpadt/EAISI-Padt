test_that("Debugging paths for initialization", {
  withr::with_envvar(c("R_CMD_CHECK" = "TRUE"), {
    .padt_env_initialize()

    # Build expected path
    expected_root <- fs::path(tempdir(), .PACKAGE_NAME)

    message("tempdir() = ", tempdir())
    message(".padt_env$root_dir = ", .padt_env$root_dir)

    # Check if the root directory is set correctly
    expect_equal(.padt_env$root_dir, expected_root)
    expect_true(fs::dir_exists(.padt_env$root_dir))
  })
})
