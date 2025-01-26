test_that("Initialization during R CMD check uses tempdir", {
  withr::with_envvar(c("R_CMD_CHECK" = "TRUE"), {
    .padt_env_initialize()
    expect_true(grepl(tempdir(), .padt_env$root_dir))
    expect_true(fs::dir_exists(fs::path(.padt_env$root_dir, "_config")))
    expect_true(fs::dir_exists(fs::path(.padt_env$root_dir, "sample_data")))
  })
})
