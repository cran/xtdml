test_that("xtdml_data_from_data_frame returns correct class and fields", {
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)

  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time",
    approach = "fd-exact", transformX = "no"
  )

  expect_s3_class(dat, "xtdml_data")
  expect_equal(dat$y_col,      "y")
  expect_equal(dat$d_cols,     "d")
  expect_equal(dat$approach,   "fd-exact")
  expect_equal(dat$transformX, "no")
  expect_equal(dat$panel_id,   "id")
  expect_equal(dat$time_id,    "time")
  expect_true(dat$n_obs > 0)
  expect_equal(dat$n_treat, 1L)
})

test_that("all four approaches construct without error", {
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)

  for (app in c("fd-exact", "wg-approx", "cre", "pooled")) {
    dat <- xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
      panel_id = "id", time_id = "time",
      approach = app, transformX = "no"
    )
    expect_s3_class(dat, "xtdml_data")
  }
})

test_that("all three transformX options work under fd-exact", {
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)

  for (tr in c("no", "minmax", "poly")) {
    dat <- xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
      panel_id = "id", time_id = "time",
      approach = "fd-exact", transformX = tr
    )
    expect_s3_class(dat, "xtdml_data")
    expect_true(length(dat$x_cols) > 0,
                info = sprintf("transformX = '%s' produced empty x_cols", tr))
  }
})

test_that("CRE approach attaches dbar_col", {
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time",
    approach = "cre", transformX = "no"
  )
  expect_false(is.null(dat$dbar_col))
  expect_match(dat$dbar_col, "^m_d$")
})

test_that("FD drops the first period for each unit", {
  n <- 50L; tt <- 5L
  df <- make_plpr_data(n_obs = n, t_per = tt, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
    panel_id = "id", time_id = "time",
    approach = "fd-exact", transformX = "no"
  )
  expect_equal(dat$n_obs, n * (tt - 1L))
})

test_that("print method runs and includes key fields", {
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time",
    approach = "fd-exact"
  )
  expect_output(dat$print(), "xtdml Object")
  expect_output(dat$print(), "Outcome variable")
  expect_output(dat$print(), "Panel data approach")
})
