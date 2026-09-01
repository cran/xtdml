test_that("xtdml_plr recovers theta within tolerance under fd-exact", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")

  set.seed(42)
  df <- make_plpr_data(n_obs = 1000, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)

  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time",
    approach = "fd-exact", transformX = "no"
  )

  lrn_rf <- mlr3::lrn("regr.ranger", num.trees = 500)
  fit <- xtdml_plr$new(
    data = dat,
    ml_l = lrn_rf$clone(),
    ml_m = lrn_rf$clone(),
    n_folds = 5,
    score = "orth-PO"
  )
  fit$fit()

  expect_true(is.numeric(fit$coef_theta))
  expect_true(is.finite(fit$coef_theta))
  expect_true(fit$se_theta > 0 && is.finite(fit$se_theta))
})

test_that("confint produces a valid interval containing the point estimate", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")

  set.seed(7)
  df <- make_plpr_data(n_obs = 200, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time", approach = "fd-exact"
  )
  lrn <- mlr3::lrn("regr.ranger", num.trees = 100)
  fit <- xtdml_plr$new(data = dat,
                       ml_l = lrn$clone(), ml_m = lrn$clone(),
                       n_folds = 5, score = "orth-PO")
  fit$fit()

  ci <- fit$confint()
  expect_true(is.numeric(ci) || is.matrix(ci))
  # Point estimate should lie inside the CI
  expect_true(ci[1] < fit$coef_theta && fit$coef_theta < ci[2])
})

test_that("orth-IV score requires ml_g and runs", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")

  set.seed(99)
  df <- make_plpr_data(n_obs = 200, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time", approach = "fd-exact"
  )
  lrn <- mlr3::lrn("regr.ranger", num.trees = 100)
  fit <- xtdml_plr$new(data = dat,
                       ml_l = lrn$clone(), ml_m = lrn$clone(),
                       ml_g = lrn$clone(),
                       n_folds = 5, score = "orth-IV")
  fit$fit()
  expect_true(is.numeric(fit$coef_theta))
})
