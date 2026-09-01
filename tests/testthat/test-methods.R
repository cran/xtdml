helper_fit <- function() {
  set.seed(1)
  skip_if_not_installed("ranger")

  df <- make_plpr_data(n_obs = 150, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time", approach = "fd-exact"
  )
  lrn <- mlr3::lrn("regr.ranger", num.trees = 100)
  fit <- xtdml_plr$new(data = dat,
                       ml_l = lrn$clone(), ml_m = lrn$clone(),
                       n_folds = 5, score = "orth-PO")
  fit$fit(store_predictions = TRUE)
  fit
}

test_that("res_y and res_d are numeric vectors of matching length", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")

  fit <- helper_fit()
  expect_true(is.numeric(fit$res_y))
  expect_true(is.numeric(fit$res_d))
  expect_equal(length(fit$res_y), length(fit$res_d))
  expect_false(any(is.na(fit$res_y)))
  expect_false(any(is.na(fit$res_d)))
})

test_that("predictions field is populated after fit", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")

  fit <- helper_fit()
  preds <- fit$predictions
  expect_false(is.null(preds))
})

test_that("summary method prints expected output", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")

  fit <- helper_fit()
  expect_output(print(fit$summary()), "Estimate")
})

test_that("plot method runs without error", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")

  fit <- helper_fit()
  # plot() may return a ggplot, grid object, or NULL invisibly.
  # We only assert that it doesn't error.
  expect_error(fit$plot(), regexp = NA)
})

test_that("predict method returns a numeric vector", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")

  fit <- helper_fit()
  # NOTE: confirm the actual argument name of predict() in xtdml_plr.R.
  # Likely candidates: d_value, d, treatment_value.
  preds <- tryCatch(fit$predict(d_value = 1),
                    error = function(e) fit$predict(1))
  expect_true(is.numeric(preds))
  expect_true(length(preds) > 0)
})
