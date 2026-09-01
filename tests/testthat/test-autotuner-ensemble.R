# tests/testthat/test-autotuner-ensemble.R

test_that("xtdml tunes a standard learner with $tune()", {

  skip_if_not_installed("mlr3")
  skip_if_not_installed("paradox")
  skip_if_not_installed("mlr3pipelines")
  skip_if_not_installed("mlr3tuning")
  skip_if_not_installed("ranger")
  skip_if_not_installed("xgboost")

  library(mlr3)
  library(paradox)         # for ps, p_int, p_dbl
  library(mlr3pipelines)   # for gunion, po, %>>%, as_learner
  library(mlr3tuning)      # for auto_tuner, tnr, trm

  set.seed(123)

  # --------------------------------------------------------------------------
  # Data
  # --------------------------------------------------------------------------

  dat <- make_plpr_data(
    n_obs = 200,
    t_per = 5,
    dim_x = 10,
    theta = 1,
    rho = 0.5
  )

  x_cols <- paste0("X", 1:10)

  dml_data <- xtdml_data_from_data_frame(
    dat,
    y_col = "y",
    d_cols = "d",
    x_cols = x_cols,
    panel_id = "id",
    time_id = "time",
    approach = "fd-exact"
  )

  # --------------------------------------------------------------------------
  # Standard learner
  # --------------------------------------------------------------------------

  ml_l <- lrn("regr.ranger", num.trees = 100)
  ml_m <- lrn("regr.ranger", num.trees = 100)

  # --------------------------------------------------------------------------
  # xtdml object
  # --------------------------------------------------------------------------

  dml <- xtdml_plr$new(
    dml_data,
    ml_l = ml_l,
    ml_m = ml_m,
    n_folds = 3,
    score = "orth-PO"
  )

  # --------------------------------------------------------------------------
  # Tune through xtdml
  # --------------------------------------------------------------------------
  param_grid = list(
    ml_l = ps(num.trees = p_int(50, 100)),
    ml_m = ps(num.trees = p_int(50, 100))
  )
  tune_settings <- list(
    terminator = trm("evals", n_evals = 4),
    tuner = tnr("grid_search")
  )
  expect_no_error(
    dml$tune(
      param_set = param_grid,
      tune_settings = tune_settings
    )
  )

  # --------------------------------------------------------------------------
  # Check tuning and final estimation
  # --------------------------------------------------------------------------

  expect_true(!is.null(dml$params))

  expect_no_error(
    dml$fit()
  )

  expect_length(dml$coef_theta, 1)
  expect_length(dml$se_theta, 1)

  expect_true(is.finite(dml$coef_theta))
  expect_true(is.finite(dml$se_theta))
})


test_that("externally tuned ensemble works in xtdml", {

  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3pipelines")
  skip_if_not_installed("mlr3tuning")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("paradox")

  set.seed(123)

  # --------------------------------------------------------------------------
  # Data
  # --------------------------------------------------------------------------

  dat <- make_plpr_data(
    n_obs = 200,
    t_per = 5,
    dim_x = 10,
    theta = 1,
    rho = 0.5
  )

  x_cols <- paste0("X", 1:10)

  dml_data <- xtdml_data_from_data_frame(
    dat,
    y_col = "y",
    d_cols = "d",
    x_cols = x_cols,
    panel_id = "id",
    time_id = "time",
    approach = "fd-exact"
  )

  # --------------------------------------------------------------------------
  # Ensemble
  # --------------------------------------------------------------------------

  ensemble <- gunion(list(
    lrn("regr.cv_glmnet", s = "lambda.min"),
    lrn("regr.ranger", num.trees = 100),
    lrn("regr.xgboost", nrounds = 50)
  )) %>>%
    po("regravg")

  # Check the actual parameter IDs if necessary:
  # ensemble$param_set$ids()

  # --------------------------------------------------------------------------
  # Tuning search space
  # --------------------------------------------------------------------------

  search_space <- ps(
    `regr.ranger.num.trees` = p_int(50, 100),
    `regr.xgboost.max_depth` = p_int(2, 4)
  )

  # --------------------------------------------------------------------------
  # Create independent AutoTuners for l and m
  # --------------------------------------------------------------------------

  at_l <- AutoTuner$new(
    learner = ensemble$clone(deep = TRUE),
    resampling = rsmp("cv", folds = 3),
    measure = msr("regr.rmse"),
    search_space = search_space,
    terminator = trm("evals", n_evals = 4),
    tuner = tnr("grid_search", resolution = 2)
  )

  at_m <- AutoTuner$new(
    learner = ensemble$clone(deep = TRUE),
    resampling = rsmp("cv", folds = 3),
    measure = msr("regr.rmse"),
    search_space = search_space,
    terminator = trm("evals", n_evals = 4),
    tuner = tnr("grid_search", resolution = 2)
  )

  # --------------------------------------------------------------------------
  # Tasks for external tuning
  # --------------------------------------------------------------------------

  task_y <- TaskRegr$new(
    id = "test_y",
    backend = dat[, c("y", x_cols)],
    target = "y"
  )

  task_d <- TaskRegr$new(
    id = "test_d",
    backend = dat[, c("d", x_cols)],
    target = "d"
  )

  # --------------------------------------------------------------------------
  # Tune OUTSIDE xtdml
  # --------------------------------------------------------------------------

  expect_no_error(
    at_l$train(task_y)
  )

  expect_no_error(
    at_m$train(task_d)
  )

  expect_true(!is.null(at_l$tuning_instance))
  expect_true(!is.null(at_m$tuning_instance))

  # --------------------------------------------------------------------------
  # Pass externally tuned AutoTuners to xtdml
  # --------------------------------------------------------------------------

  dml <- xtdml_plr$new(
    dml_data,
    ml_l = at_l,
    ml_m = at_m,
    n_folds = 3,
    score = "orth-PO"
  )

  # IMPORTANT:
  # Do not call dml$tune().
  expect_no_error(
    dml$fit()
  )

  # --------------------------------------------------------------------------
  # Check final DML estimate
  # --------------------------------------------------------------------------

  expect_length(dml$coef_theta, 1)
  expect_length(dml$se_theta, 1)

  expect_true(is.numeric(dml$coef_theta))
  expect_true(is.numeric(dml$se_theta))

  expect_true(is.finite(dml$coef_theta))
  expect_true(is.finite(dml$se_theta))
})
