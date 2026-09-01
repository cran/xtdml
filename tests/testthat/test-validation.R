# ---- Existing input validation -----------------------------------------

test_that("missing panel_id triggers an error", {
  df <- make_plpr_data(n_obs = 50, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
      panel_id = NULL, time_id = "time",
      approach = "fd-exact"
    ),
    regexp = "panel_id"
  )
})

test_that("missing time_id triggers an error", {
  df <- make_plpr_data(n_obs = 50, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
      panel_id = "id", time_id = NULL,
      approach = "fd-exact"
    ),
    regexp = "time_id"
  )
})

test_that("missing or invalid approach triggers an error", {
  df <- make_plpr_data(n_obs = 50, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
      panel_id = "id", time_id = "time", approach = NULL
    ),
    regexp = "approach"
  )
  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
      panel_id = "id", time_id = "time", approach = "fixed-effects"
    )
  )
})

test_that("y_col cannot also appear in x_cols", {
  df <- make_plpr_data(n_obs = 50, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("y", "X1"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    ),
    regexp = "outcome variable"
  )
})

test_that("d_cols cannot also appear in x_cols", {
  df <- make_plpr_data(n_obs = 50, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("d", "X1"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    ),
    regexp = "treatment"
  )
})

test_that("cre, wg-approx, pooled accept gapped time index", {
  df <- make_plpr_data(n_obs = 50, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  df_gap <- df[!(df$id == 1 & df$time == 3), ]

  for (app in c("cre", "wg-approx", "pooled")) {
    expect_error(
      xtdml_data_from_data_frame(
        df_gap, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
        panel_id = "id", time_id = "time", approach = app
      ),
      regexp = NA,
      info   = sprintf("approach = '%s'", app)
    )
  }
})

## ---- Time structure checks (fd-exact): duplicates, non-increasing, irregular gaps ----

test_that("fd-exact errors on duplicate panel_id x time_id observations", {
  df <- make_plpr_data(n_obs = 20, t_per = 5, dim_x = 5, theta = 0.5, rho = 0.5)
  # Duplicate one (id, time) pair
  dup_row <- df[df$id == 1 & df$time == 2, ][1, ]
  df <- rbind(df, dup_row)

  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    ),
    regexp = "Duplicate observations"
  )
})

test_that("fd-exact errors on non-increasing time index within a unit", {
  df <- make_plpr_data(n_obs = 20, t_per = 5, dim_x = 5, theta = 0.5, rho = 0.5)
  # Force a non-increasing time value for unit 1 (e.g. swap two time stamps)
  idx <- which(df$id == 1)
  df$time[idx[2]] <- df$time[idx[1]]  # duplicate value -> non-increasing after first hit

  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    ),
    regexp = "Duplicate observations|Non-increasing"
  )
})

test_that("fd-exact accepts regular gaps without warning", {
  df <- make_plpr_data(n_obs = 20, t_per = 5, dim_x = 5, theta = 0.5, rho = 0.5)
  # Keep every other period: 1, 3, 5, 7, 9 (regular gap of 2)
  df$time <- df$time * 2 - 1

  expect_no_warning(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    )
  )
})

test_that("fd-exact accepts irregular gaps (e.g. waves 1,3,5,7,8) with an informational warning", {
  df <- make_plpr_data(n_obs = 20, t_per = 5, dim_x = 5, theta = 0.5, rho = 0.5)
  # Recode the 5 periods per unit as irregular waves: 1, 3, 5, 7, 8
  wave_map <- c(1, 3, 5, 7, 8)
  df$time <- wave_map[df$time]

  expect_warning(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    ),
    regexp = "irregular spacing"
  )
})

test_that("fd-exact drops exactly one observation per unit (the first wave)", {
  df <- make_plpr_data(n_obs = 20, t_per = 5, dim_x = 5, theta = 0.5, rho = 0.5)
  wave_map <- c(1, 3, 5, 7, 8)
  df$time <- wave_map[df$time]
  n_units <- length(unique(df$id))

  dat <- suppressWarnings(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    )
  )

  expect_equal(nrow(dat$data_model), nrow(df) - n_units)
})

test_that("fd-exact errors informatively on missing values after differencing", {
  df <- make_plpr_data(n_obs = 20, t_per = 5, dim_x = 5, theta = 0.5, rho = 0.5)
  # Introduce an NA in an interior period for one unit
  idx <- which(df$id == 1 & df$time == 3)
  df$X1[idx] <- NA

  expect_error(
    xtdml_data_from_data_frame(
      df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
      panel_id = "id", time_id = "time", approach = "fd-exact"
    ),
    regexp = "Missing values detected"
  )
})

test_that("binary D status is flagged correctly", {
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  df$d <- as.numeric(df$d > median(df$d))

  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time", approach = "fd-exact"
  )
  expect_true(isTRUE(attr(dat, "d_binary_original")))
})

test_that("continuous D is not flagged as binary", {
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time", approach = "fd-exact"
  )
  expect_false(isTRUE(attr(dat, "d_binary_original")))
})

test_that("classification learner with fd-exact + binary D errors", {
  skip_if_not_installed("mlr3learners")

  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 5,
                       theta = 0.5, rho = 0.5)
  df$d <- as.numeric(df$d > median(df$d))

  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = paste0("X", 1:5),
    panel_id = "id", time_id = "time", approach = "fd-exact"
  )

  expect_error(
    xtdml_plr$new(
      data  = dat,
      ml_l  = mlr3::lrn("regr.ranger"),
      ml_m  = mlr3::lrn("classif.ranger")
    ),
    regexp = "classification learner"
  )
})

test_that("classification learner with cre + binary D is allowed", {
  skip_if_not_installed("mlr3learners")
  df <- make_plpr_data(n_obs = 100, t_per = 5, dim_x = 10, theta = 0.5, rho = 0.5)
  df$d <- as.numeric(df$d > median(df$d))
  dat <- xtdml_data_from_data_frame(
    df, y_col = "y", d_cols = "d", x_cols = c("X1", "X2"),
    panel_id = "id", time_id = "time", approach = "cre"
  )
  expect_error(
    xtdml_plr$new(data = dat, ml_l = mlr3::lrn("regr.ranger"), ml_m = mlr3::lrn("classif.ranger")),
    regexp = NA
  )
})

test_that("factor covariates trigger informative error", {

  df <- make_plpr_data(
    n_obs = 100, t_per = 5, dim_x = 5,
    theta = 0.5, rho = 0.5
  )

  df$region <- factor(sample(c("North", "South"), nrow(df), TRUE))

  expect_error(
    xtdml_data_from_data_frame(
      df,
      y_col = "y",
      d_cols = "d",
      x_cols = c("X1", "region"),
      panel_id = "id",
      time_id = "time",
      approach = "fd-exact"
    ),
    regexp = "numeric|factor|categorical"
  )
})

test_that("character covariates trigger informative error", {

  df <- make_plpr_data(
    n_obs = 100, t_per = 5, dim_x = 5,
    theta = 0.5, rho = 0.5
  )

  df$region <- sample(c("North", "South"), nrow(df), TRUE)

  expect_error(
    xtdml_data_from_data_frame(
      df,
      y_col = "y",
      d_cols = "d",
      x_cols = c("X1", "region"),
      panel_id = "id",
      time_id = "time",
      approach = "fd-exact"
    ),
    regexp = "numeric|character|categorical"
  )
})

test_that("mixed continuous and binary covariates are accepted", {

  df <- make_plpr_data(
    n_obs = 100, t_per = 5, dim_x = 5,
    theta = 0.5, rho = 0.5
  )

  df$x_binary <- rbinom(nrow(df), 1, 0.5)

  expect_error(
    xtdml_data_from_data_frame(
      df,
      y_col = "y",
      d_cols = "d",
      x_cols = c("X1", "X2", "x_binary"),
      panel_id = "id",
      time_id = "time",
      approach = "fd-exact"
    ),
    regexp = NA
  )
})

test_that("regression learner is accepted for binary treatment under cre", {
  skip_if_not_installed("mlr3learners")

  df <- make_plpr_data(
    n_obs = 100, t_per = 5, dim_x = 5,
    theta = 0.5, rho = 0.5
  )

  df$d <- as.numeric(df$d > median(df$d))

  dat <- xtdml_data_from_data_frame(
    df,
    y_col = "y",
    d_cols = "d",
    x_cols = paste0("X", 1:5),
    panel_id = "id",
    time_id = "time",
    approach = "cre"
  )

  expect_no_error(
    xtdml_plr$new(
      data = dat,
      ml_l = mlr3::lrn("regr.ranger"),
      ml_m = mlr3::lrn("regr.ranger")
    )
  )
})
