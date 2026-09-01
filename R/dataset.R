#' @title Simulated Data Frame for the `xtdml` Package
#' @name make_plpr_data
#'
#' @description
#' Generates data from a partially linear panel regression model with fixed
#' effects, following the setting of Clarke and Polselli (2025) but with a
#' tunable data-generating process designed to expose the differences among
#' the panel data approaches implemented in `xtdml`.
#'
#' The DGP is defined as
#'
#' \deqn{Y_{it} = 1 + \theta_0 D_{it} + g_0(X_{it}) + \alpha_i + U_{it},}
#' \deqn{D_{it} = m_0(X_{it}) + \gamma_i + V_{it},}
#'
#' where \eqn{U_{it}} and \eqn{V_{it}} are AR(1) idiosyncratic errors with
#' persistence \eqn{\phi_e} and stationary unit variance; \eqn{\alpha_i} is
#' the outcome-side fixed effect; and \eqn{\gamma_i} is the treatment-side
#' fixed effect. The two fixed effects are constructed as
#'
#' \deqn{\alpha_i = \sigma_\alpha (\rho F_i + \sqrt{1-\rho^2} E_i),}
#' \deqn{\gamma_i = \sigma_\gamma \left(\rho_\gamma \frac{\alpha_i}{\sigma_\alpha} + \sqrt{1-\rho_\gamma^2} H_i\right),}
#'
#' where \eqn{F_i, E_i, H_i \sim \mathcal{N}(0,1)} are mutually independent
#' unit-level draws. The parameter \eqn{\rho} controls the correlation
#' between \eqn{\alpha_i} and the individual mean of the covariates
#' \eqn{\bar{X}_i}, while \eqn{\rho_\gamma} controls the correlation between
#' the outcome- and treatment-side fixed effects: \eqn{\rho_\gamma = 0}
#' corresponds to uncorrelated random effects and \eqn{\rho_\gamma = 1} to a
#' strict fixed-effects specification.
#'
#' Covariates follow a unit-level AR(1) process with persistence \eqn{\phi_x}:
#'
#' \deqn{X_{it,j} = F_i + \phi_x (X_{i,t-1,j} - F_i) + \eta_{itj}, \quad \eta_{itj} \sim \mathcal{N}(0,1),}
#'
#' initialised from the stationary marginal at \eqn{t = 1}. Higher \eqn{\phi_x}
#' produces more persistent within-unit variation, which preserves signal in
#' the first-differenced covariates and favours the FD-exact approach.
#'
#' The nuisance functions are smooth polynomial and trigonometric functions
#' of the first four covariates:
#'
#' \deqn{m_0(X_{it}) = 0.5 \sin(X_{it,1}) + 0.3 X_{it,2} + 0.1 (X_{it,3}^2 - 1) + 0.15 X_{it,1} X_{it,4},}
#' \deqn{g_0(X_{it}) = 0.4 \cos(X_{it,1}) + 0.5 X_{it,3} + 0.1 (X_{it,4}^2 - 1) + 0.15 X_{it,2} X_{it,3}.}
#'
#' These forms are smooth (no threshold indicators) and can be recovered by
#' tree-based, penalised, and neural learners at moderate sample sizes.
#'
#' @param n_obs (`integer(1)`) \cr
#' The number of cross-sectional units \eqn{i} to simulate. Default is 500.
#'
#' @param t_per (`integer(1)`) \cr
#' The number of time periods \eqn{t} to simulate. Default is 8. Values below
#' 4 leave too few effective periods per unit under FD.
#'
#' @param dim_x (`integer(1)`) \cr
#' The total number of covariates. The first four are used in the nuisance
#' functions; any additional columns act as noise covariates. Default is 10.
#'
#' @param theta (`numeric(1)`) \cr
#' The true value of the structural (causal) parameter \eqn{\theta_0}.
#' Default is 0.5.
#'
#' @param rho (`numeric(1)`) \cr
#' Correlation between the outcome-side fixed effect \eqn{\alpha_i} and the
#' shared unit-level component \eqn{F_i} of the covariates. Controls the
#' strength of the endogeneity of \eqn{\alpha_i} with respect to
#' \eqn{\bar{X}_i}. Should lie in \eqn{[0, 1]}. Default is 0.7.
#'
#' @param rho_gamma (`numeric(1)`) \cr
#' Correlation between the treatment-side fixed effect \eqn{\gamma_i} and the
#' outcome-side fixed effect \eqn{\alpha_i}. \eqn{\rho_\gamma = 0} yields
#' uncorrelated random effects (under which pooled PLR is consistent), and
#' \eqn{\rho_\gamma = 1} yields a strict fixed-effects setting (under which
#' pooled PLR is inconsistent). Should lie in \eqn{[0, 1]}. Default is 0.
#'
#' @param phi_x (`numeric(1)`) \cr
#' AR(1) coefficient governing the within-unit persistence of the covariates.
#' Higher values (closer to 1) preserve identifying signal in
#' first-differenced covariates and favour the FD-exact approach. Should lie
#' in \eqn{[0, 1)}. Default is 0.4.
#'
#' @param phi_e (`numeric(1)`) \cr
#' AR(1) coefficient governing the within-unit persistence of the
#' idiosyncratic errors \eqn{U_{it}} and \eqn{V_{it}}. \eqn{\phi_e = 0}
#' yields IID errors (under which WG and CRE are efficient), while
#' \eqn{\phi_e \to 1} approaches a random-walk error structure (under which
#' FD is efficient). Should lie in \eqn{[0, 1)}. Default is 0.
#'
#' @param sigma_a (`numeric(1)`) \cr
#' Scale (standard deviation) of the outcome-side fixed effect \eqn{\alpha_i}.
#' Default is 2.
#'
#' @param sigma_g (`numeric(1)`) \cr
#' Scale (standard deviation) of the treatment-side fixed effect
#' \eqn{\gamma_i}. Default is 1.
#'
#' @param seed (`integer(1)` or `NULL`) \cr
#' Optional integer seed for reproducibility. If `NULL` (the default), the
#' current state of the random number generator is used.
#'
#' @return A `data.frame` with `n_obs * t_per` rows and the following columns:
#' \describe{
#'   \item{`id`}{unit identifier, integer in \eqn{\{1, \ldots, n\_obs\}}.}
#'   \item{`time`}{time identifier, integer in \eqn{\{1, \ldots, t\_per\}}.}
#'   \item{`X1, X2, ..., X<dim_x>`}{covariates.}
#'   \item{`y`}{outcome variable.}
#'   \item{`d`}{treatment variable.}
#' }
#'
#' @examples
#' # Default: uncorrelated random effects, IID errors
#' df <- make_plpr_data(n_obs = 500, t_per = 8, dim_x = 10,
#'                      theta = 0.5, rho_gamma = 0, seed = 1234)
#'
#' # Strict fixed effects with persistent errors: FD-exact should be efficient
#' df_fd <- make_plpr_data(n_obs = 500, t_per = 8, dim_x = 10,
#'                         theta = 0.5, rho_gamma = 1,
#'                         phi_e = 0.8, phi_x = 0.7, seed = 1234)
#'
#' # Correlated fixed effects, IID errors: pooled PLR is biased; CRE/WG efficient
#' df_cre <- make_plpr_data(n_obs = 500, t_per = 8, dim_x = 10,
#'                          theta = 0.5, rho_gamma = 0.5,
#'                          phi_e = 0, phi_x = 0.4, seed = 1234)
#'
#' @export
make_plpr_data <- function(
    n_obs      = 500,
    t_per      = 8,
    dim_x      = 10,
    theta      = 0.5,
    rho        = 0.7,
    rho_gamma  = 0,
    phi_x      = 0.4,
    phi_e      = 0,
    sigma_a    = 2,
    sigma_g    = 1,
    seed       = NULL
) {

  if (dim_x < 4) {
    stop("make_plpr_data() requires dim_x >= 4 because the nuisance ",
         "functions use the first four covariates.")
  }

  if (!is.null(seed)) set.seed(seed)
  nt = n_obs * t_per

  # --- Unit-level components ---------------------------------------------
  F_i = rnorm(n_obs)
  E_i = rnorm(n_obs)  # orthogonal component of alpha_i
  H_i = rnorm(n_obs)  # orthogonal component of gamma_i

  alpha_i = sigma_a * (rho * F_i + sqrt(1 - rho^2) * E_i)

  gamma_i = sigma_g * (rho_gamma * (alpha_i / sigma_a) + sqrt(1 - rho_gamma^2) * H_i)

  # --- Covariates: AR(1) within units around a unit-specific mean --------
  X = matrix(NA_real_, nrow = nt, ncol = dim_x)
  for (j in seq_len(dim_x)) {
    # Vectorise across units, loop only over t
    x_prev = F_i + rnorm(n_obs, sd = 1 / sqrt(1 - phi_x^2))
    for (t in seq_len(t_per)) {
      x_now = F_i + phi_x * (x_prev - F_i) + rnorm(n_obs)
      rows  = ((seq_len(n_obs) - 1) * t_per) + t
      X[rows, j] = x_now
      x_prev = x_now
    }
  }
  colnames(X) = paste0("X", seq_len(dim_x))

  # --- Nuisance functions: smooth, moderate dimensionality --------------
  m0 = 0.5 * sin(X[, 1]) +
    0.3 * X[, 2] +
    0.2 * (X[, 3]^2 - 1) / 2 +          # centred quadratic
    0.3 * (X[, 1] * X[, 4]) / 2

  g0 = 0.4 * cos(X[, 1]) +
    0.5 * X[, 3] +
    0.2 * (X[, 4]^2 - 1) / 2 +
    0.3 * (X[, 2] * X[, 3]) / 2

  # --- Idiosyncratic errors: optional AR(1) within units -----------------
  draw_ar1 = function(n_obs, t_per, phi) {
    if (phi == 0) {
      return(matrix(rnorm(n_obs * t_per), n_obs, t_per))
    }
    e = matrix(NA_real_, n_obs, t_per)
    e[, 1] = rnorm(n_obs, sd = 1 / sqrt(1 - phi^2))
    for (t in 2:t_per) {
      e[, t] = phi * e[, t - 1] + rnorm(n_obs)
    }
    e
  }
  U = as.vector(t(draw_ar1(n_obs, t_per, phi_e)))
  V = as.vector(t(draw_ar1(n_obs, t_per, phi_e)))

  # --- Assemble panel ----------------------------------------------------
  id     = rep(seq_len(n_obs), each  = t_per)
  time   = rep(seq_len(t_per), times = n_obs)
  alpha  = rep(alpha_i, each = t_per)
  gamma  = rep(gamma_i, each = t_per)

  d = m0 + gamma + V
  y = 1 + theta * d + g0 + alpha + U

  data = data.frame(id = id, time = time, X, y = y, d = d)
  return(data)
}
