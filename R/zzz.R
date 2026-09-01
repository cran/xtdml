#' @import checkmate
#' @importFrom R6 R6Class
#' @importFrom mlr3 lrn rsmp msr resample default_measures Task TaskRegr
#'   TaskClassif
#' @import mlr3tuning
#' @importFrom mlr3learners LearnerRegrLM
#' @importFrom mlr3misc insert_named
#' @importFrom data.table data.table as.data.table setnafill
#' @importFrom readstata13 read.dta13
#' @importFrom stats formula model.matrix rnorm runif rexp toeplitz pnorm qnorm
#'   printCoefmat quantile p.adjust.methods p.adjust median complete.cases qqnorm qqline
#' @importFrom mvtnorm rmvnorm
#' @importFrom clusterGeneration genPositiveDefMat
#' @importFrom utils read.table compareVersion packageVersion
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by across all_of ungroup select summarise everything
#' @importFrom MLmetrics RMSE
#' @importFrom rlang .data
#' @importFrom graphics plot par abline mtext
utils::globalVariables(c("first_obs", ".time_gap", "n"))
NULL
