
##S4 class for lagged correlation result
#' An S4 class that stores the lagged correlation result
#' @slot x x
#' @slot time1 time1
#' @slot y y
#' @slot time2 time2
#' @slot idx idx
#' @slot all_cor all_cor
#' @slot all_cor_p all_cor_p
#' @slot shift_time shift_time
#' @slot which_max_idx which_max_idx
#' @slot which_global_idx which_global_idx
#' @slot max_idx max_idx
#' @slot max_cor max_cor
#' @slot global_idx global_idx
#' @slot global_cor global_cor
#' @slot parameter parameter
#' @exportClass lagged_cor_result

setClass(
  Class = "lagged_cor_result",
  representation(
    x = "numeric",
    time1 = "POSIXct",
    y = "numeric",
    time2 = "POSIXct",
    idx = "list",
    all_cor = "numeric",
    all_cor_p = "numeric",
    shift_time = "character",
    which_max_idx = "numeric",
    which_global_idx = "numeric",
    max_idx = "list",
    max_cor = "numeric",
    global_idx = "list",
    global_cor = "numeric",
    parameter = "tidymass_parameter"
  )
)

setMethod(
  f = "show",
  signature = "lagged_cor_result",
  definition = function(object) {
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::yellow("Brief information", "\n"))
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("length of x:", length(object@x), "\n"))
    cat(crayon::green("length of y:", length(object@y), "\n"))
    cat(crayon::green("length of correlations:", length(object@idx), "\n"))
    cat(crayon::green("length of correlations:", length(object@idx), "\n"))
    cat(crayon::green("Max correlation index:", object@which_max_idx, "\n"))
    cat(crayon::green("Global correlation index:", object@which_global_idx, "\n"))
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::yellow("Parameters", "\n"))
    print(object@parameter)
  }
)
