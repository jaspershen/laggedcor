#' @title Show the logo laggedcor.
#' @description The laggedcor logo, using ASCII or Unicode characters
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#' on UTF-8 platforms.
#' @return A ASCII log of laggedcor
#' @export
#' @importFrom dplyr filter mutate select group_by case_when
#' @importFrom tibble tibble
#' @importFrom rlang is_installed
#' @import ggplot2
#' @importFrom plyr dlply .
#' @importFrom utils head tail
#' @importFrom stats cor.test dnorm end loess predict start time
#' @importFrom magrittr %>%
#' @importFrom BiocParallel SnowParam MulticoreParam
#' @import ggrepel
#' @import ggsci
#' @import lubridate
#' @import scales
#' @importFrom methods new
#' @importClassesFrom massdataset tidymass_parameter
#' @examples
#' laggedcor_logo()

laggedcor_logo <- function(unicode = l10n_info()$`UTF-8`) {
  cat(crayon::green("Thank you for using laggedcor!\n"))
  cat(crayon::green("Version 0.0.1 (2021-12-24)\n"))
  cat(crayon::green("More information: searching 'jaspershen laggedcor'.\n"))
  
  logo =
    c(
      "  _                            _  _____           ",
      " | |                          | |/ ____|          ",
      " | | __ _  __ _  __ _  ___  __| | |     ___  _ __ ",
      " | |/ _` |/ _` |/ _` |/ _ \\/ _` | |    / _ \\| '__|",
      " | | (_| | (_| | (_| |  __/ (_| | |___| (_) | |   ",
      " |_|\\__,_|\\__, |\\__, |\\___|\\__,_|\\_____\\___/|_|   ",
      "           __/ | __/ |                            ",
      "          |___/ |___/                             "
    )
  
  
  hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
  if (unicode)
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]
  
  cols <- c(
    "red",
    "yellow",
    "green",
    "magenta",
    "cyan",
    "yellow",
    "green",
    "white",
    "magenta",
    "cyan"
  )
  
  col_hexa <- purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))
  
  
  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }
  
  structure(crayon::blue(logo), class = "laggedcor_logo")
}

#' @export

print.laggedcor_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
