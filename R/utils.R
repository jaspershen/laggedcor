msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("laggedcor.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  
  theme <- rstudioapi::getThemeInfo()
  
  if (isTRUE(theme$dark))
    crayon::white(x)
  else
    crayon::black(x)
  
}

#' List all packages in the laggedcor
#'
#' @param include_self Include laggedcor in the list?
#' @export
#' @return laggedcor packages
#' @examples
#' laggedcor_packages()
laggedcor_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("laggedcor")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <-
    vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
  
  if (include_self) {
    names <- c(names, "laggedcor")
  }
  
  names
}

invert <- function(x) {
  if (length(x) == 0)
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}



base_theme =
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text =  ggplot2::element_text(size = 12),
    axis.title =  ggplot2::element_text(size = 13),
    panel.grid.minor =  ggplot2::element_blank(),
    plot.background =  ggplot2::element_rect(fill = "transparent"),
    panel.background =  ggplot2::element_rect(fill = "transparent"),
    strip.text =  ggplot2::element_text(size = 12)
  )


fitpeaks <- function(y, pos) {
  names(y) <- NULL
  tabnames <- c("rt", "sd", "FWHM", "height", "area")
  noPeaksMat <- matrix(rep(NA, 5),
                       nrow = 1,
                       dimnames = list(NULL, tabnames)) %>%
    as.data.frame()
  
  if (length(pos) == 0) {
    return(noPeaksMat)
  }
  
  fitpk <- function(xloc) {
    ## find all areas higher than half the current max
    peak.loc <- which(y > 0.2 * y[xloc])
    peak.loc.diff <- diff(peak.loc)
    boundaries <-
      c(0,
        which(diff(peak.loc) != 1),
        length(peak.loc) + 1)
    
    peaknrs <- rep(seq_along(boundaries),
                   c(boundaries[1], diff(c(boundaries))))
    peaknrs[boundaries[-1]] <- NA
    current.peak <- peaknrs[peak.loc == xloc]
    current.peak <- current.peak[!is.na(current.peak)]
    if (length(current.peak) == 0)
      return(rep(NA, 5))
    
    ## only retain those points adjacent to the current max
    FWHM <- diff(range(peak.loc[peaknrs == current.peak],
                       na.rm = TRUE))
    pksd <- FWHM / (2 * sqrt(2 * log(2)))
    
    c(
      rt = xloc,
      sd = pksd,
      FWHM = FWHM,
      height = y[xloc],
      area = y[xloc] / dnorm(x = xloc, mean = xloc, sd = pksd)
    )
  }
  
  huhn <- t(sapply(pos, fitpk))
  colnames(huhn) <- tabnames
  
  huhn
}



get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else {
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}