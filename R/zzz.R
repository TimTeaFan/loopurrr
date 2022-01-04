.onAttach <- function(libname, pkgname) {

  # check if in RStudio and if rstudioapi is available
  is_rstudio <- .Platform$GUI == "RStudio"
  has_rstudioapi <- requireNamespace("rstudioapi", quietly = TRUE)
  is_linux <- Sys.info()["sysname"] == "Linux"
  has_clipr <- requireNamespace("clipr", quietly = TRUE)

  msg <-
    if (is_rstudio && !has_rstudioapi && !has_clipr) { #
      paste0("The {rstudioapi} and the {clipr} package provide additional output options and are strongly recommended.\n",
             "Both packages are currently not installed.")
      } else if (is_rstudio && !has_rstudioapi && has_clipr) {
      paste0("When using {loopurrr} in RStudio the {rstudioapi} package provides additional output options.\n",
             "The {rstudioapi} package is currently not installed.")
      } else if (!is_rstudio && !has_clipr) {
      paste0("The {clipr} package is strongly recommended for additional output options.\n",
             "The {clipr} package is currently not installed.",
              if (is_linux) {
                "\n\nAs a linux user you might need to install xsel or xclip on your system (not in R) to use clipboard contents."}
            )
    } else NULL

  if(!is.null(msg)){
    packageStartupMessage(msg)
  }
}


