.onAttach <- function(libname, pkgname) {

  # check
  is_rstudio <- .Platform$GUI == "RStudio"
  has_rstudioapi <- requireNamespace("rstudioapi", quietly = TRUE)
  is_linux <- Sys.info()["sysname"] == "Linux"
  has_clipr <- requireNamespace("clipr", quietly = TRUE)
  has_styler <- requireNamespace("styler", quietly = TRUE)
  output_opt <- get_output_opt()
  # it makes no sense to recommend styler when the default output option is not rstudio
  not_rstudio_opt <- !is.null(output_opt) && !"rstudio" %in% output_opt

  msg <-
    if (is_rstudio && !has_rstudioapi && !has_clipr) {
        paste0("The {rstudioapi} and the {clipr} package are highly recommended for additional output options.\n",
               "Both packages are currently not installed.")
      } else if (is_rstudio && !has_rstudioapi && has_clipr) {
        paste0("The {rstudioapi} package is highly recommended for additional output options.\n",
               "The {rstudioapi} package is currently not installed.")
      } else if (is_rstudio && has_rstudioapi && !has_styler && !not_rstudio_opt) {
        paste0("The {styler} package will improve the indentation of the output and is highly recommended.\n",
               "The {styler} package is currently not installed.")
      } else if (!is_rstudio && !has_clipr) {
        paste0("The {clipr} package is highly recommended for additional output options.\n",
               "The {clipr} package is currently not installed.",
              if (is_linux) {
                "\n\nAs a linux user you might need to install xsel or xclip on your system (not in R) to use clipboard contents."}
            )
    } else NULL

  if(!is.null(msg)){
    packageStartupMessage(msg)
  }
}


