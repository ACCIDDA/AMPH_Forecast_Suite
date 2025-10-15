#' Install Forecasting Packages
#'
#' This function helps install all the necessary packages for building an
#' infectious disease forecasting pipeline and hub, including hubverse packages,
#' fable, EpiEstim, EpiNow2, epidatr, and epiforecast packages.
#'
#' @param install_delphi Logical. If TRUE, installs delphi packages. Default is TRUE.
#' @param install_hubverse Logical. If TRUE, installs hubverse packages. Default is TRUE.
#' @param install_forecasting Logical. If TRUE, installs forecasting packages (fable, etc.). Default is TRUE.
#' @param install_epi Logical. If TRUE, installs epidemiological packages (EpiEstim, EpiNow2). Default is TRUE.
#' @param install_data Logical. If TRUE, installs data access packages (epidatr). Default is TRUE.
#' @param repos Character. Repository to install packages from. Default is "https://cloud.r-project.org".
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Install all packages
#' install_forecast_packages()
#'
#' # Install only hubverse packages
#' install_forecast_packages(install_forecasting = FALSE,
#'                          install_epi = FALSE,
#'                          install_data = FALSE)
#' }
install_forecast_packages <- function(install_delphi = TRUE,
                                      install_hubverse = TRUE,
                                      install_forecasting = TRUE,
                                      install_epi = TRUE,
                                      install_data = TRUE,
                                      install_nowcast = TRUE,
                                      repos = "https://cloud.r-project.org") {

  # general packages
  general_packages <- c("remotes", "jsonlite", "tidyverse", "viridisLite", "scales", "gitcreds")

  # Try to install from CRAN first, then from GitHub if not available
  for (pkg in general_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        utils::install.packages(pkg, repos = repos)
      }, error = function(e) {
        message(sprintf("Package %s not on CRAN, try GitHub.", pkg))
      })
    }
  }


  # Hubverse packages
  if (install_hubverse) {
    message("Installing hubverse packages...")
    hubverse_pkgs <- c("hubData", "hubUtils", "hubVis", "hubEnsembles",
                       "hubVis", "scoringutils")
    # other packages: "hubAdmin", "hubEvals", "hubValidations",

    # Try to install from CRAN first, then from GitHub if not available
    for (pkg in hubverse_pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        tryCatch({
          utils::install.packages(pkg, repos = repos)
        }, error = function(e) {
          message(sprintf("Package %s not on CRAN, trying GitHub...", pkg))
          if (requireNamespace("remotes", quietly = TRUE)) {
            remotes::install_github(paste0("hubverse-org/", pkg))
          } else {
            message("Please install 'remotes' package to install from GitHub")
          }
        })
      }
    }
  }

  # Forecasting packages (fable ecosystem)
  if (install_forecasting) {
    message("Installing forecasting packages...")
    forecasting_pkgs <- c("fable", "fabletools", "tsibble", "tsibbledata", "forecast")

    for (pkg in forecasting_pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        utils::install.packages(pkg, repos = repos)
      }
    }
  }


  # Delphi data packages
  if (install_delphi) {
    message("Installing epidemiological packages...")
    epi_pkgs <- c("epiprocess", "epipredict")

    for (pkg in epi_pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        tryCatch({
          remotes::install_github(paste0("cmu-delphi/", pkg), dependencies = TRUE)
        }, error = function(e) {
          message(sprintf("Package %s not on github, trying alternative sources...", pkg))
        })
      }
    }
  }


  # Epidemiological modeling packages
  if (install_epi) {
    message("Installing epidemiological packages...")
    epi_pkgs <- c("EpiEstim", "EpiNow2", "epipredict")

    for (pkg in epi_pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        tryCatch({
          utils::install.packages(pkg, repos = repos)
        }, error = function(e) {
          message(sprintf("Package %s not on CRAN, trying alternative sources...", pkg))
        })
      }
    }
  }

  # Data access packages
  if (install_data) {
    message("Installing data access packages...")
    data_pkgs <- c("epidatr","epiprocess")

    for (pkg in data_pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        tryCatch({
          utils::install.packages(pkg, repos = repos)
        }, error = function(e) {
          message(sprintf("Package %s not on CRAN, trying GitHub...", pkg))
          if (requireNamespace("remotes", quietly = TRUE)) {
            remotes::install_github(paste0("cmu-delphi/", pkg))
          }
        })
      }
    }
  }

  if (install_nowcast) {
    message("Installing nowcasting packages...")
    install.packages("epinowcast", repos = "https://epinowcast.r-universe.dev")
    remotes::install_github("epinowcast/baselinenowcast")
  }

  message("Package installation complete!")
  invisible(NULL)
}
