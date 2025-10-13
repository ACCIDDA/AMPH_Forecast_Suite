# AMPHForecastSuite 0.1.0

## Initial Release

* Initial release of AMPH Forecast Suite package
* Added `install_forecast_packages()` function to install all required forecasting packages
* Added data conversion functions between hub format and other packages:
  - Fable/tsibble converters (`convert_hub_to_fable()`, `convert_fable_to_hub()`, etc.)
- EpiEstim converters (`convert_hub_to_epiestim()`, `convert_epiestim_to_hub()`)
- EpiNow2 converters (`convert_hub_to_epinow2()`, `convert_epinow2_to_hub()`)
* Support for installing hubverse packages (hubData, hubUtils, hubValidations, hubVis, hubAdmin)
* Support for forecasting packages (fable, fabletools, feasts, tsibble)
* Support for epidemiological packages (EpiEstim, EpiNow2, epipredict)
* Support for data access packages (epidatr)
