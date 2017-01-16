merpApp
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

##### Shiny app to visualise CEFAS temperature data

merpApp is a very basic Shiny app demonstrating some of the functionalities contained in packages merpData and merpWS.

Running the app
---------------

merpApp can be run locally on a computer running R

``` r
library(shiny)
runGitHub("MarineEcosystemResearchProgramme/merpApp")
```

Development
-----------

Currently the app makes use of the merpWS package to access CEFAS temperature data. Its functionalities are limited to plotting the positions where the data was recorded and display a crude time series of temperature change for cells of a given within a given area. Further developments soon to come!
