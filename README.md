<!-- This comment enables badge extraction to pkgdown site -->

[![Travis-CI Build Status](https://travis-ci.com/IMCR-Hackathon/ggplotgui.svg?branch=master)](https://travis-ci.org/IMCR-Hackathon/datapie)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/IMCR-Hackathon/datapie?branch=master&svg=true)](https://ci.appveyor.com/project/IMCR-Hackathon/ggplotgui)
[![codecov.io](https://codecov.io/github/IMCR-Hackathon/datapie/coverage.svg?branch=master)](https://codecov.io/github/IMCR-Hackathon/ggplotgui?branch=master)

# datapie

Interactively explore the contents of a data package from the DataONE Network. Input the Digital Object Identifier (DOI) and get a variable-wise summary report and Shiny R Interface for basic plotting. This tool accelerates data reuse by enabling a deeper understanding than can be obtained from the metadata alone.

## Install

```
# Installation requires the remotes package
install.packages("remotes")
library(remotes)

# Install datapie
remotes::install_github('IMCR-Hackathon/datapie')
library(datapie)
```

## Run

See website articles for more information on:
* Finding a data package and loading the DOI
* Creating the summary report
* Creating static and interactive plots
* Description of `datapie` interface features

## Road map

* Add support for spatial data types.
* Supplement reports with metadata (if it exists).
* Develop guidelines for integrating this app into websites.

## Versioning

This project uses [semantic versioning](https://semver.org).

## Contributing

We welcome contributions! See our [Contributing Guidelines](https://github.com/IMCR-Hackathon/datapie/blob/master/CONTRIBUTING.md).

## Acknowledgements

This package was the product of the 2019 Information Management Code Registry Hackathon sponsored by the [Environmental Data Initiative](https://environmentaldatainitiative.org/).

We are very greatful to [Gert Stulp](https://www.gertstulp.com/) for the [ggplotgui](https://github.com/gertstulp/ggplotgui) package form which this repository is forked.

Dr Stulp's orginal acknowledgement follows:
> I am grateful to the people who made [R](https://www.r-project.org/), and to [Hadley Wicham](http://hadley.nz/) for making such good packages (and open access books describing them), that allow even low-skilled and low-talented programmers like myself to be able to contribute to R. This package makes use of: [ggplot2](http://ggplot2.tidyverse.org/), [Shiny](http://shiny.rstudio.com/), [stringr](http://stringr.tidyverse.org/), [plotly](https://plot.ly/r/), [readr](http://readr.tidyverse.org/), [readxl](http://readxl.tidyverse.org/), [haven](http://haven.tidyverse.org/), and [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf). Package development through [RStudio](https://www.rstudio.com/) and [Github](https://github.com/), and with the help of [R Markdown](http://rmarkdown.rstudio.com) and [devtools](https://www.rstudio.com/products/rpackages/devtools/). The code that allows for online data input was based somewhat on the [BoxPlotR Shiny app](https://github.com/VizWizard/BoxPlotR.shiny). Many thanks to Wilmer Joling who set up the illustrous "[docker](https://www.docker.com/)" and [website](https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/) where the online version of this packages runs.   
