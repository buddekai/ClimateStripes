README
================

## climateStripes

R package to create diagrams that show the impact of climate change. It
started with an R function to plot Warming Stripes, which were inspired
by Ed Hawkins. The R package fetches data from Deutscher
Wetterdienst (DWD) and is able to plot:

  - [Warming Stripes](#warming-stripes) (annual deviation from the
    average temperature in the years 1961-1990 as colored bars)
  - [Precipitation Stripes](#precipitation-stripes) (annual deviation
    from the average total precipitation in the years 1961-1990 as
    colored bars)
  - [Wind Speed Stripes](#wind-speed-stripes) (annual deviation from the
    average wind speed in the years 1961-1990 as colored bars)
  - [Sunshine Duration Stripes](#precipitation-stripes) (annual
    deviation from the average daily sunshine duration in the years
    1961-1990 as colored bars)

(Most recent additions may be found in the develop-branch.)

### How to install the package and use it

In order to create all possible plots for the DWD station in Rostock,
one needs to download the package and execute the following lines of
code in R:

``` r
library(devtools)
install()
require("climateStripes")
climateStripes(city.name = "rostock")
```

### Examples: How to create different climate plots

#### Warming Stripes

``` r
library(devtools)
install()
require("climateStripes")
climateStripes(city.name = "rostock", plot.what = "warmingstripes")
```

<img src="man/figures/WarmingStripes.png" width="100%" style="display: block; margin: auto;" /><img src="man/figures/WarmingPoints.png" width="100%" style="display: block; margin: auto;" />

#### Precipitation Stripes

``` r
library(devtools)
install()
require("climateStripes")
climateStripes(city.name = "rostock", plot.what = "precipitationstripes")
```

<img src="man/figures/PrecipitationStripes.png" width="100%" style="display: block; margin: auto;" /><img src="man/figures/PrecipitationPoints.png" width="100%" style="display: block; margin: auto;" />

#### Wind Speed Stripes

``` r
library(devtools)
install()
require("climateStripes")
climateStripes(city.name = "rostock", plot.what = "windspeedstripes")
```

<img src="man/figures/WindspeedStripes.png" width="100%" style="display: block; margin: auto;" /><img src="man/figures/WindspeedPoints.png" width="100%" style="display: block; margin: auto;" />

#### Sunshine Duration Stripes

``` r
library(devtools)
install()
require("climateStripes")
climateStripes(city.name = "rostock", plot.what = "sunshinedurationstripes")
```

<img src="man/figures/SunshineStripes.png" width="100%" style="display: block; margin: auto;" /><img src="man/figures/SunshinePoints.png" width="100%" style="display: block; margin: auto;" />
