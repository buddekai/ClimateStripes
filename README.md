# ClimateStripes

R package to create diagrams that show the impact of climate change.
It started with an R function to plot Warming Stripes which were inspired by Ed Hawkins.
The R package can now fetch data from Deutscher Wetterdienst (DWD) and plot:
 - warming stripes (annual mean temperatur as colored bars)
 - 

## How to install the package and use it

In order to create all possible plots for the DWD station in Rostock, one needs to download the package and execute the following lines of code in R:

```R
library(devtools)
install()
require(ClimateStripes)
ClimateStripes(city.name = "rostock")
```

## How to create different climate plots

### Warming Stripes
```R
library(devtools)
install()
require(ClimateStripes)
ClimateStripes(city.name = "rostock", plot.what = "warmingstripes")
```
