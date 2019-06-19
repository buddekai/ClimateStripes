# WarmingStripes

R package to create a Warming Stripes plots which are inspired by Ed Hawkins.

## How to install the package and use it

In order to create a warming stripes plot of Rostock (example data), one needs to download the package and execute the following lines of code in R:

```R
library(devtools)
install()
require(warmingStripes)
input_file <- "inst/data/data_TMK_MN004.csv"
warmingStripes(input_file = input_file)
```
