
# tidyhwsd

Tidyverse-friendly access to the Harmonized World Soil Database (HWSD) v2.0.

## Installation

```r
remotes::install_local("D:/VSCODEPROJECTS/hwsd/tidyhwsd", upgrade = "never")
```

## Workflow

1) Download the HWSD index grid once (auto-creates the directory):
```r
library(tidyhwsd)
hwsd_download(ws_path = "D:/data/HWSD2", verbose = TRUE)
```

2) Point query (returns a tibble):
```r
pt <- hwsd_extract(
  location = c(120, 30),             # lon, lat
  param    = c("SAND", "PH_WATER"),
  layer    = "D1",
  ws_path  = "D:/data/HWSD2"
)
```

3) Bounding-box query (returns a SpatRaster; tiling speeds up large areas):
```r
sand <- hwsd_extract(
  location = c(70, 18, 140, 54),
  param    = "SAND",
  layer    = "D1",
  ws_path  = "D:/data/HWSD2",
  tiles_deg = 5,
  cores     = 4,
  internal  = TRUE
)
terra::plot(sand)
```

4) Discover available properties:
```r
hwsd_props()
head(names(hwsd2))
```

### Plotting
For ggplot2 workflows, use tidyterra (optional):
```r
# install.packages("tidyterra")
library(tidyterra)
ggplot() + tidyterra::geom_spatraster(data = sand)
```
