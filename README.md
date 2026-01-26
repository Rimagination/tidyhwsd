# tidyhwsd

Tidyverse-friendly access to the Harmonized World Soil Database (HWSD) v2.0.

## Installation (GitHub)

```r
# install.packages("remotes")
remotes::install_github("Rimagination/tidyhwsd")
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
  coords = c(120, 30),               # single point: c(lon, lat)
  param = c("SAND", "PH_WATER"),
  layer = "D1",
  ws_path = "D:/data/HWSD2"
)

# multiple points via data frame
sites <- data.frame(
  lon = c(120, 121.5),
  lat = c(30, 31.2)
)
pt_multi <- hwsd_extract(
  coords = sites,
  param = "SAND",
  layer = "D1",
  ws_path = "D:/data/HWSD2"
)
```

3) Bounding-box query (returns a SpatRaster; tiling speeds up large areas):
```r
sand <- hwsd_extract(
  bbox = c(70, 18, 140, 54),
  param = "SAND",
  layer = "D1",
  ws_path = "D:/data/HWSD2",
  tiles_deg = 5,
  cores = 4,
  internal = TRUE
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
