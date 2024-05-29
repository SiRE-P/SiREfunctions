# SiREfunctions

Package that includes functions and data for use across projects

### Installation


```r
# install.packages("devtools")
devtools::install_github("SiRE-P/SiREfunctions")
```

### Functions
Here is a list of functions included in this package:

| **Function** | **Description** |
| :---         |    :----:       |
| pt_theme    | This function applies a custom theme to a ggplot object. It is based on theme_bw(), but with grid lines removed and facet boxes white with no border. |
| geom_split_violin    | Create a split violin plot |
| unscale | This function takes data on a standardized (mean = 0, sd = 1) scale and converts it back to the original scale. This is useful for converting predictions from models that used standardized variables back to their original scale. | 


### Data
Here is a list of data included in this package:

| **Data** | **Description** |
| :---         |    :----:       |
| BC_coast_low_res    | A low resolution sf polygon shape file for the BC coastline |
