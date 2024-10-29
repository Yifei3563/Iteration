Iteration
================
Yifei Yu
2024-10-29

``` r
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  fig.width = 6,
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

load key packages.

``` r
library(tidyverse)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## writing my first function!!

as an example, here’s a z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.1179308 -0.7986500  0.6200376 -1.2880794  0.9481719 -1.3913998
    ##  [7] -1.6166405  1.3801688 -1.7644742  2.0538438  0.9275314  0.3388173
    ## [13]  0.8805310  0.8802576  0.6385616 -0.6175218  0.1166727 -0.9716027
    ## [19]  0.7767688 -0.4129037  0.4545319  0.4407382 -0.4340386 -0.3513241
    ## [25] -0.9279285

Now i’ll write a function to do this.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 5) {
    stop("you need at least five numbers to compute the z score")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.1179308 -0.7986500  0.6200376 -1.2880794  0.9481719 -1.3913998
    ##  [7] -1.6166405  1.3801688 -1.7644742  2.0538438  0.9275314  0.3388173
    ## [13]  0.8805310  0.8802576  0.6385616 -0.6175218  0.1166727 -0.9716027
    ## [19]  0.7767688 -0.4129037  0.4545319  0.4407382 -0.4340386 -0.3513241
    ## [25] -0.9279285

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
z_scores(x = c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numeric
