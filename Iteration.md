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

    ##  [1]  0.33962064  0.44854243 -0.38560364 -0.18332719  1.16713775 -0.12435916
    ##  [7] -0.09912559  2.13402346 -1.12649558  0.80864330  0.21412365 -0.76899131
    ## [13]  0.53942462 -0.22825111 -1.84541663 -0.56129571 -1.61811630 -0.02928343
    ## [19] -1.98723086  0.05559622  0.65926621  0.89022488  0.70156241 -0.53615218
    ## [25]  1.53548314

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

    ##  [1]  0.33962064  0.44854243 -0.38560364 -0.18332719  1.16713775 -0.12435916
    ##  [7] -0.09912559  2.13402346 -1.12649558  0.80864330  0.21412365 -0.76899131
    ## [13]  0.53942462 -0.22825111 -1.84541663 -0.56129571 -1.61811630 -0.02928343
    ## [19] -1.98723086  0.05559622  0.65926621  0.89022488  0.70156241 -0.53615218
    ## [25]  1.53548314

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
z_scores(x = c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numeric

## A new function!!!

``` r
mean_and_sd = function(x) {
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(out_df)
}
```

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.67  3.31

## Check stuff using a simulation

``` r
sim_df = 
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.4  4.12

Simulation function to check sample mean and sd.

``` r
sim_mean_sd = function(sample_size, true_mean, true_sd) {
  
  sim_df = 
  tibble(
    x = rnorm(sample_size, true_mean, true_sd)
  )

out_df = 
  sim_df |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )

return(out_df)
  
}

sim_mean_sd(sample_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.34  12.1

``` r
sim_mean_sd(true_sd = 12, sample_size = 30, true_mean = 4)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.68  14.8
