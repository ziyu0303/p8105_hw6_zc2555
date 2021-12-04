P8105\_hw6\_zc2555
================
Ziyu Chen
12/4/2021

# r set up, lodaing packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
```

# Problem 1, Clean the dataset, recode certain variables into factors

-   As we can see that variables: babyset, frace, malform, mrace are
    categorical data so we need to tranform them into factors.

``` r
birthweight_df = 
  read_csv("data/birthweight.csv") %>%
  janitor::clean_names() %>%
  mutate(
    babysex = recode_factor(
      as_factor(babysex), 
      "1" = "male",
      "2" = "female"),
    frace = recode_factor(
      as_factor(frace), 
      "1" = "White", 
      "2" = "Black", 
      "3" = "Asian", 
      "4" = "Puerto Rican", 
      "8" = "Other", 
      "9" = "Unknown"),
     malform = recode_factor(
       as_factor(malform), 
       "1" = "present", 
       "0" = "absent"),
     mrace = recode_factor(
       as_factor(mrace), 
       "1" = "White", 
       "2" = "Black", 
       "3" = "Asian", 
       "4" = "Puerto Rican", 
       "8" = "Other"))
```

    ## Rows: 4342 Columns: 20

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The birth\_df is the cleaned dataset.

-   Fit a regression model

Since the outcome variable is birthweight, we might be interested in
looking at baby’s sex and it’s head circumference as well as it’s length
at birth. We also want to see the parental effect like father and
mother’s races and mother’s height on baby’s weight.

``` r
fit <- lm(bwt ~ babysex + bhead + blength + mheight + gaweeks + mrace + frace, data = birthweight_df)

fit %>% broom::tidy()
```

    ## # A tibble: 13 × 5
    ##    term              estimate std.error statistic   p.value
    ##    <chr>                <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)       -6505.      136.    -48.0    0        
    ##  2 babysexfemale        32.3       8.63    3.75   1.81e-  4
    ##  3 bhead               137.        3.50   39.1    7.62e-287
    ##  4 blength              78.4       2.04   38.4    1.98e-277
    ##  5 mheight              10.8       1.66    6.48   9.91e- 11
    ##  6 gaweeks              12.0       1.48    8.07   9.28e- 16
    ##  7 mraceBlack         -152.       46.9    -3.24   1.21e-  3
    ##  8 mraceAsian         -113.       73.2    -1.54   1.24e-  1
    ##  9 mracePuerto Rican   -74.2      46.1    -1.61   1.07e-  1
    ## 10 fraceBlack           35.3      47.0     0.751  4.53e-  1
    ## 11 fraceAsian           40.8      70.7     0.577  5.64e-  1
    ## 12 fracePuerto Rican   -21.9      45.5    -0.480  6.31e-  1
    ## 13 fraceOther           -3.00     75.6    -0.0398 9.68e-  1
