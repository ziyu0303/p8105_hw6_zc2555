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
