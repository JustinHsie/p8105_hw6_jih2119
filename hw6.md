P8105\_hw6\_jih2119
================
Justin Hsie
11/21/2018

Setup

``` r
library(tidyverse)
```

Tidy data

``` r
hom_data = read_csv("data/homicide-data.csv") %>% 
  unite(city_state, city:state, sep = ", ") %>% 
  mutate(solved = 
           if_else(str_detect(disposition, "Open"), 1, 0)) %>% 
  filter(city_state != "Dallas, TX", 
         city_state != "Phoenix, AZ", 
         city_state != "Kansas City, MO", 
         city_state != "Tulsa, AL") %>% 
  mutate(victim_race = 
           if_else(victim_race == "White", "white", "non-white")) %>% 
  mutate(victim_age = as.numeric(victim_age))
```

``` r
balt_data = hom_data %>% 
  filter(city_state == "Baltimore, MD")
hom_glm_balt = 
  glm(solved ~ victim_age + victim_sex + victim_race, data = balt_data)
broom::tidy(hom_glm_balt)
```

    ## # A tibble: 4 x 5
    ##   term             estimate std.error statistic  p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)       0.336    0.0391        8.59 1.38e-17
    ## 2 victim_age        0.00122  0.000739      1.66 9.75e- 2
    ## 3 victim_sexMale    0.252    0.0322        7.84 6.40e-15
    ## 4 victim_racewhite -0.204    0.0409       -4.99 6.48e- 7
