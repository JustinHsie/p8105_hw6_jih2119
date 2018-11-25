P8105\_hw6\_jih2119
================
Justin Hsie
11/21/2018

Setup

``` r
library(MASS)
library(tidyverse)
```

Tidy data

``` r
hom_data = read_csv("data/homicide-data.csv") %>% 
  unite(city_state, city:state, sep = ", ") %>% 
  mutate(solved = 
           if_else(disposition == "Closed by arrest", 1, 0)) 
  omit = c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL")
hom_data = filter(hom_data, !city_state %in% omit) %>% 
  mutate(victim_race = 
           if_else(victim_race == "White", "white", "non-white"),
         victim_race = fct_relevel(victim_race, "white"), 
         victim_age = as.numeric(victim_age))
```

``` r
balt_data = hom_data %>% 
  filter(city_state == "Baltimore, MD")
hom_glm_balt = 
  glm(solved ~ victim_age + victim_sex + victim_race, data = balt_data,
      family = binomial()) %>%
  broom::tidy(conf.int = TRUE) %>% 
  mutate(OR = exp(estimate)) %>% 
  rename(log_OR = estimate) %>% 
  knitr::kable()
hom_glm_balt
```

| term                  |     log\_OR|  std.error|  statistic|    p.value|    conf.low|   conf.high|         OR|
|:----------------------|-----------:|----------:|----------:|----------:|-----------:|-----------:|----------:|
| (Intercept)           |   1.1860305|  0.2346173|   5.055170|  0.0000004|   0.7304353|   1.6510016|  3.2740589|
| victim\_age           |  -0.0069900|  0.0032627|  -2.142423|  0.0321594|  -0.0134243|  -0.0006274|  0.9930344|
| victim\_sexMale       |  -0.8877869|  0.1360573|  -6.525097|  0.0000000|  -1.1557600|  -0.6218669|  0.4115656|
| victim\_racenon-white |  -0.8195997|  0.1746156|  -4.693738|  0.0000027|  -1.1642313|  -0.4785693|  0.4406080|
