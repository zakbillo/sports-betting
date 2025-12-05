exploring_demographic_data
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(patchwork)

knitr::opts_chunk$set(
fig.width = 6,
fig.asp = .6,
out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
ggplot2.continuous.colour = "viridis",
ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Import data

``` r
model_clean_2024 = read_csv("data/model_clean_2024.csv")
```

## Explore

Plot of Total Population in 2024 by State

``` r
population_plot = 
  model_clean_2024 |> 
    filter(State != "Washington D.C.") |> #drop DC bc all demographic data is NA
    mutate(
      State = as.factor(State),
      State = fct_reorder(State, total_pop, .desc = TRUE)
    ) |> 
    ggplot(aes(x = State, y = total_pop)) +
    geom_col(aes(fill = State)) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
      legend.position = "none") +
    labs(
        y = "Total Population",
        title = "Total Population by State in 2024"
      )
```

Plot of Gambling Revenue in 2024 by State

``` r
revenue_plot = 
  model_clean_2024 |> 
    filter(State != "Washington D.C.") |> #drop DC bc all demographic data is NA
    mutate(
      State = as.factor(State),
      State = fct_reorder(State, total_pop, .desc = TRUE)
    ) |> 
    ggplot(aes(x = State, y = Revenue)) +
    geom_col(aes(fill = State)) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
      legend.position = "none") +
    labs(
        x = "State (in order of largest to smallest total population)",
        y = "Revenue",
        title = "Gambling Revenue by State in 2024"
      )
```

Patchwork Plot of Total Population and Gambling Revenue in 2024 by State

``` r
combined_revenue_population = (population_plot + revenue_plot)

combined_revenue_population
```

<img src="exploring_demographic_data_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

Racial Makeup of States in 2024

``` r
model_clean_2024 |> 
  filter(State != "Washington D.C.") |> #drop DC bc all demographic data is NA
  select(State, Revenue, pct_white, pct_black, pct_asian, pct_hispanic) |> 
  pivot_longer(
    cols = pct_white:pct_hispanic,
    names_to = "race",
    values_to = "percentage"
  ) |> 
  mutate(
      State = as.factor(State),
      State = fct_reorder(State, Revenue, .desc = TRUE),
      race = factor(race,
                  levels = c("pct_white", "pct_black", "pct_asian", "pct_hispanic"),
                  labels = c("% White", "% Black", "% Asian", "% Hispanic"))
  ) |> 
  ggplot(aes(x = State, y = percentage, color = race, group = race)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
    labs(
        x = "State (in order of largest to smallest gambling revenue)",
        y = "Percentage of Population",
        title = "Racial Makeup by State in 2024",
        fill = "Race"
    ) 
```

<img src="exploring_demographic_data_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

% Poverty and Unemployment Rate by State in 2024

``` r
model_clean_2024 |> 
  filter(State != "Washington D.C.") |> #drop DC bc all demographic data is NA
  select(State, Revenue, unemployment_rate, pct_poverty) |> 
  pivot_longer(
    cols = c("pct_poverty", "unemployment_rate"),
    names_to = "measure",
    values_to = "percentage"
  ) |> 
  mutate(
      State = as.factor(State),
      State = fct_reorder(State, Revenue, .desc = TRUE),
      measure = factor(measure,
                  levels = c("pct_poverty", "unemployment_rate"),
                  labels = c("% in Poverty", "% Unemployed"))
  ) |> 
  ggplot(aes(x = State, y = percentage, color = measure, group = measure)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
    labs(
        x = "State (in order of largest to smallest gambling revenue)",
        y = "Percentage of Population",
        title = "% Poverty and Unemployment Rate by State in 2024",
        fill = "Measure"
    ) 
```

<img src="exploring_demographic_data_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />
