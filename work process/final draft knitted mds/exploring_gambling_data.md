exploring_gambling_data
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
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
library(jsonlite)
```

    ## 
    ## Attaching package: 'jsonlite'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

``` r
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
total_state_revenue = read_csv("data/total_state_revenue.csv")
```

    ## New names:
    ## Rows: 273 Columns: 7
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (1): State dbl (6): ...1, Year, Handle, Revenue, Hold, Taxes
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

## Some preliminary visuals

``` r
# Aggregate total revenue by year
yearly_revenue_summary = total_state_revenue |> 
  group_by(Year) |> 
  summarize(Total_Revenue = sum(Revenue, na.rm = TRUE))

yearly_revenue_summary |> 
  ggplot(aes(x = Year, y = Total_Revenue)) +
  geom_col(aes(fill = as.factor(Year)), show.legend = FALSE) +
  geom_text(aes(label = scales::dollar(Total_Revenue, scale = 1e-9, suffix = "B")), 
            vjust = -0.5) + # Add labels in Billions
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(
    title = "Total US Gambling Revenue (2018-2024)",
    subtitle = "Aggregated revenue across all states",
    x = "Year",
    y = "Total Revenue"
  )
```

<img src="exploring_gambling_data_files/figure-gfm/Plotting gambling revenue by Year-1.png" width="90%" />

``` r
# Identify top 5 states by 2024 revenue to keep things looking clean
top_states = total_state_revenue |> 
  filter(Year == 2019) |> 
  slice_max(Revenue, n = 5) |> 
  pull(State)

state_growth_plot_data = total_state_revenue |> 
  mutate(
    State_Group = ifelse(State %in% top_states, State, "All Other")
  ) |> 
  group_by(Year, State_Group) |> 
  summarize(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop") |> 
  mutate(State_Group = fct_reorder(State_Group, Revenue, .desc = TRUE))


ggplot(state_growth_plot_data, aes(x = Year, y = Revenue, fill = State_Group)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(
    title = "US Gambling Revenue Growth by State",
    x = "Year",
    y = "Revenue",
    fill = "State"
  )
```

<img src="exploring_gambling_data_files/figure-gfm/Plotting gambling revenue by State Over time-1.png" width="90%" />

``` r
# Calculate Ranks for every state, every year
ranked_data = total_state_revenue |> 
  group_by(Year) |> 
  mutate(Rank = min_rank(desc(Revenue))) |> 
  ungroup()

# Identify "States of Interest"
# We want to track any state that was Top 5 at any point
focus_states = ranked_data |> 
  filter(Rank <= 5) |>  # can change if we want less point to start and end points: filter((Year == 2018 & Rank <= 5) | (Year == 2024 & Rank <= 5)) |>
  pull(State) |> 
  unique()

# Filter the dataset to just those states
plot_data = ranked_data |> 
  filter(State %in% focus_states)

plot_data |> 
  ggplot(aes(x = Year, y = Rank, color = State, group = State)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.2) + 
  scale_y_reverse(breaks = 1:max(plot_data$Rank, na.rm = TRUE)) + 
  scale_x_continuous(breaks = 2018:2024) +
  geom_text(data = plot_data |> filter(Year == 2018), 
            aes(label = paste(Rank, State)), x = 2017.8, hjust = 1, size = 3) +
  geom_text(data = plot_data |> filter(Year == 2024), 
            aes(label = paste(Rank, State)), x = 2024.2, hjust = 0, size = 3) +
  labs(
    title = "Shift in Top Gambling Markets (2018 vs 2024)",
    subtitle = "Tracking the rank of states that were in the top 5 in either 2018 or 2024",
    y = "Revenue Rank",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",        
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(2017, 2025))
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

    ## Warning: Removed 7 rows containing missing values or values outside the scale range
    ## (`geom_text()`).

<img src="exploring_gambling_data_files/figure-gfm/plotting revenue rank: top 5 gambling markets by year since 2018-1.png" width="90%" />
