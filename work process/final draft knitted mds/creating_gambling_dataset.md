creating_gambling_dataset
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

## Data scraping gambling revenue and growth

``` r
url = "https://rg.org/statistics/us"
gambling_html = read_html(url)
```

``` r
gambling_html |>
  html_table()
```

    ## [[1]]
    ## # A tibble: 40 × 5
    ##    State       Handle          Revenue        Hold   Taxes       
    ##    <chr>       <chr>           <chr>          <chr>  <chr>       
    ##  1 Arizona     $7,959,647,194  $427,397,087   5.53%  $42,739,709 
    ##  2 Arkansas    -               $107,275,202   -      $13,945,776 
    ##  3 Colorado    $6,187,564,044  $475,176,731   7.87%  $31,934,112 
    ##  4 Connecticut $2,188,745,557  $187,349,516   8.67%  $25,760,559 
    ##  5 Delaware    $216,240,459    $14,585,100    6.99%  $8,474,813  
    ##  6 Florida     -               -              -      -           
    ##  7 Illinois    $14,016,684,933 $1,866,642,282 13.52% $280,390,692
    ##  8 Indiana     $5,212,332,663  $484,278,308   9.48%  $46,245,605 
    ##  9 Iowa        $2,770,339,580  $218,482,781   8.07%  $14,747,588 
    ## 10 Kansas      $2,503,492,581  $145,263,244   5.96%  $13,873,150 
    ## # ℹ 30 more rows
    ## 
    ## [[2]]
    ## # A tibble: 12 × 5
    ##    Month          Handle          Revenue        Hold   Taxes       
    ##    <chr>          <chr>           <chr>          <chr>  <chr>       
    ##  1 January 2025   $15,721,477,197 $1,650,645,417 11.21% $354,670,341
    ##  2 February 2025  $12,382,871,791 $1,239,102,150 9.69%  $262,962,206
    ##  3 March 2025     $15,481,330,121 $996,986,477   6.75%  $220,983,072
    ##  4 April 2025     $12,792,017,473 $1,164,871,656 9.35%  $260,844,090
    ##  5 May 2025       $12,787,489,120 $1,430,346,654 11.14% $325,897,450
    ##  6 June 2025      $10,010,350,813 $1,559,587,432 12.69% $285,024,313
    ##  7 July 2025      $8,811,387,460  $963,982,575   10.81% $208,715,680
    ##  8 August 2025    $10,765,611,897 $1,164,308,572 11.98% $254,251,721
    ##  9 September 2025 $15,438,117,768 $1,228,482,775 8.65%  $264,083,960
    ## 10 October 2025   $13,859,996,349 $1,185,962,986 11.70% $284,880,585
    ## 11 November 2025  -               -              -      -           
    ## 12 Total          -               -              -      -           
    ## 
    ## [[3]]
    ## # A tibble: 38 × 1
    ##    X1                                
    ##    <chr>                             
    ##  1 Arizona Sports Betting Revenue    
    ##  2 Arkansas Sports Betting Revenue   
    ##  3 Colorado Sports Betting Revenue   
    ##  4 Connecticut Sports Betting Revenue
    ##  5 Delaware Sports Betting Revenue   
    ##  6 Florida Sports Betting Revenue    
    ##  7 Illinois Sports Betting Revenue   
    ##  8 Indiana Sports Betting Revenue    
    ##  9 Iowa Sports Betting Revenue       
    ## 10 Kansas Sports Betting Revenue     
    ## # ℹ 28 more rows
    ## 
    ## [[4]]
    ## # A tibble: 3 × 3
    ##   Challenge             `Key Stat (2025)`                 Impact on Sports Bet…¹
    ##   <chr>                 <chr>                             <chr>                 
    ## 1 Regulatory Compliance 39 U.S. states + Washington D.C.… Increases operational…
    ## 2 Gambling Addiction    2.5M U.S. adults have severe gam… Drives demand for str…
    ## 3 High Tax Rates        51% GGR tax in NY, RI, NH         Limits profitability,…
    ## # ℹ abbreviated name: ¹​`Impact on Sports Betting Industry`
    ## 
    ## [[5]]
    ## # A tibble: 6 × 3
    ##   Trend                 `2025 Highlight`                         `What It Means`
    ##   <chr>                 <chr>                                    <chr>          
    ## 1 Mobile Dominance      Over 80% of online gambling via mobile   Convenience, a…
    ## 2 Bettor Engagement     20% of U.S. adults bet on sports         Growing user b…
    ## 3 Popular Formats       Parlays = 27% of bets                    Multi-leg bets…
    ## 4 Media Integration     ESPN BET’s FanCenter syncs fantasy with… Seamless conve…
    ## 5 Major Event Impact    Super Bowl handle hit $1.39B             Live events re…
    ## 6 Demographic Expansion Bettor participation rose to 26%, drive… Market broaden…
    ## 
    ## [[6]]
    ## # A tibble: 3 × 3
    ##   Technology                   `Use in Sports Betting`    Impact on Bettors & …¹
    ##   <chr>                        <chr>                      <chr>                 
    ## 1 Artificial Intelligence (AI) Analyzes player performan… Enables more accurate…
    ## 2 Machine Learning & Big Data  Processes massive dataset… Shifts betting from i…
    ## 3 Blockchain & Web3            Creates transparent, dece… Improves trust, autom…
    ## # ℹ abbreviated name: ¹​`Impact on Bettors & Industry`
    ## 
    ## [[7]]
    ## # A tibble: 5 × 3
    ##   Measure             Description                                         Impact
    ##   <chr>               <chr>                                               <chr> 
    ## 1 AI Risk Detection   Uses machine learning to identify harmful betting … Enabl…
    ## 2 Loss Limits         Caps on daily, weekly, or monthly spending          Preve…
    ## 3 Self-Exclusion      Lets bettors block access for set periods           Suppo…
    ## 4 Education Campaigns Public awareness on gambling risks                  Reduc…
    ## 5 Cooling-Off Periods Mandatory breaks between sessions                   Encou…

### pull in 2024 revenue by state table

``` r
state_revenue_24 = 
  gambling_html |> 
  html_table() |> 
  first() 
```

### pull in 2025 revenue growth by month

``` r
revenue_growth_25= 
  gambling_html |> 
  html_table() |> 
  purrr::pluck(2)
```

The website that was scrapped to obtain this data had dynamic filters
that switched the year of the displayed data. Due to a lack of static
data, we manually scraped the tables for previous years and saved the
data in a CSV format

``` r
# 2018 Data
revenue_growth_18 = read_csv("data/gambling_revenue_data/2018_monthly_revenue.csv")
```

    ## Rows: 8 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_revenue_18  = read_csv("data/gambling_revenue_data/2018_state_revenue.csv")
```

    ## Rows: 40 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): State
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 2019 Data
revenue_growth_19 = read_csv("data/gambling_revenue_data/2019_monthly_revenue.csv")
```

    ## Rows: 13 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_revenue_19  = read_csv("data/gambling_revenue_data/2019_state_revenue.csv")
```

    ## Rows: 40 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): State
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 2020 Data
revenue_growth_20 = read_csv("data/gambling_revenue_data/2020_monthly_revenue.csv")
```

    ## Rows: 13 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_revenue_20  = read_csv("data/gambling_revenue_data/2020_state_revenue.csv")
```

    ## Rows: 40 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): State
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 2021 Data
revenue_growth_21 = read_csv("data/gambling_revenue_data/2021_monthly_revenue.csv")
```

    ## Rows: 13 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_revenue_21  = read_csv("data/gambling_revenue_data/2021_state_revenue.csv")
```

    ## Rows: 40 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): State
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 2022 Data
revenue_growth_22 = read_csv("data/gambling_revenue_data/2022_monthly_revenue.csv")
```

    ## Rows: 13 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_revenue_22  = read_csv("data/gambling_revenue_data/2022_state_revenue.csv")
```

    ## Rows: 40 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): State
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 2023 Data
revenue_growth_23 = read_csv("data/gambling_revenue_data/2023_monthly_revenue.csv")
```

    ## Rows: 13 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_revenue_23  = read_csv("data/gambling_revenue_data/2023_state_revenue.csv")
```

    ## Rows: 40 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): State
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 2024 Data
revenue_growth_24 = read_csv("data/gambling_revenue_data/2024_monthly_revenue.csv")
```

    ## Rows: 13 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (4): Handle, Revenue, Hold, Taxes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### cleaning the url scraped data so that it matches data types with the manually scraped data

``` r
# --- Clean Scraped Data to match CSV format ---
# Function to remove '$', ',' and '%', convert to numeric and to handle '-' corresponding to missing data
clean_money_col = function(x) {
  # Remove currency symbols
  x_clean = gsub("[$,%]", "", x)
  # Replace dashes or empty strings with NA explicitly
  x_clean[x_clean == "-" | x_clean == ""] = NA
  as.numeric(x_clean)
}

# Clean the Scraped 2024 State Data to match CSV data types
state_revenue_24_clean = state_revenue_24 |> 
  mutate(
    across(c(Handle, Revenue, Taxes, Hold), clean_money_col), # Convert money columns
  )

# Clean the Scraped 2025 Monthly Data to match CSV data types
revenue_growth_25_clean = revenue_growth_25 |> 
  mutate(
    across(c(Handle, Revenue, Taxes, Hold), clean_money_col)
  )
```

``` r
# --- Create Master State Revenue File ---
total_state_revenue_raw = bind_rows(
  state_revenue_18 |> mutate(Year = 2018, Hold = Hold * 100),
  state_revenue_19 |> mutate(Year = 2019, Hold = Hold * 100),
  state_revenue_20 |> mutate(Year = 2020, Hold = Hold * 100),
  state_revenue_21 |> mutate(Year = 2021, Hold = Hold * 100),
  state_revenue_22 |> mutate(Year = 2022, Hold = Hold * 100),
  state_revenue_23 |> mutate(Year = 2023, Hold = Hold * 100),
  state_revenue_24_clean |> mutate(Year = 2024)
  ) |> 
  relocate(Year, .before = State) # Move Year to the front

# Filter out individual yearly "Total" rows
total_state_revenue = total_state_revenue_raw |> 
  filter(State != "Total") |> 
  relocate(Year, .before = State)

# Calculate the True Grand Total
state_grand_total = total_state_revenue |> 
  summarize(
    Year = NA, # Grand total spans all years
    State = "Total",
    Handle = sum(Handle, na.rm = TRUE),
    Revenue = sum(Revenue, na.rm = TRUE),
    Taxes = sum(Taxes, na.rm = TRUE),
    Hold = sum(Revenue, na.rm = TRUE) / sum(Handle, na.rm = TRUE)
  )

# Bind the Grand Total to the clean data
final_state_revenue = bind_rows(total_state_revenue, state_grand_total)

# --- Create Master Monthly Revenue File ---
total_revenue_growth_raw = bind_rows(
  revenue_growth_18 |> mutate(Hold = Hold * 100),
  revenue_growth_19 |> mutate(Hold = Hold * 100),
  revenue_growth_20 |> mutate(Hold = Hold * 100),
  revenue_growth_21 |> mutate(Hold = Hold * 100),
  revenue_growth_22 |> mutate(Hold = Hold * 100),
  revenue_growth_23 |> mutate(Hold = Hold * 100),
  revenue_growth_24 |> mutate(Hold = Hold * 100),
  revenue_growth_25_clean
)

# Filter out individual yearly "Total" rows
total_revenue_growth = total_revenue_growth_raw |> 
  filter(Month != "Total")

# Calculate the True Grand Total
monthly_grand_total = total_revenue_growth |> 
  summarize(
    Month = "Total",
    Handle = sum(Handle, na.rm = TRUE),
    Revenue = sum(Revenue, na.rm = TRUE),
    Taxes = sum(Taxes, na.rm = TRUE),
    Hold = (sum(Revenue, na.rm = TRUE) / sum(Handle, na.rm = TRUE)) * 100
  )

# Bind the Grand Total to the clean data
final_revenue_growth = bind_rows(total_revenue_growth, monthly_grand_total)
```

## saving relevant datasets

``` r
write.csv(total_state_revenue, "data/total_state_revenue.csv")
write.csv(final_state_revenue, "data/final_state_revenue.csv")
```
