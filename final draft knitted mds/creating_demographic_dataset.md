creating_demographic_dataset
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
library(glmnet)
```

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-10

``` r
library(dplyr)
library(webshot2)
```

## Import Gambling Data

``` r
final_state_revenue = read_csv("data/final_state_revenue.csv")
```

    ## New names:
    ## Rows: 274 Columns: 7
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (1): State dbl (6): ...1, Year, Handle, Revenue, Hold, Taxes
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

## Census Info

``` r
library(tidycensus)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
vars_base = c(
  median_income = "B19013_001",
  total_pop = "B01003_001"
)

acs_base_2024 =
  get_acs(
    geography = "state",
    variables = vars_base,
    year = 2024,
    survey = "acs1"
  ) |>
  select(NAME, variable, estimate) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  clean_names()
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
# Pull raw ACS education data
educ_2024 = get_acs(
  geography = "state",
  variables = c(
    total = "B15003_001",    # total population age 25+
    ba1 = "B15003_022",      # BA degree
    ba2 = "B15003_023",      # Master's
    ba3 = "B15003_024",      # Professional degree
    ba4 = "B15003_025"       # Doctorate
  ),
  survey = "acs1",
  year = 2024
)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
# Clean + compute % bachelor’s degree or higher
educ_clean_2024 =
  educ_2024 |>
  group_by(NAME) |>
  summarize(
    total = estimate[variable == "total"],
    ba_plus = sum(estimate[variable %in% c("ba1", "ba2", "ba3", "ba4")]),
    .groups = "drop"
  ) |>
  mutate(pct_bachelors = ba_plus / total)
```

``` r
age_2024 = get_acs(
  geography = "state",
  variables = c(
    total = "B01001_001",
    male_20_34 = paste0("B01001_", sprintf("%03d", 8:10)),
    female_20_34 = paste0("B01001_", sprintf("%03d", 32:34))
  ),
  survey = "acs1",
  year = 2024
)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
age_clean_2024 =
  age_2024 |>
  mutate(
    group = case_when(
      variable == "total" ~ "total",
      str_starts(variable, "male_20_34") ~ "young",
      str_starts(variable, "female_20_34") ~ "young",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(group)) |>
  group_by(NAME, group) |>
  summarize(estimate = sum(estimate), .groups = "drop") |>
  pivot_wider(names_from = group, values_from = estimate) |>
  mutate(pct_age_20_34 = young / total)

median_age_2024 = get_acs(
  geography = "state",
  variables = c(median_age = "B01002_001"),
  survey = "acs1",
  year = 2024
  ) |>
  select(NAME, median_age = estimate)
```

    ## Getting data from the 2024 1-year ACS
    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
urban_raw <-read_csv("data/Census_Urbanization_Data/DECENNIALDHC2020.P2-Data.csv")
```

    ## New names:
    ## Rows: 53 Columns: 7
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (6): GEO_ID, NAME, P2_001N, P2_002N, P2_003N, P2_004N lgl (1): ...7
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...7`

``` r
urban_clean_2020 =
  urban_raw |>
  filter(str_detect(GEO_ID, "^040")) |>                      # keep States only
  transmute(
    State = NAME,
    total = as.numeric(P2_001N),
    urban = as.numeric(P2_002N),
    rural = as.numeric(P2_003N),
    pct_urban = urban / total,
    pct_rural = rural / total
  )
```

``` r
poverty_2024 =
  get_acs(
    geography = "state",
    variables = c(total = "B17001_001",
                  below_pov = "B17001_002"),
    survey = "acs1",
    year = 2024
  )
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
poverty_clean_2024 =
  poverty_2024 |>
  group_by(NAME) |>
  summarize(
    total = sum(estimate[variable == "total"]),
    poverty = sum(estimate[variable == "below_pov"]),
    .groups = "drop"
  ) |>
  mutate(
    pct_poverty = poverty / total
  )
```

``` r
# % Male
sex_2024 = get_acs(
  geography = "state",
  variables = c(
    male = "B01001_002",
    total = "B01001_001"
  ),
  survey = "acs1",
  year = 2024
)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
sex_clean_2024 =
  sex_2024 |>
  group_by(NAME) |>
  summarize(
    male = sum(estimate[variable == "male"]),
    total = sum(estimate[variable == "total"]),
    .groups = "drop"
  ) |>
  mutate(pct_male = male / total)
```

``` r
# Unemployment rate
unemp_2024 = get_acs(
  geography = "state",
  variables = c(
    labor_force = "B23025_003",   # In labor force
    unemployed = "B23025_005"     # Unemployed
  ),
  survey = "acs1",
  year = 2024
)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
unemp_clean_2024 =
  unemp_2024 |>
  group_by(NAME) |>
  summarize(
    labor_force = sum(estimate[variable == "labor_force"]),
    unemployed = sum(estimate[variable == "unemployed"]),
    .groups = "drop"
  ) |>
  mutate(unemployment_rate = unemployed / labor_force)
```

``` r
race_2024 = get_acs(
  geography = "state",
  variables = c(
    total = "B02001_001",
    white = "B02001_002",
    black = "B02001_003",
    asian = "B02001_005",
    hispanic = "B03003_003"
  ),
  survey = "acs1",
  year = 2024
)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
race_clean_2024 =
  race_2024 |>
  group_by(NAME) |>
  summarize(
    total = sum(estimate[variable == "total"]),
    white = sum(estimate[variable == "white"]),
    black = sum(estimate[variable == "black"]),
    asian = sum(estimate[variable == "asian"]),
    hispanic = sum(estimate[variable == "hispanic"]),
    .groups = "drop"
  ) |>
  mutate(
    pct_white = white / total,
    pct_black = black / total,
    pct_asian = asian / total,
    pct_hispanic = hispanic / total
  )
```

``` r
internet_2024 = get_acs(
  geography = "state",
  variables = c(
    total = "B28002_001",
    broadband = "B28002_004"
  ),
  survey = "acs1",
  year = 2024
)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
internet_clean_2024 =
  internet_2024 |>
  group_by(NAME) |>
  summarize(
    total = sum(estimate[variable == "total"]),
    broadband = sum(estimate[variable == "broadband"]),
    .groups = "drop"
  ) |>
  mutate(pct_broadband = broadband / total)
```

``` r
# here is the def if you do not know what this is: The Gini index, or Gini coefficient, is a measure of income, wealth, or consumption inequality within a population, ranging from 0 (perfect equality) to 1 (perfect inequality)

gini_2024 = get_acs(
  geography = "state",
  variables = c(gini = "B19083_001"),
  survey = "acs1",
  year = 2024
) |>
  select(NAME, gini = estimate)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
rent_2024 = get_acs(
  geography = "state",
  variables = c(
    total = "B25003_001",
    owner = "B25003_002",
    renter = "B25003_003"
  ),
  survey = "acs1",
  year = 2024
)
```

    ## Getting data from the 2024 1-year ACS

    ## The 1-year ACS provides data for geographies with populations of 65,000 and greater.

``` r
rent_clean_2024 =
  rent_2024 |>
  group_by(NAME) |>
  summarize(
    total = sum(estimate[variable == "total"]),
    owner = sum(estimate[variable == "owner"]),
    renter = sum(estimate[variable == "renter"]),
    .groups = "drop"
  ) |>
  mutate(
    pct_rent = renter / total,
    pct_own = owner / total
  )
```

``` r
#Rename so all are State
acs_base_2024 = acs_base_2024 |> rename(State = name)
educ_clean_2024 = educ_clean_2024 |> rename(State = NAME)
age_clean_2024 = age_clean_2024 |> rename(State = NAME)
poverty_clean_2024 = poverty_clean_2024 |> rename(State = NAME)
sex_clean_2024 = sex_clean_2024 |> rename(State = NAME)
unemp_clean_2024 = unemp_clean_2024 |> rename(State = NAME)
race_clean_2024 = race_clean_2024 |> rename(State = NAME)
median_age_2024 = median_age_2024 |> rename(State = NAME)
internet_clean_2024 = internet_clean_2024 |> rename(State = NAME)
gini_2024 = gini_2024 |> rename(State = NAME)
rent_clean_2024 = rent_clean_2024 |> rename(State = NAME)

# Final combined demographic dataset
demographics_2024 =
  acs_base_2024 |> 
  left_join(educ_clean_2024 |> 
              select(State, pct_bachelors), 
            by = "State") |>
  left_join(age_clean_2024 |> 
              select(State, pct_age_20_34), 
            by = "State") |>
  left_join(urban_clean_2020 |> 
              select(State, pct_urban, pct_rural), 
            by = "State") |>
  left_join(poverty_clean_2024 |> 
              select(State, pct_poverty), 
            by = "State") |>
  left_join(sex_clean_2024 |> 
              select(State, pct_male), 
            by = "State") |>
  left_join(unemp_clean_2024 |> 
              select(State, unemployment_rate), 
            by = "State") |>
  left_join(race_clean_2024 |> 
              select(State, pct_white, pct_black, pct_asian, pct_hispanic), 
            by = "State") |>
  left_join(median_age_2024 |> 
              select(State, median_age), 
            by = "State") |>
  left_join(internet_clean_2024 |> 
              select(State, pct_broadband), 
            by = "State") |>
  left_join(gini_2024 |> 
              select(State, gini), 
            by = "State") |>
  left_join(rent_clean_2024 |> 
              select(State, pct_rent, pct_own), 
            by = "State")

# Merge with gambling revenue
revenue_2024 =
  final_state_revenue |>
  filter(Year == 2024, State != "Total") |>
  select(State, Revenue, Handle, Taxes, Hold)

model_data_2024 =
  revenue_2024 |>
  left_join(demographics_2024, by = "State")
```

``` r
#view(demographics_2024)
summary(demographics_2024)
```

    ##     State             total_pop        median_income    pct_bachelors   
    ##  Length:52          Min.   :  587618   Min.   : 27213   Min.   :0.2438  
    ##  Class :character   1st Qu.: 1943709   1st Qu.: 72316   1st Qu.:0.3233  
    ##  Mode  :character   Median : 4430372   Median : 77803   Median :0.3560  
    ##                     Mean   : 6602198   Mean   : 80424   Mean   :0.3625  
    ##                     3rd Qu.: 7676333   3rd Qu.: 88673   3rd Qu.:0.3915  
    ##                     Max.   :39431263   Max.   :109707   Max.   :0.6546  
    ##  pct_age_20_34       pct_urban        pct_rural       pct_poverty     
    ##  Min.   :0.05403   Min.   :0.3512   Min.   :0.0000   Min.   :0.07205  
    ##  1st Qu.:0.06333   1st Qu.:0.6425   1st Qu.:0.1389   1st Qu.:0.10149  
    ##  Median :0.06591   Median :0.7324   Median :0.2676   Median :0.11579  
    ##  Mean   :0.06580   Mean   :0.7334   Mean   :0.2666   Mean   :0.12441  
    ##  3rd Qu.:0.06819   3rd Qu.:0.8611   3rd Qu.:0.3575   3rd Qu.:0.13418  
    ##  Max.   :0.08744   Max.   :1.0000   Max.   :0.6488   Max.   :0.37308  
    ##     pct_male      unemployment_rate   pct_white        pct_black       
    ##  Min.   :0.4729   Min.   :0.01910   Min.   :0.2195   Min.   :0.003091  
    ##  1st Qu.:0.4910   1st Qu.:0.03659   1st Qu.:0.5621   1st Qu.:0.036199  
    ##  Median :0.4951   Median :0.04345   Median :0.6745   Median :0.069857  
    ##  Mean   :0.4960   Mean   :0.04332   Mean   :0.6569   Mean   :0.106106  
    ##  3rd Qu.:0.5006   3rd Qu.:0.04891   3rd Qu.:0.7751   3rd Qu.:0.141908  
    ##  Max.   :0.5273   Max.   :0.08448   Max.   :0.9011   Max.   :0.413152  
    ##    pct_asian         pct_hispanic       median_age    pct_broadband   
    ##  Min.   :0.001707   Min.   :0.02317   Min.   :32.50   Min.   :0.8626  
    ##  1st Qu.:0.019852   1st Qu.:0.06562   1st Qu.:38.62   1st Qu.:0.9161  
    ##  Median :0.030595   Median :0.11586   Median :39.40   Median :0.9280  
    ##  Mean   :0.046410   Mean   :0.15332   Mean   :39.64   Mean   :0.9254  
    ##  3rd Qu.:0.050288   3rd Qu.:0.17378   3rd Qu.:40.73   3rd Qu.:0.9368  
    ##  Max.   :0.362609   Max.   :0.98877   Max.   :45.40   Max.   :0.9529  
    ##       gini           pct_rent         pct_own      
    ##  Min.   :0.4222   Min.   :0.2449   Min.   :0.4090  
    ##  1st Qu.:0.4556   1st Qu.:0.2922   1st Qu.:0.6588  
    ##  Median :0.4665   Median :0.3201   Median :0.6799  
    ##  Mean   :0.4670   Mean   :0.3295   Mean   :0.6705  
    ##  3rd Qu.:0.4747   3rd Qu.:0.3412   3rd Qu.:0.7078  
    ##  Max.   :0.5338   Max.   :0.5910   Max.   :0.7551

``` r
glimpse(demographics_2024)
```

    ## Rows: 52
    ## Columns: 19
    ## $ State             <chr> "Alabama", "Alaska", "Arizona", "Arkansas", "Califor…
    ## $ total_pop         <dbl> 5157699, 740133, 7582384, 3088354, 39431263, 5957494…
    ## $ median_income     <dbl> 66659, 95665, 81486, 62106, 100149, 97113, 96049, 87…
    ## $ pct_bachelors     <dbl> 0.2985358, 0.3275460, 0.3473430, 0.2711732, 0.381236…
    ## $ pct_age_20_34     <dbl> 0.06431007, 0.06822828, 0.06858898, 0.06647845, 0.06…
    ## $ pct_urban         <dbl> 0.5773724, 0.6489949, 0.8928516, 0.5547613, 0.942366…
    ## $ pct_rural         <dbl> 0.42262760, 0.35100513, 0.10714840, 0.44523869, 0.05…
    ## $ pct_poverty       <dbl> 0.15188039, 0.10166801, 0.11680530, 0.15535744, 0.11…
    ## $ pct_male          <dbl> 0.4828803, 0.5273390, 0.4989042, 0.4913705, 0.498972…
    ## $ unemployment_rate <dbl> 0.04231466, 0.05961084, 0.04563755, 0.03817759, 0.05…
    ## $ pct_white         <dbl> 0.6401166, 0.5870837, 0.5731275, 0.6818985, 0.380758…
    ## $ pct_black         <dbl> 0.252458897, 0.025060361, 0.047592947, 0.141759980, …
    ## $ pct_asian         <dbl> 0.01543111, 0.05955281, 0.03992623, 0.01786842, 0.16…
    ## $ pct_hispanic      <dbl> 0.05951607, 0.07732259, 0.32122628, 0.09541361, 0.40…
    ## $ median_age        <dbl> 39.6, 36.3, 39.4, 39.1, 38.4, 38.0, 41.2, 42.1, 34.9…
    ## $ pct_broadband     <dbl> 0.9164410, 0.9359704, 0.9355612, 0.9076009, 0.950047…
    ## $ gini              <dbl> 0.4745, 0.4395, 0.4623, 0.4721, 0.4852, 0.4611, 0.49…
    ## $ pct_rent          <dbl> 0.2904826, 0.3348100, 0.3224346, 0.3288997, 0.441797…
    ## $ pct_own           <dbl> 0.7095174, 0.6651900, 0.6775654, 0.6711003, 0.558202…

``` r
#view(model_data_2024)
summary(model_data_2024)
```

    ##     State              Revenue              Handle              Taxes          
    ##  Length:39          Min.   :9.209e+05   Min.   :9.237e+06   Min.   :8.288e+04  
    ##  Class :character   1st Qu.:5.414e+07   1st Qu.:5.132e+08   1st Qu.:8.523e+06  
    ##  Mode  :character   Median :2.453e+08   Median :3.235e+09   Median :3.224e+07  
    ##                     Mean   :4.095e+08   Mean   :4.669e+09   Mean   :8.348e+07  
    ##                     3rd Qu.:5.592e+08   3rd Qu.:7.045e+09   3rd Qu.:8.977e+07  
    ##                     Max.   :2.059e+09   Max.   :2.295e+10   Max.   :1.050e+09  
    ##                     NA's   :5           NA's   :7           NA's   :5          
    ##       Hold          total_pop        median_income    pct_bachelors   
    ##  Min.   : 5.530   Min.   :  587618   Min.   : 59127   Min.   :0.2438  
    ##  1st Qu.: 8.693   1st Qu.: 1828851   1st Qu.: 72256   1st Qu.:0.3230  
    ##  Median : 9.610   Median : 4430372   Median : 77213   Median :0.3591  
    ##  Mean   : 9.639   Mean   : 5877058   Mean   : 80034   Mean   :0.3606  
    ##  3rd Qu.:10.773   3rd Qu.: 7864231   3rd Qu.: 85670   3rd Qu.:0.4057  
    ##  Max.   :13.960   Max.   :23372215   Max.   :104828   Max.   :0.4827  
    ##  NA's   :7        NA's   :1          NA's   :1        NA's   :1       
    ##  pct_age_20_34       pct_urban        pct_rural        pct_poverty     
    ##  Min.   :0.05403   Min.   :0.3512   Min.   :0.05908   Min.   :0.07205  
    ##  1st Qu.:0.06180   1st Qu.:0.6123   1st Qu.:0.14076   1st Qu.:0.10120  
    ##  Median :0.06545   Median :0.7324   Median :0.26760   Median :0.11578  
    ##  Mean   :0.06491   Mean   :0.7186   Mean   :0.28143   Mean   :0.11909  
    ##  3rd Qu.:0.06752   3rd Qu.:0.8592   3rd Qu.:0.38770   3rd Qu.:0.12642  
    ##  Max.   :0.07733   Max.   :0.9409   Max.   :0.64880   Max.   :0.18688  
    ##  NA's   :1         NA's   :1        NA's   :1         NA's   :1        
    ##     pct_male      unemployment_rate   pct_white        pct_black       
    ##  Min.   :0.4818   Min.   :0.01910   Min.   :0.4635   Min.   :0.003091  
    ##  1st Qu.:0.4914   1st Qu.:0.03476   1st Qu.:0.5845   1st Qu.:0.038399  
    ##  Median :0.4943   Median :0.04311   Median :0.7066   Median :0.081159  
    ##  Mean   :0.4961   Mean   :0.04176   Mean   :0.6934   Mean   :0.100325  
    ##  3rd Qu.:0.5005   3rd Qu.:0.04872   3rd Qu.:0.7934   3rd Qu.:0.139359  
    ##  Max.   :0.5137   Max.   :0.06283   Max.   :0.9011   Max.   :0.358802  
    ##  NA's   :1        NA's   :1         NA's   :1        NA's   :1         
    ##    pct_asian         pct_hispanic       median_age    pct_broadband   
    ##  Min.   :0.007063   Min.   :0.02317   Min.   :36.70   Min.   :0.8833  
    ##  1st Qu.:0.019519   1st Qu.:0.05517   1st Qu.:39.10   1st Qu.:0.9128  
    ##  Median :0.028946   Median :0.11309   Median :39.65   Median :0.9264  
    ##  Mean   :0.038988   Mean   :0.13199   Mean   :40.07   Mean   :0.9253  
    ##  3rd Qu.:0.048242   3rd Qu.:0.17969   3rd Qu.:40.95   3rd Qu.:0.9394  
    ##  Max.   :0.107183   Max.   :0.49114   Max.   :44.90   Max.   :0.9529  
    ##  NA's   :1          NA's   :1         NA's   :1       NA's   :1       
    ##       gini           pct_rent         pct_own      
    ##  Min.   :0.4357   Min.   :0.2449   Min.   :0.5432  
    ##  1st Qu.:0.4561   1st Qu.:0.2933   1st Qu.:0.6667  
    ##  Median :0.4661   Median :0.3198   Median :0.6802  
    ##  Mean   :0.4667   Mean   :0.3227   Mean   :0.6773  
    ##  3rd Qu.:0.4742   3rd Qu.:0.3333   3rd Qu.:0.7067  
    ##  Max.   :0.5186   Max.   :0.4568   Max.   :0.7551  
    ##  NA's   :1        NA's   :1        NA's   :1

``` r
glimpse(model_data_2024)
```

    ## Rows: 39
    ## Columns: 23
    ## $ State             <chr> "Arizona", "Arkansas", "Colorado", "Connecticut", "D…
    ## $ Revenue           <dbl> 427397087, 107275202, 475176731, 187349516, 14585100…
    ## $ Handle            <dbl> 7959647194, NA, 6187564044, 2188745557, 216240459, N…
    ## $ Taxes             <dbl> 42739709, 13945776, 31934112, 25760559, 8474813, NA,…
    ## $ Hold              <dbl> 5.53, NA, 7.87, 8.67, 6.99, NA, 13.52, 9.48, 8.07, 5…
    ## $ total_pop         <dbl> 7582384, 3088354, 5957494, 3675069, 1051917, 2337221…
    ## $ median_income     <dbl> 81486, 62106, 97113, 96049, 87534, 77735, 83211, 719…
    ## $ pct_bachelors     <dbl> 0.3473430, 0.2711732, 0.4776638, 0.4256536, 0.359915…
    ## $ pct_age_20_34     <dbl> 0.06858898, 0.06647845, 0.06598143, 0.06532394, 0.06…
    ## $ pct_urban         <dbl> 0.8928516, 0.5547613, 0.8602671, 0.8625073, 0.826121…
    ## $ pct_rural         <dbl> 0.10714840, 0.44523869, 0.13973293, 0.13749271, 0.17…
    ## $ pct_poverty       <dbl> 0.11680530, 0.15535744, 0.09601167, 0.10195637, 0.09…
    ## $ pct_male          <dbl> 0.4989042, 0.4913705, 0.5063738, 0.4911189, 0.481751…
    ## $ unemployment_rate <dbl> 0.04563755, 0.03817759, 0.04277032, 0.04948289, 0.05…
    ## $ pct_white         <dbl> 0.5731275, 0.6818985, 0.6935989, 0.6369799, 0.581658…
    ## $ pct_black         <dbl> 0.047592947, 0.141759980, 0.042512338, 0.107578388, …
    ## $ pct_asian         <dbl> 0.039926229, 0.017868418, 0.034834445, 0.048035561, …
    ## $ pct_hispanic      <dbl> 0.32122628, 0.09541361, 0.23213586, 0.19232455, 0.11…
    ## $ median_age        <dbl> 39.4, 39.1, 38.0, 41.2, 42.1, 42.7, 39.4, 38.3, 39.0…
    ## $ pct_broadband     <dbl> 0.9355612, 0.9076009, 0.9489518, 0.9403395, 0.942886…
    ## $ gini              <dbl> 0.4623, 0.4721, 0.4611, 0.4989, 0.4478, 0.4799, 0.48…
    ## $ pct_rent          <dbl> 0.3224346, 0.3288997, 0.3405580, 0.3332788, 0.262221…
    ## $ pct_own           <dbl> 0.6775654, 0.6711003, 0.6594420, 0.6667212, 0.737778…

``` r
model_clean_2024 <-
  model_data_2024 |>
  drop_na(Revenue, Handle, Taxes, Hold)

#See final Amount
nrow(model_clean_2024)
```

    ## [1] 32

``` r
unique(model_clean_2024$State)
```

    ##  [1] "Arizona"         "Colorado"        "Connecticut"     "Delaware"       
    ##  [5] "Illinois"        "Indiana"         "Iowa"            "Kansas"         
    ##  [9] "Kentucky"        "Louisiana"       "Maine"           "Maryland"       
    ## [13] "Massachusetts"   "Michigan"        "Mississippi"     "Montana"        
    ## [17] "Nevada"          "New Hampshire"   "New Jersey"      "New York"       
    ## [21] "North Carolina"  "Ohio"            "Oregon"          "Pennsylvania"   
    ## [25] "Rhode Island"    "South Dakota"    "Tennessee"       "Vermont"        
    ## [29] "Virginia"        "Washington D.C." "West Virginia"   "Wyoming"

``` r
#See who was dropped
anti_join(model_data_2024, model_clean_2024, by = "State") |>
  select(State)
```

    ## # A tibble: 7 × 1
    ##   State       
    ##   <chr>       
    ## 1 Arkansas    
    ## 2 Florida     
    ## 3 Missouri    
    ## 4 Nebraska    
    ## 5 New Mexico  
    ## 6 North Dakota
    ## 7 Washington

Save final datasets

``` r
write.csv(model_clean_2024, "data/model_clean_2024.csv")
write.csv(demographics_2024, "data/demographics_2024.csv")
```
