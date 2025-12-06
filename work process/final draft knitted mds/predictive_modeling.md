predictive_modeling
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
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(knitr)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(viridis)
```

    ## Loading required package: viridisLite
    ## 
    ## Attaching package: 'viridis'
    ## 
    ## The following object is masked from 'package:scales':
    ## 
    ##     viridis_pal

``` r
library(corrplot)
```

    ## corrplot 0.95 loaded

``` r
library(Metrics)
library(kableExtra)
```

## Import data

``` r
model_clean_2024 = read_csv("data/model_clean_2024.csv")
```

    ## New names:
    ## Rows: 32 Columns: 24
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (1): State dbl (23): ...1, Revenue, Handle, Taxes, Hold, total_pop,
    ## median_income, pct_...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
demographics_2024 = read_csv("data/demographics_2024.csv")
```

    ## New names:
    ## Rows: 52 Columns: 20
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (1): State dbl (19): ...1, total_pop, median_income, pct_bachelors,
    ## pct_age_20_34, pct_...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

## Building predictive model

``` r
# Build training dataset
training_data <- model_clean_2024 |>
  select(
    State,
    Revenue,
    total_pop,
    median_income,
    pct_bachelors,
    pct_age_20_34,
    pct_urban,
    pct_poverty
  ) |>
  drop_na()   # make sure no missing values in predictors or Revenue
```

``` r
# Fit linear regression model

# Model: Revenue = β0 + β1 * total_pop + β2 * median_income + ... + error
fit_lm <- lm(
  Revenue ~ total_pop + median_income + pct_bachelors +
    pct_age_20_34 + pct_urban + pct_poverty,
  data = training_data
  )

summary(fit_lm)
```

    ## 
    ## Call:
    ## lm(formula = Revenue ~ total_pop + median_income + pct_bachelors + 
    ##     pct_age_20_34 + pct_urban + pct_poverty, data = training_data)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -343955707  -71243106    2818238  102251402  763021755 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -2.966e+08  1.216e+09  -0.244    0.809    
    ## total_pop      9.210e+01  1.200e+01   7.674 6.54e-08 ***
    ## median_income  6.809e+03  1.322e+04   0.515    0.611    
    ## pct_bachelors -4.983e+07  1.972e+09  -0.025    0.980    
    ## pct_age_20_34 -1.020e+10  1.352e+10  -0.755    0.458    
    ## pct_urban      1.618e+08  4.739e+08   0.341    0.736    
    ## pct_poverty    1.876e+09  3.013e+09   0.623    0.539    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 227400000 on 24 degrees of freedom
    ## Multiple R-squared:  0.8359, Adjusted R-squared:  0.7948 
    ## F-statistic: 20.37 on 6 and 24 DF,  p-value: 2.503e-08

``` r
states_no_gambling <-
  demographics_2024 |>
  anti_join(training_data, by = "State")

newdata_illegal <-
  states_no_gambling |>
  select(
    State,
    total_pop,
    median_income,
    pct_bachelors,
    pct_age_20_34,
    pct_urban,
    pct_poverty
  ) |>
  drop_na()  # just in case a state is missing some demographic
```

``` r
# Predict revenue if these states legalized gambling

pred_illegal <-
  newdata_illegal |>
  mutate(
    predicted_revenue = predict(fit_lm, newdata = newdata_illegal)
  ) |>
  arrange(desc(predicted_revenue))
```

``` r
# Compare distribution of real vs predicted
summary(training_data$Revenue)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## 9.209e+05 6.063e+07 4.274e+08 4.438e+08 5.993e+08 2.059e+09

``` r
summary(pred_illegal$predicted_revenue)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -191325313   78725694  339787241  636990268  454658976 3706182311

``` r
# Fit LASSO regression model

# Build design matrix (same predictors as your LM model)
X_train <- model.matrix(
  Revenue ~ total_pop + median_income + pct_bachelors +
    pct_age_20_34 + pct_urban + pct_poverty,
  data = training_data)[, -1]   # remove intercept column (glmnet adds its own)

# Outcome vector
y_train <- training_data$Revenue

set.seed(123)

# Cross-validated LASSO to choose lambda
lasso_cv <- cv.glmnet(
  X_train,
  y_train,
  alpha = 1,          # LASSO penalty
  nfolds = 5,
  standardize = TRUE  # recommended because predictors differ in scale
)

# Final LASSO model at best lambda
lasso_model <- glmnet(
  X_train,
  y_train,
  alpha = 1,
  lambda = lasso_cv$lambda.min,
  standardize = TRUE
)

lasso_cv$lambda.min #This is the penalty 
```

    ## [1] 10761247

``` r
# Build the prediction matrix for the non-gambling states
X_new <- model.matrix(
  ~ total_pop + median_income + pct_bachelors +
    pct_age_20_34 + pct_urban + pct_poverty,
  data = newdata_illegal)[, -1]   # remove intercept column

# LASSO predictions
pred_illegal_lasso <-
  newdata_illegal |>
  mutate(
    predicted_revenue_lasso = as.numeric(
      predict(lasso_model, newx = X_new)
    )
  ) |>
  arrange(desc(predicted_revenue_lasso))

pred_illegal_lasso <- pred_illegal_lasso |> 
  mutate(predicted_revenue_lasso = pmax(predicted_revenue_lasso, 0))
```

``` r
summary(training_data$Revenue)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## 9.209e+05 6.063e+07 4.274e+08 4.438e+08 5.993e+08 2.059e+09

``` r
summary(pred_illegal$predicted_revenue)          # from LM model
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -191325313   78725694  339787241  636990268  454658976 3706182311

``` r
summary(pred_illegal_lasso$predicted_revenue_lasso)  # from LASSO model
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## 0.000e+00 6.132e+07 2.018e+08 6.279e+08 4.718e+08 3.612e+09

``` r
#LASSO better
```

## Some tables and Figures

Table of Predicted Annual Gambling Revenue for Non-Legal States (LASSO
Model)

``` r
# Clean negative values
pred_illegal_lasso <- pred_illegal_lasso |>
  mutate(predicted_revenue_lasso = pmax(predicted_revenue_lasso, 0))

# Clean and format final table
pred_table <- pred_illegal_lasso |>
  mutate(
    Predicted_Revenue = dollar(predicted_revenue_lasso),
    Population = comma(total_pop),
    Median_Income = dollar(median_income)
  ) |>
  select(
    State,
    Population,
    Median_Income,
    pct_bachelors,
    pct_age_20_34,
    pct_urban,
    pct_poverty,
    Predicted_Revenue
  )

kable(pred_table, format = "html", caption = "Predicted Annual Gambling Revenue for Non-Legal States (LASSO Model)") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Predicted Annual Gambling Revenue for Non-Legal States (LASSO Model)
</caption>

<thead>

<tr>

<th style="text-align:left;">

State
</th>

<th style="text-align:left;">

Population
</th>

<th style="text-align:left;">

Median_Income
</th>

<th style="text-align:right;">

pct_bachelors
</th>

<th style="text-align:right;">

pct_age_20_34
</th>

<th style="text-align:right;">

pct_urban
</th>

<th style="text-align:right;">

pct_poverty
</th>

<th style="text-align:left;">

Predicted_Revenue
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

California
</td>

<td style="text-align:left;">

39,431,263
</td>

<td style="text-align:left;">

\$100,149
</td>

<td style="text-align:right;">

0.3812365
</td>

<td style="text-align:right;">

0.0652504
</td>

<td style="text-align:right;">

0.9423663
</td>

<td style="text-align:right;">

0.1180257
</td>

<td style="text-align:left;">

\$3,611,645,904
</td>

</tr>

<tr>

<td style="text-align:left;">

Texas
</td>

<td style="text-align:left;">

31,290,831
</td>

<td style="text-align:left;">

\$79,721
</td>

<td style="text-align:right;">

0.3516383
</td>

<td style="text-align:right;">

0.0694787
</td>

<td style="text-align:right;">

0.8372028
</td>

<td style="text-align:right;">

0.1340909
</td>

<td style="text-align:left;">

\$2,765,125,447
</td>

</tr>

<tr>

<td style="text-align:left;">

Florida
</td>

<td style="text-align:left;">

23,372,215
</td>

<td style="text-align:left;">

\$77,735
</td>

<td style="text-align:right;">

0.3583638
</td>

<td style="text-align:right;">

0.0582353
</td>

<td style="text-align:right;">

0.9153419
</td>

<td style="text-align:right;">

0.1204723
</td>

<td style="text-align:left;">

\$2,135,268,431
</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia
</td>

<td style="text-align:left;">

11,180,878
</td>

<td style="text-align:left;">

\$79,991
</td>

<td style="text-align:right;">

0.3631028
</td>

<td style="text-align:right;">

0.0673810
</td>

<td style="text-align:right;">

0.7406697
</td>

<td style="text-align:right;">

0.1258716
</td>

<td style="text-align:left;">

\$922,707,171
</td>

</tr>

<tr>

<td style="text-align:left;">

Washington
</td>

<td style="text-align:left;">

7,958,180
</td>

<td style="text-align:left;">

\$99,389
</td>

<td style="text-align:right;">

0.4101713
</td>

<td style="text-align:right;">

0.0616117
</td>

<td style="text-align:right;">

0.8337185
</td>

<td style="text-align:right;">

0.0993223
</td>

<td style="text-align:left;">

\$736,907,744
</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota
</td>

<td style="text-align:left;">

5,793,151
</td>

<td style="text-align:left;">

\$87,117
</td>

<td style="text-align:right;">

0.4003043
</td>

<td style="text-align:right;">

0.0634185
</td>

<td style="text-align:right;">

0.7187871
</td>

<td style="text-align:right;">

0.0933292
</td>

<td style="text-align:left;">

\$471,813,698
</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri
</td>

<td style="text-align:left;">

6,245,466
</td>

<td style="text-align:left;">

\$71,589
</td>

<td style="text-align:right;">

0.3347161
</td>

<td style="text-align:right;">

0.0670586
</td>

<td style="text-align:right;">

0.6946748
</td>

<td style="text-align:right;">

0.1226321
</td>

<td style="text-align:left;">

\$443,937,391
</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin
</td>

<td style="text-align:left;">

5,960,975
</td>

<td style="text-align:left;">

\$77,488
</td>

<td style="text-align:right;">

0.3455598
</td>

<td style="text-align:right;">

0.0673242
</td>

<td style="text-align:right;">

0.6708314
</td>

<td style="text-align:right;">

0.1032844
</td>

<td style="text-align:left;">

\$425,197,336
</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina
</td>

<td style="text-align:left;">

5,478,831
</td>

<td style="text-align:left;">

\$72,350
</td>

<td style="text-align:right;">

0.3333325
</td>

<td style="text-align:right;">

0.0634482
</td>

<td style="text-align:right;">

0.6794803
</td>

<td style="text-align:right;">

0.1325996
</td>

<td style="text-align:left;">

\$398,844,164
</td>

</tr>

<tr>

<td style="text-align:left;">

Alabama
</td>

<td style="text-align:left;">

5,157,699
</td>

<td style="text-align:left;">

\$66,659
</td>

<td style="text-align:right;">

0.2985358
</td>

<td style="text-align:right;">

0.0643101
</td>

<td style="text-align:right;">

0.5773724
</td>

<td style="text-align:right;">

0.1518804
</td>

<td style="text-align:left;">

\$327,792,352
</td>

</tr>

<tr>

<td style="text-align:left;">

Oklahoma
</td>

<td style="text-align:left;">

4,095,393
</td>

<td style="text-align:left;">

\$66,148
</td>

<td style="text-align:right;">

0.2931934
</td>

<td style="text-align:right;">

0.0701015
</td>

<td style="text-align:right;">

0.6462195
</td>

<td style="text-align:right;">

0.1490811
</td>

<td style="text-align:left;">

\$201,795,708
</td>

</tr>

<tr>

<td style="text-align:left;">

Hawaii
</td>

<td style="text-align:left;">

1,446,146
</td>

<td style="text-align:left;">

\$100,745
</td>

<td style="text-align:right;">

0.3775749
</td>

<td style="text-align:right;">

0.0586905
</td>

<td style="text-align:right;">

0.8606301
</td>

<td style="text-align:right;">

0.0998640
</td>

<td style="text-align:left;">

\$172,063,243
</td>

</tr>

<tr>

<td style="text-align:left;">

Utah
</td>

<td style="text-align:left;">

3,503,613
</td>

<td style="text-align:left;">

\$96,658
</td>

<td style="text-align:right;">

0.3911757
</td>

<td style="text-align:right;">

0.0874363
</td>

<td style="text-align:right;">

0.8978141
</td>

<td style="text-align:right;">

0.0832570
</td>

<td style="text-align:left;">

\$148,267,944
</td>

</tr>

<tr>

<td style="text-align:left;">

Puerto Rico
</td>

<td style="text-align:left;">

3,203,295
</td>

<td style="text-align:left;">

\$27,213
</td>

<td style="text-align:right;">

0.2969966
</td>

<td style="text-align:right;">

0.0663738
</td>

<td style="text-align:right;">

0.9187534
</td>

<td style="text-align:right;">

0.3730844
</td>

<td style="text-align:left;">

\$111,180,446
</td>

</tr>

<tr>

<td style="text-align:left;">

Arkansas
</td>

<td style="text-align:left;">

3,088,354
</td>

<td style="text-align:left;">

\$62,106
</td>

<td style="text-align:right;">

0.2711732
</td>

<td style="text-align:right;">

0.0664785
</td>

<td style="text-align:right;">

0.5547613
</td>

<td style="text-align:right;">

0.1553574
</td>

<td style="text-align:left;">

\$107,040,120
</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico
</td>

<td style="text-align:left;">

2,130,256
</td>

<td style="text-align:left;">

\$67,816
</td>

<td style="text-align:right;">

0.3178310
</td>

<td style="text-align:right;">

0.0681773
</td>

<td style="text-align:right;">

0.7454714
</td>

<td style="text-align:right;">

0.1641940
</td>

<td style="text-align:left;">

\$61,324,867
</td>

</tr>

<tr>

<td style="text-align:left;">

Idaho
</td>

<td style="text-align:left;">

2,001,619
</td>

<td style="text-align:left;">

\$81,166
</td>

<td style="text-align:right;">

0.3302656
</td>

<td style="text-align:right;">

0.0708042
</td>

<td style="text-align:right;">

0.6924218
</td>

<td style="text-align:right;">

0.1046070
</td>

<td style="text-align:left;">

\$51,404,787
</td>

</tr>

<tr>

<td style="text-align:left;">

District of Columbia
</td>

<td style="text-align:left;">

702,250
</td>

<td style="text-align:left;">

\$109,707
</td>

<td style="text-align:right;">

0.6546496
</td>

<td style="text-align:right;">

0.0729740
</td>

<td style="text-align:right;">

1.0000000
</td>

<td style="text-align:right;">

0.1731169
</td>

<td style="text-align:left;">

\$51,027,257
</td>

</tr>

<tr>

<td style="text-align:left;">

Nebraska
</td>

<td style="text-align:left;">

2,005,466
</td>

<td style="text-align:left;">

\$76,376
</td>

<td style="text-align:right;">

0.3535887
</td>

<td style="text-align:right;">

0.0714562
</td>

<td style="text-align:right;">

0.7300536
</td>

<td style="text-align:right;">

0.1087362
</td>

<td style="text-align:left;">

\$43,401,693
</td>

</tr>

<tr>

<td style="text-align:left;">

Alaska
</td>

<td style="text-align:left;">

740,133
</td>

<td style="text-align:left;">

\$95,665
</td>

<td style="text-align:right;">

0.3275460
</td>

<td style="text-align:right;">

0.0682283
</td>

<td style="text-align:right;">

0.6489949
</td>

<td style="text-align:right;">

0.1016680
</td>

<td style="text-align:left;">

\$0
</td>

</tr>

<tr>

<td style="text-align:left;">

North Dakota
</td>

<td style="text-align:left;">

796,568
</td>

<td style="text-align:left;">

\$77,871
</td>

<td style="text-align:right;">

0.3395173
</td>

<td style="text-align:right;">

0.0773305
</td>

<td style="text-align:right;">

0.6096684
</td>

<td style="text-align:right;">

0.1112829
</td>

<td style="text-align:left;">

\$0
</td>

</tr>

</tbody>

</table>

Bar Plot of Predicted Annual Gambling Revenue (if legalized)

``` r
pred_illegal_lasso_plot <- pred_illegal_lasso |>
  mutate(predicted_revenue_lasso = pmax(predicted_revenue_lasso, 0)) |>
  arrange(predicted_revenue_lasso) |>
  mutate(State = factor(State, levels = State))

ggplot(pred_illegal_lasso_plot, aes(x = State, y = predicted_revenue_lasso)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Predicted Annual Gambling Revenue (if legalized)",
    subtitle = "LASSO model predictions for states without legal sports gambling",
    x = "",
    y = "Predicted Revenue"
  ) +
  theme_minimal(base_size = 13)
```

![](predictive_modeling_files/figure-gfm/Bar%20Plot-1.png)<!-- -->

Density Plot of Distribution of Actual vs Predicted Gambling Revenue

``` r
compare_df <- tibble(
  group = c(rep("Actual Revenue (Legal States)", nrow(training_data)),
            rep("Predicted Revenue (Non-Legal States)", nrow(pred_illegal_lasso))),
  revenue = c(training_data$Revenue,
              pred_illegal_lasso$predicted_revenue_lasso)
)

ggplot(compare_df, aes(x = revenue, fill = group)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(labels = dollar) +
  labs(
    title = "Distribution of Actual vs Predicted Gambling Revenue",
    x = "Revenue",
    y = "Density",
    fill = ""
  ) +
  theme_minimal(base_size = 13)
```

![](predictive_modeling_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Scatterplot of Population vs Gambling Revenue Comparing Actual and
Predicted states

``` r
scatter_df <- bind_rows(
  training_data |> mutate(type = "Actual Revenue"),
  pred_illegal_lasso |> 
    mutate(Revenue = predicted_revenue_lasso,
           type = "Predicted Revenue")
)

ggplot(scatter_df, aes(x = total_pop, y = Revenue, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Population vs Gambling Revenue",
    subtitle = "Comparing actual and predicted states",
    x = "Total Population",
    y = "Revenue",
    color = ""
  ) +
  theme_minimal(base_size = 13)
```

![](predictive_modeling_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Table of Distribution Comparison of Actual vs. Predicted Revenue

``` r
summary_table <- tibble(
  Metric = c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"),
  Actual_Revenue = summary(training_data$Revenue)[c(1,2,3,4,5,6)],
  Predicted_Revenue = summary(pred_illegal_lasso$predicted_revenue_lasso)[c(1,2,3,4,5,6)]
)

kable(summary_table, format = "html", caption = "Distribution Comparison: Actual vs Predicted Revenue") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Distribution Comparison: Actual vs Predicted Revenue
</caption>

<thead>

<tr>

<th style="text-align:left;">

Metric
</th>

<th style="text-align:right;">

Actual_Revenue
</th>

<th style="text-align:right;">

Predicted_Revenue
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Min
</td>

<td style="text-align:right;">

920918
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

1st Quartile
</td>

<td style="text-align:right;">

60627294
</td>

<td style="text-align:right;">

61324867
</td>

</tr>

<tr>

<td style="text-align:left;">

Median
</td>

<td style="text-align:right;">

427397087
</td>

<td style="text-align:right;">

201795708
</td>

</tr>

<tr>

<td style="text-align:left;">

Mean
</td>

<td style="text-align:right;">

443822044
</td>

<td style="text-align:right;">

627940272
</td>

</tr>

<tr>

<td style="text-align:left;">

3rd Quartile
</td>

<td style="text-align:right;">

599344939
</td>

<td style="text-align:right;">

471813698
</td>

</tr>

<tr>

<td style="text-align:left;">

Max
</td>

<td style="text-align:right;">

2059297238
</td>

<td style="text-align:right;">

3611645904
</td>

</tr>

</tbody>

</table>

``` r
# 1. Prepare actual revenue table
actual_table <- training_data %>%
  select(State, Revenue) %>%
  mutate(
    revenue_source = "Actual (Legal States)",
    Revenue = as.numeric(Revenue)
  )

# 2. Prepare predicted revenue table (LASSO)
predicted_table <- pred_illegal_lasso %>%
  select(State, predicted_revenue_lasso) %>%
  rename(Revenue = predicted_revenue_lasso) %>%
  mutate(
    revenue_source = "Predicted (Non-Legal States)"
  )

# 3. Combine both tables
ranked_table <- bind_rows(actual_table, predicted_table) %>%
  arrange(desc(Revenue)) %>%
  mutate(
    Rank = row_number(),
    Revenue = scales::dollar(Revenue)
  ) %>%
  select(Rank, State, Revenue, revenue_source)
```

LASSO Cross-Validation Plot (lamda vs. cross-validation error)

``` r
plot(lasso_cv)
```

![](predictive_modeling_files/figure-gfm/LASSO%20Cross-Validation%20Plot%20(λ%20vs%20CV%20error)-1.png)<!-- -->

LASSO Coefficient Path Plot

``` r
plot(lasso_cv$glmnet.fit, xvar = "lambda", label = TRUE)
```

![](predictive_modeling_files/figure-gfm/LASSO%20Coefficient%20Path%20Plot-1.png)<!-- -->

``` r
coef_mat <- as.matrix(coef(lasso_cv, s = "lambda.min"))
coef_df <- tibble(
  variable = rownames(coef_mat),
  coefficient = coef_mat[, 1]
) %>%
  filter(variable != "(Intercept)")
```

Plot of Actual vs. Predicted Revenue (LASSO)

``` r
pred_lasso_train <- as.numeric(predict(lasso_model, newx = X_train))

training_data_lasso <- training_data %>%
mutate(pred_lasso = pred_lasso_train)

ggplot(training_data_lasso, aes(x = Revenue, y = pred_lasso)) +
geom_point(alpha = 0.7, color = "darkgreen") +
geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
scale_x_continuous(labels = dollar) +
scale_y_continuous(labels = dollar) +
labs(
title = "Actual vs Predicted Revenue (LASSO)",
x = "Actual Revenue",
y = "Predicted Revenue"
) +
theme_minimal(base_size = 13)
```

![](predictive_modeling_files/figure-gfm/Actual%20vs%20Predicted-1.png)<!-- -->

Correlation Map of Potential Predictors

``` r
predictor_mat <- training_data %>%
  select(total_pop, median_income, pct_bachelors, pct_age_20_34,
         pct_urban, pct_poverty) %>%
  as.matrix()

corr_mat <- cor(predictor_mat)

corrplot(
  corr_mat,
  method = "color",
  type = "upper",
  col = viridis(200),   # <-- viridis palette applied here
  tl.col = "black",
  tl.srt = 45
)
```

![](predictive_modeling_files/figure-gfm/CORR%20Map-1.png)<!-- -->

Scatterplot of Revenue vs. Each Predictor

``` r
plot_df <- training_data %>%
select(Revenue, total_pop, median_income, pct_bachelors,
pct_age_20_34, pct_urban, pct_poverty) %>%
pivot_longer(-Revenue, names_to = "predictor", values_to = "value")

ggplot(plot_df, aes(x = value, y = Revenue)) +
geom_point(alpha = 0.7) +
facet_wrap(~ predictor, scales = "free_x") +
scale_y_continuous(labels = dollar) +
labs(
title = "Revenue vs Key Demographic Predictors (Legal States)",
x = "Predictor Value",
y = "Revenue"
) +
theme_minimal(base_size = 13)
```

![](predictive_modeling_files/figure-gfm/Rev%20vs%20each%20predictor%20scatter%20plot-1.png)<!-- -->

Table of Model Performance Comparison Between Linear Model and LASSO

``` r
# --- Linear Model Predictions ---
lm_pred <- predict(fit_lm)

# --- LASSO Predictions ---
lasso_pred <- as.numeric(predict(lasso_model, newx = X_train))

# --- True values ---
y_true <- training_data$Revenue

# --- Comparison Metrics ---
model_metrics <- tibble(
  Model = c("Linear Model", "LASSO Model"),
  RMSE  = c(rmse(y_true, lm_pred),
            rmse(y_true, lasso_pred)),
  MAE   = c(mae(y_true, lm_pred),
            mae(y_true, lasso_pred)),
  MAPE  = c(mape(y_true, lm_pred),
            mape(y_true, lasso_pred))
)

model_metrics
```

    ## # A tibble: 2 × 4
    ##   Model              RMSE        MAE  MAPE
    ##   <chr>             <dbl>      <dbl> <dbl>
    ## 1 Linear Model 200127907. 133430779.  3.44
    ## 2 LASSO Model  202606893. 132201269.  1.91

``` r
model_metrics %>%
  mutate(
    RMSE = scales::dollar(RMSE),
    MAE  = scales::dollar(MAE),
    MAPE = scales::percent(MAPE)
  ) %>%
  kable(format = "html", caption = "Model Performance Comparison: Linear Model vs LASSO") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Model Performance Comparison: Linear Model vs LASSO
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:left;">

RMSE
</th>

<th style="text-align:left;">

MAE
</th>

<th style="text-align:left;">

MAPE
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Linear Model
</td>

<td style="text-align:left;">

\$200,127,907
</td>

<td style="text-align:left;">

\$133,430,779
</td>

<td style="text-align:left;">

344%
</td>

</tr>

<tr>

<td style="text-align:left;">

LASSO Model
</td>

<td style="text-align:left;">

\$202,606,893
</td>

<td style="text-align:left;">

\$132,201,269
</td>

<td style="text-align:left;">

191%
</td>

</tr>

</tbody>

</table>

Column Chart of LASSO Coefficients at Lambda Min

``` r
coef_mat <- as.matrix(coef(lasso_cv, s = "lambda.min"))

coef_df <- tibble(
  variable = rownames(coef_mat),
  coefficient = coef_mat[, "lambda.min"]
) %>%
  filter(variable != "(Intercept)") %>%
  arrange(desc(abs(coefficient)))

coef_df %>%
  mutate(
    coefficient = scales::comma(coefficient)
  ) %>%
  kable(format = "html", caption = "LASSO Feature Importance (λ_min)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

LASSO Feature Importance (λ_min)
</caption>

<thead>

<tr>

<th style="text-align:left;">

variable
</th>

<th style="text-align:left;">

coefficient
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

pct_age_20_34
</td>

<td style="text-align:left;">

-7,298,980,844
</td>

</tr>

<tr>

<td style="text-align:left;">

pct_urban
</td>

<td style="text-align:left;">

211,985,931
</td>

</tr>

<tr>

<td style="text-align:left;">

median_income
</td>

<td style="text-align:left;">

2,418
</td>

</tr>

<tr>

<td style="text-align:left;">

total_pop
</td>

<td style="text-align:left;">

91
</td>

</tr>

<tr>

<td style="text-align:left;">

pct_bachelors
</td>

<td style="text-align:left;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

pct_poverty
</td>

<td style="text-align:left;">

0
</td>

</tr>

</tbody>

</table>

``` r
ggplot(coef_df, aes(x = reorder(variable, abs(coefficient)), 
                    y = coefficient, 
                    fill = coefficient > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
                    guide = "none") +
  labs(
    title = "LASSO Coefficients at λ_min",
    x = "",
    y = "Coefficient Estimate"
  ) +
  theme_minimal(base_size = 13)
```

![](predictive_modeling_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

We compared a standard linear regression to a LASSO regularized model.
Both models have sizeable error due to the extreme variability in state
gambling revenues. While the linear model slightly outperforms LASSO in
RMSE, the LASSO model achieves a much better MAPE (191% vs 344%),
indicating more stable performance for low-revenue states.

LASSO also provides valuable model simplification: only two predictors
retain nonzero coefficients—pct_age_20_34 and pct_urban—while income,
population, poverty, and education are shrunk to zero. This suggests
strong multicollinearity among socioeconomic indicators, with LASSO
consolidating their predictive signal into a smaller set of variables.
The results highlight that demographic structure (pct_age_20_34) and
urbanization are the strongest predictors of sports gambling markets.
