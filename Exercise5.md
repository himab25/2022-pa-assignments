Exercise 5
================

## Loading data and preparing variables

Here, I’m using the steps from exercise 2 example to create the
necessary variables.

``` r
# set path for R to find our data
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.6     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
data_path <- "C:/Users/Hima25/OneDrive/Desktop/Ppl Ana/USPTO_data/"
library(arrow) # to be able to load data in the .parquet format
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
examiner_gs <- read_csv(paste0(data_path,"examiner_gs.csv"))
```

    ## Rows: 52109 Columns: 6

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): examiner_name, start_date, end_date
    ## dbl (3): examiner_grade, old_pid, new_pid
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it

# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)

examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

# remove extra columns from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4587290 245.0    8176881 436.7  4607485 246.1
    ## Vcells 49940873 381.1   93517906 713.5 80256671 612.4

``` r
# Examiners' race
library(wru)

examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race <- examiner_race %>% 
  select(surname,race)

app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  5001744 267.2    8176881 436.7  8176881 436.7
    ## Vcells 53739461 410.0   93517906 713.5 91931162 701.4

``` r
# Examiner's tenure

library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018) %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )

app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  5016255 267.9   14706799  785.5  14706799  785.5
    ## Vcells 66118973 504.5  134841784 1028.8 134775155 1028.3

## Adding paygrade data

First, we load the paygrade file.

``` r
examiner_gs <- read_csv(paste0(data_path,"examiner_gs.csv"))
```

    ## Rows: 52109 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): examiner_name, start_date, end_date
    ## dbl (3): examiner_grade, old_pid, new_pid
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
examiner_ids <- read_csv(paste0(data_path,"examiner_ids.csv"))
```

    ## Rows: 19454 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): examiner_name
    ## dbl (3): old_pid, new_pid, patex_id
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### We need to replace various IDs with examiner ID

The ID fields in the `examiner_gs.csv` file don’t match those in the
application data file. Because we’ll need to join these files later, we
need to bring in the proper ID field, by using the cross-walk file
`examiner_ids`.

``` r
examiner_gs <- examiner_gs %>% 
  left_join(examiner_ids) %>% 
  select(
    grade = examiner_grade,
    start_date,
    end_date,
    examiner_id = patex_id
  )
```

    ## Joining, by = c("examiner_name", "old_pid", "new_pid")

## Estimate time in grade

Now we need to estimate the average time each examiner spends in a given
GS paygrade. Note that the less-biased way to do that is to exclude the
latest or highest grade for each examiner. This is because after
examiners reach the highest grade (which is usually grade 14 in this
context), they cannot advance to the next grade. Imagine someone who
advances through grades 11, 12 and 13 in just 18 months (so, the average
for that examiner is 6 months per grade), but then stays in grade 14 and
works at the agency for another ten years. If you were to average all
grades, it would look like it took the examiner several years on average
to get promoted through each grade. This is because the last grade
biases the average positively.

Similarly, for examiners who get promoted right before the end of our
sample’s time window, the very short time they stay in the highest
observed grade will negatively bias the average. It will look like the
examiner has progressed through the grades way faster than she did.

``` r
time_in_grade <- examiner_gs %>% 
  mutate(
    start_date = mdy(start_date), # converting into proper date type
    end_date = mdy(end_date), # converting into proper date type
    days_in_grade = interval(start_date, end_date) %/% days(1)
  ) %>% 
  group_by(examiner_id) %>% 
  filter(grade!=max(grade, na.rm = TRUE)) %>% # dropping the highest grade record
  summarise(mean_days_in_grade = mean(days_in_grade, na.rm = TRUE))
time_in_grade
```

    ## # A tibble: 10,860 × 2
    ##    examiner_id mean_days_in_grade
    ##          <dbl>              <dbl>
    ##  1       59012               356.
    ##  2       59015               783 
    ##  3       59016               341.
    ##  4       59018               368.
    ##  5       59019               293 
    ##  6       59025               485 
    ##  7       59027               364.
    ##  8       59030               493.
    ##  9       59033               258.
    ## 10       59035               308.
    ## # … with 10,850 more rows

## Prepare application data

Let’s get the measure of application processing time for each examiner.
We’ll do this by “collapsing” the dataframe from application level to
examiner level (i.e., one record for each examiner, not multiple
records).

``` r
examiner_data <- app_data_sample %>% 
  filter(disposal_type!="PEND") %>% # here, we exclude in-process applications
  mutate(
    app_start_date = ymd(filing_date),
    app_end_date = case_when(
      disposal_type == "ISS" ~ ymd(patent_issue_date), # for issued patents
      disposal_type == "ABN" ~ ymd(abandon_date), # for abandoned applications
      TRUE ~ NA_Date_
    ),
    app_proc_days = interval(app_start_date, app_end_date) %/% days(1)) %>% 
  filter(app_proc_days>0 & app_proc_days < 3650) %>% # limit to 0-10 years
  group_by(examiner_id) %>% 
  summarise(
    app_count = n(),
    tc = min(tc, na.rm = TRUE),
    gender = first(gender),
    race = first(race),
    tenure_days = max(tenure_days, na.rm = TRUE),
    mean_app_proc_days = mean(app_proc_days, na.rm = TRUE)
  )
examiner_data
```

    ## # A tibble: 5,549 × 7
    ##    examiner_id app_count    tc gender race  tenure_days mean_app_proc_days
    ##          <dbl>     <int> <dbl> <chr>  <chr>       <dbl>              <dbl>
    ##  1       59012        84  1700 male   white        4013              1295.
    ##  2       59025        96  2400 male   Asian        2761              1152.
    ##  3       59030       358  2400 <NA>   black        4179              1008.
    ##  4       59040       233  1700 female Asian        3542              1305.
    ##  5       59052         8  2100 male   Asian        2017               535.
    ##  6       59054        10  2100 <NA>   Asian        5887              1297 
    ##  7       59055         2  2100 male   Asian        1149               932.
    ##  8       59056      1019  2100 male   Asian        6268              1077.
    ##  9       59074       166  2100 <NA>   white        6255              1579.
    ## 10       59081        48  2400 male   Asian        2220              1317.
    ## # … with 5,539 more rows

Now, let’s join in the time in grade data.

``` r
examiner_data <- examiner_data %>% 
  left_join(time_in_grade)
```

    ## Joining, by = "examiner_id"

``` r
examiner_data
```

    ## # A tibble: 5,549 × 8
    ##    examiner_id app_count    tc gender race  tenure_days mean_app_proc_days
    ##          <dbl>     <int> <dbl> <chr>  <chr>       <dbl>              <dbl>
    ##  1       59012        84  1700 male   white        4013              1295.
    ##  2       59025        96  2400 male   Asian        2761              1152.
    ##  3       59030       358  2400 <NA>   black        4179              1008.
    ##  4       59040       233  1700 female Asian        3542              1305.
    ##  5       59052         8  2100 male   Asian        2017               535.
    ##  6       59054        10  2100 <NA>   Asian        5887              1297 
    ##  7       59055         2  2100 male   Asian        1149               932.
    ##  8       59056      1019  2100 male   Asian        6268              1077.
    ##  9       59074       166  2100 <NA>   white        6255              1579.
    ## 10       59081        48  2400 male   Asian        2220              1317.
    ## # … with 5,539 more rows, and 1 more variable: mean_days_in_grade <dbl>

## Descriptive statistics & Questions

## Q1. How long does it take, on average, men and women to advance to the next pay grade?

``` r
gender_mean_advancement_time = examiner_data %>% group_by(gender) %>% summarise(Days_to_next_paygrade = mean(mean_days_in_grade, na.rm = TRUE)) %>% filter(!is.na(gender))
gender_mean_advancement_time
```

    ## # A tibble: 2 × 2
    ##   gender Days_to_next_paygrade
    ##   <chr>                  <dbl>
    ## 1 female                  542.
    ## 2 male                    546.

On average, it takes men 578 days to advance to the next pay grade and
it takes women 575 days. There is not a significant difference between
days required for promotion for men and women.

``` r
ggplot(data = gender_mean_advancement_time, mapping = aes(x = as_factor(gender), y = Days_to_next_paygrade)) + geom_bar(stat="identity")
```

![](Exercise5_files/figure-gfm/plot%20gender_mean_advancement_time-1.png)<!-- -->

## Q2. Are these differences themselves different by examiners’ race?

``` r
race_mean_advancement_time = examiner_data %>% group_by(race) %>% summarise(Days_to_next_paygrade = mean(mean_days_in_grade, na.rm = TRUE))
race_mean_advancement_time
```

    ## # A tibble: 5 × 2
    ##   race     Days_to_next_paygrade
    ##   <chr>                    <dbl>
    ## 1 Asian                     535.
    ## 2 black                     589.
    ## 3 Hispanic                  504.
    ## 4 other                     467.
    ## 5 white                     551.

It took white agent average 589 days to advance to the next pay grade,
An Asian agent 554 days, Black agent 625 day, Hispanic agent 509 days
and Others 467 days to advance to the next pay grade.

There are slight differences between various races to advance to the
next paygrade with Blacks taking the longest amount of time and other
races taking the least but the difference is not substantial to jump to
any conclusions. In addition, the amount of time spent per grade for
each race is also quite varied. Hence, race may or may not have an
effect on the average number of days to advance to the next pay grade.

``` r
ggplot(data = race_mean_advancement_time, mapping = aes(x = as_factor(race), y = Days_to_next_paygrade)) + geom_bar(stat="identity")
```

![](Exercise5_files/figure-gfm/plot%20race_mean_advancement_time-1.png)<!-- -->

## Q3. Is there a relationship (an association) between average time in grade and average application prosecution time?

### Calculate correlation

``` r
library(ggpubr)

cor.test(examiner_data$mean_days_in_grade,examiner_data$mean_app_proc_days, method=c("pearson", "kendall", "spearman"))
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  examiner_data$mean_days_in_grade and examiner_data$mean_app_proc_days
    ## t = 0.39953, df = 4501, p-value = 0.6895
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.02325809  0.03515805
    ## sample estimates:
    ##         cor 
    ## 0.005955058

Correlation coefficient between average time in grade and average
application prosecution time equals to 0.006 or 0.6%, which is very
small hence no correlation.

``` r
ggscatter(examiner_data, x = "mean_days_in_grade", y = "mean_app_proc_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          )
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1046 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1046 rows containing non-finite values (stat_cor).

    ## Warning: Removed 1046 rows containing missing values (geom_point).

![](Exercise5_files/figure-gfm/plot%20procdays%20vs%20mean%20days%20in%20grade-1.png)<!-- -->

Scatterplot shows there is no linear regression relationship between
these two factors.

### Calculating Mean days in grade based on Gender & race

``` r
gen_race_mean_advancement_time = examiner_data %>% group_by(gender,race) %>% summarise(Days_to_next_paygrade = mean(mean_days_in_grade, na.rm = TRUE)) %>% filter(!is.na(gender))
```

    ## `summarise()` has grouped output by 'gender'. You can override using the
    ## `.groups` argument.

``` r
ggplot(data = gen_race_mean_advancement_time, mapping = aes(x = as_factor(race), y = Days_to_next_paygrade, fill = as_factor(gender))) + geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = round(Days_to_next_paygrade,1)),position = position_dodge(0.9), color="white",vjust = 1,hjust = 0.5)
```

![](Exercise5_files/figure-gfm/barplot-1.png)<!-- -->

Plot for correlation between prosecution days, gender and days in grade

``` r
ggplot(data=examiner_data, 
       mapping=aes(x=mean_app_proc_days, 
                   y=mean_days_in_grade, 
                   color=gender))+ 
  geom_point(size=2)+ 
  geom_smooth(method=lm, se = F)+ 
  xlab("Average Application Prosecution Time")+
  ylab("Average Time in Grade")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1046 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1046 rows containing missing values (geom_point).

![](Exercise5_files/figure-gfm/scatter%20plot1-1.png)<!-- -->

Plot for correlation between prosecution days, race and days in grade

``` r
ggplot(data=examiner_data, 
       mapping=aes(x=mean_app_proc_days, 
                   y=mean_days_in_grade, 
                   color=race))+ 
  geom_point(size=2)+ 
  geom_smooth(method=lm, se = F)+ 
  xlab("Average Application Prosecution Time")+
  ylab("Average Time in Grade")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1046 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1046 rows containing missing values (geom_point).

![](Exercise5_files/figure-gfm/scatter%20plot2-1.png)<!-- -->

## Regression models

Let’s run a couple of simple regressions.

``` r
library(modelsummary)
models <- list()
models[['m1']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days, data = examiner_data) 
models[['m2']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(gender), 
         data = examiner_data) 
models[['m3']] <- lm(mean_days_in_grade ~ 1 + as_factor(gender), data = examiner_data)
models[['m4']] <- lm(mean_days_in_grade ~ 1 + as_factor(race), data = examiner_data) 
models[['m5']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(race), 
         data = examiner_data) 


modelsummary(models)
```

|                         |     m1     |     m2     |     m3     |     m4     |     m5     |
|:------------------------|:----------:|:----------:|:----------:|:----------:|:----------:|
| (Intercept)             |  528.481   |  550.975   |  546.177   |  551.004   |  531.761   |
|                         |  (43.856)  |  (49.860)  |  (13.223)  |  (12.504)  |  (44.213)  |
| mean_app_proc_days      |   0.014    |   -0.004   |            |            |   0.016    |
|                         |  (0.035)   |  (0.039)   |            |            |  (0.035)   |
| as_factor(gender)female |            |   -4.166   |   -4.021   |            |            |
|                         |            |  (23.854)  |  (23.807)  |            |            |
| as_factor(race)Asian    |            |            |            |  -16.496   |  -17.130   |
|                         |            |            |            |  (21.580)  |  (21.627)  |
| as_factor(race)black    |            |            |            |   38.269   |   38.196   |
|                         |            |            |            |  (49.226)  |  (49.231)  |
| as_factor(race)Hispanic |            |            |            |  -46.798   |  -46.940   |
|                         |            |            |            |  (49.348)  |  (49.354)  |
| as_factor(race)other    |            |            |            |  -83.804   |  -86.266   |
|                         |            |            |            | (654.666)  | (654.746)  |
| Num.Obs.                |    4503    |    3838    |    3838    |    4503    |    4503    |
| R2                      |   0.000    |   0.000    |   0.000    |   0.000    |   0.001    |
| R2 Adj.                 |   0.000    |   -0.001   |   0.000    |   0.000    |   -0.001   |
| AIC                     |  71176.4   |  60975.0   |  60973.0   |  71180.3   |  71182.1   |
| BIC                     |  71195.6   |  61000.0   |  60991.8   |  71218.8   |  71227.0   |
| Log.Lik.                | -35585.191 | -30483.507 | -30483.512 | -35584.174 | -35584.071 |
| F                       |   0.160    |   0.019    |   0.029    |   0.548    |   0.480    |
| RMSE                    |   654.48   |   681.30   |   681.21   |   654.55   |   654.60   |

The regression models output table above with an adjusted R^2 values for
each (all less than 0.001) doesn’t clearly show any correlation between
average application prosecution time, gender and race, hence no
significant relationship between average time in one grade and the
average application prosecution time.

## Q4. Write a substantive conclusion, discussing what these results could mean. Make sure to discuss implications but also the limitations of your approach and possible threats to inference (this point must be done individually and not in a group)

Conclusions :

1.  An agent’s promotion is clearly not based upon application
    processing time as well as gender or race. Hence. it doesn’t
    directly reflect the agent’s performance and thus his promotion.
    There could eb other factors(not considered here) that are driving
    the promotion. From this analysis, the promotion seems to be very
    time bound and linear.

2.  No matter what the processing time of the application, agents get
    promoted eventually. This shows either there are other important
    factors driving an employee promotion in this organization other
    than race/gender (ex: contacts, quality of processing,etc.) or there
    is no incentive at all other than time in the organization.

Limitations :

1.  Missing gender of some examiners and the unknown average days in a
    pay grade of some examiners can cause discrepancy.
2.  There is a wide gap in the number of employees within each race
    which itself is not taken into account when trying to find
    correlation.
3.  There were certain negative mean days in grade values potentially
    caused by mismatched startdate and enddates. Data handling had to be
    corrected.

Due to the above limitations of the analysis,maybe a non-linear
relationship exists between the average days per grade and the factors
been considered which got unnoticed.

Code contributors : Ranvir,Patrick,Hima,Shan
