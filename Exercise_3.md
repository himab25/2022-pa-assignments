Exercise 3
================

#### Code from Exercise 2 to be reused

### Setting up data and environment

### We first need to do a few things before we can manipulate the data.

``` r
# set path for R to find our data
data_path <- "C:/Users/Hima25/OneDrive/Documents/USPTO_data/"
```

## 1. Load data

We’ll load application data only here (you are welcome to load the other
three files as well). Because we are loading from a .parquet format
file, we’ll use library `arrow` and the functions `read_parquet()`. For
the rest of the files, we can use function `read_csv()` which comes with
a package `readr` (which is included in `tidyverse` set of packages, so
if we are loading `tidyverse` there is no need to also load `readr`).
Note that the path to the data file on my computer is defined above, in
the `data_path` variable.

``` r
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
```

To inspect the top slice of the data, we can simply call it:

``` r
app_data_sample
```

    ## # A tibble: 2,018,477 × 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # … with 2,018,467 more rows, and 12 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>

### Get gender for examiners

We’ll get gender based on the first name of the examiner, which is
recorded in the field `examiner_name_first`. We’ll use library `gender`
for that, relying on a modified version of their own
[example](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html).

Note that there are over 2 million records in the applications table –
that’s because there are many records for each examiner, as many as the
number of applications that examiner worked on during this time frame.
Our first step therefore is to get all *unique* names in a separate list
`examiner_names`. We will then guess gender for each one and will join
this table back to the original dataset. So, let’s get names without
repetition:

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names
```

    ## # A tibble: 2,595 × 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # … with 2,585 more rows

Now let’s use function `gender()` as shown in the example for the
package to attach a gender and probability to each name and put the
results into the table `examiner_names_gender`

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
examiner_names_gender
```

    ## # A tibble: 1,822 × 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # … with 1,812 more rows

Finally, let’s join that table back to our original applications data
and discard the temporary tables we have just created to reduce clutter
in our environment.

``` r
# remove extra colums from the gender table
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
    ## Ncells  4550737 243.1    7560111 403.8  4949557 264.4
    ## Vcells 49541600 378.0   92450865 705.4 79857387 609.3

### Guess the examiner’s race

We’ll now use package `wru` to estimate likely race of an examiner. Just
like with gender, we’ll get a list of unique names first, only now we
are using surnames.

``` r
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```

    ## # A tibble: 3,806 × 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # … with 3,796 more rows

We’ll follow the instructions for the package outlined here
<https://github.com/kosukeimai/wru>.

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 3,806 × 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198
    ## # … with 3,796 more rows

As you can see, we get probabilities across five broad US Census
categories: white, black, Hispanic, Asian and other. (Some of you may
correctly point out that Hispanic is not a race category in the US
Census, but these are the limitations of this package.)

Our final step here is to pick the race category that has the highest
probability for each last name and then join the table back to the main
applications table. See this example for comparing values across
columns: <https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/>.
And this one for `case_when()` function:
<https://dplyr.tidyverse.org/reference/case_when.html>.

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
examiner_race
```

    ## # A tibble: 3,806 × 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333      0.643 white
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372      0.861 white
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309      0.702 white
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185      0.947 white
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271      0.827 white
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324      0.687 white
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463      0.574 black
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313      0.620 white
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318      0.666 white
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198      0.945 Asian
    ## # … with 3,796 more rows

Let’s join the data back to the applications table.

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4965454 265.2    7560111 403.8  7560111 403.8
    ## Vcells 53341207 407.0   92450865 705.4 90524111 690.7

### Examiner’s tenure

To figure out the timespan for which we observe each examiner in the
applications data, let’s find the first and the last observed date for
each examiner. We’ll first get examiner IDs and application dates in a
separate table, for ease of manipulation. We’ll keep examiner ID (the
field `examiner_id`), and earliest and latest dates for each application
(`filing_date` and `appl_status_date` respectively). We’ll use functions
in package `lubridate` to work with date and time values.

``` r
library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # … with 2,018,467 more rows

The dates look inconsistent in terms of formatting. Let’s make them
consistent. We’ll create new variables `start_date` and `end_date`.

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018)
```

Let’s now identify the earliest and the latest date for each examiner
and calculate the difference in days, which is their tenure in the
organization.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
examiner_dates
```

    ## # A tibble: 5,649 × 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # … with 5,639 more rows

Joining back to the applications data.

``` r
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  4979748 266.0   13551084  723.8  13551084  723.8
    ## Vcells 65720306 501.5  133305245 1017.1 133304793 1017.1

## Exercise 3

#### Libraries for the regression models

``` r
library(modelsummary)
library(dplyr)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

### First we calculate art_unit_size and art_unit_gender_ratio variables

``` r
## calculate art unit size
art_unit_size <- app_data_sample %>% group_by(examiner_art_unit) %>% 
  count()  %>% 
  select(
    art_unit_size = n,
    examiner_art_unit
  )

## joining art_unit_size back to dataset
app_data_sample <- app_data_sample %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

## calculate art_unit_gender_ratio (female to male ratio)

art_unit_gender_size = app_data_sample %>% group_by(examiner_art_unit,gender) %>%
  count() %>%  filter(!is.na(gender)) %>% filter(gender == 'female')

Table_gender_size <- art_unit_gender_size %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

Table_gender_size$female_to_male_ratio = Table_gender_size$n/(Table_gender_size$art_unit_size - Table_gender_size$n)

# remove unused columns
Table_gender_size = subset(Table_gender_size, select = -c(gender))

## joining gender_ratio back to dataset
 
app_data_sample <- app_data_sample %>% 
  left_join(Table_gender_size %>% dplyr::select(female_to_male_ratio), by = "examiner_art_unit")
```

    ## Adding missing grouping variables: `examiner_art_unit`

``` r
# cleaning up
rm(art_unit_size)
rm(art_unit_gender_size)
rm(Table_gender_size)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  5018537 268.1   13551084  723.8  13551084  723.8
    ## Vcells 68817088 525.1  133305245 1017.1 133304793 1017.1

Visually Analysing the two new variables - art_unit_size and
female_to_male_ratio against tenure

``` r
plot(app_data_sample$tenure_days~app_data_sample$art_unit_size)
abline(reg = lm(app_data_sample$tenure_days~app_data_sample$art_unit_size)) 
```

![](Exercise_3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
plot(app_data_sample$tenure_days~app_data_sample$female_to_male_ratio)
abline(reg = lm(app_data_sample$tenure_days~app_data_sample$female_to_male_ratio)) 
```

![](Exercise_3_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

## Linear Regression Models

### Model 1 - Using gender and race to explain effect on tenure_days

``` r
app_data_sample$likely_race = as.factor(app_data_sample$race)
levels(app_data_sample$likely_race)
```

    ## [1] "Asian"    "black"    "Hispanic" "other"    "white"

``` r
mreg_1 = lm(tenure_days ~ as.factor(gender) + likely_race, data=app_data_sample)
summary(mreg_1)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ as.factor(gender) + likely_race, data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5565.1  -561.5   546.9   794.5  1220.2 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           5571.755      2.059 2705.66   <2e-16 ***
    ## as.factor(gender)male -126.557      1.768  -71.57   <2e-16 ***
    ## likely_raceblack       154.695      4.812   32.15   <2e-16 ***
    ## likely_raceHispanic   -319.414      5.009  -63.77   <2e-16 ***
    ## likely_raceother      -649.198     51.299  -12.65   <2e-16 ***
    ## likely_racewhite        75.326      1.967   38.29   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1090 on 1714612 degrees of freedom
    ##   (303859 observations deleted due to missingness)
    ## Multiple R-squared:  0.007932,   Adjusted R-squared:  0.007929 
    ## F-statistic:  2742 on 5 and 1714612 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_1)
```

### Model 2 - Using gender and art_unit_size to explain effect on tenure_days

``` r
mreg_2 = lm(tenure_days ~ art_unit_size + as.factor(gender) , data=app_data_sample)
summary(mreg_2)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(gender), 
    ##     data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5525.7  -552.8   445.3   766.1  1237.6 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            5.175e+03  2.161e+00 2394.58   <2e-16 ***
    ## art_unit_size          4.209e-02  1.543e-04  272.75   <2e-16 ***
    ## as.factor(gender)male -8.279e+01  1.741e+00  -47.54   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1070 on 1714615 degrees of freedom
    ##   (303859 observations deleted due to missingness)
    ## Multiple R-squared:  0.04454,    Adjusted R-squared:  0.04454 
    ## F-statistic: 3.997e+04 on 2 and 1714615 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_2)
```

### Model 3 - Using art_unit_size and race variabes to explain Tenure_days

``` r
mreg_3 <- lm(tenure_days ~ art_unit_size + as.factor(race)  , data=app_data_sample)
summary(mreg_3)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(race), data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5613.2  -539.2   429.1   766.7  1399.2 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              5.090e+03  1.881e+00 2705.26   <2e-16 ***
    ## art_unit_size            4.345e-02  1.403e-04  309.68   <2e-16 ***
    ## as.factor(race)black     9.389e+01  3.842e+00   24.44   <2e-16 ***
    ## as.factor(race)Hispanic -3.035e+02  4.610e+00  -65.83   <2e-16 ***
    ## as.factor(race)other    -5.781e+02  5.049e+01  -11.45   <2e-16 ***
    ## as.factor(race)white     3.313e+01  1.699e+00   19.50   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1073 on 2018471 degrees of freedom
    ## Multiple R-squared:  0.05003,    Adjusted R-squared:  0.05003 
    ## F-statistic: 2.126e+04 on 5 and 2018471 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_3)
```

### Model 4 - Using gender,race and art_unit_size variabes to explain Tenure_days

``` r
mreg_4 = lm(tenure_days ~ art_unit_size + as.factor(gender) + as.factor(race)  , data=app_data_sample)
summary(mreg_4)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(gender) + 
    ##     as.factor(race), data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5532.5  -549.9   448.1   762.8  1407.6 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)              5.171e+03  2.512e+00 2058.903   <2e-16 ***
    ## art_unit_size            4.159e-02  1.552e-04  267.926   <2e-16 ***
    ## as.factor(gender)male   -7.992e+01  1.741e+00  -45.902   <2e-16 ***
    ## as.factor(race)black     1.454e+02  4.714e+00   30.836   <2e-16 ***
    ## as.factor(race)Hispanic -3.070e+02  4.907e+00  -62.568   <2e-16 ***
    ## as.factor(race)other    -5.673e+02  5.026e+01  -11.288   <2e-16 ***
    ## as.factor(race)white     1.736e+01  1.940e+00    8.951   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1068 on 1714611 degrees of freedom
    ##   (303859 observations deleted due to missingness)
    ## Multiple R-squared:  0.0478, Adjusted R-squared:  0.04779 
    ## F-statistic: 1.434e+04 on 6 and 1714611 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_4)
```

### Model 5 - Using gender,race, art_unit_size and female_to_male_ratio variabes to explain Tenure_days

``` r
mreg_5 = lm(tenure_days ~ art_unit_size + as.factor(gender) + as.factor(race) + female_to_male_ratio , data=app_data_sample)
summary(mreg_5)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(gender) + 
    ##     as.factor(race) + female_to_male_ratio, data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5515.5  -549.7   445.3   750.2  1420.9 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)              5.067e+03  2.739e+00 1850.043   <2e-16 ***
    ## art_unit_size            3.985e-02  1.559e-04  255.651   <2e-16 ***
    ## as.factor(gender)male   -2.630e+01  1.821e+00  -14.437   <2e-16 ***
    ## as.factor(race)black     1.521e+02  4.716e+00   32.243   <2e-16 ***
    ## as.factor(race)Hispanic -3.067e+02  4.894e+00  -62.670   <2e-16 ***
    ## as.factor(race)other    -5.762e+02  5.011e+01  -11.499   <2e-16 ***
    ## as.factor(race)white    -4.036e+00  1.948e+00   -2.072   0.0382 *  
    ## female_to_male_ratio     2.109e+02  2.194e+00   96.144   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1065 on 1711234 degrees of freedom
    ##   (307235 observations deleted due to missingness)
    ## Multiple R-squared:  0.05273,    Adjusted R-squared:  0.05273 
    ## F-statistic: 1.361e+04 on 7 and 1711234 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_5)
```

### Model 6 - Using gender, art_unit_size and female_to_male_ratio variabes to explain Tenure_days

``` r
mreg_6 = lm(tenure_days ~ art_unit_size + as.factor(gender) + female_to_male_ratio , data=app_data_sample)
summary(mreg_6)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(gender) + 
    ##     female_to_male_ratio, data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5513.2  -550.4   449.4   753.1  1268.4 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            5.058e+03  2.473e+00 2045.04   <2e-16 ***
    ## art_unit_size          4.011e-02  1.552e-04  258.41   <2e-16 ***
    ## as.factor(gender)male -2.963e+01  1.818e+00  -16.29   <2e-16 ***
    ## female_to_male_ratio   2.126e+02  2.182e+00   97.45   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1067 on 1711238 degrees of freedom
    ##   (307235 observations deleted due to missingness)
    ## Multiple R-squared:  0.04959,    Adjusted R-squared:  0.04958 
    ## F-statistic: 2.976e+04 on 3 and 1711238 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_6)
```

### Model 7 - Using race, art_unit_size and female_to_male_ratio variabes to explain Tenure_days

``` r
mreg_7 = lm(tenure_days ~ art_unit_size + as.factor(race) + female_to_male_ratio , data=app_data_sample)
summary(mreg_7)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(race) + 
    ##     female_to_male_ratio, data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5682.1  -531.5   438.0   757.9  1430.6 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)              5.019e+03  1.974e+00 2542.282  < 2e-16 ***
    ## art_unit_size            4.047e-02  1.418e-04  285.414  < 2e-16 ***
    ## as.factor(race)black     1.047e+02  3.838e+00   27.282  < 2e-16 ***
    ## as.factor(race)Hispanic -3.016e+02  4.593e+00  -65.654  < 2e-16 ***
    ## as.factor(race)other    -5.695e+02  5.029e+01  -11.323  < 2e-16 ***
    ## as.factor(race)white     1.231e+01  1.701e+00    7.237 4.58e-13 ***
    ## female_to_male_ratio     2.430e+02  1.991e+00  122.024  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1069 on 2014150 degrees of freedom
    ##   (4320 observations deleted due to missingness)
    ## Multiple R-squared:  0.0567, Adjusted R-squared:  0.0567 
    ## F-statistic: 2.018e+04 on 6 and 2014150 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_7)
```

### Model 8 - Using art_unit_size and female_to_male_ratio variabes to explain Tenure_days

``` r
mreg_8 = lm(tenure_days ~ art_unit_size + female_to_male_ratio , data=app_data_sample)
summary(mreg_8)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + female_to_male_ratio, 
    ##     data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5681.5  -533.9   443.6   759.8  1290.7 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          5.017e+03  1.706e+00  2941.3   <2e-16 ***
    ## art_unit_size        4.090e-02  1.414e-04   289.2   <2e-16 ***
    ## female_to_male_ratio 2.462e+02  1.983e+00   124.2   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1070 on 2014154 degrees of freedom
    ##   (4320 observations deleted due to missingness)
    ## Multiple R-squared:  0.05395,    Adjusted R-squared:  0.05395 
    ## F-statistic: 5.743e+04 on 2 and 2014154 DF,  p-value: < 2.2e-16

``` r
#modelsummary(mreg_8)
```

## Model Analysis

After performing almost all possible combinations of the multiple linear
models using the four variables, the common observations are :

1.  All 4 variables are significant enough to consider based on the Pr
    value which negates the null hypothesis; denoted by \*\*\*. However,
    model 1 clearly indicates that the variables art_unit_size and
    ar_unit_gender_ratio are most significant for the model than race
    and gender alone (based on a very low value of Adjusted R-squared:
    0.007929)

2.  The Residual standard error ratio is better for models without the
    gender as the given data source results in about 15% of records with
    missing gender (ssa method issue) which when included in the model
    results in a higher Residual standard error ratio as the
    observations get deleted.

3.  The individual Std Error is higher for the race variable compared to
    the other three variables.

Based on the Adjusted R-Squared value (which should be closest to 1),
the below 2 models are best for the final analysis : Model 5 and 7 above

``` r
# Model 5 - Using gender,race, art_unit_size and female_to_male_ratio variabes to explain Tenure_days
# Model 7 - Using race, art_unit_size and female_to_male_ratio variabes to explain Tenure_days

models <- list(
  "Model 5" = mreg_5,
  "Model 7" = mreg_7
)

modelsummary(models)
```

|                         |    Model 5    |    Model 7    |
|:------------------------|:-------------:|:-------------:|
| (Intercept)             |   5066.715    |   5018.805    |
|                         |    (2.739)    |    (1.974)    |
| art_unit_size           |     0.040     |     0.040     |
|                         |    (0.000)    |    (0.000)    |
| as.factor(gender)male   |    -26.295    |               |
|                         |    (1.821)    |               |
| as.factor(race)black    |    152.052    |    104.719    |
|                         |    (4.716)    |    (3.838)    |
| as.factor(race)Hispanic |   -306.718    |   -301.570    |
|                         |    (4.894)    |    (4.593)    |
| as.factor(race)other    |   -576.222    |   -569.473    |
|                         |   (50.113)    |   (50.293)    |
| as.factor(race)white    |    -4.036     |    12.314     |
|                         |    (1.948)    |    (1.701)    |
| female_to_male_ratio    |    210.896    |    242.993    |
|                         |    (2.194)    |    (1.991)    |
| Num.Obs.                |    1711242    |    2014157    |
| R2                      |     0.053     |     0.057     |
| R2 Adj.                 |     0.053     |     0.057     |
| AIC                     |  28712662.9   |  33810632.3   |
| BIC                     |  28712774.1   |  33810732.5   |
| Log.Lik.                | -14356322.456 | -16905308.167 |
| F                       |   13608.718   |   20179.179   |
| RMSE                    |    1064.73    |    1068.81    |

``` r
# cleaning up
rm(mreg_1)
rm(mreg_2)
rm(mreg_3)
rm(mreg_4)
rm(mreg_6)
rm(mreg_8)
gc()
```

    ##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells   6932961  370.3   13551084  723.8  13551084  723.8
    ## Vcells 132441912 1010.5  334324102 2550.7 334207347 2549.8

##### Model Selection :

As seen above from the Regression Output, the Adjusted R-Squared value
for model 7 (tenure \~ race + art_unit_size + female_to_male_ratio) is
the highest(0.057) and is adjusted after removing gender from the model
as seen in model 5 (tenure \~ gender + race + art_unit_size +
female_to_male_ratio) that had a lower value (0.053). This is also
understandable as we have seen missing data values for gender in our
data source mentioned earlier that wouldn’t give us accurate results,
Hence, assessing the fit without gender but with female/male ratio
within an art_unit renders a better model. Chosen model is Model 7.

Now let’s analyse the chosen model 7 and estimate the effects of each
variable in the model

##### Regression Equation from the output is as below :

Tenure = 5018.805 + 0.040*art_unit_size + 104.719*black -
301.570*Hispanic + 12.314*white - 569.473*other +
242.993*female_to_male_ratio

If we had to make a tenure prediction for a male black employee in the
art_unit = 2157 Tenure = 5018.805 + 0.040*5285 + 104.719*1 +
242.993\*0.59 = 5478.29 days

##### Variable Analysis :

For a given examiner_art_unit, -\> more number of females in the unit
than males results in higher tenure irrespective of race -\> greater
unit size results in higher tenure irrespective of race -\> A black
person has additional tenure of 104.7 days -\> Hispanic and other races
have lower tenure compared to black, whites and even Asians

Thus, Art_unit_size and female_to_male_ratio bear significant
relationship with tenure days. Race alone not extremely significant. In
addition, from the data itself it can be seen that there are certain
units (e.g unit 2456) where the ratio of females is extremely low (4 in
10000), these entries may need correction.

##### Conclusions and Recommendations :

While gender may be a significant factor in determining tenure , its not
a necessary one. Based on the above chosen model(excluding gender), it
shows that for an art_unit, the size of the unit combined with more or
equal number of females results in higher tenure days for the employees
overall. Hence,USTPO can focus on having larger unit sizes with an equal
distribution of gender to maintain a healthy tenure. Moreover having
Whites,Blacks or Asians may have more or less same tenure days but other
races have slightly reduced tenure,so efforts can be made to look into
why these races such as Hispanic are having higher attrition from the
unit.
