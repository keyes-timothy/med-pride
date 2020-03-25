Needs Assessment General Report
================
Timothy Keyes
2020-03-24

  - [General data summary](#general-data-summary)
  - [Wrangling](#wrangling)
  - [General Demographics report](#general-demographics-report)
      - [What school do you attend?](#what-school-do-you-attend)
      - [What year of medical school are you
        in?](#what-year-of-medical-school-are-you-in)
      - [Do you identify as LGBTQ+?](#do-you-identify-as-lgbtq)
      - [What is your sex assigned at
        birth?](#what-is-your-sex-assigned-at-birth)
      - [What is your gender identity?](#what-is-your-gender-identity)

``` r
# Libraries
library(tidyverse)
library(lubridate)

# Parameters
input_path <- here::here("data", "mspa_na_data.rds")
metadata_path <- here::here("data-raw", "school_metadata.csv")

my_theme <- 
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 12, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold")
  ) 

#===============================================================================

# read in data
na_data <- 
  input_path %>% 
  read_rds()

metadata <- 
  metadata_path %>% 
  read_csv()
```

## General data summary

First, we want to see if we were able to obtain consent from everyone
who took the survey:

``` r
na_data %>% 
  count(consent)
```

    ## # A tibble: 2 x 2
    ##   consent     n
    ##   <chr>   <int>
    ## 1 no         32
    ## 2 yes      1148

Thus, we can see that there are 32 people who didn’t actually check the
“consent” box. They will need to be removed from the analysis.

We can also look, for our own purposes, of the number of responses over
time window that the survey was open.

``` r
na_data %>% 
  count(date = lubridate::as_date(timestamp), name = "responses") %>% 
  ggplot(aes(x = date, y = responses)) + 
  geom_line() + 
  scale_x_date(
    breaks = scales::breaks_width(width = "2 months"), 
    labels = scales::label_date(format = "%b %Y")
  ) + 
  labs(
    title = "Number of responses to the N-A survey by date", 
    x = NULL, 
    y = "Responses (# of students per day)"
  )
```

    ## Warning: 26 failed to parse.

    ## Warning: Removed 1 rows containing missing values (geom_path).

![](na_general_report_1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

From this, we can see that recruitment tends to happen in a punctuated
fashion rather than by trickle-in over time. Each of the peaks
corresponds to pushes coordinated by our team to send the survey to
additional schools.

For a cumulative plot…

``` r
num_students <- function(date) { 
  na_data %>% 
    mutate(my_date = as_date(timestamp)) %>% 
    filter(my_date <= date) %>% 
    count() %>% 
    pull(n)
}

na_data %>% 
  transmute(
    my_date = as_date(timestamp), 
    unique_students = 
      map_int(my_date, num_students)
  ) %>% 
  ggplot(aes(x = my_date, y = unique_students)) + 
  geom_line() + 
  scale_x_date(
    breaks = scales::breaks_width(width = "2 months"), 
    labels = scales::label_date(format = "%b %Y")
  ) + 
  labs(
    title = "Respondent recruitment over time", 
    x = NULL, 
    y = "Number of unique students"
  )
```

![](na_general_report_1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We can also look at our recruitment of unique schools over time:

``` r
num_schools <- function(date) { 
  na_data %>% 
    mutate(my_date = as_date(timestamp)) %>% 
    filter(my_date <= date) %>% 
    pull(school_attend) %>% 
    n_distinct()
}


na_data %>% 
  transmute(
    my_date = as_date(timestamp), 
    unique_schools = 
      map_int(my_date, num_schools)
  ) %>% 
  ggplot(aes(x = my_date, y = unique_schools)) + 
  geom_line() + 
  scale_x_date(
    breaks = scales::breaks_width(width = "2 months"), 
    labels = scales::label_date(format = "%b %Y")
  ) + 
  labs(
    title = "School recruitment over time", 
    x = NULL, 
    y = "Number of unique schools recruited"
  )
```

![](na_general_report_1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Together, these plots indicate that, in general, having a survey link
passively open over time does not really lead to the steady “trickling
in” of responses over time. Rather, it seems that coordinated pushes of
survey instruments to specific schools (with wide desemination) yield
the most responses, Thus, one strategic recommendation for MSPA might be
to build its survey instrument distribution infrastructure (either
through a portal, an app that MSPA members can have on their phones, or
some other architecture that leverages our campus network).

## Wrangling

First, we want to remove people who did not consent to the survey.

``` r
na_data <- 
  na_data %>% 
  filter(consent == "yes")
```

And then we can filter out some variables that we don’t need.

``` r
na_data <- 
  na_data %>% 
  select(-survey_id, -consent, -complete)
```

And create a few variables that we’ll need later:

``` r
na_data <- 
  na_data %>% 
  mutate(
    sex = 
      case_when(
        sab_is_male == "yes"   ~ "male", 
        sab_is_female == "yes" ~ "female", 
        TRUE                   ~ NA_character_
      )
  )
```

## General Demographics report

The first section of the survey asked questions about respondents’
demographic identifiers, including the following:

  - What school they attend
  - What year of medical school they’re in
  - Whether or not they identify as LGBTQ+
  - What sex they were assigned at birth
  - What their current gender identity is
  - What they current sexual orientation is
  - Their race/ethnicity

Below, we report the breakdown of our survey respondents by each of
these demographics.

### What school do you attend?

Schools are arranged in order of decreasing number of responses.

``` r
na_data %>%
  count(school_attend, name = "Number of responses") %>% 
  mutate(
    `Percentage of total responses` = 
      ((`Number of responses` / sum(`Number of responses`)) * 100) %>% 
      round(digits = 1)
  ) %>% 
  rename(School = school_attend) %>% 
  arrange(desc(`Number of responses`)) %>% 
  knitr::kable()
```

| School                                                                                | Number of responses | Percentage of total responses |
| :------------------------------------------------------------------------------------ | ------------------: | ----------------------------: |
| Temple University School of Medicine                                                  |                  84 |                           7.3 |
| University of Louisville School of Medicine                                           |                  62 |                           5.4 |
| University of Oklahoma College of Medicine                                            |                  57 |                           5.0 |
| Geisinger Commonwealth School of Medicine                                             |                  44 |                           3.8 |
| Stanford University School of Medicine                                                |                  40 |                           3.5 |
| University of Pittsburgh School of Medicine                                           |                  39 |                           3.4 |
| University of New Mexico School of Medicine                                           |                  37 |                           3.2 |
| Saint Louis University School of Medicine                                             |                  34 |                           3.0 |
| University of Alabama School of Medicine                                              |                  34 |                           3.0 |
| University of Arkansas for Medical Sciences/UAMS College of Medicine                  |                  34 |                           3.0 |
| Johns Hopkins University School of Medicine                                           |                  32 |                           2.8 |
| University of Michigan Medical School                                                 |                  32 |                           2.8 |
| Western Michigan University Homer Stryker M.D. School of Medicine                     |                  30 |                           2.6 |
| Dell Medical School at The University of Texas at Austin                              |                  28 |                           2.4 |
| Chicago Medical School of Rosalind Franklin University of Medicine and Science        |                  26 |                           2.3 |
| Touro University California College of Osteopathic Medicine                           |                  23 |                           2.0 |
| University of Wisconsin School of Medicine and Public Health                          |                  23 |                           2.0 |
| Georgetown University School of Medicine                                              |                  22 |                           1.9 |
| Tulane University School of Medicine                                                  |                  22 |                           1.9 |
| Jacobs School of Medicine and Biomedical Sciences, University at Buffalo              |                  21 |                           1.8 |
| Rutgers New Jersey Medical School                                                     |                  18 |                           1.6 |
| University of Texas Southwestern Medical School at Dallas                             |                  17 |                           1.5 |
| Harvard Medical School                                                                |                  15 |                           1.3 |
| Columbia University Roy and Diana Vagelos College of Physicians and Surgeons          |                  14 |                           1.2 |
| Washington University School of Medicine                                              |                  14 |                           1.2 |
| Weill Cornell Medical College                                                         |                  14 |                           1.2 |
| NA                                                                                    |                  14 |                           1.2 |
| Albert Einstein College of Medicine                                                   |                  13 |                           1.1 |
| Alpert Medical School at Brown University                                             |                  13 |                           1.1 |
| University of California, Irvine School of Medicine                                   |                  13 |                           1.1 |
| University of Texas School of Medicine at San Antonio                                 |                  11 |                           1.0 |
| University of Vermont College of Medicine                                             |                  11 |                           1.0 |
| Cooper Medical School of Rowan University                                             |                  10 |                           0.9 |
| University of Iowa Roy J. and Lucille A. Carver College of Medicine                   |                   9 |                           0.8 |
| Vanderbilt University School of Medicine                                              |                   9 |                           0.8 |
| West Virginia University School of Medicine                                           |                   9 |                           0.8 |
| Keck School of Medicine of University of Southern California                          |                   8 |                           0.7 |
| University of North Texas Health Science Center Texas College of Osteopathic Medicine |                   8 |                           0.7 |
| Baylor College of Medicine                                                            |                   7 |                           0.6 |
| New York University School of Medicine                                                |                   7 |                           0.6 |
| Philadelphia College of Osteopathic Medicine - Georgia Campus                         |                   7 |                           0.6 |
| State University of New York Downstate Medical Center College of Medicine             |                   7 |                           0.6 |
| University of South Alabama College of Medicine                                       |                   7 |                           0.6 |
| International                                                                         |                   6 |                           0.5 |
| Michigan State University College of Human Medicine                                   |                   6 |                           0.5 |
| University of California, San Fransisco School of Medicine                            |                   6 |                           0.5 |
| Yale School of Medicine                                                               |                   6 |                           0.5 |
| David Geffen School of Medicine at UCLA                                               |                   5 |                           0.4 |
| Northwestern University Feinberg School of Medicine                                   |                   5 |                           0.4 |
| University of California, Davis School of Medicine                                    |                   5 |                           0.4 |
| University of Florida College of Medicine                                             |                   5 |                           0.4 |
| Edward Via College of Osteopathic Medicine                                            |                   4 |                           0.3 |
| Pennsylvania State University College of Medicine                                     |                   4 |                           0.3 |
| Perelman School of Medicine at the University of Pennsylvania                         |                   4 |                           0.3 |
| Philadelphia College of Osteopathic Medicine                                          |                   4 |                           0.3 |
| University of Utah School of Medicine                                                 |                   4 |                           0.3 |
| University of Washington School of Medicine                                           |                   4 |                           0.3 |
| Medical College of Wisconsin                                                          |                   3 |                           0.3 |
| Oklahoma State University Center for Health Sciences College of Osteopathic Medicine  |                   3 |                           0.3 |
| Sidney Kimmel Medical College at Thomas Jefferson University                          |                   3 |                           0.3 |
| Tufts University School of Medicine                                                   |                   3 |                           0.3 |
| University of Chicago Pritzker School of Medicine                                     |                   3 |                           0.3 |
| University of Texas Medical School at Houston                                         |                   3 |                           0.3 |
| A. T. Still University Kirksville College of Osteopathic Medicine                     |                   2 |                           0.2 |
| Dartmouth College Geisel School of Medicine                                           |                   2 |                           0.2 |
| Des Moines University College of Osteopathic Medicine                                 |                   2 |                           0.2 |
| Emory University School of Medicine                                                   |                   2 |                           0.2 |
| Hackensack Meridian School of Medicine                                                |                   2 |                           0.2 |
| Icahn School of Medicine at Mount Sinai                                               |                   2 |                           0.2 |
| Indiana University School of Medicine                                                 |                   2 |                           0.2 |
| Loma Linda University School of Medicine                                              |                   2 |                           0.2 |
| Ohio University Heritage College of Osteopathic Medicine                              |                   2 |                           0.2 |
| Oregon Health & Science University School of Medicine                                 |                   2 |                           0.2 |
| Rowan University School of Osteopathic Medicine                                       |                   2 |                           0.2 |
| Rutgers Robert Wood Johnson Medical School                                            |                   2 |                           0.2 |
| Sanford School of Medicine of the University of South Dakota                          |                   2 |                           0.2 |
| Stony Brook University School of Medicine                                             |                   2 |                           0.2 |
| Texas Tech University Health Sciences Center School of Medicine                       |                   2 |                           0.2 |
| Touro College of Osteopathic Medicine                                                 |                   2 |                           0.2 |
| Touro University Nevada College of Osteopathic Medicine                               |                   2 |                           0.2 |
| University of Arizona College of Medicine - Phoenix                                   |                   2 |                           0.2 |
| University of Illinois at Urbana-Champaign Carle Illinois College of Medicine         |                   2 |                           0.2 |
| University of Illinois College of Medicine                                            |                   2 |                           0.2 |
| University of Massachusetts Medical School                                            |                   2 |                           0.2 |
| University of Nebraska College of Medicine                                            |                   2 |                           0.2 |
| University of Rochester School of Medicine and Dentistry                              |                   2 |                           0.2 |
| University of Toledo College of Medicine                                              |                   2 |                           0.2 |
| Wayne State University School of Medicine                                             |                   2 |                           0.2 |
| Boonshoft School of Medicine at Wright State University                               |                   1 |                           0.1 |
| Boston University School of Medicine                                                  |                   1 |                           0.1 |
| Burrell College of Osteopathic Medicine at New Mexico State University                |                   1 |                           0.1 |
| Campbell University School of Osteopathic Medicine                                    |                   1 |                           0.1 |
| Charles R. Drew University of Medicine and Science                                    |                   1 |                           0.1 |
| Creighton University School of Medicine                                               |                   1 |                           0.1 |
| Donald and Barbara Zucker School of Medicine at Hofstra/Northwell                     |                   1 |                           0.1 |
| Drexel University College of Medicine                                                 |                   1 |                           0.1 |
| Duke University School of Medicine                                                    |                   1 |                           0.1 |
| Edward Via College of Osteopathic Medicine- Carolinas Campus                          |                   1 |                           0.1 |
| Florida Atlantic University Charles E. Schmidt College of Medicine                    |                   1 |                           0.1 |
| Marian University College of Osteopathic Medicine                                     |                   1 |                           0.1 |
| Mayo Clinic College of Medicine                                                       |                   1 |                           0.1 |
| Meharry Medical College School of Medicine                                            |                   1 |                           0.1 |
| Nova Southeastern University College of Osteopathic Medicine                          |                   1 |                           0.1 |
| Oakland University William Beaumont School of Medicine                                |                   1 |                           0.1 |
| Ross University School of Medicine & Veterinary Medicine                              |                   1 |                           0.1 |
| Rush Medical College                                                                  |                   1 |                           0.1 |
| San Juan Bautista School of Medicine                                                  |                   1 |                           0.1 |
| Southern Illinois University School of Medicine                                       |                   1 |                           0.1 |
| State University of New York Upstate Medical University                               |                   1 |                           0.1 |
| The Ohio State University College of Medicine                                         |                   1 |                           0.1 |
| University of Colorado School of Medicine                                             |                   1 |                           0.1 |
| University of Kansas School of Medicine                                               |                   1 |                           0.1 |
| University of Kentucky College of Medicine                                            |                   1 |                           0.1 |
| University of Maryland School of Medicine                                             |                   1 |                           0.1 |
| University of Minnesota Medical School                                                |                   1 |                           0.1 |
| University of Nevada, Las Vegas School of Medicine                                    |                   1 |                           0.1 |
| University of North Carolina School of Medicine                                       |                   1 |                           0.1 |
| University of Texas Medical Branch School of Medicine                                 |                   1 |                           0.1 |
| University of Virginia School of Medicine                                             |                   1 |                           0.1 |
| Washington State University Elson S. Floyd College of Medicine                        |                   1 |                           0.1 |
| West Virginia School of Osteopathic Medicine                                          |                   1 |                           0.1 |

### What year of medical school are you in?

``` r
n <- 
  na_data %>% 
  filter(!is.na(med_school_year)) %>% 
  count() %>% 
  pull(n)

na_data %>% 
  drop_na(med_school_year) %>% 
  mutate(
    med_school_year = fct_infreq(med_school_year %>% str_wrap(width = 25))
  ) %>% 
  ggplot(aes(x = med_school_year)) + 
  geom_bar() + 
  my_theme + 
  labs(
    x = NULL, 
    y = "Number of students", 
    caption = str_glue("N = ", {n})
  )
```

![](na_general_report_1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Do you identify as LGBTQ+?

``` r
n <- 
  na_data %>% 
  filter(!is.na(is_lgbtq)) %>% 
  count() %>% 
  pull(n)

na_data %>% 
  drop_na(is_lgbtq) %>% 
  ggplot(aes(x = is_lgbtq)) + 
  geom_bar() + 
  labs(
    x = NULL, 
    y = "Number of respondents", 
    caption = str_glue("N = {n}")
  )
```

![](na_general_report_1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### What is your sex assigned at birth?

``` r
n <- 
  na_data %>% 
  filter(!is.na(sex)) %>% 
  count() %>% 
  pull(n)

na_data %>% 
  drop_na(sex) %>% 
  ggplot(aes(x = sex)) + 
  geom_bar() + 
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 16)
  ) + 
  labs(
    x = NULL, 
    y = "Number of students", 
    caption = str_glue("N = {n}")
  )
```

![](na_general_report_1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### What is your gender identity?

``` r
n <- 
  na_data %>% 
  filter_at(
    .vars = vars(starts_with("gender_")), 
    .vars_predicate = any_vars(!is.na(.))
  ) %>% 
  count() %>% 
  pull(n)

na_data %>% 
  summarize_at(
    .vars = vars(starts_with("gender")), 
    ~ sum(. == "yes")
  ) %>% 
  select(-gender_another_description) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "gender", 
    values_to = "Number of respondents", 
    names_prefix = "gender_"
  ) %>% 
  mutate(
    gender = fct_reorder(gender, desc(`Number of respondents`)), 
    percentage = (`Number of respondents` / sum(`Number of respondents`)) * 100
  ) %>% 
  ggplot(aes(x = gender, y = `Number of respondents`)) + 
  geom_col() + 
  geom_text(
    aes(
      y = `Number of respondents` + 15, 
      label = str_c(percentage %>% round(digits = 1), "%")
    )
  ) + 
  labs(
    x = NULL, 
    caption = str_glue("N = {n}")
  )
```

![](na_general_report_1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
