Pre-med webinar analyses
================
2020-07-20

  - [Read in and clean data](#read-in-and-clean-data)
  - [Visualizations](#visualizations)
      - [Did the webinar help?](#did-the-webinar-help)
      - [Pre and post comparisons](#pre-and-post-comparisons)
  - [Final comments](#final-comments)

``` r
# Libraries
library(tidyverse)
library(ggthemes)
library(readxl)
library(waffle)

# source utilities

source(here::here("scripts", "utils_mspa.R"))

# Parameters
input_path <- 
  here::here("data-raw", "webinar.xlsx")

likert_colors <- c("darkgreen","green","orange","red","darkred")

mspa_hex <- "#666699"


agreement_levels <- 
  c(
    "Strongly disagree", 
    "Somewhat disagree", 
    "Neither agree nor disagree", 
    "Somewhat agree", 
    "Strongly agree"
  )

concern_levels <- 
  c(
    "Strongly concerned", 
    "Somewhat concerned", 
    "Moderately concerned", 
    "Slightly concerned", 
    "Not at all concerned"
  ) %>% 
  rev()

prepared_levels <- 
  c(
    "Strongly prepared",
    "Moderately prepared", 
    "Somewhat prepared", 
    "Slightly prepared", 
    "Not at all prepared"
  ) %>% 
  rev()

asset_levels <- 
  c(
    "Hindrance", 
    "Slight hindrance", 
    "Neither hindrance/asset", 
    "Slight asset", 
    "Asset"
  )

agreement_variables <- 
  c(
    "pre_confident_out", 
    "pre_support_advisors", 
    "pre_important_community", 
    "pre_value_mentors_advisors", 
    "post_confident_out", 
    "post_support_advisors", 
    "post_important_community", 
    "post_value_mentors_advisors", 
    "webinar_helped"
  )

col_names <-
  c(
    "pre_confident_out", 
    "pre_support_advisors", 
    "pre_important_community", 
    "pre_value_mentors_advisors", 
    "pre_concern_applying", 
    "pre_prepared_applying", 
    "pre_asset_hindrance", 
    "pre_asset_hindrance_comment", 
    "what_are_your_concerns",
    "experiences_premed_advising",
    "post_confident_out", 
    "post_support_advisors", 
    "post_important_community", 
    "post_value_mentors_advisors", 
    "webinar_helped", 
    "post_concern_applying", 
    "post_prepared_applying", 
    "post_asset_hindrance", 
    "asset_hindrance_changed", 
    "new_concerns", 
    "final_comments"
  )
  
#===============================================================================
```

## Read in and clean data

``` r
webinar_data <- 
  input_path %>% 
  read_excel(col_names = col_names, skip = 2) %>% 
  mutate(
    respondent_id = 1:n(), 
    across(contains("asset"), ordered, levels = asset_levels), 
    across(any_of(agreement_variables), ordered, levels = agreement_levels), 
    across(contains("prepared"), ordered, levels = prepared_levels), 
    across(contains("concern"), ordered, levels = concern_levels)
  )
  
col_descriptors <- 
  input_path %>% 
  read_excel() %>% 
  .[1,] %>% 
  as.character() %>% 
  setNames(object = ., nm = col_names)

webinar_data
```

    ## # A tibble: 37 x 22
    ##    pre_confident_o… pre_support_adv… pre_important_c… pre_value_mento…
    ##    <ord>            <ord>            <ord>            <ord>           
    ##  1 Somewhat disagr… Neither agree n… Strongly agree   Strongly agree  
    ##  2 Strongly agree   Strongly agree   Strongly agree   Strongly agree  
    ##  3 Somewhat agree   Strongly agree   Strongly agree   Strongly agree  
    ##  4 Somewhat agree   Neither agree n… Strongly agree   Strongly agree  
    ##  5 Strongly agree   Somewhat agree   Strongly agree   Somewhat agree  
    ##  6 Somewhat disagr… Somewhat agree   Strongly agree   Strongly agree  
    ##  7 Somewhat agree   Strongly disagr… Somewhat agree   Strongly agree  
    ##  8 Strongly agree   Neither agree n… Strongly agree   Strongly agree  
    ##  9 Strongly agree   Strongly agree   Strongly agree   Strongly agree  
    ## 10 Strongly agree   Somewhat agree   Strongly agree   Strongly agree  
    ## # … with 27 more rows, and 18 more variables: pre_concern_applying <ord>,
    ## #   pre_prepared_applying <ord>, pre_asset_hindrance <ord>,
    ## #   pre_asset_hindrance_comment <ord>, what_are_your_concerns <ord>,
    ## #   experiences_premed_advising <chr>, post_confident_out <ord>,
    ## #   post_support_advisors <ord>, post_important_community <ord>,
    ## #   post_value_mentors_advisors <ord>, webinar_helped <ord>,
    ## #   post_concern_applying <ord>, post_prepared_applying <ord>,
    ## #   post_asset_hindrance <ord>, asset_hindrance_changed <ord>,
    ## #   new_concerns <ord>, final_comments <chr>, respondent_id <int>

``` r
colnames(webinar_data)
```

    ##  [1] "pre_confident_out"           "pre_support_advisors"       
    ##  [3] "pre_important_community"     "pre_value_mentors_advisors" 
    ##  [5] "pre_concern_applying"        "pre_prepared_applying"      
    ##  [7] "pre_asset_hindrance"         "pre_asset_hindrance_comment"
    ##  [9] "what_are_your_concerns"      "experiences_premed_advising"
    ## [11] "post_confident_out"          "post_support_advisors"      
    ## [13] "post_important_community"    "post_value_mentors_advisors"
    ## [15] "webinar_helped"              "post_concern_applying"      
    ## [17] "post_prepared_applying"      "post_asset_hindrance"       
    ## [19] "asset_hindrance_changed"     "new_concerns"               
    ## [21] "final_comments"              "respondent_id"

A function for plotting and significance testing for pre- and
post-webinar questions

``` r
analyze_pre_post <- function(name = "") { 
  
  subtitle <- 
    col_descriptors[str_c("pre_",name)] %>% 
    str_remove(pattern = "^.+-") %>% 
    str_wrap(width = 70)
  
 my_data <- 
   webinar_data %>% 
    select(contains(name), respondent_id, -contains("comment"), -contains("changed")) %>% 
    pivot_longer(
      cols = contains(name), 
      names_to = c("timepoint"), 
      values_to = "response"
    ) %>% 
    drop_na() %>%
   group_by(respondent_id) %>% 
   filter(n() > 1) %>% 
   ungroup() %>% 
    mutate(
      timepoint = 
        str_extract(timepoint, pattern = "^[:alpha:]+") %>% 
        factor(levels = c("pre", "post"))
    )
 
 contingency_table <-
   table(
     my_data %>% 
       pull(timepoint), 
     my_data %>% 
       pull(response)
   ) %>% 
   t()
 
 overall_chi_squared <- 
   contingency_table %>% 
   chisq.test(simulate.p.value = TRUE, B = 2000)
 
 t_statistic <- t.test(
   x = 
     my_data %>% 
     filter(timepoint == "pre") %>% 
     pull(response) %>% 
     as.numeric(), 
   y = 
     my_data %>% 
     filter(timepoint == "post") %>% 
     pull(response) %>% 
     as.numeric(),
   paired = TRUE
 )
 
 mean_data <- 
   my_data %>% 
   group_by(timepoint) %>% 
   summarize(response = mean(as.numeric(response)))
    
 
 my_likert <- 
   my_data %>% 
   likert_plot(
     my_var = response, 
     break_var = timepoint, 
     title = NULL, 
     n = n_distinct(my_data$respondent_id)
   ) + 
   labs(
     caption = 
       str_glue(
         "N = {my_n}; p = {my_p}", 
         my_n = n_distinct(my_data$respondent_id), 
         my_p = t_statistic$p.value %>% round(4)
       )
   )
  
  my_plot <-
    my_data %>% 
    ggplot(aes(x = timepoint, y = response)) + 
    geom_line(aes(group = respondent_id), color = "gray", alpha = 0.6) + 
    geom_count(color = "gray", alpha = 0.6) +  
    geom_line(
      aes(x = timepoint, y = response, group = 1), 
      data = mean_data, 
      color = "black", 
      size = 1.5
    ) + 
    geom_point(
      aes(x = timepoint, y = response), 
      data = mean_data, 
      color = "black", 
      size = 4
    ) + 
    scale_size_area(breaks = 1:5) + 
    labs(
      subtitle = subtitle, 
      x = NULL, 
      y = NULL, 
      size = "Number of students",
      caption = 
        str_glue(
          "N = {my_n}; p = {my_p}", 
          my_n = n_distinct(my_data$respondent_id), 
          my_p = t_statistic$p.value %>% round(4)
        )
    )
  
  my_result <- 
    list(
      means = mean_data, 
      plot = my_plot, 
      likert = my_likert, 
      t = t_statistic
    )
  
  return(my_result)
}
```

## Visualizations

### Did the webinar help?

``` r
n <- 
  webinar_data %>% 
  filter(!is.na(webinar_helped)) %>% 
  count() %>%
  pull(n)

 ns <- 
  webinar_data %>% 
  filter(!is.na(webinar_helped)) %>% 
  count(webinar_helped) %>% 
   pull(n)
 
 n_somewhat <- ns[[1]]
 n_strongly <- ns[[2]]

webinar_data %>% 
  drop_na(webinar_helped) %>% 
  count(webinar_helped) %>% 
  ggplot(aes(fill = webinar_helped, values = n)) + 
  geom_waffle(color = "white", n_rows = 3, size = 0.25, make_proportional = FALSE) + 
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(
    labels = function(x) x * 3, 
    expand = c(0, 0)
  ) +
  ggthemes::scale_fill_tableau(
    name = NULL, 
    labels = 
      c(
        str_glue("Somewhat agree ({n_somewhat})"), 
        str_glue("Strongly agree ({n_strongly})")
      ),
    palette = "Tableau 10"
  ) +
  coord_equal() +
  theme_minimal() + 
  theme(
    strip.text = element_text(angle = 0, hjust = 0), 
    panel.grid = element_blank(), 
    axis.ticks.x = element_line(), 
    legend.position = "bottom"
  ) + 
  labs(
    subtitle = "\"This webinar helped me decide how to best to apply as an LGBTQ+ student\"",
    x = "Number of students"
  )
```

![](webinar_report_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

So overall, it looks like most students self-reported that the webinar
was helpful to them.

### Pre and post comparisons

Now we can delve into some of the pre- and post-webinar comparisons. For
these analyses, I mostly treated the likert data on an ordinal 1-5
scale. For the significance testing, I either averaged the ordinal
values for pre- and post- and performed a paired t-test or used a
Chi-squared test for goodness of fit on the overall contingency tables.
I also made two ways of visualizing the responses…any of this can be
changed, but it was my first-pass for feedback.

Note that for the line plots, the big bold line represents the mean
change and each of the gray, smaller lines represents an individual
person.

Also note that, right now, for any given question, I throw away all the
responses that don’t have both pre- and post-webinar answers. For many
people, there are answers in the pre-webinar section (but not the
post-webinar section), and in theory I could include those responses in
the analyses if we want…I’m trying to make up my mind if that’s
something I want to do, since it will change the statistics a little
bit.

**Confidence applying to medical school as an out LGBTQ+ person**

``` r
analyze_pre_post(name = "confident_out")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## $means
    ## # A tibble: 2 x 2
    ##   timepoint response
    ##   <fct>        <dbl>
    ## 1 pre           3.65
    ## 2 post          4.2 
    ## 
    ## $plot

![](webinar_report_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## 
    ## $likert

![](webinar_report_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

    ## 
    ## $t
    ## 
    ##  Paired t-test
    ## 
    ## data:  my_data %>% filter(timepoint == "pre") %>% pull(response) %>%  and my_data %>% filter(timepoint == "post") %>% pull(response) %>%     as.numeric() and     as.numeric()
    ## t = -1.9926, df = 19, p-value = 0.06086
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.12770577  0.02770577
    ## sample estimates:
    ## mean of the differences 
    ##                   -0.55

**Confidence applying to medical school as an out LGBTQ+ person**

``` r
analyze_pre_post(name = "support_advisors")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## $means
    ## # A tibble: 2 x 2
    ##   timepoint response
    ##   <fct>        <dbl>
    ## 1 pre           3.25
    ## 2 post          3.45
    ## 
    ## $plot

![](webinar_report_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## 
    ## $likert

![](webinar_report_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

    ## 
    ## $t
    ## 
    ##  Paired t-test
    ## 
    ## data:  my_data %>% filter(timepoint == "pre") %>% pull(response) %>%  and my_data %>% filter(timepoint == "post") %>% pull(response) %>%     as.numeric() and     as.numeric()
    ## t = -2.1794, df = 19, p-value = 0.04209
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.39206906 -0.00793094
    ## sample estimates:
    ## mean of the differences 
    ##                    -0.2

**Confidence applying to medical school as an out LGBTQ+ person**

``` r
analyze_pre_post(name = "important_community")
```

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): cannot compute
    ## simulated p-value with zero marginals

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): Chi-squared
    ## approximation may be incorrect

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## $means
    ## # A tibble: 2 x 2
    ##   timepoint response
    ##   <fct>        <dbl>
    ## 1 pre           4.84
    ## 2 post          4.79
    ## 
    ## $plot

![](webinar_report_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## 
    ## $likert

![](webinar_report_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

    ## 
    ## $t
    ## 
    ##  Paired t-test
    ## 
    ## data:  my_data %>% filter(timepoint == "pre") %>% pull(response) %>%  and my_data %>% filter(timepoint == "post") %>% pull(response) %>%     as.numeric() and     as.numeric()
    ## t = 0.56695, df = 18, p-value = 0.5778
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1424041  0.2476673
    ## sample estimates:
    ## mean of the differences 
    ##              0.05263158

**Confidence applying to medical school as an out LGBTQ+ person**

``` r
analyze_pre_post(name = "value_mentors_advisors")
```

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): cannot compute
    ## simulated p-value with zero marginals

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): Chi-squared
    ## approximation may be incorrect

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## $means
    ## # A tibble: 2 x 2
    ##   timepoint response
    ##   <fct>        <dbl>
    ## 1 pre           4.95
    ## 2 post          4.7 
    ## 
    ## $plot

![](webinar_report_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## 
    ## $likert

![](webinar_report_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

    ## 
    ## $t
    ## 
    ##  Paired t-test
    ## 
    ## data:  my_data %>% filter(timepoint == "pre") %>% pull(response) %>%  and my_data %>% filter(timepoint == "post") %>% pull(response) %>%     as.numeric() and     as.numeric()
    ## t = 1.3143, df = 19, p-value = 0.2044
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1481381  0.6481381
    ## sample estimates:
    ## mean of the differences 
    ##                    0.25

**Confidence applying to medical school as an out LGBTQ+ person**

``` r
analyze_pre_post(name = "concern_applying")
```

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): cannot compute
    ## simulated p-value with zero marginals

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): Chi-squared
    ## approximation may be incorrect

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## $means
    ## # A tibble: 2 x 2
    ##   timepoint response
    ##   <fct>        <dbl>
    ## 1 pre           3.17
    ## 2 post          2.44
    ## 
    ## $plot

![](webinar_report_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## 
    ## $likert

![](webinar_report_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

    ## 
    ## $t
    ## 
    ##  Paired t-test
    ## 
    ## data:  my_data %>% filter(timepoint == "pre") %>% pull(response) %>%  and my_data %>% filter(timepoint == "post") %>% pull(response) %>%     as.numeric() and     as.numeric()
    ## t = 2.6, df = 17, p-value = 0.01868
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1361623 1.3082821
    ## sample estimates:
    ## mean of the differences 
    ##               0.7222222

**Confidence applying to medical school as an out LGBTQ+ person**

``` r
analyze_pre_post(name = "prepared_applying")
```

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): cannot compute
    ## simulated p-value with zero marginals

    ## Warning in chisq.test(., simulate.p.value = TRUE, B = 2000): Chi-squared
    ## approximation may be incorrect

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## $means
    ## # A tibble: 2 x 2
    ##   timepoint response
    ##   <fct>        <dbl>
    ## 1 pre           2.5 
    ## 2 post          3.44
    ## 
    ## $plot

![](webinar_report_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ## 
    ## $likert

![](webinar_report_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

    ## 
    ## $t
    ## 
    ##  Paired t-test
    ## 
    ## data:  my_data %>% filter(timepoint == "pre") %>% pull(response) %>%  and my_data %>% filter(timepoint == "post") %>% pull(response) %>%     as.numeric() and     as.numeric()
    ## t = -3.0325, df = 15, p-value = 0.008397
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.5964348 -0.2785652
    ## sample estimates:
    ## mean of the differences 
    ##                 -0.9375

**Confidence applying to medical school as an out LGBTQ+ person**

``` r
analyze_pre_post(name = "asset_hindrance")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## $means
    ## # A tibble: 2 x 2
    ##   timepoint response
    ##   <fct>        <dbl>
    ## 1 pre           3.3 
    ## 2 post          3.95
    ## 
    ## $plot

![](webinar_report_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ## 
    ## $likert

![](webinar_report_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

    ## 
    ## $t
    ## 
    ##  Paired t-test
    ## 
    ## data:  my_data %>% filter(timepoint == "pre") %>% pull(response) %>%  and my_data %>% filter(timepoint == "post") %>% pull(response) %>%     as.numeric() and     as.numeric()
    ## t = -2.9419, df = 19, p-value = 0.008369
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.1124389 -0.1875611
    ## sample estimates:
    ## mean of the differences 
    ##                   -0.65

## Final comments

``` r
webinar_data %>% 
  select(`Any final comments?` = final_comments) %>% 
  drop_na() %>% 
  knitr::kable()
```

| Any final comments?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| thank you for this. I hope to be involved more in MSPA as a premed and one day medical student.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| Thank you\! It was great hearing everyone’s thoughts and experiences, and I appreciate the time you all took to speak to us. Stay well\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| It would be good to learn about what structures are in place to prevent adcoms from outing applicants without their consent either after they become matriculants, during interviews, or anything similar. Wasn’t comfortable raising this during the webinar, but am super appreciative of how helpful it was and how thoughtful all the panelists are\!                                                                                                                                                                                                                                                                                                                                                                                  |
| Thank you all so much\!\! This was wonderful and I felt very seen and learned a lot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Thank you all for your time and insight. It’s important for us to see folks like ourselves.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Thank you so much for coordinating this Zoom\! It was so helpful and refreshing to talk to Queer medical students. There aren’t nearly enough LGBTQ+ people in STEM fields, but I’m glad MSPA provided this connection.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Thank you for this event\! This was amazing. I loved it\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Thanks for the webinar. It was the first time that I saw something for queer pre-meds\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Thanks so much for putting this on, it means a lot to me and eases many of my concerns.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| N/A. Thanks\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| Thank you so much for putting the webinar together – it’s great to feel solidarity with queer medical students who’ve been through the process and had to make similar calculations. Some of the information towards the beginning of the webinar was pretty basic and not specific to being LGBT, and I found myself tuning out for that part. But hearing the personal stories and insights from the different panelists was awesome. Also, I wish there was more comfort around naming names, in terms of schools – we want to know which schools are doing a good job at recruiting and respecting LGBT students… and which schools aren’t. Thanks again, and I look forward to getting more involved when I’m an actual med student\! |
| It was a nice session. Thank you for discussing such a great topic.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| It enhanced everybody’s way of thinking and processing their lives.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| I really appreciate that you covered the general application process and interwove your queer experiences throughout it, as well as answering specific questions toward the end\! Thank you for having a diverse panel to speak to different experiences and intersections within the queer community :)                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Thanks for putting the event on\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| Thank you again\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
