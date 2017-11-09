The Last Data Analysis to Rule Them All? (study1adults)
================
Adam Stone, PhD
11-09-2017

-   [Putting It All Back Together](#putting-it-all-back-together)
-   [Group Changes and Participant Tables](#group-changes-and-participant-tables)
-   [Gist & Lexical Recall Data](#gist-lexical-recall-data)
    -   [Tables & Charts](#tables-charts)
    -   [ANOVAs](#anovas)
    -   [AoA Correlations](#aoa-correlations)
-   [Eye Gaze Data](#eye-gaze-data)
    -   [ANOVAS](#anovas-1)
    -   [FaceChest](#facechest)
-   [Rain's Notes](#rains-notes)

Putting It All Back Together
============================

Throughout all the data analysis we've done, the datasets have become more fragmented - lexical recall, gist, and eye tracking datasets. I want to put them all together in one whole dataset again so we can perform some analyses more efficiently (particularly correlations). The only thing I need to remember is we'll have a new column called `eye_exclude` and if it is set to `TRUE` it means we can't include that row in any analysis relating to eye gaze (usually because that trial was less than 25% looking).

``` r
# Libraries
library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
library(scales)

# Load lex and eye data
cleanlexdata <- read_csv("cleandata.csv") %>%
  select(-(forehead:total))

cleaneyedata <- read_csv("cleanpercentdata.csv") %>%
  spread(aoi,percent) %>%
  add_column(eye_exclude = FALSE)

# What rows were removed from the eye data back in 03eyegaze? Let's add back in
# With a new column - eye_exclude
removed <- anti_join(cleanlexdata, cleaneyedata) %>%
  add_column(eye_exclude = TRUE)
eyelexdata <- bind_rows(cleaneyedata, removed)

# Load gist data
gist <- read_csv('gist_indiv.csv', col_types = cols(
  participant = col_character(),
  gist.fw1 = col_integer(),
  gist.rv2 = col_integer(),
  gist.fw3 = col_integer(),
  gist.rv4 = col_integer()
)) %>%
  gather(video, gist, gist.fw1:gist.rv4) %>%
  mutate(video = str_sub(video,6,8))

# Presto, our full reunified dataset - 'fulldata'
fulldata <- left_join(eyelexdata, gist)
```

Group Changes and Participant Tables
====================================

We have some changes to make to the groups. First, fix Josh as learning ASL when he was 6. Next, drop the DeafNative Group and reclassify all who learned ASL &lt; 3.9 as DeafEarly and ASL =&gt; 4.0 as DeafLate.

``` r
# Change Josh's AoASL to 6
fulldata <- fulldata %>%
  mutate(aoasl = as.double(aoasl)) %>%
  mutate(aoasl = case_when(
    participant == "Josh" ~ 6,
    TRUE ~ aoasl
  ))

# Reclassify Groups
fulldata <- fulldata %>%
  mutate(maingroup = case_when(
    hearing == "Deaf" & aoasl < 4 ~ "DeafEarly",
    hearing == "Deaf" & aoasl >= 4 ~ "DeafLate",
    maingroup == "HearingLateASL" ~ "HearingLate",
    maingroup == "HearingNoviceASL" ~ "HearingNovice"
  ))

# Create Participant Demographics Table
participant_info <- fulldata %>%
  select(-(acc:gist)) %>%
  select(-(video:direction)) %>%
  distinct() %>% 
  group_by(maingroup) %>%
  summarise(n = n(),
            age_mean = mean(age),
            age_sd = sd(age),
            aoasl_mean = mean(aoasl),
            aoasl_sd = sd(aoasl),
            signyrs_mean = mean(signyrs),
            signyrs_sd = sd(signyrs),
            selfrate_mean = mean(selfrate),
            selfrate_sd = sd(selfrate)) %>%
  ungroup() %>%
  mutate_if(is.double, funs(round(., 2))) %>%
  mutate(age = paste(age_mean, "±", age_sd, sep = " "),
         aoasl = paste(aoasl_mean, "±", aoasl_sd, sep = " "),
         signyrs = paste(signyrs_mean, "±", signyrs_sd, sep = " "),
         selfrate = paste(selfrate_mean, "±", selfrate_sd, sep = " ")) %>%
  select(-(age_mean:selfrate_sd)) %>%
  print()
```

    ## # A tibble: 4 x 6
    ##       maingroup     n          age        aoasl      signyrs    selfrate
    ##           <chr> <int>        <chr>        <chr>        <chr>       <chr>
    ## 1     DeafEarly    15  33.6 ± 9.16  0.67 ± 1.05  32.8 ± 9.32       5 ± 0
    ## 2      DeafLate    14 36.36 ± 5.64    11 ± 4.64 25.21 ± 5.62       5 ± 0
    ## 3   HearingLate    12  28.92 ± 6.2 17.25 ± 3.39 11.75 ± 4.85 4.62 ± 0.48
    ## 4 HearingNovice    11 20.25 ± 1.28  17.64 ± 1.8  2.44 ± 1.01 3.05 ± 0.65

Gist & Lexical Recall Data
==========================

Tables & Charts
---------------

Let's generate a table for lexical recall and gist for forward vs. reversed stories.

``` r
lexgist_info <- fulldata %>%
  group_by(maingroup, direction) %>%
  summarise(lex_mean = mean(acc, na.rm = TRUE),
            lex_sd = sd(acc, na.rm = TRUE),
            gist_mean = mean(gist),
            gist_sd = sd(gist)) %>%
  ungroup() %>%
  mutate_if(is.double, funs(round(., 2))) %>% 
  mutate(lex = paste(lex_mean, "±", lex_sd, sep = " "),
         gist = paste(gist_mean, "±", gist_sd, sep = " ")) %>%
  select(-(lex_mean:gist_sd)) %>%
  gather(metric, value, lex:gist) %>%
  unite("metric", c(metric, direction), sep = "_") %>%
  spread(metric, value) %>%
  print()
```

    ## # A tibble: 4 x 5
    ##       maingroup gist_forward gist_reversed lex_forward lex_reversed
    ## *         <chr>        <chr>         <chr>       <chr>        <chr>
    ## 1     DeafEarly  0.97 ± 0.18   0.67 ± 0.48 0.85 ± 0.07  0.76 ± 0.12
    ## 2      DeafLate        1 ± 0    0.39 ± 0.5 0.85 ± 0.08  0.69 ± 0.12
    ## 3   HearingLate        1 ± 0   0.33 ± 0.48 0.87 ± 0.09  0.69 ± 0.14
    ## 4 HearingNovice  0.45 ± 0.51   0.23 ± 0.43 0.78 ± 0.13  0.64 ± 0.13

And then bar charts too after that with error bars.

``` r
# Gist bar chart
gist_bar <- fulldata %>% select(participant, maingroup, direction, gist) %>%
  group_by(maingroup, participant, direction) %>%
  summarise(gist = mean(gist)) %>%
  group_by(maingroup, direction) %>%
  summarise(mean = mean(gist),
            sd = sd(gist),
            count = n(),
            se = sd/sqrt(count))

ggplot(gist_bar, aes(x = maingroup, y = mean, fill = direction)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.9), width = 0.5) +
  labs(title = "Story Comprehension (Gist)", subtitle = "Error bars represent SE", x = "", y = "mean gist") +
  scale_y_continuous(labels = percent)
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/lex%20and%20gist%20bar%20charts-1.png)

``` r
# Gist bar chart
lex_bar <- fulldata %>% select(participant, maingroup, direction, acc) %>%
  group_by(maingroup, participant, direction) %>%
  summarise(acc = mean(acc, na.rm = TRUE)) %>%
  group_by(maingroup, direction) %>%
  summarise(mean = mean(acc, na.rm = TRUE),
            sd = sd(acc, na.rm = TRUE),
            count = n(),
            se = sd/sqrt(count))

ggplot(lex_bar, aes(x = maingroup, y = mean, fill = direction)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.9), width = 0.5) +
  labs(title = "Lexical Recall", subtitle = "Error bars represent SE", x = "", y = "mean accuracy") +
  scale_y_continuous(labels = percent, limits = c(0,1))
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/lex%20and%20gist%20bar%20charts-2.png)

ANOVAs
------

Here, ANOVAs are done with **Direction Included.** Gist first, with Tukey's comparisons.

``` r
# Gist ANOVA. We have to make participant-level means first. 
gist_aov <- fulldata %>%
  group_by(maingroup, participant, direction) %>%
  summarise(gist = mean(gist, na.rm = TRUE)) %>%
#  filter(direction != "reversed") %>%
  lm(gist ~ maingroup * direction, data = .)
summary(aov(gist_aov))
```

    ##                     Df Sum Sq Mean Sq F value   Pr(>F)    
    ## maingroup            3  3.002   1.001  13.284 2.53e-07 ***
    ## direction            1  5.310   5.310  70.491 4.10e-13 ***
    ## maingroup:direction  3  0.896   0.299   3.965   0.0103 *  
    ## Residuals           96  7.232   0.075                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(aov(gist_aov))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = gist_aov)
    ## 
    ## $maingroup
    ##                                 diff        lwr         upr     p adj
    ## DeafLate-DeafEarly        -0.1202381 -0.3088051  0.06832887 0.3466547
    ## HearingLate-DeafEarly     -0.1500000 -0.3465269  0.04652685 0.1968334
    ## HearingNovice-DeafEarly   -0.4757576 -0.6771861 -0.27432905 0.0000001
    ## HearingLate-DeafLate      -0.0297619 -0.2293839  0.16986005 0.9797740
    ## HearingNovice-DeafLate    -0.3555195 -0.5599689 -0.15107005 0.0000928
    ## HearingNovice-HearingLate -0.3257576 -0.5375709 -0.11394428 0.0006581
    ## 
    ## $direction
    ##                        diff        lwr        upr p adj
    ## reversed-forward -0.4519231 -0.5587683 -0.3450778     0
    ## 
    ## $`maingroup:direction`
    ##                                                       diff        lwr
    ## DeafLate:forward-DeafEarly:forward            3.333333e-02 -0.2827280
    ## HearingLate:forward-DeafEarly:forward         3.333333e-02 -0.2960697
    ## HearingNovice:forward-DeafEarly:forward      -5.121212e-01 -0.8497401
    ## DeafEarly:reversed-DeafEarly:forward         -3.000000e-01 -0.6105642
    ## DeafLate:reversed-DeafEarly:forward          -5.738095e-01 -0.8898709
    ## HearingLate:reversed-DeafEarly:forward       -6.333333e-01 -0.9627364
    ## HearingNovice:reversed-DeafEarly:forward     -7.393939e-01 -1.0770128
    ## HearingLate:forward-DeafLate:forward          1.110223e-16 -0.3345908
    ## HearingNovice:forward-DeafLate:forward       -5.454545e-01 -0.8881368
    ## DeafEarly:reversed-DeafLate:forward          -3.333333e-01 -0.6493947
    ## DeafLate:reversed-DeafLate:forward           -6.071429e-01 -0.9286073
    ## HearingLate:reversed-DeafLate:forward        -6.666667e-01 -1.0012575
    ## HearingNovice:reversed-DeafLate:forward      -7.727273e-01 -1.1154096
    ## HearingNovice:forward-HearingLate:forward    -5.454545e-01 -0.9004796
    ## DeafEarly:reversed-HearingLate:forward       -3.333333e-01 -0.6627364
    ## DeafLate:reversed-HearingLate:forward        -6.071429e-01 -0.9417337
    ## HearingLate:reversed-HearingLate:forward     -6.666667e-01 -1.0138880
    ## HearingNovice:reversed-HearingLate:forward   -7.727273e-01 -1.1277523
    ## DeafEarly:reversed-HearingNovice:forward      2.121212e-01 -0.1254977
    ## DeafLate:reversed-HearingNovice:forward      -6.168831e-02 -0.4043706
    ## HearingLate:reversed-HearingNovice:forward   -1.212121e-01 -0.4762371
    ## HearingNovice:reversed-HearingNovice:forward -2.272727e-01 -0.5899336
    ## DeafLate:reversed-DeafEarly:reversed         -2.738095e-01 -0.5898709
    ## HearingLate:reversed-DeafEarly:reversed      -3.333333e-01 -0.6627364
    ## HearingNovice:reversed-DeafEarly:reversed    -4.393939e-01 -0.7770128
    ## HearingLate:reversed-DeafLate:reversed       -5.952381e-02 -0.3941146
    ## HearingNovice:reversed-DeafLate:reversed     -1.655844e-01 -0.5082667
    ## HearingNovice:reversed-HearingLate:reversed  -1.060606e-01 -0.4610856
    ##                                                       upr     p adj
    ## DeafLate:forward-DeafEarly:forward            0.349394663 0.9999794
    ## HearingLate:forward-DeafEarly:forward         0.362736402 0.9999845
    ## HearingNovice:forward-DeafEarly:forward      -0.174502341 0.0002270
    ## DeafEarly:reversed-DeafEarly:forward          0.010564191 0.0661325
    ## DeafLate:reversed-DeafEarly:forward          -0.257748194 0.0000050
    ## HearingLate:reversed-DeafEarly:forward       -0.303930265 0.0000012
    ## HearingNovice:reversed-DeafEarly:forward     -0.401775069 0.0000000
    ## HearingLate:forward-DeafLate:forward          0.334590838 1.0000000
    ## HearingNovice:forward-DeafLate:forward       -0.202772260 0.0000904
    ## DeafEarly:reversed-DeafLate:forward          -0.017272004 0.0311735
    ## DeafLate:reversed-DeafLate:forward           -0.285678379 0.0000018
    ## HearingLate:reversed-DeafLate:forward        -0.332075829 0.0000004
    ## HearingNovice:reversed-DeafLate:forward      -0.430044987 0.0000000
    ## HearingNovice:forward-HearingLate:forward    -0.190429523 0.0001790
    ## DeafEarly:reversed-HearingLate:forward       -0.003930265 0.0452077
    ## DeafLate:reversed-HearingLate:forward        -0.272552020 0.0000050
    ## HearingLate:reversed-HearingLate:forward     -0.319445345 0.0000012
    ## HearingNovice:reversed-HearingLate:forward   -0.417702250 0.0000000
    ## DeafEarly:reversed-HearingNovice:forward      0.549740083 0.5227399
    ## DeafLate:reversed-HearingNovice:forward       0.280993974 0.9992560
    ## HearingLate:reversed-HearingNovice:forward    0.233812901 0.9637494
    ## HearingNovice:reversed-HearingNovice:forward  0.135388115 0.5260785
    ## DeafLate:reversed-DeafEarly:reversed          0.042251806 0.1400787
    ## HearingLate:reversed-DeafEarly:reversed      -0.003930265 0.0452077
    ## HearingNovice:reversed-DeafEarly:reversed    -0.101775069 0.0027006
    ## HearingLate:reversed-DeafLate:reversed        0.275067028 0.9993113
    ## HearingNovice:reversed-DeafLate:reversed      0.177097870 0.8070333
    ## HearingNovice:reversed-HearingLate:reversed   0.248964416 0.9828143

And Lexical Recall ANOVA with Tukey's comparisons.

``` r
# Lex ANOVA. We have to make participant-level means first. 
lex_aov <- fulldata %>%
  group_by(maingroup, participant, direction) %>%
  summarise(acc = mean(acc, na.rm = TRUE)) %>%
#  filter(direction != "reversed") %>%
  lm(acc ~ maingroup * direction, data = .)
summary(aov(lex_aov))
```

    ##                     Df Sum Sq Mean Sq F value   Pr(>F)    
    ## maingroup            3 0.1175  0.0392   5.348  0.00189 ** 
    ## direction            1 0.5211  0.5211  71.149 3.38e-13 ***
    ## maingroup:direction  3 0.0294  0.0098   1.336  0.26731    
    ## Residuals           96 0.7031  0.0073                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(aov(lex_aov))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = lex_aov)
    ## 
    ## $maingroup
    ##                                   diff         lwr          upr     p adj
    ## DeafLate-DeafEarly        -0.035439881 -0.09423688  0.023357113 0.3971317
    ## HearingLate-DeafEarly     -0.030916667 -0.09219563  0.030362296 0.5531130
    ## HearingNovice-DeafEarly   -0.095575758 -0.15838311 -0.032768407 0.0007654
    ## HearingLate-DeafLate       0.004523214 -0.05772083  0.066767259 0.9975516
    ## HearingNovice-DeafLate    -0.060135877 -0.12388518  0.003613423 0.0718133
    ## HearingNovice-HearingLate -0.064659091 -0.13070451  0.001386332 0.0573100
    ## 
    ## $direction
    ##                        diff        lwr        upr p adj
    ## reversed-forward -0.1415702 -0.1748856 -0.1082548     0
    ## 
    ## $`maingroup:direction`
    ##                                                      diff         lwr
    ## DeafLate:forward-DeafEarly:forward           -0.001880952 -0.10043192
    ## HearingLate:forward-DeafEarly:forward         0.013000000 -0.08971105
    ## HearingNovice:forward-DeafEarly:forward      -0.071393939 -0.17666675
    ## DeafEarly:reversed-DeafEarly:forward         -0.093000000 -0.18983690
    ## DeafLate:reversed-DeafEarly:forward          -0.161998810 -0.26054977
    ## HearingLate:reversed-DeafEarly:forward       -0.167833333 -0.27054438
    ## HearingNovice:reversed-DeafEarly:forward     -0.212757576 -0.31803039
    ## HearingLate:forward-DeafLate:forward          0.014880952 -0.08944769
    ## HearingNovice:forward-DeafLate:forward       -0.069512987 -0.17636462
    ## DeafEarly:reversed-DeafLate:forward          -0.091119048 -0.18967001
    ## DeafLate:reversed-DeafLate:forward           -0.160117857 -0.26035357
    ## HearingLate:reversed-DeafLate:forward        -0.165952381 -0.27028102
    ## HearingNovice:reversed-DeafLate:forward      -0.210876623 -0.31772826
    ## HearingNovice:forward-HearingLate:forward    -0.084393939 -0.19509416
    ## DeafEarly:reversed-HearingLate:forward       -0.106000000 -0.20871105
    ## DeafLate:reversed-HearingLate:forward        -0.174998810 -0.27932745
    ## HearingLate:reversed-HearingLate:forward     -0.180833333 -0.28910028
    ## HearingNovice:reversed-HearingLate:forward   -0.225757576 -0.33645779
    ## DeafEarly:reversed-HearingNovice:forward     -0.021606061 -0.12687887
    ## DeafLate:reversed-HearingNovice:forward      -0.090604870 -0.19745650
    ## HearingLate:reversed-HearingNovice:forward   -0.096439394 -0.20713961
    ## HearingNovice:reversed-HearingNovice:forward -0.141363636 -0.25444478
    ## DeafLate:reversed-DeafEarly:reversed         -0.068998810 -0.16754977
    ## HearingLate:reversed-DeafEarly:reversed      -0.074833333 -0.17754438
    ## HearingNovice:reversed-DeafEarly:reversed    -0.119757576 -0.22503039
    ## HearingLate:reversed-DeafLate:reversed       -0.005834524 -0.11016317
    ## HearingNovice:reversed-DeafLate:reversed     -0.050758766 -0.15761040
    ## HearingNovice:reversed-HearingLate:reversed  -0.044924242 -0.15562446
    ##                                                       upr     p adj
    ## DeafLate:forward-DeafEarly:forward            0.096670010 1.0000000
    ## HearingLate:forward-DeafEarly:forward         0.115711045 0.9999287
    ## HearingNovice:forward-DeafEarly:forward       0.033878872 0.4214465
    ## DeafEarly:reversed-DeafEarly:forward          0.003836902 0.0691753
    ## DeafLate:reversed-DeafEarly:forward          -0.063447847 0.0000469
    ## HearingLate:reversed-DeafEarly:forward       -0.065122288 0.0000531
    ## HearingNovice:reversed-DeafEarly:forward     -0.107484764 0.0000003
    ## HearingLate:forward-DeafLate:forward          0.119209594 0.9998406
    ## HearingNovice:forward-DeafLate:forward        0.037338646 0.4768076
    ## DeafEarly:reversed-DeafLate:forward           0.007431915 0.0914889
    ## DeafLate:reversed-DeafLate:forward           -0.059882140 0.0000842
    ## HearingLate:reversed-DeafLate:forward        -0.061623739 0.0000916
    ## HearingNovice:reversed-DeafLate:forward      -0.104024990 0.0000006
    ## HearingNovice:forward-HearingLate:forward     0.026306278 0.2718825
    ## DeafEarly:reversed-HearingLate:forward       -0.003288955 0.0380371
    ## DeafLate:reversed-HearingLate:forward        -0.070670168 0.0000305
    ## HearingLate:reversed-HearingLate:forward     -0.072566385 0.0000335
    ## HearingNovice:reversed-HearingLate:forward   -0.115057359 0.0000002
    ## DeafEarly:reversed-HearingNovice:forward      0.083666751 0.9982644
    ## DeafLate:reversed-HearingNovice:forward       0.016246763 0.1589599
    ## HearingLate:reversed-HearingNovice:forward    0.014260823 0.1353842
    ## HearingNovice:reversed-HearingNovice:forward -0.028282497 0.0046665
    ## DeafLate:reversed-DeafEarly:reversed          0.029552153 0.3792970
    ## HearingLate:reversed-DeafEarly:reversed       0.027877712 0.3277729
    ## HearingNovice:reversed-DeafEarly:reversed    -0.014484764 0.0144605
    ## HearingLate:reversed-DeafLate:reversed        0.098494118 0.9999997
    ## HearingNovice:reversed-DeafLate:reversed      0.056092867 0.8202120
    ## HearingNovice:reversed-HearingLate:reversed   0.065775975 0.9118925

AoA Correlations
----------------

Next, we want to look at correlations between AoA and Gist, and betwen AoA and Lexical Recall. Rain asked for forward and reversed separately (1) deaf only, (2) hearing only, and (3) both. Let's make it work.

``` r
# Let's make participant-level data, and have forward/reversed in separate columns
lexgist_data <- fulldata %>%
  group_by(maingroup, participant, direction) %>%
  mutate(gist = mean(gist, na.rm = TRUE),
         lex = mean(acc, na.rm = TRUE)) %>%
  ungroup() %>%
  select(maingroup, participant, hearing, direction, aoasl, gist, lex) %>%
  distinct() %>%
  gather(metric, value, gist:lex) %>%
  unite(metricvalue, c(metric, direction), sep = "_") %>%
  spread(metricvalue, value) %>%
  select(-participant, -maingroup)

lexgist_deaf <- lexgist_data %>% filter(hearing == "Deaf") %>% select(-hearing)
lexgist_hearing <- lexgist_data %>% filter(hearing == "Hearing") %>% select(-hearing)
lexgist_all <- lexgist_data %>% select(-hearing)

# Load awesome function to make correlation tables with stars for significance
# From: https://myowelt.blogspot.co.uk/2008/04/beautiful-correlation-tables-in-r.html
corstarsl <- function(x){ 
require(Hmisc) 
x <- as.matrix(x) 
R <- Hmisc::rcorr(x)$r 
p <- Hmisc::rcorr(x)$P 
## define notions for significance levels; spacing is important.
mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
## trunctuate the matrix that holds the correlations to two decimal
R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
## build a new matrix that includes the correlations with their apropriate stars 
Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
diag(Rnew) <- paste(diag(R), " ", sep="") 
rownames(Rnew) <- colnames(x) 
colnames(Rnew) <- paste(colnames(x), "", sep="") 
## remove upper triangle
Rnew <- as.matrix(Rnew)
Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
Rnew <- as.data.frame(Rnew) 
## remove last column and return the matrix (which is now a data frame)
Rnew <- cbind(Rnew[1:length(Rnew)-1])
return(Rnew) 
}

# Correlations for Deaf
print("DEAF Correlations - Pearson's r")
```

    ## [1] "DEAF Correlations - Pearson's r"

``` r
#corstarsl(lexgist_deaf)
Hmisc::rcorr(as.matrix(lexgist_deaf))$r
```

    ##                     aoasl gist_forward gist_reversed lex_forward
    ## aoasl          1.00000000   0.08266371   -0.25585943  0.14743346
    ## gist_forward   0.08266371   1.00000000    0.01762269 -0.08155560
    ## gist_reversed -0.25585943   0.01762269    1.00000000  0.02667231
    ## lex_forward    0.14743346  -0.08155560    0.02667231  1.00000000
    ## lex_reversed  -0.25601873  -0.09770666    0.36021802  0.35984892
    ##               lex_reversed
    ## aoasl          -0.25601873
    ## gist_forward   -0.09770666
    ## gist_reversed   0.36021802
    ## lex_forward     0.35984892
    ## lex_reversed    1.00000000

``` r
print("DEAF Correlations - P-values")
```

    ## [1] "DEAF Correlations - P-values"

``` r
Hmisc::rcorr(as.matrix(lexgist_deaf))$P
```

    ##                   aoasl gist_forward gist_reversed lex_forward
    ## aoasl                NA    0.6698839    0.18035489  0.44533530
    ## gist_forward  0.6698839           NA    0.92770438  0.67406661
    ## gist_reversed 0.1803549    0.9277044            NA  0.89076140
    ## lex_forward   0.4453353    0.6740666    0.89076140          NA
    ## lex_reversed  0.1800743    0.6140990    0.05492031  0.05518766
    ##               lex_reversed
    ## aoasl           0.18007434
    ## gist_forward    0.61409897
    ## gist_reversed   0.05492031
    ## lex_forward     0.05518766
    ## lex_reversed            NA

``` r
cat(paste("","\n",""))
```

``` r
# Correlations for Hearing
print("HEARING Correlations - Pearson's r")
```

    ## [1] "HEARING Correlations - Pearson's r"

``` r
#corstarsl(lexgist_hearing)
Hmisc::rcorr(as.matrix(lexgist_hearing))$r
```

    ##                     aoasl gist_forward gist_reversed lex_forward
    ## aoasl          1.00000000  -0.15525565    0.07815751  0.02500269
    ## gist_forward  -0.15525565   1.00000000    0.29502174  0.57154566
    ## gist_reversed  0.07815751   0.29502174    1.00000000  0.35682374
    ## lex_forward    0.02500269   0.57154566    0.35682374  1.00000000
    ## lex_reversed   0.01411303   0.07645807    0.57951176  0.36558339
    ##               lex_reversed
    ## aoasl           0.01411303
    ## gist_forward    0.07645807
    ## gist_reversed   0.57951176
    ## lex_forward     0.36558339
    ## lex_reversed    1.00000000

``` r
print("HEARING Correlations - P-values")
```

    ## [1] "HEARING Correlations - P-values"

``` r
Hmisc::rcorr(as.matrix(lexgist_hearing))$P
```

    ##                   aoasl gist_forward gist_reversed lex_forward
    ## aoasl                NA  0.479339909   0.722986497 0.909841021
    ## gist_forward  0.4793399           NA   0.171744860 0.004385475
    ## gist_reversed 0.7229865  0.171744860            NA 0.094647552
    ## lex_forward   0.9098410  0.004385475   0.094647552          NA
    ## lex_reversed  0.9490402  0.728786878   0.003755275 0.086260140
    ##               lex_reversed
    ## aoasl          0.949040176
    ## gist_forward   0.728786878
    ## gist_reversed  0.003755275
    ## lex_forward    0.086260140
    ## lex_reversed            NA

``` r
cat(paste("","\n",""))
```

``` r
# Correlations for All
print("ALL Correlations - Pearson's r")
```

    ## [1] "ALL Correlations - Pearson's r"

``` r
#corstarsl(lexgist_all)
Hmisc::rcorr(as.matrix(lexgist_all))$r
```

    ##                     aoasl gist_forward gist_reversed lex_forward
    ## aoasl          1.00000000   -0.3230903    -0.3922803 -0.08115845
    ## gist_forward  -0.32309031    1.0000000     0.2712761  0.49279109
    ## gist_reversed -0.39228034    0.2712761     1.0000000  0.21701819
    ## lex_forward   -0.08115845    0.4927911     0.2170182  1.00000000
    ## lex_reversed  -0.33945209    0.1521110     0.4921550  0.38007715
    ##               lex_reversed
    ## aoasl           -0.3394521
    ## gist_forward     0.1521110
    ## gist_reversed    0.4921550
    ## lex_forward      0.3800772
    ## lex_reversed     1.0000000

``` r
print("ALL Correlations - P-values")
```

    ## [1] "ALL Correlations - P-values"

``` r
Hmisc::rcorr(as.matrix(lexgist_all))$P
```

    ##                     aoasl gist_forward gist_reversed  lex_forward
    ## aoasl                  NA 0.0194786567  0.0040239673 0.5673489939
    ## gist_forward  0.019478657           NA  0.0517394597 0.0002061625
    ## gist_reversed 0.004023967 0.0517394597            NA 0.1222542968
    ## lex_forward   0.567348994 0.0002061625  0.1222542968           NA
    ## lex_reversed  0.013820178 0.2817023354  0.0002107074 0.0054478274
    ##               lex_reversed
    ## aoasl         0.0138201778
    ## gist_forward  0.2817023354
    ## gist_reversed 0.0002107074
    ## lex_forward   0.0054478274
    ## lex_reversed            NA

I'm also including nicely formatted tables with \*\*\* indicators of significance for quick referencing. Order: Deaf, Hearing, All.

``` r
corstarsl(lexgist_deaf)
```

    ## Loading required package: Hmisc

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     combine, src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

    ##                aoasl gist_forward gist_reversed lex_forward
    ## aoasl                                                      
    ## gist_forward   0.08                                        
    ## gist_reversed -0.26         0.02                           
    ## lex_forward    0.15        -0.08          0.03             
    ## lex_reversed  -0.26        -0.10          0.36        0.36

``` r
corstarsl(lexgist_hearing)
```

    ##                aoasl gist_forward gist_reversed lex_forward
    ## aoasl                                                      
    ## gist_forward  -0.16                                        
    ## gist_reversed  0.08         0.30                           
    ## lex_forward    0.03       0.57**          0.36             
    ## lex_reversed   0.01         0.08        0.58**        0.37

``` r
corstarsl(lexgist_all)
```

    ##                  aoasl gist_forward gist_reversed lex_forward
    ## aoasl                                                        
    ## gist_forward   -0.32*                                        
    ## gist_reversed -0.39**         0.27                           
    ## lex_forward     -0.08       0.49***         0.22             
    ## lex_reversed   -0.34*         0.15        0.49***     0.38**

Eye Gaze Data
=============

Now eye gaze data. Boxplots first. Also here, we're renaming "chin" to "neck" because that's what it actually is!

``` r
# rename chin to neck
fulldata <- fulldata %>%
  rename(neck = chin)

fulldata %>%
  filter(eye_exclude == FALSE) %>%
  select(direction, moutheye:upperchest) %>%
  select(-moutheye, -facechest, -face, -chest) %>%
  gather(aoi, percent, belly:upperchest) %>%
  ggplot(aes(x = aoi, y = percent, fill = direction)) + geom_boxplot()
```

    ## Warning: Removed 835 rows containing non-finite values (stat_boxplot).

![](09finaldata_files/figure-markdown_github-ascii_identifiers/eye%20gaze%20boxplot-1.png)

ANOVAS
------

We have three big AOIs. Eyes, Mouth, and Neck. To some extent we also have Forehead and UpperChest. Let's run ANOVAs on all those five AOIs and see if either direction or group membership affect looking behavior for any of those AOIs.

``` r
# Define the 5 AOIs
aoi5 <- c('forehead','eyes','mouth','neck','upperchest')

# Organize the data by those AOIs (but again we need participant-level first)
aoi5_data <- fulldata %>%
  filter(eye_exclude == FALSE) %>%
  select(id, maingroup, direction, belly:upperchest) %>%
  gather(aoi, percent, belly:upperchest) %>%
  group_by(id, direction, aoi) %>%
  mutate(percent = mean(percent, na.rm = TRUE)) %>%
  ungroup () %>%
  distinct() %>%
  filter(aoi %in% aoi5) %>%
  spread(aoi, percent)

print("Forehead ANOVA")
```

    ## [1] "Forehead ANOVA"

``` r
forehead_aov <- lm(forehead ~ maingroup * direction, data = aoi5_data)
summary(aov(forehead_aov))
```

    ##                     Df  Sum Sq  Mean Sq F value Pr(>F)
    ## maingroup            3 0.01249 0.004164   0.862  0.468
    ## direction            1 0.00879 0.008786   1.819  0.185
    ## maingroup:direction  3 0.01411 0.004702   0.973  0.414
    ## Residuals           42 0.20287 0.004830               
    ## 53 observations deleted due to missingness

``` r
cat(paste("","\n",""))
```

``` r
print("Eyes ANOVA")
```

    ## [1] "Eyes ANOVA"

``` r
eyes_aov <- lm(eyes ~ maingroup * direction, data = aoi5_data)
summary(aov(eyes_aov))
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup            3  0.324 0.10784   1.961  0.125
    ## direction            1  0.002 0.00221   0.040  0.842
    ## maingroup:direction  3  0.034 0.01118   0.203  0.894
    ## Residuals           92  5.060 0.05500               
    ## 3 observations deleted due to missingness

``` r
cat(paste("","\n",""))
```

``` r
print("Mouth ANOVA")
```

    ## [1] "Mouth ANOVA"

``` r
mouth_aov <- lm(mouth ~ maingroup * direction, data = aoi5_data)
summary(aov(mouth_aov))
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)  
    ## maingroup            3  0.499 0.16623   2.533 0.0616 .
    ## direction            1  0.246 0.24582   3.745 0.0559 .
    ## maingroup:direction  3  0.029 0.00955   0.146 0.9323  
    ## Residuals           95  6.235 0.06563                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
cat(paste("","\n",""))
```

``` r
print("Neck ANOVA")
```

    ## [1] "Neck ANOVA"

``` r
neck_aov <- lm(neck ~ maingroup * direction, data = aoi5_data)
summary(aov(neck_aov))
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup            3  0.181 0.06038   1.465  0.229
    ## direction            1  0.103 0.10254   2.488  0.118
    ## maingroup:direction  3  0.037 0.01228   0.298  0.827
    ## Residuals           94  3.874 0.04121               
    ## 1 observation deleted due to missingness

``` r
cat(paste("","\n",""))
```

``` r
print("UpperChest ANOVA")
```

    ## [1] "UpperChest ANOVA"

``` r
upperchest_aov <- lm(upperchest ~ maingroup * direction, data = aoi5_data)
summary(aov(upperchest_aov))
```

    ##                     Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## maingroup            3 0.02998 0.009993   5.588 0.00154 **
    ## direction            1 0.00356 0.003560   1.991 0.16206   
    ## maingroup:direction  3 0.00591 0.001970   1.101 0.35337   
    ## Residuals           82 0.14663 0.001788                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 13 observations deleted due to missingness

``` r
cat(paste("","\n",""))
```

FaceChest
---------

Rain's Notes
============

About Adults:

-I think I want to write it up as an ANCOVA, with direction included. And LSD comparisons instead of Tukey. (I will do my own corrections) -You often have one liners summarizing results, in all tabs, those are nice, keep them coming. -(If you have reasons to present anything other than the ANCOVA, put that in your results tab)

I think if we do it this way then we get a really important story to tell: That the *critical* AoA cutoff is below 4 vs above 4 years of age (two groups 0-4 vs 4-13). This suggest early ASL is important.
