The Last Data Analysis to Rule Them All? (study1adults)
================
Adam Stone, PhD
11-10-2017

-   [Putting It All Back Together](#putting-it-all-back-together)
-   [Group Changes and Participant Tables](#group-changes-and-participant-tables)
-   [Gist & Lexical Recall Data](#gist-lexical-recall-data)
    -   [Tables & Charts](#tables-charts)
    -   [ANOVA Plan](#anova-plan)
    -   [Gist ANOVAs](#gist-anovas)
    -   [Summary of Gist ANOVAs](#summary-of-gist-anovas)
    -   [Lexical Recall ANOVAs](#lexical-recall-anovas)
    -   [Summary of Lexical Recall ANOVAs](#summary-of-lexical-recall-anovas)
    -   [AoA Correlations](#aoa-correlations)
-   [Eye Gaze Data](#eye-gaze-data)
    -   [Forehead](#forehead)
    -   [Eyes](#eyes)
    -   [Mouth](#mouth)
    -   [Neck](#neck)
    -   [Upperchest](#upperchest)
    -   [FaceChest](#facechest)
    -   [MouthEye](#moutheye)
-   [Heat Maps](#heat-maps)
-   [Summary](#summary)
-   [Rain's Notes](#rains-notes)
-   [Gist as binomial logit-link function model](#gist-as-binomial-logit-link-function-model)

Putting It All Back Together
============================

Throughout all the data analysis we've done, the datasets have become more fragmented - lexical recall, gist, and eye tracking datasets. I want to put them all together in one whole dataset again so we can perform some analyses more efficiently (particularly correlations). The only thing I need to remember is we'll have a new column called `eye_exclude` and if it is set to `TRUE` it means we can't include that row in any analysis relating to eye gaze (usually because that trial was less than 25% looking).

``` r
# Libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(scales)
library(viridis)
library(agricolae)

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
  scale_y_continuous(labels = percent, limits = c(0,1))
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/lex%20and%20gist%20bar%20charts-1.png)

``` r
# Lex bar chart
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
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  geom_hline(yintercept = .5, linetype = "dotted") +
  coord_cartesian(ylim = c(.5,1))
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/lex%20and%20gist%20bar%20charts-2.png)

ANOVA Plan
----------

Next, we're going to do ANOVAs and ANCOVAs. We'll always do it in this order. The first three ANOVAs will be followed by LSD of the four maingroups with *uncorrected p-values.*

1.  ANOVA with factors MainGroup & Direction
2.  ANOVA with factor MainGroup, for Forward only
3.  ANOVA with factor MainGroup, for Reverse only
4.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age
5.  ANCOVA with factor Hearing and covariates AoASL and Age, for Forward only
6.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

I did not include Age as a covariate in the first 3 ANOVAs because they did not add to or change the model in any significant way.

Gist ANOVAs
-----------

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value   Pr(>F)    
    ## maingroup            3  3.002   1.001  13.284 2.53e-07 ***
    ## direction            1  5.310   5.310  70.491 4.10e-13 ***
    ## maingroup:direction  3  0.896   0.299   3.965   0.0103 *  
    ## Residuals           96  7.232   0.075                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                             difference pvalue signif.           LCL
    ## DeafEarly - DeafLate         0.1202381 0.0987       . -0.0229201333
    ## DeafEarly - HearingLate      0.1500000 0.0488       *  0.0007987047
    ## DeafEarly - HearingNovice    0.4757576 0.0000     ***  0.3228349784
    ## DeafLate - HearingLate       0.0297619 0.6975         -0.1217891618
    ## DeafLate - HearingNovice     0.3555195 0.0000     ***  0.2003034374
    ## HearingLate - HearingNovice  0.3257576 0.0001     ***  0.1649509584
    ##                                   UCL
    ## DeafEarly - DeafLate        0.2633963
    ## DeafEarly - HearingLate     0.2992013
    ## DeafEarly - HearingNovice   0.6286802
    ## DeafLate - HearingLate      0.1813130
    ## DeafLate - HearingNovice    0.5107355
    ## HearingLate - HearingNovice 0.4865642

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## maingroup    3  2.477  0.8256   16.11 2.23e-07 ***
    ## Residuals   48  2.461  0.0513                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                              difference pvalue signif.        LCL
    ## DeafEarly - DeafLate        -0.03333333 0.6937         -0.2025032
    ## DeafEarly - HearingLate     -0.03333333 0.7055         -0.2096443
    ## DeafEarly - HearingNovice    0.51212121 0.0000     ***  0.3314128
    ## DeafLate - HearingLate       0.00000000 1.0000         -0.1790877
    ## DeafLate - HearingNovice     0.54545455 0.0000     ***  0.3620360
    ## HearingLate - HearingNovice  0.54545455 0.0000     ***  0.3554296
    ##                                   UCL
    ## DeafEarly - DeafLate        0.1358365
    ## DeafEarly - HearingLate     0.1429776
    ## DeafEarly - HearingNovice   0.6928296
    ## DeafLate - HearingLate      0.1790877
    ## DeafLate - HearingNovice    0.7288731
    ## HearingLate - HearingNovice 0.7354795

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## maingroup    3  1.421  0.4737   4.766 0.00548 **
    ## Residuals   48  4.771  0.0994                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                             difference pvalue signif.         LCL
    ## DeafEarly - DeafLate        0.27380952 0.0237       *  0.03824420
    ## DeafEarly - HearingLate     0.33333333 0.0088      **  0.08782421
    ## DeafEarly - HearingNovice   0.43939394 0.0010     ***  0.18776145
    ## DeafLate - HearingLate      0.05952381 0.6335         -0.18985184
    ## DeafLate - HearingNovice    0.16558442 0.1986         -0.08982191
    ## HearingLate - HearingNovice 0.10606061 0.4243         -0.15854495
    ##                                   UCL
    ## DeafEarly - DeafLate        0.5093748
    ## DeafEarly - HearingLate     0.5788425
    ## DeafEarly - HearingNovice   0.6910264
    ## DeafLate - HearingLate      0.3088995
    ## DeafLate - HearingNovice    0.4209907
    ## HearingLate - HearingNovice 0.3706662

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## hearing                      1  1.575   1.575  18.158 5.08e-05 ***
    ## direction                    1  5.310   5.310  61.234 1.05e-11 ***
    ## aoasl                        1  0.133   0.133   1.529  0.21961    
    ## age                          1  0.269   0.269   3.102  0.08169 .  
    ## hearing:direction            1  0.000   0.000   0.005  0.94363    
    ## hearing:aoasl                1  0.007   0.007   0.083  0.77329    
    ## direction:aoasl              1  0.068   0.068   0.783  0.37853    
    ## hearing:age                  1  0.731   0.731   8.434  0.00465 ** 
    ## direction:age                1  0.189   0.189   2.185  0.14296    
    ## aoasl:age                    1  0.013   0.013   0.150  0.69923    
    ## hearing:direction:aoasl      1  0.212   0.212   2.442  0.12174    
    ## hearing:direction:age        1  0.069   0.069   0.791  0.37627    
    ## hearing:aoasl:age            1  0.219   0.219   2.530  0.11530    
    ## direction:aoasl:age          1  0.004   0.004   0.044  0.83481    
    ## hearing:direction:aoasl:age  1  0.010   0.010   0.110  0.74132    
    ## Residuals                   88  7.631   0.087                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value  Pr(>F)   
    ## hearing            1 0.7613  0.7613  12.103 0.00115 **
    ## aoasl              1 0.0054  0.0054   0.085 0.77191   
    ## age                1 0.4549  0.4549   7.232 0.01008 * 
    ## hearing:aoasl      1 0.1486  0.1486   2.363 0.13140   
    ## hearing:age        1 0.6240  0.6240   9.919 0.00294 **
    ## aoasl:age          1 0.0154  0.0154   0.245 0.62273   
    ## hearing:aoasl:age  1 0.1601  0.1601   2.545 0.11777   
    ## Residuals         44 2.7677  0.0629                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value  Pr(>F)   
    ## hearing            1  0.814  0.8137   7.362 0.00947 **
    ## aoasl              1  0.195  0.1951   1.765 0.19080   
    ## age                1  0.003  0.0035   0.031 0.86010   
    ## hearing:aoasl      1  0.070  0.0703   0.636 0.42934   
    ## hearing:age        1  0.176  0.1760   1.593 0.21361   
    ## aoasl:age          1  0.001  0.0014   0.012 0.91151   
    ## hearing:aoasl:age  1  0.069  0.0688   0.622 0.43448   
    ## Residuals         44  4.863  0.1105                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Summary of Gist ANOVAs
----------------------

Lexical Recall ANOVAs
---------------------

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value   Pr(>F)    
    ## maingroup            3 0.1175  0.0392   5.348  0.00189 ** 
    ## direction            1 0.5211  0.5211  71.149 3.38e-13 ***
    ## maingroup:direction  3 0.0294  0.0098   1.336  0.26731    
    ## Residuals           96 0.7031  0.0073                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                               difference pvalue signif.          LCL
    ## DeafEarly - DeafLate         0.035439881 0.1183         -0.009198232
    ## DeafEarly - HearingLate      0.030916667 0.1903         -0.015605732
    ## DeafEarly - HearingNovice    0.095575758 0.0001     ***  0.047893021
    ## DeafLate - HearingLate      -0.004523214 0.8497         -0.051778295
    ## DeafLate - HearingNovice     0.060135877 0.0154       *  0.011738021
    ## HearingLate - HearingNovice  0.064659091 0.0120       *  0.014518041
    ##                                    UCL
    ## DeafEarly - DeafLate        0.08007799
    ## DeafEarly - HearingLate     0.07743907
    ## DeafEarly - HearingNovice   0.14325849
    ## DeafLate - HearingLate      0.04273187
    ## DeafLate - HearingNovice    0.10853373
    ## HearingLate - HearingNovice 0.11480014

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## maingroup    3 0.04988 0.016626   2.984 0.0404 *
    ## Residuals   48 0.26749 0.005573                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                               difference pvalue signif.          LCL
    ## DeafEarly - DeafLate         0.001880952 0.9462         -0.053895965
    ## DeafEarly - HearingLate     -0.013000000 0.6550         -0.071131400
    ## DeafEarly - HearingNovice    0.071393939 0.0199       *  0.011812655
    ## DeafLate - HearingLate      -0.014880952 0.6147         -0.073927864
    ## DeafLate - HearingNovice     0.069512987 0.0252       *  0.009038137
    ## HearingLate - HearingNovice  0.084393939 0.0093      **  0.021740905
    ##                                    UCL
    ## DeafEarly - DeafLate        0.05765787
    ## DeafEarly - HearingLate     0.04513140
    ## DeafEarly - HearingNovice   0.13097522
    ## DeafLate - HearingLate      0.04416596
    ## DeafLate - HearingNovice    0.12998784
    ## HearingLate - HearingNovice 0.14704697

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## maingroup    3 0.0970 0.03233   3.562 0.0209 *
    ## Residuals   48 0.4356 0.00908                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                              difference pvalue signif.           LCL
    ## DeafEarly - DeafLate        0.068998810 0.0571       . -0.0021805250
    ## DeafEarly - HearingLate     0.074833333 0.0481       *  0.0006493411
    ## DeafEarly - HearingNovice   0.119757576 0.0027      **  0.0437233245
    ## DeafLate - HearingLate      0.005834524 0.8769         -0.0695177922
    ## DeafLate - HearingNovice    0.050758766 0.1923         -0.0264158033
    ## HearingLate - HearingNovice 0.044924242 0.2642         -0.0350300021
    ##                                    UCL
    ## DeafEarly - DeafLate        0.14017814
    ## DeafEarly - HearingLate     0.14901733
    ## DeafEarly - HearingNovice   0.19579183
    ## DeafLate - HearingLate      0.08118684
    ## DeafLate - HearingNovice    0.12793334
    ## HearingLate - HearingNovice 0.12487849

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## hearing                      1 0.0513  0.0513   6.465   0.0128 *  
    ## direction                    1 0.5211  0.5211  65.627 2.88e-12 ***
    ## aoasl                        1 0.0027  0.0027   0.340   0.5610    
    ## age                          1 0.0237  0.0237   2.991   0.0872 .  
    ## hearing:direction            1 0.0086  0.0086   1.079   0.3017    
    ## hearing:aoasl                1 0.0003  0.0003   0.032   0.8575    
    ## direction:aoasl              1 0.0126  0.0126   1.583   0.2117    
    ## hearing:age                  1 0.0132  0.0132   1.658   0.2012    
    ## direction:age                1 0.0099  0.0099   1.243   0.2680    
    ## aoasl:age                    1 0.0141  0.0141   1.774   0.1864    
    ## hearing:direction:aoasl      1 0.0027  0.0027   0.336   0.5634    
    ## hearing:direction:age        1 0.0008  0.0008   0.103   0.7492    
    ## hearing:aoasl:age            1 0.0092  0.0092   1.164   0.2836    
    ## direction:aoasl:age          1 0.0001  0.0001   0.015   0.9013    
    ## hearing:direction:aoasl:age  1 0.0021  0.0021   0.261   0.6106    
    ## Residuals                   88 0.6987  0.0079                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df  Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.00898 0.00898   1.553 0.2193  
    ## aoasl              1 0.00181 0.00181   0.313 0.5789  
    ## age                1 0.03212 0.03212   5.557 0.0229 *
    ## hearing:aoasl      1 0.00064 0.00064   0.110 0.7418  
    ## hearing:age        1 0.00371 0.00371   0.642 0.4271  
    ## aoasl:age          1 0.00579 0.00579   1.001 0.3225  
    ## hearing:aoasl:age  1 0.01004 0.01004   1.737 0.1944  
    ## Residuals         44 0.25430 0.00578                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.0509 0.05092   5.042 0.0298 *
    ## aoasl              1 0.0135 0.01346   1.333 0.2545  
    ## age                1 0.0015 0.00150   0.149 0.7018  
    ## hearing:aoasl      1 0.0023 0.00229   0.227 0.6360  
    ## hearing:age        1 0.0103 0.01027   1.017 0.3188  
    ## aoasl:age          1 0.0084 0.00842   0.833 0.3663  
    ## hearing:aoasl:age  1 0.0013 0.00128   0.127 0.7236  
    ## Residuals         44 0.4444 0.01010                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Summary of Lexical Recall ANOVAs
--------------------------------

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

Forehead
--------

This is going to get complicated - we need to run all 6 ANOVAs/ANCOVAs on 5 AOIs. That's 30 models. Thank goodness for find-and-replace! I'm going to skip the LSD comparisons for now, to make it easier to just scroll through.

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df  Sum Sq  Mean Sq F value Pr(>F)
    ## maingroup            3 0.01249 0.004164   0.862  0.468
    ## direction            1 0.00879 0.008786   1.819  0.185
    ## maingroup:direction  3 0.01411 0.004702   0.973  0.414
    ## Residuals           42 0.20287 0.004830               
    ## 53 observations deleted due to missingness

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## maingroup    3 0.003528 0.0011760    1.61  0.217
    ## Residuals   21 0.015338 0.0007304               
    ## 27 observations deleted due to missingness

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq  Mean Sq F value Pr(>F)
    ## maingroup    3 0.0228 0.007601   0.851  0.482
    ## Residuals   21 0.1875 0.008930               
    ## 26 observations deleted due to missingness

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## hearing                      1 0.00222 0.002221   0.469  0.498
    ## direction                    1 0.00905 0.009055   1.913  0.176
    ## aoasl                        1 0.00912 0.009124   1.928  0.174
    ## age                          1 0.00794 0.007942   1.678  0.204
    ## hearing:direction            1 0.00061 0.000609   0.129  0.722
    ## hearing:aoasl                1 0.00000 0.000001   0.000  0.989
    ## direction:aoasl              1 0.00762 0.007618   1.610  0.213
    ## hearing:age                  1 0.00042 0.000417   0.088  0.768
    ## direction:age                1 0.00493 0.004932   1.042  0.315
    ## aoasl:age                    1 0.01179 0.011793   2.492  0.124
    ## hearing:direction:aoasl      1 0.00771 0.007708   1.629  0.211
    ## hearing:direction:age        1 0.00287 0.002868   0.606  0.442
    ## hearing:aoasl:age            1 0.00023 0.000231   0.049  0.826
    ## direction:aoasl:age          1 0.01285 0.012846   2.715  0.109
    ## hearing:direction:aoasl:age  1 0.00000 0.000001   0.000  0.988
    ## Residuals                   34 0.16089 0.004732               
    ## 53 observations deleted due to missingness

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df   Sum Sq  Mean Sq F value Pr(>F)  
    ## hearing            1 0.002672 0.002672   4.210 0.0559 .
    ## aoasl              1 0.000031 0.000031   0.048 0.8289  
    ## age                1 0.000105 0.000105   0.165 0.6899  
    ## hearing:aoasl      1 0.004628 0.004628   7.291 0.0152 *
    ## hearing:age        1 0.000364 0.000364   0.573 0.4596  
    ## aoasl:age          1 0.000051 0.000051   0.080 0.7804  
    ## hearing:aoasl:age  1 0.000224 0.000224   0.353 0.5603  
    ## Residuals         17 0.010792 0.000635                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 27 observations deleted due to missingness

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df  Sum Sq  Mean Sq F value Pr(>F)
    ## hearing            1 0.00022 0.000224   0.025  0.875
    ## aoasl              1 0.01679 0.016785   1.901  0.186
    ## age                1 0.01272 0.012719   1.441  0.247
    ## hearing:aoasl      1 0.00390 0.003903   0.442  0.515
    ## hearing:age        1 0.00260 0.002601   0.295  0.594
    ## aoasl:age          1 0.02370 0.023696   2.684  0.120
    ## hearing:aoasl:age  1 0.00031 0.000309   0.035  0.854
    ## Residuals         17 0.15010 0.008829               
    ## 26 observations deleted due to missingness

Eyes
----

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup            3  0.324 0.10784   1.961  0.125
    ## direction            1  0.002 0.00221   0.040  0.842
    ## maingroup:direction  3  0.034 0.01118   0.203  0.894
    ## Residuals           92  5.060 0.05500               
    ## 3 observations deleted due to missingness

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.1002 0.03339   0.666  0.577
    ## Residuals   48 2.4060 0.05013

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.2571 0.08570   1.421  0.249
    ## Residuals   44 2.6542 0.06032               
    ## 3 observations deleted due to missingness

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing                      1  0.131 0.13130   2.335 0.1302  
    ## direction                    1  0.002 0.00150   0.027 0.8706  
    ## aoasl                        1  0.022 0.02159   0.384 0.5372  
    ## age                          1  0.112 0.11159   1.985 0.1626  
    ## hearing:direction            1  0.000 0.00030   0.005 0.9417  
    ## hearing:aoasl                1  0.213 0.21335   3.795 0.0548 .
    ## direction:aoasl              1  0.002 0.00199   0.035 0.8512  
    ## hearing:age                  1  0.056 0.05552   0.987 0.3232  
    ## direction:age                1  0.039 0.03933   0.700 0.4053  
    ## aoasl:age                    1  0.005 0.00518   0.092 0.7622  
    ## hearing:direction:aoasl      1  0.007 0.00669   0.119 0.7309  
    ## hearing:direction:age        1  0.018 0.01779   0.316 0.5753  
    ## hearing:aoasl:age            1  0.065 0.06515   1.159 0.2848  
    ## direction:aoasl:age          1  0.001 0.00069   0.012 0.9122  
    ## hearing:direction:aoasl:age  1  0.025 0.02471   0.440 0.5092  
    ## Residuals                   84  4.723 0.05622                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 3 observations deleted due to missingness

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.0625 0.06254   1.265 0.2668  
    ## aoasl              1 0.0187 0.01868   0.378 0.5420  
    ## age                1 0.0089 0.00885   0.179 0.6743  
    ## hearing:aoasl      1 0.1469 0.14694   2.972 0.0918 .
    ## hearing:age        1 0.0060 0.00598   0.121 0.7297  
    ## aoasl:age          1 0.0010 0.00096   0.019 0.8898  
    ## hearing:aoasl:age  1 0.0866 0.08656   1.751 0.1926  
    ## Residuals         44 2.1757 0.04945                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)
    ## hearing            1 0.0685 0.06850   1.076  0.306
    ## aoasl              1 0.0049 0.00489   0.077  0.783
    ## age                1 0.1459 0.14589   2.291  0.138
    ## hearing:aoasl      1 0.0674 0.06735   1.058  0.310
    ## hearing:age        1 0.0688 0.06877   1.080  0.305
    ## aoasl:age          1 0.0059 0.00587   0.092  0.763
    ## hearing:aoasl:age  1 0.0029 0.00292   0.046  0.832
    ## Residuals         40 2.5471 0.06368               
    ## 3 observations deleted due to missingness

Mouth
-----

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value Pr(>F)  
    ## maingroup            3  0.499 0.16623   2.533 0.0616 .
    ## direction            1  0.246 0.24582   3.745 0.0559 .
    ## maingroup:direction  3  0.029 0.00955   0.146 0.9323  
    ## Residuals           95  6.235 0.06563                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.1506 0.05021   0.909  0.444
    ## Residuals   48 2.6511 0.05523

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  0.368 0.12263   1.608    0.2
    ## Residuals   47  3.584 0.07625

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing                      1  0.433  0.4333   6.621 0.0118 *
    ## direction                    1  0.249  0.2489   3.803 0.0544 .
    ## aoasl                        1  0.237  0.2374   3.627 0.0601 .
    ## age                          1  0.076  0.0764   1.168 0.2828  
    ## hearing:direction            1  0.027  0.0271   0.414 0.5215  
    ## hearing:aoasl                1  0.000  0.0000   0.000 0.9918  
    ## direction:aoasl              1  0.013  0.0133   0.204 0.6528  
    ## hearing:age                  1  0.004  0.0036   0.055 0.8151  
    ## direction:age                1  0.000  0.0003   0.004 0.9490  
    ## aoasl:age                    1  0.215  0.2152   3.288 0.0732 .
    ## hearing:direction:aoasl      1  0.020  0.0201   0.307 0.5812  
    ## hearing:direction:age        1  0.000  0.0002   0.004 0.9521  
    ## hearing:aoasl:age            1  0.006  0.0062   0.095 0.7585  
    ## direction:aoasl:age          1  0.002  0.0021   0.032 0.8590  
    ## hearing:direction:aoasl:age  1  0.031  0.0309   0.472 0.4939  
    ## Residuals                   87  5.693  0.0654                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)
    ## hearing            1 0.1225 0.12247   2.212  0.144
    ## aoasl              1 0.0697 0.06969   1.259  0.268
    ## age                1 0.0430 0.04301   0.777  0.383
    ## hearing:aoasl      1 0.0096 0.00960   0.173  0.679
    ## hearing:age        1 0.0010 0.00100   0.018  0.893
    ## aoasl:age          1 0.0876 0.08761   1.582  0.215
    ## hearing:aoasl:age  1 0.0324 0.03242   0.586  0.448
    ## Residuals         44 2.4360 0.05536

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1  0.331  0.3314   4.374 0.0424 *
    ## aoasl              1  0.182  0.1816   2.398 0.1288  
    ## age                1  0.034  0.0339   0.447 0.5071  
    ## hearing:aoasl      1  0.010  0.0104   0.137 0.7129  
    ## hearing:age        1  0.003  0.0028   0.037 0.8476  
    ## aoasl:age          1  0.130  0.1297   1.713 0.1976  
    ## hearing:aoasl:age  1  0.005  0.0047   0.062 0.8047  
    ## Residuals         43  3.257  0.0758                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Neck
----

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup            3  0.181 0.06038   1.465  0.229
    ## direction            1  0.103 0.10254   2.488  0.118
    ## maingroup:direction  3  0.037 0.01228   0.298  0.827
    ## Residuals           94  3.874 0.04121               
    ## 1 observation deleted due to missingness

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.0549 0.01831   0.576  0.633
    ## Residuals   48 1.5259 0.03179

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.1561 0.05202   1.019  0.393
    ## Residuals   46 2.3483 0.05105               
    ## 1 observation deleted due to missingness

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## hearing                      1 0.0004  0.0004   0.012 0.91288   
    ## direction                    1 0.1098  0.1098   3.058 0.08391 . 
    ## aoasl                        1 0.1047  0.1047   2.916 0.09131 . 
    ## age                          1 0.2173  0.2173   6.050 0.01590 * 
    ## hearing:direction            1 0.0072  0.0072   0.202 0.65446   
    ## hearing:aoasl                1 0.3381  0.3381   9.415 0.00288 **
    ## direction:aoasl              1 0.0125  0.0125   0.348 0.55679   
    ## hearing:age                  1 0.0016  0.0016   0.044 0.83402   
    ## direction:age                1 0.0071  0.0071   0.199 0.65696   
    ## aoasl:age                    1 0.2108  0.2108   5.869 0.01751 * 
    ## hearing:direction:aoasl      1 0.0047  0.0047   0.131 0.71855   
    ## hearing:direction:age        1 0.0170  0.0170   0.473 0.49330   
    ## hearing:aoasl:age            1 0.0545  0.0545   1.519 0.22117   
    ## direction:aoasl:age          1 0.0024  0.0024   0.068 0.79535   
    ## hearing:direction:aoasl:age  1 0.0176  0.0176   0.490 0.48587   
    ## Residuals                   86 3.0888  0.0359                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.0057 0.00571   0.200  0.657  
    ## aoasl              1 0.0227 0.02273   0.798  0.377  
    ## age                1 0.0737 0.07365   2.585  0.115  
    ## hearing:aoasl      1 0.1319 0.13186   4.628  0.037 *
    ## hearing:age        1 0.0041 0.00414   0.145  0.705  
    ## aoasl:age          1 0.0840 0.08401   2.949  0.093 .
    ## hearing:aoasl:age  1 0.0051 0.00509   0.179  0.675  
    ## Residuals         44 1.2536 0.02849                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.0015 0.00150   0.034 0.8539  
    ## aoasl              1 0.0945 0.09454   2.164 0.1488  
    ## age                1 0.1513 0.15135   3.464 0.0697 .
    ## hearing:aoasl      1 0.2111 0.21112   4.832 0.0335 *
    ## hearing:age        1 0.0145 0.01451   0.332 0.5675  
    ## aoasl:age          1 0.1292 0.12916   2.956 0.0929 .
    ## hearing:aoasl:age  1 0.0670 0.06704   1.534 0.2223  
    ## Residuals         42 1.8351 0.04369                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

Upperchest
----------

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## maingroup            3 0.02998 0.009993   5.588 0.00154 **
    ## direction            1 0.00356 0.003560   1.991 0.16206   
    ## maingroup:direction  3 0.00591 0.001970   1.101 0.35337   
    ## Residuals           82 0.14663 0.001788                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 13 observations deleted due to missingness

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## maingroup    3 0.00810 0.002701   1.568  0.211
    ## Residuals   43 0.07406 0.001722               
    ## 5 observations deleted due to missingness

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## maingroup    3 0.02832 0.009441   5.074 0.00462 **
    ## Residuals   39 0.07257 0.001861                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 8 observations deleted due to missingness

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## hearing                      1 0.01525 0.015252   7.887 0.00636 **
    ## direction                    1 0.00309 0.003090   1.598 0.21017   
    ## aoasl                        1 0.00034 0.000336   0.174 0.67789   
    ## age                          1 0.01102 0.011018   5.698 0.01954 * 
    ## hearing:direction            1 0.00233 0.002328   1.204 0.27608   
    ## hearing:aoasl                1 0.00001 0.000007   0.003 0.95380   
    ## direction:aoasl              1 0.00037 0.000367   0.190 0.66441   
    ## hearing:age                  1 0.00395 0.003946   2.041 0.15735   
    ## direction:age                1 0.00014 0.000143   0.074 0.78628   
    ## aoasl:age                    1 0.00000 0.000001   0.001 0.97903   
    ## hearing:direction:aoasl      1 0.00012 0.000117   0.061 0.80616   
    ## hearing:direction:age        1 0.00082 0.000816   0.422 0.51785   
    ## hearing:aoasl:age            1 0.00004 0.000037   0.019 0.89028   
    ## direction:aoasl:age          1 0.00316 0.003162   1.635 0.20495   
    ## hearing:direction:aoasl:age  1 0.00236 0.002360   1.220 0.27288   
    ## Residuals                   74 0.14310 0.001934                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 13 observations deleted due to missingness

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df  Sum Sq  Mean Sq F value Pr(>F)
    ## hearing            1 0.00343 0.003431   1.924  0.173
    ## aoasl              1 0.00072 0.000719   0.403  0.529
    ## age                1 0.00463 0.004632   2.597  0.115
    ## hearing:aoasl      1 0.00008 0.000084   0.047  0.830
    ## hearing:age        1 0.00064 0.000637   0.357  0.554
    ## aoasl:age          1 0.00188 0.001882   1.055  0.311
    ## hearing:aoasl:age  1 0.00123 0.001229   0.689  0.412
    ## Residuals         39 0.06955 0.001783               
    ## 5 observations deleted due to missingness

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## hearing            1 0.01389 0.013892   6.611 0.0145 *
    ## aoasl              1 0.00001 0.000006   0.003 0.9564  
    ## age                1 0.00678 0.006782   3.227 0.0811 .
    ## hearing:aoasl      1 0.00003 0.000029   0.014 0.9066  
    ## hearing:age        1 0.00418 0.004184   1.991 0.1671  
    ## aoasl:age          1 0.00132 0.001320   0.628 0.4333  
    ## hearing:aoasl:age  1 0.00113 0.001133   0.539 0.4677  
    ## Residuals         35 0.07355 0.002101                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 8 observations deleted due to missingness

FaceChest
---------

We originally defined FaceChest such that

1.  Face = eyes + mouth + chin
2.  Chest = upperchest + midchest + lowerchest

BUT. Chin is actually neck. It's not even part of the face if you think about it. So I'm redefining FaceChest as:

1.  Face = forehead + eyes + mouth
2.  Chest = neck + upperchest + midchest + lowerchest

So let's do this. Then see what's happening across groups for FaceChest. First I want a boxplot to comapre the new and old FaceChest variables.

    ## Warning: Removed 30 rows containing non-finite values (stat_boxplot).

![](09finaldata_files/figure-markdown_github-ascii_identifiers/recalculate%20facechest-1.png)

Cool. Next we'll do error bar charts using the new FaceChest across groups.

``` r
facechest_info <- fulldata %>%
  filter(eye_exclude == FALSE) %>%
  group_by(maingroup, direction, participant) %>%
  summarise(facechest = mean(facechest, na.rm = TRUE)) %>%
  group_by(maingroup, direction) %>%
  summarise(mean = mean(facechest),
            sd = sd(facechest),
            n = n(),
            se = sd/sqrt(n))

ggplot(facechest_info, aes(x = maingroup, y = mean, fill = direction, color = direction)) +
  geom_point(stat = "identity", position = position_dodge(0.5), size = 2) + 
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.5), width = 0.3, size = 1) +
  labs(title = "FaceChest Ratio", subtitle = "Error bars represent SE", x = "", y = "facechest ratio") +
  scale_y_continuous(limits = c(-1,1))
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/facechest%20bars-1.png)

Now let's do the ANOVAs. Also skipping LSDs here.

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value Pr(>F)  
    ## maingroup            3  1.317  0.4389   2.111 0.1040  
    ## direction            1  0.712  0.7116   3.422 0.0674 .
    ## maingroup:direction  3  0.283  0.0942   0.453 0.7157  
    ## Residuals           95 19.754  0.2079                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  0.295 0.09847   0.608  0.613
    ## Residuals   48  7.777 0.16202

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  1.279  0.4264   1.673  0.185
    ## Residuals   47 11.977  0.2548

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## hearing                      1  0.161  0.1609   0.868 0.35414   
    ## direction                    1  0.730  0.7303   3.939 0.05033 . 
    ## aoasl                        1  0.435  0.4350   2.346 0.12924   
    ## age                          1  1.654  1.6535   8.918 0.00367 **
    ## hearing:direction            1  0.124  0.1244   0.671 0.41499   
    ## hearing:aoasl                1  1.378  1.3779   7.431 0.00775 **
    ## direction:aoasl              1  0.033  0.0329   0.178 0.67442   
    ## hearing:age                  1  0.101  0.1009   0.544 0.46271   
    ## direction:age                1  0.056  0.0563   0.303 0.58316   
    ## aoasl:age                    1  0.839  0.8393   4.526 0.03621 * 
    ## hearing:direction:aoasl      1  0.008  0.0082   0.044 0.83419   
    ## hearing:direction:age        1  0.086  0.0860   0.464 0.49768   
    ## hearing:aoasl:age            1  0.286  0.2856   1.540 0.21792   
    ## direction:aoasl:age          1  0.000  0.0000   0.000 0.98981   
    ## hearing:direction:aoasl:age  1  0.042  0.0421   0.227 0.63487   
    ## Residuals                   87 16.131  0.1854                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1  0.001  0.0012   0.008 0.9281  
    ## aoasl              1  0.117  0.1165   0.809 0.3733  
    ## age                1  0.551  0.5512   3.828 0.0568 .
    ## hearing:aoasl      1  0.588  0.5880   4.083 0.0494 *
    ## hearing:age        1  0.000  0.0003   0.002 0.9645  
    ## aoasl:age          1  0.425  0.4248   2.950 0.0929 .
    ## hearing:aoasl:age  1  0.054  0.0542   0.376 0.5427  
    ## Residuals         44  6.336  0.1440                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1  0.275  0.2748   1.206 0.2782  
    ## aoasl              1  0.353  0.3525   1.548 0.2202  
    ## age                1  1.160  1.1602   5.093 0.0292 *
    ## hearing:aoasl      1  0.799  0.7986   3.506 0.0680 .
    ## hearing:age        1  0.187  0.1867   0.820 0.3703  
    ## aoasl:age          1  0.414  0.4143   1.819 0.1845  
    ## hearing:aoasl:age  1  0.274  0.2735   1.201 0.2793  
    ## Residuals         43  9.795  0.2278                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

MouthEye
--------

We've also got a moutheye ratio! Maybe something there, too.

``` r
moutheye_info <- fulldata %>%
  filter(eye_exclude == FALSE) %>%
  group_by(maingroup, direction, participant) %>%
  summarise(moutheye = mean(moutheye, na.rm = TRUE)) %>%
  group_by(maingroup, direction) %>%
  summarise(mean = mean(moutheye, na.rm = TRUE),
            sd = sd(moutheye, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n))

ggplot(moutheye_info, aes(x = maingroup, y = mean, fill = direction, color = direction)) +
  geom_point(stat = "identity", position = position_dodge(0.5), size = 2) + 
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.5), width = 0.3, size = 1) +
  labs(title = "MouthEye Ratio", subtitle = "Error bars represent SE", x = "", y = "moutheye ratio") +
  scale_y_continuous(limits = c(-1,1))
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/moutheye%20bar%20chart-1.png)

And Moutheye ANOVAs. Also skipping LSDs here.

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value Pr(>F)  
    ## maingroup            3  2.165  0.7218   2.652 0.0533 .
    ## direction            1  0.131  0.1312   0.482 0.4892  
    ## maingroup:direction  3  0.241  0.0804   0.296 0.8286  
    ## Residuals           92 25.035  0.2721                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 3 observations deleted due to missingness

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  0.595  0.1984   0.872  0.462
    ## Residuals   48 10.920  0.2275

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  1.814  0.6046   1.885  0.146
    ## Residuals   44 14.115  0.3208               
    ## 3 observations deleted due to missingness

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing                      1  0.962  0.9625   3.399 0.0688 .
    ## direction                    1  0.118  0.1177   0.416 0.5209  
    ## aoasl                        1  0.171  0.1706   0.602 0.4399  
    ## age                          1  0.450  0.4498   1.589 0.2110  
    ## hearing:direction            1  0.016  0.0161   0.057 0.8123  
    ## hearing:aoasl                1  0.898  0.8982   3.172 0.0785 .
    ## direction:aoasl              1  0.001  0.0014   0.005 0.9440  
    ## hearing:age                  1  0.330  0.3305   1.167 0.2831  
    ## direction:age                1  0.153  0.1531   0.541 0.4642  
    ## aoasl:age                    1  0.017  0.0173   0.061 0.8055  
    ## hearing:direction:aoasl      1  0.082  0.0821   0.290 0.5917  
    ## hearing:direction:age        1  0.103  0.1035   0.365 0.5471  
    ## hearing:aoasl:age            1  0.437  0.4375   1.545 0.2173  
    ## direction:aoasl:age          1  0.002  0.0016   0.006 0.9406  
    ## hearing:direction:aoasl:age  1  0.048  0.0479   0.169 0.6820  
    ## Residuals                   84 23.783  0.2831                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 3 observations deleted due to missingness

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1  0.381  0.3806   1.710 0.1978  
    ## aoasl              1  0.104  0.1039   0.466 0.4982  
    ## age                1  0.038  0.0376   0.169 0.6829  
    ## hearing:aoasl      1  0.752  0.7516   3.376 0.0729 .
    ## hearing:age        1  0.037  0.0370   0.166 0.6857  
    ## aoasl:age          1  0.004  0.0038   0.017 0.8960  
    ## hearing:aoasl:age  1  0.405  0.4051   1.820 0.1843  
    ## Residuals         44  9.796  0.2226                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)
    ## hearing            1  0.586  0.5859   1.675  0.203
    ## aoasl              1  0.067  0.0671   0.192  0.664
    ## age                1  0.580  0.5804   1.660  0.205
    ## hearing:aoasl      1  0.201  0.2011   0.575  0.453
    ## hearing:age        1  0.408  0.4081   1.167  0.286
    ## aoasl:age          1  0.020  0.0199   0.057  0.813
    ## hearing:aoasl:age  1  0.079  0.0786   0.225  0.638
    ## Residuals         40 13.987  0.3497               
    ## 3 observations deleted due to missingness

Heat Maps
=========

And finally, we're going to do heat maps.

``` r
eyegaze_heat <- fulldata %>%
  ungroup() %>%
  filter(eye_exclude == FALSE) %>%
  select(id:direction, belly, lowerchest, midchest, upperchest, neck, mouth, eyes, forehead) %>%
  gather(aoi, percent, belly:forehead) %>%
  group_by(maingroup, participant, direction, aoi) %>%
  summarise(percent = mean(percent, na.rm=TRUE)) %>%
  group_by(maingroup,direction,aoi) %>%
  summarise(percent = mean(percent, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(aoi)) %>%
  mutate(aoi = factor(aoi,levels=c("belly","lowerchest","midchest",
                                   "upperchest","neck","mouth","eyes","forehead")))

eyegaze_heat_all <- fulldata %>%
  ungroup() %>%
  filter(eye_exclude == FALSE) %>%
  select(id:direction, belly, lowerchest, midchest, upperchest, neck, mouth, eyes, forehead) %>%
  gather(aoi, percent, belly:forehead) %>%
  group_by(maingroup,participant,direction,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  group_by(maingroup,direction,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  group_by(maingroup,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(aoi)) %>%
  mutate(aoi = factor(aoi,levels=c("belly","lowerchest","midchest",
                                   "upperchest","neck","mouth","eyes","forehead")))

ggplot(eyegaze_heat, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + facet_grid(. ~ direction) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map, by Direction")
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

``` r
ggplot(eyegaze_heat_all, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map (Direction Collapsed)")
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-2.png)

Summary
=======

1.  For **gist**, there are significant main effects of group and direction. The effect of group is driven by HearingNovice doing poorly. Also, all groups do worse with reversed stories. There is also a significant group x direction interaction which we need to sort out.

2.  For **lexical recall**, there are significant main effects of group and direction. The group effect is driven again by HearingNovice. All groups perform poorly when asked to recall signs from reversed stories. There is no group x direction interaction.

3.  For **AoA correlations** there are no significant correlations for deaf and hearing groups separately. When we combine them all in one dataset, there are significant correlations between AoASL and Forward Gist, Reversed Gist, and Reversed Lexical Recall. I worry those may just be driven by HearingNovice, however.

4.  For **eye gaze** there are no significant effects of group or direction on looking at the forehead, eyes, mouth, or neck AOIs. There is a significant effect of group on looking at the upperchest AOI which is driven by HearingNovice.

5.  I modified the **FaceChest** ratio so that the neck AOI is now in Chest, instead of Face (similar to how we do it with babies/children). This significantly changes the variability of the FaceChest measure. However, an ANOVA showed no significant effects of direction or group on this metric. Similar ANOVA outcome for **MouthEye**.

6.  These analysis does not give us any concrete differences between early and late AoA for any of the above measurements...it's not finding the critical AoA cutoff.

Rain's Notes
============

About Adults:

-I think I want to write it up as an ANCOVA, with direction included. And LSD comparisons instead of Tukey. (I will do my own corrections) -You often have one liners summarizing results, in all tabs, those are nice, keep them coming. -(If you have reasons to present anything other than the ANCOVA, put that in your results tab)

I think if we do it this way then we get a really important story to tell: That the *critical* AoA cutoff is below 4 vs above 4 years of age (two groups 0-4 vs 4-13). This suggest early ASL is important.

Gist as binomial logit-link function model
==========================================

``` r
gist_glmm <- glmer(gist ~ direction * maingroup + (1|id) + (1|story), data = fulldata, family=binomial (link="logit"))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
summary(gist_glmm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: gist ~ direction * maingroup + (1 | id) + (1 | story)
    ##    Data: fulldata
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    167.6    200.9    -73.8    147.6      198 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3261 -0.3017  0.0000  0.2294  1.4668 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  id     (Intercept) 1.745    1.321   
    ##  story  (Intercept) 1.952    1.397   
    ## Number of obs: 208, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                          Estimate Std. Error z value
    ## (Intercept)                                 4.696      1.492   3.147
    ## directionreversed                          -3.490      1.259  -2.773
    ## maingroupDeafLate                          16.931    286.428   0.059
    ## maingroupHearingLate                       16.229    276.008   0.059
    ## maingroupHearingNovice                     -4.926      1.512  -3.258
    ## directionreversed:maingroupDeafLate       -18.937    286.427  -0.066
    ## directionreversed:maingroupHearingLate    -18.605    276.009  -0.067
    ## directionreversed:maingroupHearingNovice    1.606      1.484   1.082
    ##                                          Pr(>|z|)   
    ## (Intercept)                               0.00165 **
    ## directionreversed                         0.00556 **
    ## maingroupDeafLate                         0.95286   
    ## maingroupHearingLate                      0.95311   
    ## maingroupHearingNovice                    0.00112 **
    ## directionreversed:maingroupDeafLate       0.94729   
    ## directionreversed:maingroupHearingLate    0.94626   
    ## directionreversed:maingroupHearingNovice  0.27934   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr mngrDL mngrHL mngrHN drc:DL drc:HL
    ## dirctnrvrsd -0.772                                          
    ## maingrpDfLt -0.002  0.001                                   
    ## mngrpHrngLt  0.001 -0.001 -0.130                            
    ## mngrpHrngNv -0.785  0.776  0.002 -0.001                     
    ## drctnrvr:DL  0.001 -0.001 -1.000  0.130 -0.001              
    ## drctnrvr:HL -0.002  0.001  0.130 -1.000  0.002 -0.130       
    ## drctnrvr:HN  0.546 -0.774  0.000  0.002 -0.690  0.001 -0.001
    ## convergence code: 0
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?
