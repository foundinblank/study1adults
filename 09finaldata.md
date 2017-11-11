The Last Data Analysis to Rule Them All? (study1adults)
================
Adam Stone, PhD
11-11-2017

-   [Putting It All Back Together](#putting-it-all-back-together)
-   [Group Changes and Participant Tables](#group-changes-and-participant-tables)
-   [Gist & Lexical Recall Data](#gist-lexical-recall-data)
    -   [Tables & Charts](#tables-charts)
    -   [ANOVA Plan](#anova-plan)
    -   [Gist ANOVAs](#gist-anovas)
    -   [Lexical Recall ANOVAs](#lexical-recall-anovas)
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
# But I want to remove columns I don't want anymore
fulldata <- left_join(eyelexdata, gist) %>%
  select(-moutheye, -facechest, -face, -chest)
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

AoA Correlations
================

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

Now eye gaze data. Boxplots first. Also here, we're renaming "chin" to "neck" because that's what it actually is! But we also have to fix all NA's in the percentages to zeros, becuase that's what they actually are.

``` r
# rename chin to neck
fulldata <- fulldata %>%
  rename(neck = chin) %>%
  gather(aoi, percent, belly:upperchest)

# Fix all NA's in Percent column to 0
fixpercent <- fulldata$percent
fulldata$percent <- coalesce(fixpercent, 0)
fulldata <- fulldata %>%
  spread(aoi, percent)

fulldata %>%
  filter(eye_exclude == FALSE) %>%
  select(direction, belly:upperchest) %>%
  gather(aoi, percent, belly:upperchest) %>%
  ggplot(aes(x = aoi, y = percent, fill = direction)) + geom_boxplot()
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/eye%20gaze%20boxplot-1.png)

Forehead
--------

This is going to get complicated - we need to run all 6 ANOVAs/ANCOVAs on 5 AOIs. That's 30 models. Thank goodness for find-and-replace! I'm going to skip the LSD comparisons for now, to make it easier to just scroll through.

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## maingroup            3 0.00957 0.003191   2.309 0.0814 .
    ## direction            1 0.00304 0.003044   2.202 0.1411  
    ## maingroup:direction  3 0.00382 0.001273   0.921 0.4340  
    ## Residuals           95 0.13132 0.001382                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## maingroup    3 0.001415 0.0004717   1.782  0.163
    ## Residuals   48 0.012704 0.0002647

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## maingroup    3 0.01193 0.003977   1.576  0.208
    ## Residuals   47 0.11862 0.002524

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df  Sum Sq   Mean Sq F value Pr(>F)
    ## hearing                      1 0.00156 0.0015592   1.001  0.320
    ## direction                    1 0.00305 0.0030530   1.961  0.165
    ## aoasl                        1 0.00105 0.0010505   0.675  0.414
    ## age                          1 0.00016 0.0001639   0.105  0.746
    ## hearing:direction            1 0.00003 0.0000345   0.022  0.882
    ## hearing:aoasl                1 0.00087 0.0008728   0.561  0.456
    ## direction:aoasl              1 0.00070 0.0007043   0.452  0.503
    ## hearing:age                  1 0.00084 0.0008374   0.538  0.465
    ## direction:age                1 0.00000 0.0000021   0.001  0.971
    ## aoasl:age                    1 0.00031 0.0003086   0.198  0.657
    ## hearing:direction:aoasl      1 0.00162 0.0016219   1.042  0.310
    ## hearing:direction:age        1 0.00058 0.0005841   0.375  0.542
    ## hearing:aoasl:age            1 0.00098 0.0009848   0.632  0.429
    ## direction:aoasl:age          1 0.00021 0.0002114   0.136  0.713
    ## hearing:direction:aoasl:age  1 0.00030 0.0003044   0.195  0.659
    ## Residuals                   87 0.13547 0.0015571

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df   Sum Sq   Mean Sq F value  Pr(>F)   
    ## hearing            1 0.000559 0.0005586   2.256 0.14026   
    ## aoasl              1 0.000018 0.0000176   0.071 0.79123   
    ## age                1 0.000101 0.0001009   0.408 0.52646   
    ## hearing:aoasl      1 0.002434 0.0024339   9.829 0.00306 **
    ## hearing:age        1 0.000011 0.0000115   0.046 0.83045   
    ## aoasl:age          1 0.000005 0.0000047   0.019 0.89052   
    ## hearing:aoasl:age  1 0.000097 0.0000972   0.392 0.53428   
    ## Residuals         44 0.010895 0.0002476                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df  Sum Sq   Mean Sq F value Pr(>F)
    ## hearing            1 0.00100 0.0009959   0.344  0.561
    ## aoasl              1 0.00174 0.0017410   0.601  0.442
    ## age                1 0.00006 0.0000642   0.022  0.882
    ## hearing:aoasl      1 0.00006 0.0000578   0.020  0.888
    ## hearing:age        1 0.00141 0.0014104   0.487  0.489
    ## aoasl:age          1 0.00052 0.0005162   0.178  0.675
    ## hearing:aoasl:age  1 0.00119 0.0011923   0.412  0.525
    ## Residuals         43 0.12457 0.0028970

Eyes
----

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup            3  0.315 0.10508   1.936  0.129
    ## direction            1  0.000 0.00000   0.000  0.999
    ## maingroup:direction  3  0.013 0.00425   0.078  0.972
    ## Residuals           95  5.157 0.05429

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.1234 0.04114   0.863  0.467
    ## Residuals   48 2.2876 0.04766

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.2046 0.06820   1.117  0.352
    ## Residuals   47 2.8695 0.06105

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing                      1  0.151 0.15074   2.769 0.0997 .
    ## direction                    1  0.000 0.00000   0.000 0.9935  
    ## aoasl                        1  0.017 0.01675   0.308 0.5805  
    ## age                          1  0.126 0.12629   2.320 0.1313  
    ## hearing:direction            1  0.000 0.00011   0.002 0.9648  
    ## hearing:aoasl                1  0.288 0.28757   5.283 0.0239 *
    ## direction:aoasl              1  0.000 0.00000   0.000 0.9958  
    ## hearing:age                  1  0.032 0.03219   0.591 0.4439  
    ## direction:age                1  0.018 0.01805   0.332 0.5662  
    ## aoasl:age                    1  0.000 0.00005   0.001 0.9769  
    ## hearing:direction:aoasl      1  0.002 0.00202   0.037 0.8475  
    ## hearing:direction:age        1  0.017 0.01749   0.321 0.5722  
    ## hearing:aoasl:age            1  0.086 0.08610   1.582 0.2119  
    ## direction:aoasl:age          1  0.002 0.00195   0.036 0.8501  
    ## hearing:direction:aoasl:age  1  0.010 0.01017   0.187 0.6666  
    ## Residuals                   87  4.736 0.05443                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.0800 0.08002   1.718 0.1968  
    ## aoasl              1 0.0084 0.00844   0.181 0.6725  
    ## age                1 0.0245 0.02448   0.526 0.4723  
    ## hearing:aoasl      1 0.1690 0.16898   3.628 0.0634 .
    ## hearing:age        1 0.0011 0.00111   0.024 0.8779  
    ## aoasl:age          1 0.0007 0.00069   0.015 0.9034  
    ## hearing:aoasl:age  1 0.0777 0.07773   1.669 0.2032  
    ## Residuals         44 2.0495 0.04658                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)
    ## hearing            1 0.0708 0.07083   1.134  0.293
    ## aoasl              1 0.0083 0.00830   0.133  0.717
    ## age                1 0.1198 0.11983   1.918  0.173
    ## hearing:aoasl      1 0.1207 0.12066   1.932  0.172
    ## hearing:age        1 0.0486 0.04857   0.777  0.383
    ## aoasl:age          1 0.0013 0.00130   0.021  0.886
    ## hearing:aoasl:age  1 0.0185 0.01854   0.297  0.589
    ## Residuals         43 2.6861 0.06247

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

    ##                              difference pvalue signif.         LCL
    ## DeafEarly - DeafLate        -0.06025012 0.3776         -0.19516772
    ## DeafEarly - HearingLate      0.11851495 0.0945       . -0.02077004
    ## DeafEarly - HearingNovice    0.08382286 0.2467         -0.05893611
    ## DeafLate - HearingLate       0.17876508 0.0146       *  0.03608239
    ## DeafLate - HearingNovice     0.14407298 0.0532       . -0.00200290
    ## HearingLate - HearingNovice -0.03469210 0.6474         -0.18481109
    ##                                    UCL
    ## DeafEarly - DeafLate        0.07466748
    ## DeafEarly - HearingLate     0.25779995
    ## DeafEarly - HearingNovice   0.22658183
    ## DeafLate - HearingLate      0.32144776
    ## DeafLate - HearingNovice    0.29014886
    ## HearingLate - HearingNovice 0.11542690

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
    ## maingroup            3  0.195 0.06505   1.584  0.198
    ## direction            1  0.095 0.09484   2.309  0.132
    ## maingroup:direction  3  0.040 0.01326   0.323  0.809
    ## Residuals           95  3.902 0.04107

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.0557 0.01857   0.583  0.629
    ## Residuals   48 1.5296 0.03187

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.1758 0.05860   1.161  0.335
    ## Residuals   47 2.3723 0.05048

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## hearing                      1 0.0002  0.0002   0.005 0.94261   
    ## direction                    1 0.0984  0.0984   2.734 0.10181   
    ## aoasl                        1 0.1045  0.1045   2.904 0.09194 . 
    ## age                          1 0.2307  0.2307   6.413 0.01312 * 
    ## hearing:direction            1 0.0098  0.0098   0.273 0.60259   
    ## hearing:aoasl                1 0.3334  0.3334   9.267 0.00309 **
    ## direction:aoasl              1 0.0119  0.0119   0.330 0.56715   
    ## hearing:age                  1 0.0013  0.0013   0.036 0.85024   
    ## direction:age                1 0.0075  0.0075   0.210 0.64811   
    ## aoasl:age                    1 0.2110  0.2110   5.864 0.01753 * 
    ## hearing:direction:aoasl      1 0.0050  0.0050   0.140 0.70959   
    ## hearing:direction:age        1 0.0129  0.0129   0.358 0.55122   
    ## hearing:aoasl:age            1 0.0527  0.0527   1.465 0.22945   
    ## direction:aoasl:age          1 0.0028  0.0028   0.077 0.78139   
    ## hearing:direction:aoasl:age  1 0.0196  0.0196   0.544 0.46278   
    ## Residuals                   87 3.1301  0.0360                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.0064 0.00640   0.223 0.6388  
    ## aoasl              1 0.0234 0.02336   0.816 0.3712  
    ## age                1 0.0776 0.07762   2.711 0.1068  
    ## hearing:aoasl      1 0.1286 0.12857   4.491 0.0398 *
    ## hearing:age        1 0.0030 0.00302   0.106 0.7467  
    ## aoasl:age          1 0.0827 0.08268   2.888 0.0963 .
    ## hearing:aoasl:age  1 0.0040 0.00402   0.140 0.7097  
    ## Residuals         44 1.2596 0.02863                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1 0.0033 0.00328   0.075 0.7849  
    ## aoasl              1 0.0930 0.09304   2.139 0.1509  
    ## age                1 0.1609 0.16086   3.698 0.0611 .
    ## hearing:aoasl      1 0.2100 0.20999   4.828 0.0334 *
    ## hearing:age        1 0.0112 0.01118   0.257 0.6147  
    ## aoasl:age          1 0.1311 0.13107   3.013 0.0897 .
    ## hearing:aoasl:age  1 0.0682 0.06824   1.569 0.2171  
    ## Residuals         43 1.8705 0.04350                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Upperchest
----------

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## maingroup            3 0.02930 0.009767   5.796 0.00111 **
    ## direction            1 0.00191 0.001909   1.133 0.28987   
    ## maingroup:direction  3 0.00304 0.001013   0.601 0.61589   
    ## Residuals           95 0.16009 0.001685                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                               difference pvalue signif.         LCL
    ## DeafEarly - DeafLate         0.001035362 0.9245         -0.02058361
    ## DeafEarly - HearingLate     -0.005252260 0.6414         -0.02757106
    ## DeafEarly - HearingNovice   -0.041960305 0.0004     *** -0.06483576
    ## DeafLate - HearingLate      -0.006287622 0.5864         -0.02915086
    ## DeafLate - HearingNovice    -0.042995666 0.0004     *** -0.06640262
    ## HearingLate - HearingNovice -0.036708044 0.0032      ** -0.06076286
    ##                                     UCL
    ## DeafEarly - DeafLate         0.02265433
    ## DeafEarly - HearingLate      0.01706654
    ## DeafEarly - HearingNovice   -0.01908484
    ## DeafLate - HearingLate       0.01657562
    ## DeafLate - HearingNovice    -0.01958871
    ## HearingLate - HearingNovice -0.01265323

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## maingroup    3 0.00916 0.003053   1.958  0.133
    ## Residuals   48 0.07484 0.001559

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## maingroup    3 0.02309 0.007695   4.243 0.00984 **
    ## Residuals   47 0.08525 0.001814                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                               difference pvalue signif.         LCL
    ## DeafEarly - DeafLate        -0.008436433 0.6036         -0.04090276
    ## DeafEarly - HearingLate     -0.013171868 0.4286         -0.04635498
    ## DeafEarly - HearingNovice   -0.057042252 0.0015      ** -0.09105300
    ## DeafLate - HearingLate      -0.004735435 0.7824         -0.03903426
    ## DeafLate - HearingNovice    -0.048605819 0.0077      ** -0.08370598
    ## HearingLate - HearingNovice -0.043870384 0.0173       * -0.07963458
    ##                                     UCL
    ## DeafEarly - DeafLate         0.02402990
    ## DeafEarly - HearingLate      0.02001124
    ## DeafEarly - HearingNovice   -0.02303150
    ## DeafLate - HearingLate       0.02956339
    ## DeafLate - HearingNovice    -0.01350566
    ## HearingLate - HearingNovice -0.00810619

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df  Sum Sq  Mean Sq F value  Pr(>F)   
    ## hearing                      1 0.01382 0.013818   7.638 0.00697 **
    ## direction                    1 0.00191 0.001913   1.058 0.30661   
    ## aoasl                        1 0.00027 0.000271   0.150 0.69952   
    ## age                          1 0.01050 0.010504   5.806 0.01808 * 
    ## hearing:direction            1 0.00125 0.001249   0.690 0.40836   
    ## hearing:aoasl                1 0.00005 0.000048   0.026 0.87140   
    ## direction:aoasl              1 0.00016 0.000158   0.088 0.76799   
    ## hearing:age                  1 0.00504 0.005038   2.785 0.09876 . 
    ## direction:age                1 0.00004 0.000041   0.023 0.87998   
    ## aoasl:age                    1 0.00012 0.000122   0.068 0.79557   
    ## hearing:direction:aoasl      1 0.00042 0.000422   0.233 0.63019   
    ## hearing:direction:age        1 0.00035 0.000353   0.195 0.65996   
    ## hearing:aoasl:age            1 0.00050 0.000504   0.279 0.59898   
    ## direction:aoasl:age          1 0.00199 0.001989   1.099 0.29733   
    ## hearing:direction:aoasl:age  1 0.00051 0.000512   0.283 0.59602   
    ## Residuals                   87 0.15740 0.001809                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## hearing            1 0.00342 0.003416   2.110 0.1534  
    ## aoasl              1 0.00043 0.000429   0.265 0.6091  
    ## age                1 0.00462 0.004617   2.852 0.0984 .
    ## hearing:aoasl      1 0.00038 0.000376   0.232 0.6322  
    ## hearing:age        1 0.00136 0.001363   0.842 0.3639  
    ## aoasl:age          1 0.00155 0.001546   0.955 0.3338  
    ## hearing:aoasl:age  1 0.00102 0.001017   0.628 0.4324  
    ## Residuals         44 0.07124 0.001619                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## hearing            1 0.01154 0.011545   5.761 0.0208 *
    ## aoasl              1 0.00001 0.000008   0.004 0.9511  
    ## age                1 0.00594 0.005938   2.963 0.0924 .
    ## hearing:aoasl      1 0.00009 0.000093   0.046 0.8303  
    ## hearing:age        1 0.00403 0.004028   2.010 0.1634  
    ## aoasl:age          1 0.00056 0.000564   0.281 0.5986  
    ## hearing:aoasl:age  1 0.00000 0.000000   0.000 0.9978  
    ## Residuals         43 0.08616 0.002004                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

FaceChest
---------

We originally defined FaceChest such that

1.  Face = eyes + mouth + chin
2.  Chest = upperchest + midchest + lowerchest

BUT. Chin is actually neck. It's not even part of the face if you think about it. So I'm redefining FaceChest as:

1.  Face = forehead + eyes + mouth
2.  Chest = neck + upperchest + midchest + lowerchest

So let's do this. Then see what's happening across groups for FaceChest.

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
  scale_y_continuous(limits = c(-1,1)) + 
  geom_hline(yintercept = 0, linetype = "dotted")
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/facechest%20bars-1.png)

Now let's do the ANOVAs. Also skipping LSDs here.

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ## Warning: Grouping rowwise data frame strips rowwise nature

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
  scale_y_continuous(limits = c(-1,1)) + 
  geom_hline(yintercept = 0, linetype = "dotted")
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/moutheye%20bar%20chart-1.png)

And Moutheye ANOVAs. Also skipping LSDs here.

1.  ANOVA with factors MainGroup & Direction.

<!-- -->

    ##                     Df Sum Sq Mean Sq F value Pr(>F)  
    ## maingroup            3  2.077  0.6923   2.538 0.0612 .
    ## direction            1  0.061  0.0612   0.225 0.6367  
    ## maingroup:direction  3  0.104  0.0347   0.127 0.9437  
    ## Residuals           95 25.912  0.2728                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##                              difference pvalue signif.          LCL
    ## DeafEarly - DeafLate         0.07578075 0.5857         -0.199262170
    ## DeafEarly - HearingLate      0.37662195 0.0099      **  0.092675672
    ## DeafEarly - HearingNovice    0.09217742 0.5310         -0.198850901
    ## DeafLate - HearingLate       0.30084120 0.0428       *  0.009968386
    ## DeafLate - HearingNovice     0.01639667 0.9132         -0.281393506
    ## HearingLate - HearingNovice -0.28444453 0.0681       . -0.590476999
    ##                                    UCL
    ## DeafEarly - DeafLate        0.35082367
    ## DeafEarly - HearingLate     0.66056823
    ## DeafEarly - HearingNovice   0.38320574
    ## DeafLate - HearingLate      0.59171402
    ## DeafLate - HearingNovice    0.31418685
    ## HearingLate - HearingNovice 0.02158794

1.  ANOVA with factor MainGroup, for Forward only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  0.712  0.2374   1.086  0.364
    ## Residuals   48 10.494  0.2186

1.  ANOVA with factor MainGroup, for Reverse only.

<!-- -->

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  1.466  0.4887    1.49  0.229
    ## Residuals   47 15.419  0.3281

1.  ANCOVA with factors Hearing & Direction, and covariate AoASL and Age.

<!-- -->

    ##                             Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing                      1  1.067  1.0665   3.853 0.0529 .
    ## direction                    1  0.059  0.0593   0.214 0.6446  
    ## aoasl                        1  0.145  0.1449   0.523 0.4714  
    ## age                          1  0.510  0.5099   1.842 0.1782  
    ## hearing:direction            1  0.004  0.0042   0.015 0.9023  
    ## hearing:aoasl                1  1.288  1.2885   4.655 0.0337 *
    ## direction:aoasl              1  0.004  0.0038   0.014 0.9075  
    ## hearing:age                  1  0.199  0.1989   0.718 0.3990  
    ## direction:age                1  0.062  0.0615   0.222 0.6385  
    ## aoasl:age                    1  0.000  0.0005   0.002 0.9675  
    ## hearing:direction:aoasl      1  0.032  0.0321   0.116 0.7341  
    ## hearing:direction:age        1  0.094  0.0937   0.338 0.5622  
    ## hearing:aoasl:age            1  0.596  0.5956   2.152 0.1460  
    ## direction:aoasl:age          1  0.005  0.0045   0.016 0.8987  
    ## hearing:direction:aoasl:age  1  0.007  0.0068   0.025 0.8760  
    ## Residuals                   87 24.084  0.2768                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates Age and AoASL, for Forward only

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)  
    ## hearing            1  0.471  0.4710   2.221 0.1433  
    ## aoasl              1  0.051  0.0507   0.239 0.6274  
    ## age                1  0.109  0.1087   0.513 0.4778  
    ## hearing:aoasl      1  0.864  0.8640   4.073 0.0497 *
    ## hearing:age        1  0.010  0.0098   0.046 0.8309  
    ## aoasl:age          1  0.004  0.0039   0.018 0.8928  
    ## hearing:aoasl:age  1  0.365  0.3648   1.720 0.1965  
    ## Residuals         44  9.333  0.2121                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1.  ANCOVA with factor Hearing and covariates AoASL and Age, for Reverse only.

<!-- -->

    ##                   Df Sum Sq Mean Sq F value Pr(>F)
    ## hearing            1  0.595  0.5952   1.735  0.195
    ## aoasl              1  0.098  0.0984   0.287  0.595
    ## age                1  0.462  0.4624   1.348  0.252
    ## hearing:aoasl      1  0.457  0.4567   1.331  0.255
    ## hearing:age        1  0.283  0.2828   0.824  0.369
    ## aoasl:age          1  0.001  0.0010   0.003  0.956
    ## hearing:aoasl:age  1  0.238  0.2376   0.693  0.410
    ## Residuals         43 14.751  0.3430

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
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,.75)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + facet_grid(. ~ direction) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map, by Direction")
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/heat%20map-1.png)

``` r
ggplot(eyegaze_heat_all, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,.75)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map (Direction Collapsed)")
```

![](09finaldata_files/figure-markdown_github-ascii_identifiers/heat%20map-2.png)

Summary
=======

Below are the p-values from the ANOVAs with 4 MainGroups. I never included Age as a covariate because it never improved the model. I included all ANOVAs for Gist and Lex Recall, and ANOVAs for any eye AOI or ratio was included only if either maingroup or direction was significant. Deafearly-Deaflate shows the LSD p-value for that comparison.

``` r
results1 <- structure(list(model = c("gist-maingroup-both", "gist-maingroup-fw", 
"gist-maingroup-rv", "lexrecall-maingroup-both", "lexrecall-maingroup-fw", 
"lexrecall-maingroup-rv", "mouth-maingroup-both", "upperchest-maingroup-both", 
"upperchest-maingroup-rv", "facechest-maingroup-both", "moutheye-maingroup-both"
), maingroup = c(0, 0, 0.01, 0, 0.04, 0.02, 0.06, 0, 0.01, 0.1, 
0.05), direction = c(0, NA, NA, 0, NA, NA, 0.06, 0.16, NA, 0.07, 
0.48), `deafearly-deaflate` = c(0.1, 0.69, 0.02, 0.11, 0.95, 
0.06, 0.38, 0.94, 0.52, 0.08, 0.68)), .Names = c("model", "maingroup", 
"direction", "deafearly-deaflate"), class = c("tbl_df", "tbl", 
"data.frame"), row.names = c(NA, -11L))

results1
```

    ## # A tibble: 11 x 4
    ##                        model maingroup direction `deafearly-deaflate`
    ##                        <chr>     <dbl>     <dbl>                <dbl>
    ##  1       gist-maingroup-both      0.00      0.00                 0.10
    ##  2         gist-maingroup-fw      0.00        NA                 0.69
    ##  3         gist-maingroup-rv      0.01        NA                 0.02
    ##  4  lexrecall-maingroup-both      0.00      0.00                 0.11
    ##  5    lexrecall-maingroup-fw      0.04        NA                 0.95
    ##  6    lexrecall-maingroup-rv      0.02        NA                 0.06
    ##  7      mouth-maingroup-both      0.06      0.06                 0.38
    ##  8 upperchest-maingroup-both      0.00      0.16                 0.94
    ##  9   upperchest-maingroup-rv      0.01        NA                 0.52
    ## 10  facechest-maingroup-both      0.10      0.07                 0.08
    ## 11   moutheye-maingroup-both      0.05      0.48                 0.68

And below are the p-values from the ANCOVAs with Hearing & AoASL. I included all ANCOVAs for Gist and Lex Recall, and ANCOVAs for any eye AOI or ratio was included only if any main factor was significant. LSD comparisons are not needed because there's only 2 levels in each group!

``` r
results2 <- structure(list(model = c("gist-both", "gist-fw", "gist-rv", "lex-both", 
"lex-fw", "lex-rv", "forehead-fw", "mouth-both", "mouth-rv", 
"upperchest-both", "upperchest-rv", "facechest-both", "moutheye-both"
), hearing = c(0, 0.00, 0.01, 0.01, 0.22, 0.03, 0.06, 0.01, 
0.04, 0.01, 0.01, 0.35, 0.07), direction = c(0, NA, NA, 0, NA, 
NA, NA, 0.05, NA, 0.21, NA, 0.05, 0.52), aoasl = c(0.22, 0.77, 
0.19, 0.56, 0.58, 0.25, 0.08, 0.06, 0.12, 0.68, 0.95, 0.12, 0.44
), age = c(0.08, 0.01, 0.86, 0.09, 0.02, 0.7, 0.68, 0.28, 0.5, 
0.02, 0.08, 0.00, 0.21)), .Names = c("model", "hearing", "direction", 
"aoasl", "age"), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
-13L))
results2
```

    ## # A tibble: 13 x 5
    ##              model hearing direction aoasl   age
    ##              <chr>   <dbl>     <dbl> <dbl> <dbl>
    ##  1       gist-both    0.00      0.00  0.22  0.08
    ##  2         gist-fw    0.00        NA  0.77  0.01
    ##  3         gist-rv    0.01        NA  0.19  0.86
    ##  4        lex-both    0.01      0.00  0.56  0.09
    ##  5          lex-fw    0.22        NA  0.58  0.02
    ##  6          lex-rv    0.03        NA  0.25  0.70
    ##  7     forehead-fw    0.06        NA  0.08  0.68
    ##  8      mouth-both    0.01      0.05  0.06  0.28
    ##  9        mouth-rv    0.04        NA  0.12  0.50
    ## 10 upperchest-both    0.01      0.21  0.68  0.02
    ## 11   upperchest-rv    0.01        NA  0.95  0.08
    ## 12  facechest-both    0.35      0.05  0.12  0.00
    ## 13   moutheye-both    0.07      0.52  0.44  0.21

Finally, the correlations for Deaf and Hearing separately are not significant. But there are significant correlations across all participants. I worry it is caused by HearingNovice, though...

``` r
results3 <- tribble(
  ~ metric, ~ AoASLcorrelationRvalue, ~ Pvalue,
  "gist-fw", -0.32, 0.019,
  "gist-rv", -0.39, 0.004,
  "lex-fw", -0.08, 0.567,
  "lex-rv", -0.34, 0.014
)
results3
```

    ## # A tibble: 4 x 3
    ##    metric AoASLcorrelationRvalue Pvalue
    ##     <chr>                  <dbl>  <dbl>
    ## 1 gist-fw                  -0.32  0.019
    ## 2 gist-rv                  -0.39  0.004
    ## 3  lex-fw                  -0.08  0.567
    ## 4  lex-rv                  -0.34  0.014

Rain's Notes
============

About Adults:

-I think I want to write it up as an ANCOVA, with direction included. And LSD comparisons instead of Tukey. (I will do my own corrections) -You often have one liners summarizing results, in all tabs, those are nice, keep them coming. -(If you have reasons to present anything other than the ANCOVA, put that in your results tab)

I think if we do it this way then we get a really important story to tell: That the *critical* AoA cutoff is below 4 vs above 4 years of age (two groups 0-4 vs 4-13). This suggest early ASL is important.
