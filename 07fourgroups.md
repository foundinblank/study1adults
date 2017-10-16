Four Groups (study1adults)
================
Adam Stone, PhD
10-16-2017

-   [Introduction](#introduction)
-   [Participants](#participants)
-   [Lexical Recall](#lexical-recall)
    -   [Recovery](#recovery)
    -   [Reversed Only Analysis](#reversed-only-analysis)
-   [Eye Gaze](#eye-gaze)
    -   [Eyes AOI](#eyes-aoi)
    -   [Mouth AOI](#mouth-aoi)
    -   [Chin AOI](#chin-aoi)
    -   [FaceChest Ratio](#facechest-ratio)
-   [Summary](#summary)

Introduction
============

Let's see what happens when we use FOUR groups instead of FIVE. I'm cutting off AoASL at 7 years - younger than that is "Early", older than that is "Late."

Participants
============

``` r
# Load libraries
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(stringr)
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

``` r
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(prettydoc)
library(broom)
library(knitr)
library(xtable)
library(kableExtra)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     ggsave

``` r
options(knitr.table.format = "html") 

# Import data!
data <- read_csv('cleanpercentdata.csv',col_types = 
                   cols(
                     id = col_integer(),
                     participant = col_character(),
                     hearing = col_character(),
                     videogroup = col_character(),
                     aoagroup = col_character(),
                     languagegroup = col_character(),
                     maingroup = col_character(),
                     video = col_character(),
                     story = col_character(),
                     direction = col_character(),
                     age = col_double(),
                     selfrate = col_double(),
                     signyrs = col_double(),
                     aoasl = col_integer(),
                     acc = col_double(),
                     aoi = col_character(),
                     percent = col_double()
                   ))

# And factorize
data <- data %>%
  mutate(participant = as.factor(participant)) %>%
  mutate(id = as.factor(id)) %>%
  mutate(hearing = as.factor(hearing)) %>%
  mutate(videogroup = as.factor(videogroup)) %>%
  mutate(aoagroup = as.factor(aoagroup)) %>%
  mutate(languagegroup = as.factor(languagegroup)) %>%
  mutate(maingroup = as.factor(maingroup)) %>%
  mutate(video = as.factor(video)) %>%
  mutate(story = as.factor(story)) %>%
  mutate(direction = as.factor(direction)) %>%
  mutate(aoi = as.factor(aoi))

# Remove ASL from the end of MainGroup names
data <- data %>%
  mutate(maingroup = case_when(
    str_detect(maingroup,"DeafNative") ~ "DeafNative",
    str_detect(maingroup,"DeafEarlyASL") ~ "DeafEarly",
    str_detect(maingroup,"DeafLateASL") ~ "DeafLate",
    str_detect(maingroup,"HearingLateASL") ~ "HearingLate",
    str_detect(maingroup,"HearingNoviceASL") ~ "HearingNovice"
  )) %>%
  mutate(maingroup = if_else(hearing=="Deaf" & aoasl < 7, "DeafEarly", maingroup)) %>% 
  mutate(maingroup = if_else(hearing=="Deaf" & aoasl > 7, "DeafLate", maingroup)) %>%
  mutate(maingroup = as.factor(maingroup))

# Set reference levels for maingroup
data$maingroup <- relevel(data$maingroup, ref="DeafEarly")

dataoriginal <- data # Save item-level data just in case

# Take out HearingNoviceASL
# data <- data %>%
#   filter(maingroup!="HearingNoviceASL")

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


# # Now collapse eye gaze data to subject-level 
# data <- data %>%
#   group_by(participant,direction,aoi) %>%
#   dplyr::summarize(percent = mean(percent,na.rm=TRUE))
# data[data=="NaN"] <- NA
# 
# # Join subject info with data that's now subject-level
# data <- left_join(data,data.subjectinfo, by=c("participant","direction"))


# But now we need to go back and add in a complete lexical recall dataset, even including those trials that got thrown out in 03eyegaze.nb.html. Because the lexical accuracy data is still good. So let's work on that. 
cleanlexdata <- read_csv('cleandata.csv',col_types = 
                   cols(
                     id = col_integer(),
                     participant = col_character(),
                     hearing = col_character(),
                     videogroup = col_character(),
                     aoagroup = col_character(),
                     languagegroup = col_character(),
                     maingroup = col_character(),
                     video = col_character(),
                     story = col_character(),
                     direction = col_character(),
                     age = col_double(),
                     selfrate = col_double(),
                     signyrs = col_double(),
                     aoasl = col_integer(),
                     acc = col_double(),
                     forehead = col_double(),
                     eyes = col_double(),
                     mouth = col_double(),
                     chin = col_double(),
                     upperchest = col_double(),
                     midchest = col_double(),
                     lowerchest = col_double(),
                     belly = col_double(),
                     left = col_double(),
                     right = col_double(),
                     total = col_double()
                   )) %>%
  mutate(maingroup = case_when(
    str_detect(maingroup,"DeafNative") ~ "DeafNative",
    str_detect(maingroup,"DeafEarlyASL") ~ "DeafEarly",
    str_detect(maingroup,"DeafLateASL") ~ "DeafLate",
    str_detect(maingroup,"HearingLateASL") ~ "HearingLate",
    str_detect(maingroup,"HearingNoviceASL") ~ "HearingNovice"
  )) %>%
  mutate(maingroup = if_else(hearing=="Deaf" & aoasl < 7, "DeafEarly", maingroup)) %>% 
  mutate(maingroup = if_else(hearing=="Deaf" & aoasl > 7, "DeafLate", maingroup)) %>%
  mutate(maingroup = as.factor(maingroup))

cleanlexdata$maingroup <- relevel(cleanlexdata$maingroup, ref="DeafEarly")


# Pull out subject info for later in summary tables
subjectinfo <- data %>%
  select(-aoi,-percent,-video,-story,-direction,-acc) %>%
  distinct()

# Participant Characteristics Table (using cleanlexdata because it's more complete)
groupmeans <- cleanlexdata %>%
  ungroup() %>%
  select(id,participant,maingroup,age,selfrate,signyrs,aoasl) %>%
  distinct() %>%
  group_by(maingroup) %>%
  dplyr::summarize(n = n(),
            age.m = mean(age),
            age.sd = sd(age),
            selfrate.m = mean(selfrate),
            selfrate.sd = sd(selfrate),
            signyrs.m = mean(signyrs),
            signyrs.sd = sd(signyrs),
            aoasl.m = mean(aoasl),
            aoasl.sd = sd(aoasl)) %>%
  mutate(maingroup =  factor(maingroup, levels = c("DeafEarly","DeafLate",
                                                   "HearingLate","HearingNovice"))) %>%
  arrange(maingroup)    
kable(groupmeans, digits=1) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
maingroup
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
age.m
</th>
<th style="text-align:right;">
age.sd
</th>
<th style="text-align:right;">
selfrate.m
</th>
<th style="text-align:right;">
selfrate.sd
</th>
<th style="text-align:right;">
signyrs.m
</th>
<th style="text-align:right;">
signyrs.sd
</th>
<th style="text-align:right;">
aoasl.m
</th>
<th style="text-align:right;">
aoasl.sd
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
DeafEarly
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
33.8
</td>
<td style="text-align:right;">
8.4
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
32.4
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
1.7
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafLate
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
36.8
</td>
<td style="text-align:right;">
6.1
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
23.8
</td>
<td style="text-align:right;">
5.3
</td>
<td style="text-align:right;">
12.6
</td>
<td style="text-align:right;">
3.7
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingLate
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
28.9
</td>
<td style="text-align:right;">
6.2
</td>
<td style="text-align:right;">
4.6
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
11.8
</td>
<td style="text-align:right;">
4.8
</td>
<td style="text-align:right;">
17.2
</td>
<td style="text-align:right;">
3.4
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingNovice
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
20.2
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
3.0
</td>
<td style="text-align:right;">
0.7
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
17.6
</td>
<td style="text-align:right;">
1.8
</td>
</tr>
</tbody>
</table>
Let's see the distribution of AoASL among the four groups.

``` r
data %>%
  select(participant,maingroup,aoasl) %>%
  distinct() %>%
  ggplot(aes(x = aoasl, fill = maingroup)) + 
  geom_histogram(binwidth = .5) + 
  facet_grid(maingroup ~ .) +
  ylab("Age of ASL Acquisition") + ggtitle("Distribution of ASL Acquisition Ages")
```

![](07fourgroups_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

Lexical Recall
==============

Let's get their lex recall scores.

``` r
cleanlexdata %>%
  ggplot(aes(x = maingroup, y = acc, fill = direction)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  ggtitle("Lexical Recall Scores") + xlab("") + ylab("Accuracy")
```

    ## Warning: Removed 4 rows containing non-finite values (stat_boxplot).

![](07fourgroups_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Would a LMM show differences? Let's see. Below, there is a main effect of direction (p &lt; 0.001) and a main effect of maingroup for Hearing Novice (p = 0.03), and then a weak interaction for Direction & HearingLate (p = 0.047).

``` r
lex_recall_lmm <- lmer(acc ~ maingroup * direction + (1|id) + (1|story), data = cleanlexdata)
summary(lex_recall_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ maingroup * direction + (1 | id) + (1 | story)
    ##    Data: cleanlexdata
    ## 
    ## REML criterion at convergence: -303.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.19143 -0.62953  0.01657  0.58032  2.33790 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.002839 0.05328 
    ##  story    (Intercept) 0.001515 0.03892 
    ##  Residual             0.008625 0.09287 
    ## Number of obs: 204, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                                0.852352   0.028018  10.130000
    ## maingroupDeafLate                          0.001893   0.032883  95.780000
    ## maingroupHearingLate                       0.014094   0.031707  93.540000
    ## maingroupHearingNovice                    -0.070440   0.032596  93.900000
    ## directionreversed                         -0.110067   0.022276 146.090000
    ## maingroupDeafLate:directionreversed       -0.045469   0.036444 146.210000
    ## maingroupHearingLate:directionreversed    -0.070326   0.035027 146.270000
    ## maingroupHearingNovice:directionreversed  -0.030576   0.036115 146.540000
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                               30.421 2.72e-11 ***
    ## maingroupDeafLate                          0.058   0.9542    
    ## maingroupHearingLate                       0.445   0.6577    
    ## maingroupHearingNovice                    -2.161   0.0332 *  
    ## directionreversed                         -4.941 2.10e-06 ***
    ## maingroupDeafLate:directionreversed       -1.248   0.2142    
    ## maingroupHearingLate:directionreversed    -2.008   0.0465 *  
    ## maingroupHearingNovice:directionreversed  -0.847   0.3986    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL mngrHN drctnr mngDL: mngHL:
    ## maingrpDfLt -0.442                                          
    ## mngrpHrngLt -0.458  0.392                                   
    ## mngrpHrngNv -0.447  0.383  0.397                            
    ## dirctnrvrsd -0.398  0.341  0.354  0.346                     
    ## mngrpDfLt:d  0.245 -0.554 -0.219 -0.215 -0.615              
    ## mngrpHrngL:  0.255 -0.220 -0.552 -0.225 -0.641  0.397       
    ## mngrpHrngN:  0.248 -0.216 -0.224 -0.554 -0.624  0.389  0.406

How about a traditional ANOVA? It tells us - main effect of direction (p &lt; 0.001), main effect of group (p = 0.004), but no significant interactions. The Tukey's posthoc tells us the main effect of group is driven by a difference between DeafEarly and HearingNovice (p = 0.002), and no other contrasts.

``` r
lexdata_subjects <- cleanlexdata %>%
  group_by(maingroup,participant,direction) %>%
  dplyr::summarize(acc = mean(acc,na.rm=TRUE))

lex_recall_aov <- aov(acc ~ maingroup * direction, data = lexdata_subjects)
summary(lex_recall_aov)
```

    ##                     Df Sum Sq Mean Sq F value   Pr(>F)    
    ## maingroup            3 0.1058  0.0353   4.685  0.00426 ** 
    ## direction            1 0.5211  0.5211  69.212 5.97e-13 ***
    ## maingroup:direction  3 0.0214  0.0071   0.946  0.42160    
    ## Residuals           96 0.7228  0.0075                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(lex_recall_aov, "maingroup", conf.level = 0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = acc ~ maingroup * direction, data = lexdata_subjects)
    ## 
    ## $maingroup
    ##                                    diff         lwr          upr     p adj
    ## DeafLate-DeafEarly        -0.0218174242 -0.08321121  0.039576357 0.7893031
    ## HearingLate-DeafEarly     -0.0220833333 -0.08186826  0.037701597 0.7691496
    ## HearingNovice-DeafEarly   -0.0867424242 -0.14813621 -0.025348643 0.0020482
    ## HearingLate-DeafLate      -0.0002659091 -0.06722891  0.066697095 0.9999996
    ## HearingNovice-DeafLate    -0.0649250000 -0.13332823  0.003478234 0.0692526
    ## HearingNovice-HearingLate -0.0646590909 -0.13162210  0.002303913 0.0624412

### Recovery

What about the recovery metric for reversed stories?

``` r
lex_recovery <- cleanlexdata %>%
  select(maingroup, participant, video, acc) %>%
  filter(video == "rv2" | video == "rv4") %>%
  spread(video, acc) %>%
  mutate(recov = rv4-rv2) %>%
  select(maingroup, participant, recov)

lex_recovery %>%
  ggplot(aes(x = maingroup, y = recov, fill = maingroup)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Recovery in Reversed Stories") + xlab("") + ylab("Accuracy Change") +
  geom_hline(yintercept = 0, linetype = "dotted")
```

    ## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

![](07fourgroups_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

Is there a real effect of group? Simple LM first. No effect of group. No need to do ANOVA, it's the same math.

``` r
lex_recov_lm <- lm(recov ~ maingroup, data = lex_recovery)
summary(lex_recov_lm)
```

    ## 
    ## Call:
    ## lm(formula = recov ~ maingroup, data = lex_recovery)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.26941 -0.10599 -0.00333  0.10250  0.25500 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)             0.06941    0.03302   2.102   0.0411 *
    ## maingroupDeafLate       0.08392    0.05426   1.546   0.1288  
    ## maingroupHearingLate    0.07559    0.05134   1.472   0.1477  
    ## maingroupHearingNovice -0.05123    0.05269  -0.972   0.3360  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1362 on 46 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1409, Adjusted R-squared:  0.08488 
    ## F-statistic: 2.515 on 3 and 46 DF,  p-value: 0.06999

### Reversed Only Analysis

Now let's do all this again with only reversed stories data. LMM, then ANOVA.

``` r
lex_recall_lmm_r <- lmer(acc ~ maingroup * (1|id) + (1|story), data = filter(cleanlexdata,direction=="reversed"))
summary(lex_recall_lmm_r)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ maingroup * (1 | id) + (1 | story)
    ##    Data: filter(cleanlexdata, direction == "reversed")
    ## 
    ## REML criterion at convergence: -122.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.67721 -0.64098  0.03689  0.57540  2.02351 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.004488 0.06699 
    ##  story    (Intercept) 0.002662 0.05160 
    ##  Residual             0.010233 0.10116 
    ## Number of obs: 102, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)             0.74348    0.03482  7.15000  21.351 9.65e-08 ***
    ## maingroupDeafLate      -0.04380    0.03817 49.12000  -1.148  0.25666    
    ## maingroupHearingLate   -0.05905    0.03685 47.82000  -1.602  0.11568    
    ## maingroupHearingNovice -0.10487    0.03797 47.96000  -2.761  0.00813 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL
    ## maingrpDfLt -0.413              
    ## mngrpHrngLt -0.429  0.395       
    ## mngrpHrngNv -0.418  0.386  0.402

``` r
lexdata_subjects <- cleanlexdata %>%
  group_by(maingroup,participant,direction) %>%
  dplyr::summarize(acc = mean(acc,na.rm=TRUE))

lex_recall_aov_r <- aov(acc ~ maingroup, data = filter(lexdata_subjects, direction == "reversed"))
summary(lex_recall_aov_r)
```

    ##             Df Sum Sq  Mean Sq F value Pr(>F)  
    ## maingroup    3 0.0773 0.025760   2.716  0.055 .
    ## Residuals   48 0.4553 0.009486                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(lex_recall_aov_r, "maingroup", conf.level = 0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = acc ~ maingroup, data = filter(lexdata_subjects, direction == "reversed"))
    ## 
    ## $maingroup
    ##                                  diff        lwr          upr     p adj
    ## DeafLate-DeafEarly        -0.04651364 -0.1457132  0.052685889 0.5999831
    ## HearingLate-DeafEarly     -0.05916667 -0.1557666  0.037433292 0.3718206
    ## HearingNovice-DeafEarly   -0.10409091 -0.2032904 -0.004891384 0.0363763
    ## HearingLate-DeafLate      -0.01265303 -0.1208513  0.095545197 0.9894181
    ## HearingNovice-DeafLate    -0.05757727 -0.1681026  0.052948064 0.5138660
    ## HearingNovice-HearingLate -0.04492424 -0.1531225  0.063273984 0.6882181

Eye Gaze
========

We already know the best AOIs are eyes, mouth, chin, and FaceChest Ratio. So let's just plot all those out. The y-axis for FaceChest is of course not percent but a ratio, 1.0 to -1.0 (but no one goes below 0).

``` r
eyegaze <- data %>%
  filter(aoi == "eyes" | aoi == "mouth" | aoi == "chin" | aoi == "facechest")

ggplot(eyegaze, aes(x = maingroup, y = percent, fill = direction)) + 
  geom_boxplot() + 
  facet_wrap("aoi") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent Looking") + xlab("") + ggtitle("Eye Gaze Beahvior") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
```

    ## Warning: Removed 21 rows containing non-finite values (stat_boxplot).

![](07fourgroups_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

Heat map next.

``` r
eyegaze_heat <- data %>%
  ungroup() %>%
  filter(aoi != "left" & aoi != "right" & aoi != "facechest" & aoi != "face" & aoi != "chest") %>%
  group_by(maingroup,participant,direction,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  group_by(maingroup,direction,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(aoi)) %>%
  mutate(aoi = factor(aoi,levels=c("belly","lowerchest","midchest",
                                   "upperchest","chin","mouth","eyes","forehead")))

eyegaze_heat_all <- data %>%
  ungroup() %>%
  filter(aoi != "left" & aoi != "right" & aoi != "facechest" & aoi != "face" & aoi != "chest") %>%
  group_by(maingroup,participant,direction,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  group_by(maingroup,direction,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  group_by(maingroup,aoi) %>%
  dplyr::summarize(percent = mean(percent, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(aoi)) %>%
  mutate(aoi = factor(aoi,levels=c("belly","lowerchest","midchest",
                                   "upperchest","chin","mouth","eyes","forehead")))


ggplot(eyegaze_heat, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + facet_grid(. ~ direction) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map, by Direction")
```

![](07fourgroups_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

``` r
ggplot(eyegaze_heat_all, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map (Direction Collapsed)")
```

![](07fourgroups_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-2.png)

Let's run LMMs and ANOVAs, AOI by AOI.

Eyes AOI
--------

Eyes AOI. LMM and ANOVA tell us there was no effect of direction or maingroup.

``` r
eyedata <- eyegaze %>%
  spread(aoi,percent)

eyedata_subject <- eyedata %>%
  group_by(maingroup,participant,direction) %>%
  dplyr::summarize(eyes = mean(eyes,na.rm=TRUE),
                   mouth = mean(mouth,na.rm=TRUE),
                   chin = mean(chin,na.rm=TRUE),
                   fcr = mean(facechest,na.rm=TRUE))

eyes_lmm <- lmer(eyes ~ maingroup * direction + (1|id) + (1|story), data = eyedata)
summary(eyes_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: eyes ~ maingroup * direction + (1 | id) + (1 | story)
    ##    Data: eyedata
    ## 
    ## REML criterion at convergence: -37.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6981 -0.4587 -0.1219  0.3015  3.7241 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.034564 0.18591 
    ##  story    (Intercept) 0.001451 0.03809 
    ##  Residual             0.024172 0.15547 
    ## Number of obs: 176, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                                0.166401   0.055950  47.920000
    ## maingroupDeafLate                          0.037378   0.084662  61.520000
    ## maingroupHearingLate                       0.121867   0.083181  63.530000
    ## maingroupHearingNovice                     0.040524   0.084171  60.440000
    ## directionreversed                          0.001274   0.041799 119.920000
    ## maingroupDeafLate:directionreversed       -0.026772   0.067959 122.680000
    ## maingroupHearingLate:directionreversed     0.056165   0.065563 119.300000
    ## maingroupHearingNovice:directionreversed  -0.020143   0.064333 119.450000
    ##                                          t value Pr(>|t|)   
    ## (Intercept)                                2.974  0.00459 **
    ## maingroupDeafLate                          0.441  0.66040   
    ## maingroupHearingLate                       1.465  0.14783   
    ## maingroupHearingNovice                     0.481  0.63194   
    ## directionreversed                          0.030  0.97573   
    ## maingroupDeafLate:directionreversed       -0.394  0.69431   
    ## maingroupHearingLate:directionreversed     0.857  0.39335   
    ## maingroupHearingNovice:directionreversed  -0.313  0.75475   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL mngrHN drctnr mngDL: mngHL:
    ## maingrpDfLt -0.584                                          
    ## mngrpHrngLt -0.595  0.393                                   
    ## mngrpHrngNv -0.588  0.389  0.396                            
    ## dirctnrvrsd -0.356  0.234  0.240  0.237                     
    ## mngrpDfLt:d  0.222 -0.354 -0.151 -0.151 -0.620              
    ## mngrpHrngL:  0.226 -0.149 -0.373 -0.151 -0.637  0.395       
    ## mngrpHrngN:  0.234 -0.156 -0.159 -0.374 -0.654  0.416  0.417

``` r
eyes_aov <- aov(eyes ~ maingroup * direction, data = eyedata_subject)
summary(eyes_aov)
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup            3  0.315 0.10491   1.901  0.135
    ## direction            1  0.002 0.00224   0.041  0.841
    ## maingroup:direction  3  0.026 0.00851   0.154  0.927
    ## Residuals           92  5.077 0.05518               
    ## 3 observations deleted due to missingness

``` r
#TukeyHSD(eyes_aov,"maingroup",conf.level = 0.95)
```

How about reversed stories only? It shows that for reversed stories, HearingLate looked significantly more at the eyes than any other group.

``` r
eyes_lmm_r <- lmer(eyes ~ maingroup + (1|id) + (1|story), data = filter(eyedata,direction=="reversed"))
summary(eyes_lmm_r)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: eyes ~ maingroup + (1 | id) + (1 | story)
    ##    Data: filter(eyedata, direction == "reversed")
    ## 
    ## REML criterion at convergence: -6.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1267 -0.4300 -0.1797  0.4120  2.5721 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.036665 0.19148 
    ##  story    (Intercept) 0.006897 0.08305 
    ##  Residual             0.020208 0.14215 
    ## Number of obs: 85, groups:  id, 48; story, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error       df t value Pr(>|t|)  
    ## (Intercept)             0.17814    0.06836  7.92000   2.606   0.0316 *
    ## maingroupDeafLate       0.03218    0.09154 39.21000   0.352   0.7271  
    ## maingroupHearingLate    0.19085    0.08611 39.41000   2.216   0.0325 *
    ## maingroupHearingNovice  0.02061    0.08580 39.00000   0.240   0.8114  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL
    ## maingrpDfLt -0.474              
    ## mngrpHrngLt -0.500  0.377       
    ## mngrpHrngNv -0.505  0.389  0.402

``` r
eyes_aov_r <- aov(eyes ~ maingroup, data = filter(eyedata_subject, direction == "reversed"))
summary(eyes_aov_r)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3 0.2313 0.07710   1.266  0.298
    ## Residuals   44 2.6800 0.06091               
    ## 3 observations deleted due to missingness

``` r
#TukeyHSD(eyes_aov,"maingroup",conf.level = 0.95)
```

Mouth AOI
---------

Mouth AOI. LMM tells us there was no effect of maingroup and a weak effect of reversal (p = 0.056). The ANOVA tells us there were weak effects of group (p = 0.056) and of direction (0.058), no interactions.

Look at the collapsed heat map. HearingLate looked *less* at the mouth than any of the deaf. HearingNovice looked *less* than DeafLate. Really, DeafLate looks at the mouth **a lot** compared to any other group.

``` r
mouth_lmm <- lmer(mouth ~ maingroup * direction + (1|id) + (1|story), data = eyedata)
summary(mouth_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: mouth ~ maingroup * direction + (1 | id) + (1 | story)
    ##    Data: eyedata
    ## 
    ## REML criterion at convergence: -7.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.56049 -0.50387  0.08368  0.58913  2.10900 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.045477 0.21325 
    ##  story    (Intercept) 0.003106 0.05573 
    ##  Residual             0.029111 0.17062 
    ## Number of obs: 193, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error        df
    ## (Intercept)                                0.66760    0.06463  36.42000
    ## maingroupDeafLate                          0.02688    0.09509  62.21000
    ## maingroupHearingLate                      -0.09303    0.09248  61.99000
    ## maingroupHearingNovice                    -0.05537    0.09420  60.19000
    ## directionreversed                         -0.08143    0.04227 134.21000
    ## maingroupDeafLate:directionreversed        0.07823    0.07115 136.80000
    ## maingroupHearingLate:directionreversed    -0.06758    0.06666 134.64000
    ## maingroupHearingNovice:directionreversed  -0.08133    0.06701 134.47000
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                               10.329 2.27e-12 ***
    ## maingroupDeafLate                          0.283   0.7783    
    ## maingroupHearingLate                      -1.006   0.3183    
    ## maingroupHearingNovice                    -0.588   0.5588    
    ## directionreversed                         -1.927   0.0561 .  
    ## maingroupDeafLate:directionreversed        1.100   0.2735    
    ## maingroupHearingLate:directionreversed    -1.014   0.3125    
    ## maingroupHearingNovice:directionreversed  -1.214   0.2270    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL mngrHN drctnr mngDL: mngHL:
    ## maingrpDfLt -0.553                                          
    ## mngrpHrngLt -0.569  0.387                                   
    ## mngrpHrngNv -0.559  0.381  0.392                            
    ## dirctnrvrsd -0.315  0.214  0.221  0.217                     
    ## mngrpDfLt:d  0.188 -0.347 -0.133 -0.133 -0.598              
    ## mngrpHrngL:  0.200 -0.137 -0.361 -0.140 -0.635  0.385       
    ## mngrpHrngN:  0.200 -0.138 -0.142 -0.350 -0.634  0.388  0.408

``` r
mouth_aov <- aov(mouth ~ maingroup * direction, data = eyedata_subject)
summary(mouth_aov)
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)  
    ## maingroup            3  0.505 0.16838   2.584 0.0578 .
    ## direction            1  0.245 0.24486   3.758 0.0555 .
    ## maingroup:direction  3  0.067 0.02250   0.345 0.7927  
    ## Residuals           95  6.191 0.06517                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(mouth_aov,"maingroup",conf.level = 0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = mouth ~ maingroup * direction, data = eyedata_subject)
    ## 
    ## $maingroup
    ##                                  diff        lwr        upr     p adj
    ## DeafLate-DeafEarly         0.06616215 -0.1171418 0.24946609 0.7812726
    ## HearingLate-DeafEarly     -0.12267896 -0.2985985 0.05324056 0.2687176
    ## HearingNovice-DeafEarly   -0.08798686 -0.2686405 0.09266677 0.5818581
    ## HearingLate-DeafLate      -0.18884111 -0.3883151 0.01063288 0.0703127
    ## HearingNovice-DeafLate    -0.15414901 -0.3578103 0.04951230 0.2030652
    ## HearingNovice-HearingLate  0.03469210 -0.1623492 0.23173338 0.9674220

For reversed stories. The LMM tells us HearingLate was significantly different for mouth-looking (p = 0.033) and HearingNovice just barely (p = 0.058). The ANOVA is not significant.

``` r
mouth_lmm_r <- lmer(mouth ~ maingroup + (1|id) + (1|story), data = filter(eyedata,direction == "reversed"))
summary(mouth_lmm_r)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: mouth ~ maingroup + (1 | id) + (1 | story)
    ##    Data: filter(eyedata, direction == "reversed")
    ## 
    ## REML criterion at convergence: 8.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.94643 -0.47193  0.02508  0.48393  2.10474 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.03574  0.1891  
    ##  story    (Intercept) 0.02494  0.1579  
    ##  Residual             0.02706  0.1645  
    ## Number of obs: 95, groups:  id, 51; story, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error       df t value Pr(>|t|)   
    ## (Intercept)             0.59357    0.09556  4.72000   6.211  0.00195 **
    ## maingroupDeafLate       0.06952    0.09051 47.39000   0.768  0.44627   
    ## maingroupHearingLate   -0.18568    0.08439 45.48000  -2.200  0.03290 * 
    ## maingroupHearingNovice -0.16811    0.08652 44.81000  -1.943  0.05833 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL
    ## maingrpDfLt -0.339              
    ## mngrpHrngLt -0.360  0.390       
    ## mngrpHrngNv -0.353  0.385  0.406

``` r
mouth_aov_r <- aov(mouth ~ maingroup, data = filter(eyedata_subject,direction=="reversed"))
summary(mouth_aov_r)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  0.430 0.14330   1.912   0.14
    ## Residuals   47  3.522 0.07493

``` r
TukeyHSD(mouth_aov_r,"maingroup",conf.level = 0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = mouth ~ maingroup, data = filter(eyedata_subject, direction == "reversed"))
    ## 
    ## $maingroup
    ##                                  diff        lwr        upr     p adj
    ## DeafLate-DeafEarly         0.12085184 -0.1667010 0.40840463 0.6795485
    ## HearingLate-DeafEarly     -0.13245624 -0.4041681 0.13925561 0.5685501
    ## HearingNovice-DeafEarly   -0.10396253 -0.3829863 0.17506126 0.7544622
    ## HearingLate-DeafLate      -0.25330808 -0.5654812 0.05886507 0.1492238
    ## HearingNovice-DeafLate    -0.22481437 -0.5433721 0.09374335 0.2505673
    ## HearingNovice-HearingLate  0.02849371 -0.2758412 0.33282863 0.9944805

Chin AOI
--------

Chin AOI. LMM tells us there was no effect of maingroup and a weak effect of reversal (p = 0.050). The ANOVA was not significant.

``` r
chin_lmm <- lmer(chin ~ maingroup * direction + (1|id) + (1|story), data = eyedata)
summary(chin_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: chin ~ maingroup * direction + (1 | id) + (1 | story)
    ##    Data: eyedata
    ## 
    ## REML criterion at convergence: -104.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3963 -0.4427 -0.1022  0.2076  3.8969 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0301635 0.17368 
    ##  story    (Intercept) 0.0004859 0.02204 
    ##  Residual             0.0165956 0.12882 
    ## Number of obs: 189, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error        df
    ## (Intercept)                                0.16115    0.04795  55.14000
    ## maingroupDeafLate                         -0.07739    0.07601  60.02000
    ## maingroupHearingLate                      -0.05146    0.07422  60.65000
    ## maingroupHearingNovice                    -0.05089    0.07537  58.21000
    ## directionreversed                          0.06403    0.03230 131.07000
    ## maingroupDeafLate:directionreversed       -0.06084    0.05381 133.62000
    ## maingroupHearingLate:directionreversed    -0.02744    0.05096 131.17000
    ## maingroupHearingNovice:directionreversed   0.07508    0.05166 131.94000
    ##                                          t value Pr(>|t|)   
    ## (Intercept)                                3.361  0.00142 **
    ## maingroupDeafLate                         -1.018  0.31268   
    ## maingroupHearingLate                      -0.693  0.49074   
    ## maingroupHearingNovice                    -0.675  0.50221   
    ## directionreversed                          1.983  0.04951 * 
    ## maingroupDeafLate:directionreversed       -1.131  0.26025   
    ## maingroupHearingLate:directionreversed    -0.538  0.59116   
    ## maingroupHearingNovice:directionreversed   1.453  0.14853   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL mngrHN drctnr mngDL: mngHL:
    ## maingrpDfLt -0.597                                          
    ## mngrpHrngLt -0.612  0.386                                   
    ## mngrpHrngNv -0.603  0.381  0.390                            
    ## dirctnrvrsd -0.311  0.196  0.201  0.198                     
    ## mngrpDfLt:d  0.187 -0.324 -0.122 -0.121 -0.602              
    ## mngrpHrngL:  0.197 -0.125 -0.344 -0.127 -0.634  0.385       
    ## mngrpHrngN:  0.195 -0.125 -0.127 -0.320 -0.626  0.384  0.400

``` r
chin_aov <- aov(chin ~ maingroup * direction, data = eyedata_subject)
summary(chin_aov)
```

    ##                     Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup            3  0.174 0.05787   1.407  0.246
    ## direction            1  0.108 0.10808   2.627  0.108
    ## maingroup:direction  3  0.046 0.01519   0.369  0.775
    ## Residuals           94  3.867 0.04114               
    ## 1 observation deleted due to missingness

``` r
#TukeyHSD(chin_aov,"maingroup",conf.level = 0.95)
```

For reversed stories. LMM and ANOVA tells us there was no effect of maingroup.

``` r
chin_lmm_r <- lmer(chin ~ maingroup + (1|id) + (1|story), data = filter(eyedata, direction == "reversed"))
summary(chin_lmm_r)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: chin ~ maingroup + (1 | id) + (1 | story)
    ##    Data: filter(eyedata, direction == "reversed")
    ## 
    ## REML criterion at convergence: -19.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9905 -0.3544 -0.1603  0.2401  2.3543 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.03790  0.1947  
    ##  story    (Intercept) 0.00000  0.0000  
    ##  Residual             0.01793  0.1339  
    ## Number of obs: 92, groups:  id, 50; story, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)             0.23208    0.05327 44.11000   4.357 7.76e-05 ***
    ## maingroupDeafLate      -0.14643    0.08763 44.31000  -1.671    0.102    
    ## maingroupHearingLate   -0.08209    0.08244 43.39000  -0.996    0.325    
    ## maingroupHearingNovice  0.01348    0.08501 44.15000   0.159    0.875    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL
    ## maingrpDfLt -0.608              
    ## mngrpHrngLt -0.646  0.393       
    ## mngrpHrngNv -0.627  0.381  0.405

``` r
chin_aov_r <- aov(chin ~ maingroup, data = filter(eyedata_subject, direction == "reversed"))
summary(chin_aov_r)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## maingroup    3  0.177 0.05901   1.166  0.333
    ## Residuals   46  2.327 0.05059               
    ## 1 observation deleted due to missingness

``` r
#TukeyHSD(chin_aov_r,"maingroup",conf.level = 0.95)
```

FaceChest Ratio
---------------

FCR. LMM tells us there was no effects of maingroup or reversals. There is a significant interaction for HearingNovice in the Reversed condition (p = 0.002) where they do worse, and non-significant interactions of HearingLate and Reversed (p = 0.099) as well as HearingNovice overall (p = 0.055)

The ANOVA tells us there is a main effect of group (p &lt; 0.001) but a nonsignificant direction (0.064) with no interactions. The posthoc tells us HearingNovice was different from the other three groups (p &lt; 0.01) - their FCR was significantly lower.

$maingroup diff lwr upr p adj DeafLate-DeafEarly -0.01422087 -0.09856288 0.07012114 0.9711963 HearingLate-DeafEarly -0.03330513 -0.11424941 0.04763916 0.7049517 HearingNovice-DeafEarly -0.14595675 -0.22907930 -0.06283420 0.0000786 HearingLate-DeafLate -0.01908426 -0.11086645 0.07269793 0.9480092 HearingNovice-DeafLate -0.13173588 -0.22544475 -0.03802702 0.0021847 HearingNovice-HearingLate -0.11265162 -0.20331448 -0.02198877 0.0085578

``` r
fcr_lmm <- lmer(facechest ~ maingroup * direction + (1|id) + (1|story), data = eyedata)
summary(fcr_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: facechest ~ maingroup * direction + (1 | id) + (1 | story)
    ##    Data: eyedata
    ## 
    ## REML criterion at convergence: -245
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8883 -0.1749  0.0668  0.3628  1.6509 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0099407 0.09970 
    ##  story    (Intercept) 0.0002749 0.01658 
    ##  Residual             0.0087556 0.09357 
    ## Number of obs: 193, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                                0.971940   0.029723  55.080000
    ## maingroupDeafLate                          0.013829   0.046580  69.370000
    ## maingroupHearingLate                      -0.006362   0.045290  69.110000
    ## maingroupHearingNovice                    -0.089819   0.046037  66.760000
    ## directionreversed                          0.006819   0.023162 136.730000
    ## maingroupDeafLate:directionreversed       -0.060379   0.038874 140.300000
    ## maingroupHearingLate:directionreversed    -0.060507   0.036507 137.400000
    ## maingroupHearingNovice:directionreversed  -0.113524   0.036669 137.540000
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                               32.700  < 2e-16 ***
    ## maingroupDeafLate                          0.297  0.76743    
    ## maingroupHearingLate                      -0.140  0.88869    
    ## maingroupHearingNovice                    -1.951  0.05525 .  
    ## directionreversed                          0.294  0.76890    
    ## maingroupDeafLate:directionreversed       -1.553  0.12263    
    ## maingroupHearingLate:directionreversed    -1.657  0.09972 .  
    ## maingroupHearingNovice:directionreversed  -3.096  0.00238 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL mngrHN drctnr mngDL: mngHL:
    ## maingrpDfLt -0.588                                          
    ## mngrpHrngLt -0.606  0.387                                   
    ## mngrpHrngNv -0.596  0.381  0.392                            
    ## dirctnrvrsd -0.375  0.239  0.247  0.243                     
    ## mngrpDfLt:d  0.224 -0.388 -0.149 -0.148 -0.598              
    ## mngrpHrngL:  0.239 -0.153 -0.404 -0.156 -0.635  0.384       
    ## mngrpHrngN:  0.238 -0.154 -0.158 -0.392 -0.634  0.386  0.406

``` r
fcr_aov <- aov(fcr ~ maingroup * direction, data = eyedata_subject)
summary(fcr_aov)
```

    ##                     Df Sum Sq Mean Sq F value   Pr(>F)    
    ## maingroup            3 0.3193 0.10643   7.714 0.000115 ***
    ## direction            1 0.0483 0.04829   3.500 0.064437 .  
    ## maingroup:direction  3 0.0418 0.01392   1.009 0.392458    
    ## Residuals           95 1.3106 0.01380                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(fcr_aov,"maingroup",conf.level = 0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = fcr ~ maingroup * direction, data = eyedata_subject)
    ## 
    ## $maingroup
    ##                                  diff         lwr         upr     p adj
    ## DeafLate-DeafEarly        -0.01422087 -0.09856288  0.07012114 0.9711963
    ## HearingLate-DeafEarly     -0.03330513 -0.11424941  0.04763916 0.7049517
    ## HearingNovice-DeafEarly   -0.14595675 -0.22907930 -0.06283420 0.0000786
    ## HearingLate-DeafLate      -0.01908426 -0.11086645  0.07269793 0.9480092
    ## HearingNovice-DeafLate    -0.13173588 -0.22544475 -0.03802702 0.0021847
    ## HearingNovice-HearingLate -0.11265162 -0.20331448 -0.02198877 0.0085578

Reversed stories. HearingNovice is significantly different in the LMM (p &lt; 0.001). The ANOVA tells us there was a significant effect of maingroup. Posthocs tell us this effect was ddriven by differences between HearingNovice vs. DeafEarly & DeafLate. What's interesting is that HearingNovice was NOT significantly different than HearingLate.

$maingroup diff lwr upr p adj DeafLate-DeafEarly -0.04407008 -0.1844582 0.0963180293 0.8370200 HearingLate-DeafEarly -0.06232243 -0.1949767 0.0703318668 0.5979387 HearingNovice-DeafEarly -0.20008404 -0.3363081 -0.0638599310 0.0016206 HearingLate-DeafLate -0.01825235 -0.1706605 0.1341558368 0.9886279 HearingNovice-DeafLate -0.15601396 -0.3115392 -0.0004887181 0.0490161 HearingNovice-HearingLate -0.13776161 -0.2863430 0.0108198173 0.0781045

``` r
fcr_lmm_r <- lmer(facechest ~ maingroup + (1|id) + (1|story), data = filter(eyedata, direction == "reversed"))
summary(fcr_lmm_r)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: facechest ~ maingroup + (1 | id) + (1 | story)
    ##    Data: filter(eyedata, direction == "reversed")
    ## 
    ## REML criterion at convergence: -80.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5706 -0.0971  0.1564  0.3311  1.7554 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0122590 0.11072 
    ##  story    (Intercept) 0.0007929 0.02816 
    ##  Residual             0.0119511 0.10932 
    ## Number of obs: 95, groups:  id, 51; story, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)             0.97940    0.03570 22.37000  27.435  < 2e-16 ***
    ## maingroupDeafLate      -0.05018    0.05497 50.20000  -0.913 0.365609    
    ## maingroupHearingLate   -0.06785    0.05132 48.42000  -1.322 0.192339    
    ## maingroupHearingNovice -0.20421    0.05244 47.67000  -3.894 0.000306 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mngrDL mngrHL
    ## maingrpDfLt -0.552              
    ## mngrpHrngLt -0.588  0.386       
    ## mngrpHrngNv -0.577  0.380  0.404

``` r
fcr_aov_r <- aov(fcr ~ maingroup, data = filter(eyedata_subject, direction == "reversed"))
summary(fcr_aov_r)
```

    ##             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## maingroup    3 0.2811 0.09371   5.247 0.00332 **
    ## Residuals   47 0.8395 0.01786                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(fcr_aov_r,"maingroup",conf.level = 0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = fcr ~ maingroup, data = filter(eyedata_subject, direction == "reversed"))
    ## 
    ## $maingroup
    ##                                  diff        lwr           upr     p adj
    ## DeafLate-DeafEarly        -0.04407008 -0.1844582  0.0963180293 0.8370200
    ## HearingLate-DeafEarly     -0.06232243 -0.1949767  0.0703318668 0.5979387
    ## HearingNovice-DeafEarly   -0.20008404 -0.3363081 -0.0638599310 0.0016206
    ## HearingLate-DeafLate      -0.01825235 -0.1706605  0.1341558368 0.9886279
    ## HearingNovice-DeafLate    -0.15601396 -0.3115392 -0.0004887181 0.0490161
    ## HearingNovice-HearingLate -0.13776161 -0.2863430  0.0108198173 0.0781045

Summary
=======

1.  **Lexical Recall.** There is a strong effect of direction across all groups. Only HearingNovice performed significantly different than other groups.
2.  **Eye Gaze & Reversal.** Eye gaze does not appear to be strongly affected by reversal. Any effects found were weak (p = 0.05 or greater).
3.  **Eye Gaze & Group.** HearingLate looks at the eyes a lot. DeafLate looks at the mouth a lot. The FaceChest Ratio tells us novice hearing signers direct attention to the chest significantly than other groups.
