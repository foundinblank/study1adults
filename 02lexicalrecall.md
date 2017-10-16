Lexical Recall Analysis (study1adults)
================
Adam Stone, PhD
10-16-2017

-   [Re-Initializing](#re-initializing)
-   [ANOVAS](#anovas)
-   [Group ANOVA](#group-anova)
-   [AoASL & Hearing ANCOVA](#aoasl-hearing-ancova)
-   [Item-Level Modeling](#item-level-modeling)
-   [Group1/Group2 Differences?](#group1group2-differences)

Re-Initializing
===============

This assumes you've already done [01dataimportclean](01dataimportclean.nb.html) and so there'll be a nice new .csv file to re-import here. Also we gotta import all the libraries again.

``` r
# Import packages we'll need.
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(stringr)
library(lme4)
```

    ## Warning: package 'lme4' was built under R version 3.4.2

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
options(knitr.table.format = "html") 
data <- read_csv('cleandata.csv',col_types = 
                   cols(.default = col_double(),
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
                        right = col_double()
                   ))
# And factorize
data <- data %>%
  mutate(hearing = as.factor(hearing)) %>%
  mutate(videogroup = as.factor(videogroup)) %>%
  mutate(aoagroup = as.factor(aoagroup)) %>%
  mutate(languagegroup = as.factor(languagegroup)) %>%
  mutate(maingroup = as.factor(maingroup)) %>%
  mutate(video = as.factor(video)) %>%
  mutate(story = as.factor(story)) %>%
  mutate(direction = as.factor(direction))
```

ANOVAS
======

Lexical recall accuracy violins and error bar charts for forward vs. backward stories. The error bars represent standard deviations.

``` r
# Summarizing means and SDs
accdata <- data %>%
  ungroup() %>%
  group_by(maingroup,direction) %>%
  dplyr::summarize(acc.mean = mean(acc, na.rm=TRUE),
            acc.sd = sd(acc, na.rm=TRUE))

#Violin plot
ggplot(data,aes(maingroup,acc,fill=direction)) + 
  geom_violin() +
  scale_y_continuous(limits=c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 4 rows containing non-finite values (stat_ydensity).

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

``` r
# Error bar chart
ggplot(accdata,aes(maingroup,acc.mean,color=direction)) + 
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=acc.mean-acc.sd,ymax=acc.mean+acc.sd),width=0.1,position=position_dodge(0.5)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Group ANOVA
===========

Let's test for statistical significance. A simple ANOVA tell us there is a main effect of group and direction, but no interactions.

``` r
# Let's set Deaf Native as the reference level to compare all other
data$maingroup <- relevel(data$maingroup, ref="DeafNative")
# Run the ANOVA
acc.anova <- aov(data=data,acc ~ maingroup*direction)
anova(acc.anova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: acc
    ##                      Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## maingroup             4 0.25883 0.06471  5.2652 0.0004781 ***
    ## direction             1 1.03966 1.03966 84.5953 < 2.2e-16 ***
    ## maingroup:direction   4 0.06437 0.01609  1.3094 0.2678884    
    ## Residuals           194 2.38421 0.01229                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# kable(tidy(acc.anova), digits=3) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

Tukey's HSD posthoc tells us that Hearing Novice ASL is significantly different from Deaf Native.

``` r
# Run the posthoc on main group
acc.posthoc <- TukeyHSD(acc.anova,'maingroup',conf.level = 0.95) 
acc.posthoc
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = acc ~ maingroup * direction, data = data)
    ## 
    ## $maingroup
    ##                                        diff         lwr           upr
    ## DeafEarlyASL-DeafNative         -0.05435990 -0.12228806  0.0135682502
    ## DeafLateASL-DeafNative          -0.01408101 -0.08571849  0.0575564659
    ## HearingLateASL-DeafNative       -0.03505435 -0.09803939  0.0279306969
    ## HearingNoviceASL-DeafNative     -0.09971344 -0.16408426 -0.0353426175
    ## DeafLateASL-DeafEarlyASL         0.04027889 -0.03518395  0.1157417278
    ## HearingLateASL-DeafEarlyASL      0.01930556 -0.04799844  0.0866095474
    ## HearingNoviceASL-DeafEarlyASL   -0.04535354 -0.11395612  0.0232490455
    ## HearingLateASL-DeafLateASL      -0.02097333 -0.09201925  0.0500725805
    ## HearingNoviceASL-DeafLateASL    -0.08563242 -0.15790973 -0.0133551204
    ## HearingNoviceASL-HearingLateASL -0.06465909 -0.12837091 -0.0009472717
    ##                                     p adj
    ## DeafEarlyASL-DeafNative         0.1827414
    ## DeafLateASL-DeafNative          0.9828866
    ## HearingLateASL-DeafNative       0.5426520
    ## HearingNoviceASL-DeafNative     0.0002986
    ## DeafLateASL-DeafEarlyASL        0.5833527
    ## HearingLateASL-DeafEarlyASL     0.9333215
    ## HearingNoviceASL-DeafEarlyASL   0.3648014
    ## HearingLateASL-DeafLateASL      0.9264324
    ## HearingNoviceASL-DeafLateASL    0.0112978
    ## HearingNoviceASL-HearingLateASL 0.0447976

``` r
#kable(tidy(acc.posthoc), digits=3) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

Group coefficients are here. Remember our reference level ("control") is Native Deaf. Their forward accuracy is 85% with a reversal effect of -9%. ASL, Forward. Mean accuracy for that is 84%, and its reversal effect is -14%. All the other values are to be added to these coefficients. Hearing Novice ASL's forward accuracy was 78%, with a reversal effect of -6%!

``` r
#Coefficients
kable(tidy(acc.anova$coefficients), digits=3) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
names
</th>
<th style="text-align:right;">
x
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
(Intercept)
</td>
<td style="text-align:right;">
0.852
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafEarlyASL
</td>
<td style="text-align:right;">
-0.016
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL
</td>
<td style="text-align:right;">
0.024
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL
</td>
<td style="text-align:right;">
0.014
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL
</td>
<td style="text-align:right;">
-0.070
</td>
</tr>
<tr>
<td style="text-align:left;">
directionreversed
</td>
<td style="text-align:right;">
-0.082
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafEarlyASL:directionreversed
</td>
<td style="text-align:right;">
-0.077
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.077
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.099
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL:directionreversed
</td>
<td style="text-align:right;">
-0.060
</td>
</tr>
</tbody>
</table>
AoASL & Hearing ANCOVA
======================

Let's try ANCOVAs where we include AoASL and hearing status as predictors too - so now we're not using MainGroups anymore. But we do have the AoASL-hearing collinearity problem. So. Let's try it anyway. Plot first.

``` r
ggplot(data, aes(x=aoasl,y=acc)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png) And now the ANCOVA. I'm a bit surprised we're not getting a direction:AoASL:hearing interaction because definitely we can see within the deaf group, there is a reversal effect correlated with AoASL. Maybe it's because of the collinearity.

``` r
acc.ancova <- aov(data=data,acc ~ direction * aoasl * hearing)
anova(acc.ancova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: acc
    ##                          Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## direction                 1 1.03966 1.03966 79.5814 3.283e-16 ***
    ## aoasl                     1 0.07953 0.07953  6.0878   0.01447 *  
    ## hearing                   1 0.02473 0.02473  1.8933   0.17040    
    ## direction:aoasl           1 0.03649 0.03649  2.7933   0.09625 .  
    ## direction:hearing         1 0.00142 0.00142  0.1089   0.74171    
    ## aoasl:hearing             1 0.00181 0.00181  0.1386   0.71010    
    ## direction:aoasl:hearing   1 0.00287 0.00287  0.2196   0.63990    
    ## Residuals               196 2.56055 0.01306                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

In summary, ANOVA tells us there are main effects of group and direction, no interactions. The ANCOVA tells us we have a main effect of direction and AoASL, but not of hearing. All interesting.

Item-Level Modeling
===================

I'm curious if there's any item-level effects we should be watching out for. Because there are 4 different stories. Let's plot those out.

``` r
# Run summary stats grouped by story, too
accdata2 <- data %>%
  group_by(maingroup,story,direction) %>%
  dplyr::summarize(acc.mean = mean(acc, na.rm=TRUE),
            acc.sd = sd(acc, na.rm=TRUE))
# Boxplot
# ggplot(data,aes(maingroup,acc,fill=direction)) + 
#   geom_boxplot() +
#   scale_y_continuous(limits=c(0,1)) +
#   facet_wrap("story")

# Error bar chart
ggplot(accdata2,aes(maingroup,acc.mean,color=direction)) + 
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=acc.mean-acc.sd,ymax=acc.mean+acc.sd),width=0.1,position=position_dodge(0.5)) +
  scale_y_continuous(limits=c(0,1)) +
  facet_wrap("story") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 2 rows containing missing values (geom_errorbar).

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

Boy! Seems King Midas had a stronger reversal effect, while Red Riding Hood had a weaker reversal effect.

Maybe we should put those in as random effects variables in a mixed model, along with participants too. With mixed models, you define predictor variables (what we're interested in; aka, fixed effects) and grouping (what we're not interested in, aka, random effects). This is overkill for simple accuracy data but this will help set us up for eye tracking analysis and **importantly reviewers may ask us about item-level effects given we have just 4 stories.**

So here, we have fixed effects of group and direction, and random effects of story and id, with varying slopes for direction.

The output tells us now that Hearing Novice ASL is significantly different from Deaf Native ASL, that we still have a main effect of direction, with a significant interaction of direction and Hearing Late ASL. We probably don't need all this in the paper, though, the ANOVA will do and accuracy is not our main point.

``` r
acc.lm <- lmer(data=data, acc ~ maingroup*direction + (direction|id) + (1|story))
summary(acc.lm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ maingroup * direction + (direction | id) + (1 | story)
    ##    Data: data
    ## 
    ## REML criterion at convergence: -303.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.06851 -0.62549  0.04674  0.55393  2.30845 
    ## 
    ## Random effects:
    ##  Groups   Name              Variance Std.Dev. Corr
    ##  id       (Intercept)       0.001767 0.04204      
    ##           directionreversed 0.002675 0.05172  0.21
    ##  story    (Intercept)       0.001556 0.03945      
    ##  Residual                   0.007696 0.08773      
    ## Number of obs: 204, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error       df
    ## (Intercept)                                  0.85322    0.02955 11.56000
    ## maingroupDeafEarlyASL                       -0.01581    0.03334 46.51000
    ## maingroupDeafLateASL                         0.01819    0.03506 48.54000
    ## maingroupHearingLateASL                      0.01280    0.03089 46.65000
    ## maingroupHearingNoviceASL                   -0.07201    0.03161 46.76000
    ## directionreversed                           -0.08475    0.02993 47.65000
    ## maingroupDeafEarlyASL:directionreversed     -0.07618    0.04543 46.50000
    ## maingroupDeafLateASL:directionreversed      -0.06757    0.04784 48.77000
    ## maingroupHearingLateASL:directionreversed   -0.09478    0.04204 46.78000
    ## maingroupHearingNoviceASL:directionreversed -0.05448    0.04308 46.85000
    ##                                             t value Pr(>|t|)    
    ## (Intercept)                                  28.873  3.9e-12 ***
    ## maingroupDeafEarlyASL                        -0.474  0.63762    
    ## maingroupDeafLateASL                          0.519  0.60620    
    ## maingroupHearingLateASL                       0.414  0.68053    
    ## maingroupHearingNoviceASL                    -2.278  0.02732 *  
    ## directionreversed                            -2.831  0.00677 ** 
    ## maingroupDeafEarlyASL:directionreversed      -1.677  0.10026    
    ## maingroupDeafLateASL:directionreversed       -1.412  0.16419    
    ## maingroupHearingLateASL:directionreversed    -2.254  0.02889 *  
    ## maingroupHearingNoviceASL:directionreversed  -1.265  0.21223    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mnDEASL mnDLASL mnHLASL mnHNASL drctnr mDEASL: mDLASL:
    ## mngrpDfEASL -0.491                                                       
    ## mngrpDfLASL -0.468  0.410                                                
    ## mngrpHrLASL -0.531  0.467   0.450                                        
    ## mngrpHrNASL -0.519  0.455   0.442   0.499                                
    ## dirctnrvrsd -0.335  0.296   0.284   0.321   0.314                        
    ## mngrpDEASL:  0.220 -0.449  -0.180  -0.207  -0.201  -0.657                
    ## mngrpDLASL:  0.211 -0.180  -0.456  -0.205  -0.203  -0.628  0.404         
    ## mngrpHLASL:  0.239 -0.208  -0.206  -0.448  -0.227  -0.713  0.463   0.452 
    ## mngrpHNASL:  0.234 -0.201  -0.203  -0.227  -0.450  -0.696  0.449   0.445 
    ##             mHLASL:
    ## mngrpDfEASL        
    ## mngrpDfLASL        
    ## mngrpHrLASL        
    ## mngrpHrNASL        
    ## dirctnrvrsd        
    ## mngrpDEASL:        
    ## mngrpDLASL:        
    ## mngrpHLASL:        
    ## mngrpHNASL:  0.501

Here are the coefficients in a nicer format.

``` r
kable(xtable(coef(summary(acc.lm))), digits=3) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Estimate
</th>
<th style="text-align:right;">
Std. Error
</th>
<th style="text-align:right;">
df
</th>
<th style="text-align:right;">
t value
</th>
<th style="text-align:right;">
Pr(&gt;|t|)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
(Intercept)
</td>
<td style="text-align:right;">
0.853
</td>
<td style="text-align:right;">
0.030
</td>
<td style="text-align:right;">
11.558
</td>
<td style="text-align:right;">
28.873
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafEarlyASL
</td>
<td style="text-align:right;">
-0.016
</td>
<td style="text-align:right;">
0.033
</td>
<td style="text-align:right;">
46.514
</td>
<td style="text-align:right;">
-0.474
</td>
<td style="text-align:right;">
0.638
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL
</td>
<td style="text-align:right;">
0.018
</td>
<td style="text-align:right;">
0.035
</td>
<td style="text-align:right;">
48.543
</td>
<td style="text-align:right;">
0.519
</td>
<td style="text-align:right;">
0.606
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL
</td>
<td style="text-align:right;">
0.013
</td>
<td style="text-align:right;">
0.031
</td>
<td style="text-align:right;">
46.652
</td>
<td style="text-align:right;">
0.414
</td>
<td style="text-align:right;">
0.681
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL
</td>
<td style="text-align:right;">
-0.072
</td>
<td style="text-align:right;">
0.032
</td>
<td style="text-align:right;">
46.761
</td>
<td style="text-align:right;">
-2.278
</td>
<td style="text-align:right;">
0.027
</td>
</tr>
<tr>
<td style="text-align:left;">
directionreversed
</td>
<td style="text-align:right;">
-0.085
</td>
<td style="text-align:right;">
0.030
</td>
<td style="text-align:right;">
47.647
</td>
<td style="text-align:right;">
-2.831
</td>
<td style="text-align:right;">
0.007
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafEarlyASL:directionreversed
</td>
<td style="text-align:right;">
-0.076
</td>
<td style="text-align:right;">
0.045
</td>
<td style="text-align:right;">
46.502
</td>
<td style="text-align:right;">
-1.677
</td>
<td style="text-align:right;">
0.100
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.068
</td>
<td style="text-align:right;">
0.048
</td>
<td style="text-align:right;">
48.765
</td>
<td style="text-align:right;">
-1.412
</td>
<td style="text-align:right;">
0.164
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.095
</td>
<td style="text-align:right;">
0.042
</td>
<td style="text-align:right;">
46.778
</td>
<td style="text-align:right;">
-2.254
</td>
<td style="text-align:right;">
0.029
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL:directionreversed
</td>
<td style="text-align:right;">
-0.054
</td>
<td style="text-align:right;">
0.043
</td>
<td style="text-align:right;">
46.849
</td>
<td style="text-align:right;">
-1.265
</td>
<td style="text-align:right;">
0.212
</td>
</tr>
</tbody>
</table>
Group1/Group2 Differences?
==========================

One more thing I just remembered. What if a reviewer asks if Group 1 differed from Group 2 (in other words, was there an effect of stimulus order)? Easy to do now that our data is nicely organized (or "tidy").

``` r
acc.lm.order <- lm(data=data, acc ~ videogroup)
kable(tidy(summary(acc.lm.order)),digits=4)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
term
</th>
<th style="text-align:right;">
estimate
</th>
<th style="text-align:right;">
std.error
</th>
<th style="text-align:right;">
statistic
</th>
<th style="text-align:right;">
p.value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
(Intercept)
</td>
<td style="text-align:right;">
0.7612
</td>
<td style="text-align:right;">
0.0140
</td>
<td style="text-align:right;">
54.2806
</td>
<td style="text-align:right;">
0.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
videogroupGroup 2
</td>
<td style="text-align:right;">
0.0162
</td>
<td style="text-align:right;">
0.0191
</td>
<td style="text-align:right;">
0.8464
</td>
<td style="text-align:right;">
0.3983
</td>
</tr>
</tbody>
</table>
Good, good, no sig diff between groups.

``` r
ggplot(data, aes(x=videogroup, y=acc, fill=videogroup)) + 
#  geom_point(position="jitter") +
  geom_violin() +
  geom_jitter(width=.3)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)

``` r
# ggplot(filter(data,direction=="reversed"),aes(x=aoasl,y=acc)) +
#   geom_point() +
#   geom_smooth(method="lm")
```

``` r
# aoa.model <- lm(acc ~ aoasl, data=filter(data,direction=="reversed"))
# summary(aoa.model)
```

<!-- acc.lm <- lmer(data=data, acc ~ maingroup*direction + (direction|id) + (1|story)) -->
<!-- summary(acc.lm) -->
