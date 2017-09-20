Lexical Recall Analysis (study1adults)
================
Adam Stone, PhD
09-20-2017

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
  summarize(acc.mean = mean(acc, na.rm=TRUE),
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
    ## maingroup             4 0.21700 0.05425  4.2393  0.002692 ** 
    ## direction             1 0.93360 0.93360 72.9541 7.248e-15 ***
    ## maingroup:direction   4 0.06443 0.01611  1.2587  0.288248    
    ## Residuals           170 2.17549 0.01280                      
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
    ##                                        diff         lwr          upr
    ## DeafEarlyASL-DeafNative         -0.04928571 -0.12538495  0.026813518
    ## DeafLateASL-DeafNative          -0.01476039 -0.09685014  0.067329358
    ## HearingLateASL-DeafNative       -0.03461039 -0.10189770  0.032676920
    ## HearingNoviceASL-DeafNative     -0.09483766 -0.16212497 -0.027550353
    ## DeafLateASL-DeafEarlyASL         0.03452532 -0.05433943  0.123390083
    ## HearingLateASL-DeafEarlyASL      0.01467532 -0.06072892  0.090079573
    ## HearingNoviceASL-DeafEarlyASL   -0.04555195 -0.12095620  0.029852300
    ## HearingLateASL-DeafLateASL      -0.01985000 -0.10129590  0.061595897
    ## HearingNoviceASL-DeafLateASL    -0.08007727 -0.16152317  0.001368624
    ## HearingNoviceASL-HearingLateASL -0.06022727 -0.12672757  0.006273023
    ##                                     p adj
    ## DeafEarlyASL-DeafNative         0.3852557
    ## DeafLateASL-DeafNative          0.9876706
    ## HearingLateASL-DeafNative       0.6167862
    ## HearingNoviceASL-DeafNative     0.0013578
    ## DeafLateASL-DeafEarlyASL        0.8210397
    ## HearingLateASL-DeafEarlyASL     0.9834073
    ## HearingNoviceASL-DeafEarlyASL   0.4579638
    ## HearingLateASL-DeafLateASL      0.9621742
    ## HearingNoviceASL-DeafLateASL    0.0564684
    ## HearingNoviceASL-HearingLateASL 0.0960707

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
0.848
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafEarlyASL
</td>
<td style="text-align:right;">
-0.012
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL
</td>
<td style="text-align:right;">
0.025
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL
</td>
<td style="text-align:right;">
0.018
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL
</td>
<td style="text-align:right;">
-0.065
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
-0.075
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.080
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.106
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL:directionreversed
</td>
<td style="text-align:right;">
-0.059
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
    ## direction                 1 0.93360 0.93360 69.8254 2.097e-14 ***
    ## aoasl                     1 0.08387 0.08387  6.2729   0.01319 *  
    ## hearing                   1 0.01500 0.01500  1.1221   0.29094    
    ## direction:aoasl           1 0.03802 0.03802  2.8434   0.09356 .  
    ## direction:hearing         1 0.00265 0.00265  0.1985   0.65653    
    ## aoasl:hearing             1 0.01011 0.01011  0.7558   0.38585    
    ## direction:aoasl:hearing   1 0.00756 0.00756  0.5655   0.45310    
    ## Residuals               172 2.29971 0.01337                      
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
  summarize(acc.mean = mean(acc, na.rm=TRUE),
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
    ## REML criterion at convergence: -255.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.88645 -0.62603  0.07349  0.60936  2.36803 
    ## 
    ## Random effects:
    ##  Groups   Name              Variance Std.Dev. Corr
    ##  id       (Intercept)       0.002417 0.04916      
    ##           directionreversed 0.003892 0.06239  0.00
    ##  story    (Intercept)       0.001249 0.03534      
    ##  Residual                   0.007714 0.08783      
    ## Number of obs: 180, groups:  id, 46; story, 4
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error       df
    ## (Intercept)                                  0.84829    0.03005 16.03000
    ## maingroupDeafEarlyASL                       -0.01094    0.03872 40.44000
    ## maingroupDeafLateASL                         0.01877    0.04131 42.25000
    ## maingroupHearingLateASL                      0.01658    0.03411 40.41000
    ## maingroupHearingNoviceASL                   -0.06705    0.03411 40.41000
    ## directionreversed                           -0.08447    0.03310 41.59000
    ## maingroupDeafEarlyASL:directionreversed     -0.07593    0.05294 40.66000
    ## maingroupDeafLateASL:directionreversed      -0.07175    0.05629 42.77000
    ## maingroupHearingLateASL:directionreversed   -0.10164    0.04648 40.85000
    ## maingroupHearingNoviceASL:directionreversed -0.05482    0.04648 40.85000
    ##                                             t value Pr(>|t|)    
    ## (Intercept)                                  28.229 4.22e-15 ***
    ## maingroupDeafEarlyASL                        -0.283   0.7789    
    ## maingroupDeafLateASL                          0.454   0.6518    
    ## maingroupHearingLateASL                       0.486   0.6295    
    ## maingroupHearingNoviceASL                    -1.966   0.0562 .  
    ## directionreversed                            -2.552   0.0145 *  
    ## maingroupDeafEarlyASL:directionreversed      -1.434   0.1591    
    ## maingroupDeafLateASL:directionreversed       -1.275   0.2093    
    ## maingroupHearingLateASL:directionreversed    -2.187   0.0345 *  
    ## maingroupHearingNoviceASL:directionreversed  -1.179   0.2450    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mnDEASL mnDLASL mnHLASL mnHNASL drctnr mDEASL: mDLASL:
    ## mngrpDfEASL -0.509                                                       
    ## mngrpDfLASL -0.476  0.369                                                
    ## mngrpHrLASL -0.576  0.443   0.419                                        
    ## mngrpHrNASL -0.576  0.443   0.419   0.510                                
    ## dirctnrvrsd -0.370  0.289   0.269   0.325   0.325                        
    ## mngrpDEASL:  0.233 -0.460  -0.168  -0.197  -0.197  -0.628                
    ## mngrpDLASL:  0.218 -0.169  -0.462  -0.192  -0.192  -0.588  0.367         
    ## mngrpHLASL:  0.263 -0.198  -0.192  -0.456  -0.235  -0.710  0.436   0.419 
    ## mngrpHNASL:  0.263 -0.198  -0.192  -0.235  -0.456  -0.710  0.436   0.419 
    ##             mHLASL:
    ## mngrpDfEASL        
    ## mngrpDfLASL        
    ## mngrpHrLASL        
    ## mngrpHrNASL        
    ## dirctnrvrsd        
    ## mngrpDEASL:        
    ## mngrpDLASL:        
    ## mngrpHLASL:        
    ## mngrpHNASL:  0.511

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
0.848
</td>
<td style="text-align:right;">
0.030
</td>
<td style="text-align:right;">
16.034
</td>
<td style="text-align:right;">
28.229
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
-0.011
</td>
<td style="text-align:right;">
0.039
</td>
<td style="text-align:right;">
40.436
</td>
<td style="text-align:right;">
-0.283
</td>
<td style="text-align:right;">
0.779
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL
</td>
<td style="text-align:right;">
0.019
</td>
<td style="text-align:right;">
0.041
</td>
<td style="text-align:right;">
42.253
</td>
<td style="text-align:right;">
0.454
</td>
<td style="text-align:right;">
0.652
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL
</td>
<td style="text-align:right;">
0.017
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
40.413
</td>
<td style="text-align:right;">
0.486
</td>
<td style="text-align:right;">
0.629
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL
</td>
<td style="text-align:right;">
-0.067
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
40.413
</td>
<td style="text-align:right;">
-1.966
</td>
<td style="text-align:right;">
0.056
</td>
</tr>
<tr>
<td style="text-align:left;">
directionreversed
</td>
<td style="text-align:right;">
-0.084
</td>
<td style="text-align:right;">
0.033
</td>
<td style="text-align:right;">
41.588
</td>
<td style="text-align:right;">
-2.552
</td>
<td style="text-align:right;">
0.014
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
0.053
</td>
<td style="text-align:right;">
40.659
</td>
<td style="text-align:right;">
-1.434
</td>
<td style="text-align:right;">
0.159
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.072
</td>
<td style="text-align:right;">
0.056
</td>
<td style="text-align:right;">
42.772
</td>
<td style="text-align:right;">
-1.275
</td>
<td style="text-align:right;">
0.209
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.102
</td>
<td style="text-align:right;">
0.046
</td>
<td style="text-align:right;">
40.849
</td>
<td style="text-align:right;">
-2.187
</td>
<td style="text-align:right;">
0.035
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingNoviceASL:directionreversed
</td>
<td style="text-align:right;">
-0.055
</td>
<td style="text-align:right;">
0.046
</td>
<td style="text-align:right;">
40.849
</td>
<td style="text-align:right;">
-1.179
</td>
<td style="text-align:right;">
0.245
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
0.7518
</td>
<td style="text-align:right;">
0.0152
</td>
<td style="text-align:right;">
49.5296
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
0.0248
</td>
<td style="text-align:right;">
0.0206
</td>
<td style="text-align:right;">
1.2040
</td>
<td style="text-align:right;">
0.2302
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
