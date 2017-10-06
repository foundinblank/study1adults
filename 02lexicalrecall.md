Lexical Recall Analysis (study1adults)
================
Adam Stone, PhD
10-06-2017

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
    ## maingroup             4 0.23532 0.05883  4.7134  0.001196 ** 
    ## direction             1 1.03872 1.03872 83.2224 < 2.2e-16 ***
    ## maingroup:direction   4 0.05973 0.01493  1.1965  0.313765    
    ## Residuals           190 2.37143 0.01248                      
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
    ##                                         diff         lwr           upr
    ## DeafEarlyASL-DeafNative         -0.049484127 -0.11936970  0.0204014447
    ## DeafLateASL-DeafNative          -0.009205238 -0.08275768  0.0643472032
    ## HearingLateASL-DeafNative       -0.030178571 -0.09519036  0.0348332161
    ## HearingNoviceASL-DeafNative     -0.094837662 -0.16121416 -0.0284611608
    ## DeafLateASL-DeafEarlyASL         0.040278889 -0.03578459  0.1163423716
    ## HearingLateASL-DeafEarlyASL      0.019305556 -0.04853414  0.0871452511
    ## HearingNoviceASL-DeafEarlyASL   -0.045353535 -0.11450216  0.0237950853
    ## HearingLateASL-DeafLateASL      -0.020973333 -0.09258473  0.0506380679
    ## HearingNoviceASL-DeafLateASL    -0.085632424 -0.15848502 -0.0127798318
    ## HearingNoviceASL-HearingLateASL -0.064659091 -0.12887802 -0.0004401597
    ##                                     p adj
    ## DeafEarlyASL-DeafNative         0.2947202
    ## DeafLateASL-DeafNative          0.9969453
    ## HearingLateASL-DeafNative       0.7047530
    ## HearingNoviceASL-DeafNative     0.0010929
    ## DeafLateASL-DeafEarlyASL        0.5907106
    ## HearingLateASL-DeafEarlyASL     0.9350638
    ## HearingNoviceASL-DeafEarlyASL   0.3729068
    ## HearingLateASL-DeafLateASL      0.9283378
    ## HearingNoviceASL-DeafLateASL    0.0122676
    ## HearingNoviceASL-HearingLateASL 0.0475423

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
0.029
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL
</td>
<td style="text-align:right;">
0.019
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
-0.076
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
-0.098
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
    ## direction                 1 1.03872 1.03872 78.5261 5.344e-16 ***
    ## aoasl                     1 0.06217 0.06217  4.6999   0.03139 *  
    ## hearing                   1 0.02675 0.02675  2.0222   0.15663    
    ## direction:aoasl           1 0.03273 0.03273  2.4747   0.11734    
    ## direction:hearing         1 0.00132 0.00132  0.1000   0.75222    
    ## aoasl:hearing             1 0.00112 0.00112  0.0845   0.77162    
    ## direction:aoasl:hearing   1 0.00268 0.00268  0.2027   0.65305    
    ## Residuals               192 2.53971 0.01323                      
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
    ## REML criterion at convergence: -293.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0587 -0.6302  0.0509  0.5563  2.2869 
    ## 
    ## Random effects:
    ##  Groups   Name              Variance Std.Dev. Corr
    ##  id       (Intercept)       0.001770 0.04208      
    ##           directionreversed 0.002799 0.05290  0.20
    ##  story    (Intercept)       0.001610 0.04013      
    ##  Residual                   0.007809 0.08837      
    ## Number of obs: 200, groups:  id, 51; story, 4
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error       df
    ## (Intercept)                                  0.84844    0.03063 12.31000
    ## maingroupDeafEarlyASL                       -0.01096    0.03424 45.61000
    ## maingroupDeafLateASL                         0.02284    0.03586 47.24000
    ## maingroupHearingLateASL                      0.01754    0.03176 45.54000
    ## maingroupHearingNoviceASL                   -0.06729    0.03246 45.60000
    ## directionreversed                           -0.08491    0.03167 46.71000
    ## maingroupDeafEarlyASL:directionreversed     -0.07616    0.04700 45.64000
    ## maingroupDeafLateASL:directionreversed      -0.06721    0.04916 47.58000
    ## maingroupHearingLateASL:directionreversed   -0.09455    0.04346 45.80000
    ## maingroupHearingNoviceASL:directionreversed -0.05421    0.04445 45.84000
    ##                                             t value Pr(>|t|)    
    ## (Intercept)                                  27.701 1.84e-12 ***
    ## maingroupDeafEarlyASL                        -0.320   0.7505    
    ## maingroupDeafLateASL                          0.637   0.5273    
    ## maingroupHearingLateASL                       0.552   0.5835    
    ## maingroupHearingNoviceASL                    -2.073   0.0439 *  
    ## directionreversed                            -2.681   0.0101 *  
    ## maingroupDeafEarlyASL:directionreversed      -1.621   0.1120    
    ## maingroupDeafLateASL:directionreversed       -1.367   0.1780    
    ## maingroupHearingLateASL:directionreversed    -2.175   0.0348 *  
    ## maingroupHearingNoviceASL:directionreversed  -1.220   0.2289    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mnDEASL mnDLASL mnHLASL mnHNASL drctnr mDEASL: mDLASL:
    ## mngrpDfEASL -0.511                                                       
    ## mngrpDfLASL -0.487  0.432                                                
    ## mngrpHrLASL -0.550  0.490   0.471                                        
    ## mngrpHrNASL -0.538  0.478   0.462   0.520                                
    ## dirctnrvrsd -0.342  0.308   0.291   0.329   0.322                        
    ## mngrpDEASL:  0.232 -0.454  -0.191  -0.219  -0.212  -0.676                
    ## mngrpDLASL:  0.220 -0.192  -0.457  -0.215  -0.212  -0.643  0.426         
    ## mngrpHLASL:  0.249 -0.220  -0.215  -0.450  -0.237  -0.728  0.487   0.472 
    ## mngrpHNASL:  0.243 -0.213  -0.212  -0.237  -0.451  -0.711  0.472   0.464 
    ##             mHLASL:
    ## mngrpDfEASL        
    ## mngrpDfLASL        
    ## mngrpHrLASL        
    ## mngrpHrNASL        
    ## dirctnrvrsd        
    ## mngrpDEASL:        
    ## mngrpDLASL:        
    ## mngrpHLASL:        
    ## mngrpHNASL:  0.521

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
0.031
</td>
<td style="text-align:right;">
12.306
</td>
<td style="text-align:right;">
27.701
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
0.034
</td>
<td style="text-align:right;">
45.606
</td>
<td style="text-align:right;">
-0.320
</td>
<td style="text-align:right;">
0.750
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL
</td>
<td style="text-align:right;">
0.023
</td>
<td style="text-align:right;">
0.036
</td>
<td style="text-align:right;">
47.239
</td>
<td style="text-align:right;">
0.637
</td>
<td style="text-align:right;">
0.527
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupHearingLateASL
</td>
<td style="text-align:right;">
0.018
</td>
<td style="text-align:right;">
0.032
</td>
<td style="text-align:right;">
45.543
</td>
<td style="text-align:right;">
0.552
</td>
<td style="text-align:right;">
0.583
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
0.032
</td>
<td style="text-align:right;">
45.600
</td>
<td style="text-align:right;">
-2.073
</td>
<td style="text-align:right;">
0.044
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
0.032
</td>
<td style="text-align:right;">
46.709
</td>
<td style="text-align:right;">
-2.681
</td>
<td style="text-align:right;">
0.010
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
0.047
</td>
<td style="text-align:right;">
45.644
</td>
<td style="text-align:right;">
-1.621
</td>
<td style="text-align:right;">
0.112
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.067
</td>
<td style="text-align:right;">
0.049
</td>
<td style="text-align:right;">
47.584
</td>
<td style="text-align:right;">
-1.367
</td>
<td style="text-align:right;">
0.178
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
0.043
</td>
<td style="text-align:right;">
45.798
</td>
<td style="text-align:right;">
-2.175
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
-0.054
</td>
<td style="text-align:right;">
0.044
</td>
<td style="text-align:right;">
45.839
</td>
<td style="text-align:right;">
-1.220
</td>
<td style="text-align:right;">
0.229
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
0.7567
</td>
<td style="text-align:right;">
0.0144
</td>
<td style="text-align:right;">
52.6254
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
0.0207
</td>
<td style="text-align:right;">
0.0194
</td>
<td style="text-align:right;">
1.0660
</td>
<td style="text-align:right;">
0.2877
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
