Lexical Recall Analysis (study1adults)
================
Adam Stone, PhD
09-06-2017

-   [Re-Initializing](#re-initializing)
-   [Analysis by Groups](#analysis-by-groups)
-   [AOA Modeling](#aoa-modeling)

Re-Initializing
===============

This assumes you've already done 01dataimportclean and so there'll be a nice new .csv file to re-import here. Also we gotta import all the libraries again.

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

Analysis by Groups
==================

And accuracy violins and error bar charts for forward vs. backward stories.

``` r
# Summarizing means and SDs
accdata <- data %>%
  ungroup() %>%
  group_by(maingroup,direction) %>%
  summarize(acc.mean = mean(acc, na.rm=TRUE),
            acc.sd = sd(acc, na.rm=TRUE))

#Boxplot
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

Let's test for statistical significance. A simple ANOVA tell us there is a main effect of group and direction, but no interactions.

``` r
# Let's set Native Deaf as the reference level to compare all other
data$maingroup <- relevel(data$maingroup, ref="NativeDeaf")
# Run the ANOVA
acc.anova <- aov(data=data,acc ~ maingroup*direction)
kable(tidy(acc.anova), digits=3) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
term
</th>
<th style="text-align:right;">
df
</th>
<th style="text-align:right;">
sumsq
</th>
<th style="text-align:right;">
meansq
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
maingroup
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.214
</td>
<td style="text-align:right;">
0.053
</td>
<td style="text-align:right;">
4.202
</td>
<td style="text-align:right;">
0.003
</td>
</tr>
<tr>
<td style="text-align:left;">
direction
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.899
</td>
<td style="text-align:right;">
0.899
</td>
<td style="text-align:right;">
70.656
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup:direction
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.056
</td>
<td style="text-align:right;">
0.014
</td>
<td style="text-align:right;">
1.097
</td>
<td style="text-align:right;">
0.360
</td>
</tr>
<tr>
<td style="text-align:left;">
Residuals
</td>
<td style="text-align:right;">
174
</td>
<td style="text-align:right;">
2.215
</td>
<td style="text-align:right;">
0.013
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>
Tukey's HSD posthoc tells us that Hearing Novice ASL is significantly different from Deaf Native.

``` r
# Run the posthoc on main group
acc.posthoc <- TukeyHSD(acc.anova,'maingroup',conf.level = 0.95) 
kable(tidy(acc.posthoc), digits=3) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
term
</th>
<th style="text-align:left;">
comparison
</th>
<th style="text-align:right;">
estimate
</th>
<th style="text-align:right;">
conf.low
</th>
<th style="text-align:right;">
conf.high
</th>
<th style="text-align:right;">
adj.p.value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
DeafEarlyASL-NativeDeaf
</td>
<td style="text-align:right;">
-0.041
</td>
<td style="text-align:right;">
-0.114
</td>
<td style="text-align:right;">
0.032
</td>
<td style="text-align:right;">
0.537
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
DeafLateASL-NativeDeaf
</td>
<td style="text-align:right;">
-0.017
</td>
<td style="text-align:right;">
-0.104
</td>
<td style="text-align:right;">
0.071
</td>
<td style="text-align:right;">
0.985
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
HearingLateASL-NativeDeaf
</td>
<td style="text-align:right;">
-0.030
</td>
<td style="text-align:right;">
-0.096
</td>
<td style="text-align:right;">
0.036
</td>
<td style="text-align:right;">
0.712
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
HearingNoviceASL-NativeDeaf
</td>
<td style="text-align:right;">
-0.095
</td>
<td style="text-align:right;">
-0.162
</td>
<td style="text-align:right;">
-0.028
</td>
<td style="text-align:right;">
0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
DeafLateASL-DeafEarlyASL
</td>
<td style="text-align:right;">
0.024
</td>
<td style="text-align:right;">
-0.067
</td>
<td style="text-align:right;">
0.116
</td>
<td style="text-align:right;">
0.950
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
HearingLateASL-DeafEarlyASL
</td>
<td style="text-align:right;">
0.011
</td>
<td style="text-align:right;">
-0.060
</td>
<td style="text-align:right;">
0.082
</td>
<td style="text-align:right;">
0.994
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
HearingNoviceASL-DeafEarlyASL
</td>
<td style="text-align:right;">
-0.054
</td>
<td style="text-align:right;">
-0.126
</td>
<td style="text-align:right;">
0.018
</td>
<td style="text-align:right;">
0.242
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
HearingLateASL-DeafLateASL
</td>
<td style="text-align:right;">
-0.014
</td>
<td style="text-align:right;">
-0.100
</td>
<td style="text-align:right;">
0.072
</td>
<td style="text-align:right;">
0.992
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
HearingNoviceASL-DeafLateASL
</td>
<td style="text-align:right;">
-0.078
</td>
<td style="text-align:right;">
-0.165
</td>
<td style="text-align:right;">
0.009
</td>
<td style="text-align:right;">
0.100
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroup
</td>
<td style="text-align:left;">
HearingNoviceASL-HearingLateASL
</td>
<td style="text-align:right;">
-0.065
</td>
<td style="text-align:right;">
-0.130
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.051
</td>
</tr>
</tbody>
</table>
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
-0.010
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL
</td>
<td style="text-align:right;">
0.019
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
-0.061
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.071
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
In summary, ANOVA tells us there are main effects of group and direction, no interactions. All of which is a good thing. But I'm curious if there's any item-level effects we should be watching out for. Because there are 4 different stories. Let's plot those out.

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

    ## Warning: Removed 3 rows containing missing values (geom_errorbar).

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

Boy! Seems King Midas had a strong reversal effect, while Red Riding Hood had a weak reversal effect. Maybe we should put those in as random effects variables in a mixed model, along with participants too. With mixed models, you define predictor variables (what we're interested in; aka, fixed effects) and grouping (what we're not interested in, aka, random effects). This is overkill for simple accuracy data but this will help set us up for eye tracking analysis and **importantly reviewers may ask us about item-level effects given we have just 4 stories.**

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
    ## REML criterion at convergence: -263.9
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.09133 -0.59555  0.04032  0.59360  2.30454 
    ## 
    ## Random effects:
    ##  Groups   Name              Variance Std.Dev. Corr
    ##  id       (Intercept)       0.002124 0.04609      
    ##           directionreversed 0.004103 0.06405  0.10
    ##  story    (Intercept)       0.001209 0.03477      
    ##  Residual                   0.007603 0.08719      
    ## Number of obs: 184, groups:  id, 47; story, 4
    ## 
    ## Fixed effects:
    ##                                              Estimate Std. Error        df
    ## (Intercept)                                  0.848275   0.029336 15.850000
    ## maingroupDeafEarlyASL                       -0.009193   0.036101 40.230000
    ## maingroupDeafLateASL                         0.013256   0.042936 42.850000
    ## maingroupHearingLateASL                      0.017338   0.032445 40.320000
    ## maingroupHearingNoviceASL                   -0.066577   0.033120 40.240000
    ## directionreversed                           -0.084140   0.033221 42.500000
    ## maingroupDeafEarlyASL:directionreversed     -0.062773   0.050839 41.490000
    ## maingroupDeafLateASL:directionreversed      -0.064972   0.060418 44.250000
    ## maingroupHearingLateASL:directionreversed   -0.094585   0.045618 41.690000
    ## maingroupHearingNoviceASL:directionreversed -0.056074   0.046556 41.600000
    ##                                             t value Pr(>|t|)    
    ## (Intercept)                                  28.916 3.77e-15 ***
    ## maingroupDeafEarlyASL                        -0.255   0.8003    
    ## maingroupDeafLateASL                          0.309   0.7590    
    ## maingroupHearingLateASL                       0.534   0.5960    
    ## maingroupHearingNoviceASL                    -2.010   0.0512 .  
    ## directionreversed                            -2.533   0.0151 *  
    ## maingroupDeafEarlyASL:directionreversed      -1.235   0.2239    
    ## maingroupDeafLateASL:directionreversed       -1.075   0.2880    
    ## maingroupHearingLateASL:directionreversed    -2.073   0.0443 *  
    ## maingroupHearingNoviceASL:directionreversed  -1.204   0.2352    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mnDEASL mnDLASL mnHLASL mnHNASL drctnr mDEASL: mDLASL:
    ## mngrpDfEASL -0.528                                                       
    ## mngrpDfLASL -0.443  0.362                                                
    ## mngrpHrLASL -0.586  0.475   0.400                                        
    ## mngrpHrNASL -0.574  0.466   0.392   0.520                                
    ## dirctnrvrsd -0.345  0.281   0.236   0.311   0.305                        
    ## mngrpDEASL:  0.226 -0.426  -0.156  -0.201  -0.199  -0.655                
    ## mngrpDLASL:  0.190 -0.157  -0.435  -0.170  -0.168  -0.551  0.363         
    ## mngrpHLASL:  0.250 -0.202  -0.171  -0.425  -0.222  -0.727  0.473   0.399 
    ## mngrpHNASL:  0.246 -0.199  -0.168  -0.223  -0.424  -0.713  0.465   0.392 
    ##             mHLASL:
    ## mngrpDfEASL        
    ## mngrpDfLASL        
    ## mngrpHrLASL        
    ## mngrpHrNASL        
    ## dirctnrvrsd        
    ## mngrpDEASL:        
    ## mngrpDLASL:        
    ## mngrpHLASL:        
    ## mngrpHNASL:  0.520

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
0.029
</td>
<td style="text-align:right;">
15.854
</td>
<td style="text-align:right;">
28.916
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
-0.009
</td>
<td style="text-align:right;">
0.036
</td>
<td style="text-align:right;">
40.235
</td>
<td style="text-align:right;">
-0.255
</td>
<td style="text-align:right;">
0.800
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL
</td>
<td style="text-align:right;">
0.013
</td>
<td style="text-align:right;">
0.043
</td>
<td style="text-align:right;">
42.850
</td>
<td style="text-align:right;">
0.309
</td>
<td style="text-align:right;">
0.759
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
0.032
</td>
<td style="text-align:right;">
40.324
</td>
<td style="text-align:right;">
0.534
</td>
<td style="text-align:right;">
0.596
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
0.033
</td>
<td style="text-align:right;">
40.245
</td>
<td style="text-align:right;">
-2.010
</td>
<td style="text-align:right;">
0.051
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
42.496
</td>
<td style="text-align:right;">
-2.533
</td>
<td style="text-align:right;">
0.015
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafEarlyASL:directionreversed
</td>
<td style="text-align:right;">
-0.063
</td>
<td style="text-align:right;">
0.051
</td>
<td style="text-align:right;">
41.489
</td>
<td style="text-align:right;">
-1.235
</td>
<td style="text-align:right;">
0.224
</td>
</tr>
<tr>
<td style="text-align:left;">
maingroupDeafLateASL:directionreversed
</td>
<td style="text-align:right;">
-0.065
</td>
<td style="text-align:right;">
0.060
</td>
<td style="text-align:right;">
44.248
</td>
<td style="text-align:right;">
-1.075
</td>
<td style="text-align:right;">
0.288
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
0.046
</td>
<td style="text-align:right;">
41.686
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
maingroupHearingNoviceASL:directionreversed
</td>
<td style="text-align:right;">
-0.056
</td>
<td style="text-align:right;">
0.047
</td>
<td style="text-align:right;">
41.601
</td>
<td style="text-align:right;">
-1.204
</td>
<td style="text-align:right;">
0.235
</td>
</tr>
</tbody>
</table>
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
0.7461
</td>
<td style="text-align:right;">
0.0142
</td>
<td style="text-align:right;">
52.5216
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
0.0412
</td>
<td style="text-align:right;">
0.0199
</td>
<td style="text-align:right;">
2.0726
</td>
<td style="text-align:right;">
0.0396
</td>
</tr>
</tbody>
</table>
Yikes. Group has an significant effect (p = 0.0396). On average they perform 4% better than Group 1. What's going on here...let's chart it.

``` r
ggplot(data, aes(x=videogroup, y=acc, fill=videogroup)) + 
#  geom_point(position="jitter") +
  geom_violin() +
  geom_jitter(width=.3)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

So Group 1 has a lot more bad test results compared to Group 2. But maybe that's a good reason to be using mixed models, and we can account for that by allowing subjects and items (and item order, by definition...I think) to vary randomly.

AOA Modeling
============

Below are models for AOA...I need to look at forward/reversed together and reversed only.

``` r
ggplot(filter(data,direction=="reversed"),aes(x=aoasl,y=acc)) +
  geom_point() +
  geom_smooth(method="lm")
```

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](02lexicalrecall_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png)

``` r
aoa.model <- lm(acc ~ aoasl, data=filter(data,direction=="reversed"))
summary(aoa.model)
```

    ## 
    ## Call:
    ## lm(formula = acc ~ aoasl, data = filter(data, direction == "reversed"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.31394 -0.09919  0.01370  0.10081  0.22659 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.749191   0.023559  31.801  < 2e-16 ***
    ## aoasl       -0.004736   0.001770  -2.676  0.00886 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.128 on 90 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.07369,    Adjusted R-squared:  0.0634 
    ## F-statistic: 7.159 on 1 and 90 DF,  p-value: 0.008859

<!-- acc.lm <- lmer(data=data, acc ~ maingroup*direction + (direction|id) + (1|story)) -->
<!-- summary(acc.lm) -->
