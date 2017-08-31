Study 1 Adult Data Analysis
================
Adam Stone, PhD
August 31, 2017

-   [Importing and Reshaping Data](#importing-and-reshaping-data)
-   [Participant Demographics](#participant-demographics)
-   [Accuracy Data Analysis](#accuracy-data-analysis)
-   [Eye Data](#eye-data)

Importing and Reshaping Data
----------------------------

Here we're going to import the data, remove dropped participants, and reshape the data so story and direction are grouping variables (and the dataset will be more tall than wide).

``` r
# Import packages we'll need.
library(tidyverse)
library(stringr)
library(lme4)
library(prettydoc)
library(broom)
library(knitr)
# Import the data
data <- read_csv('finaladultdata.csv', col_types=
                   cols(
                     id = col_integer(),
                     participant = col_character(),
                     hearing = col_character(),
                     videogroup = col_character(),
                     aoagroup = col_character(),
                     languagegroup = col_character(),
                     maingroup = col_character(),
                     selfrate = col_double(),
                     age = col_double(),
                     signyrs = col_double(),
                     aoasl = col_integer(),
                     acc.fw1 = col_double(),
                     acc.rv2 = col_double(),
                     acc.fw3 = col_double(),
                     acc.rv4 = col_double(),
                     forehead.fw1 = col_double(),
                     forehead.fw3 = col_double(),
                     forehead.rv2 = col_double(),
                     forehead.rv4 = col_double(),
                     eyes.fw1 = col_double(),
                     eyes.fw3 = col_double(),
                     eyes.rv2 = col_double(),
                     eyes.rv4 = col_double(),
                     mouth.fw1 = col_double(),
                     mouth.fw3 = col_double(),
                     mouth.rv2 = col_double(),
                     mouth.rv4 = col_double(),
                     chin.fw1 = col_double(),
                     chin.fw3 = col_double(),
                     chin.rv2 = col_double(),
                     chin.rv4 = col_double(),
                     upperchest.fw1 = col_double(),
                     upperchest.fw3 = col_double(),
                     upperchest.rv2 = col_double(),
                     upperchest.rv4 = col_double(),
                     midchest.fw1 = col_double(),
                     midchest.fw3 = col_double(),
                     midchest.rv2 = col_double(),
                     midchest.rv4 = col_double(),
                     lowerchest.fw1 = col_double(),
                     lowerchest.fw3 = col_double(),
                     lowerchest.rv2 = col_double(),
                     lowerchest.rv4 = col_double(),
                     belly.fw1 = col_double(),
                     belly.fw3 = col_double(),
                     belly.rv2 = col_double(),
                     belly.rv4 = col_double(),
                     left.fw1 = col_double(),
                     left.fw3 = col_double(),
                     left.rv2 = col_double(),
                     left.rv4 = col_double(),
                     right.fw1 = col_double(),
                     right.fw3 = col_double(),
                     right.rv2 = col_double(),
                     right.rv4 = col_double()
                     )
)
data
```

    ## # A tibble: 52 x 55
    ##       id               participant hearing videogroup aoagroup
    ##    <int>                     <chr>   <chr>      <chr>    <chr>
    ##  1     1                   Jessika    Deaf    Group 1    Early
    ##  2     2                     Derek    Deaf    Group 1    Early
    ##  3     3              Vanessa_Deaf    Deaf    Group 2    Early
    ##  4     4                      Josh    Deaf    Group 2    Early
    ##  5     5                  Lynnette    Deaf    Group 1    Early
    ##  6     6 Laura P (missing stories)    Deaf    Group 1    Early
    ##  7     7                   Rebecca    Deaf    Group 1    Early
    ##  8     8                     Cathy    Deaf    Group 2    Early
    ##  9     9                   Crystal    Deaf    Group 2    Early
    ## 10    10                Chrissy G.    Deaf    Group 1    Early
    ## # ... with 42 more rows, and 50 more variables: languagegroup <chr>,
    ## #   maingroup <chr>, selfrate <dbl>, age <dbl>, signyrs <dbl>,
    ## #   aoasl <int>, acc.fw1 <dbl>, acc.rv2 <dbl>, acc.fw3 <dbl>,
    ## #   acc.rv4 <dbl>, forehead.fw1 <dbl>, forehead.fw3 <dbl>,
    ## #   forehead.rv2 <dbl>, forehead.rv4 <dbl>, eyes.fw1 <dbl>,
    ## #   eyes.fw3 <dbl>, eyes.rv2 <dbl>, eyes.rv4 <dbl>, mouth.fw1 <dbl>,
    ## #   mouth.fw3 <dbl>, mouth.rv2 <dbl>, mouth.rv4 <dbl>, chin.fw1 <dbl>,
    ## #   chin.fw3 <dbl>, chin.rv2 <dbl>, chin.rv4 <dbl>, upperchest.fw1 <dbl>,
    ## #   upperchest.fw3 <dbl>, upperchest.rv2 <dbl>, upperchest.rv4 <dbl>,
    ## #   midchest.fw1 <dbl>, midchest.fw3 <dbl>, midchest.rv2 <dbl>,
    ## #   midchest.rv4 <dbl>, lowerchest.fw1 <dbl>, lowerchest.fw3 <dbl>,
    ## #   lowerchest.rv2 <dbl>, lowerchest.rv4 <dbl>, belly.fw1 <dbl>,
    ## #   belly.fw3 <dbl>, belly.rv2 <dbl>, belly.rv4 <dbl>, left.fw1 <dbl>,
    ## #   left.fw3 <dbl>, left.rv2 <dbl>, left.rv4 <dbl>, right.fw1 <dbl>,
    ## #   right.fw3 <dbl>, right.rv2 <dbl>, right.rv4 <dbl>

Remove dropped participants (they have no main group name). These are who we dropped:

``` r
dropped <- filter(data, is.na(maingroup)==TRUE)
data <- filter(data, is.na(maingroup)==FALSE)
dropped
```

    ## # A tibble: 3 x 55
    ##      id       participant hearing videogroup aoagroup languagegroup
    ##   <int>             <chr>   <chr>      <chr>    <chr>         <chr>
    ## 1    27             Megan Hearing    Group 2     Late      EarlyASL
    ## 2    40 DustinHearingCODA Hearing    Group 2   Native        Native
    ## 3    41         DanFisher Hearing    Group 1   Native        Native
    ## # ... with 49 more variables: maingroup <chr>, selfrate <dbl>, age <dbl>,
    ## #   signyrs <dbl>, aoasl <int>, acc.fw1 <dbl>, acc.rv2 <dbl>,
    ## #   acc.fw3 <dbl>, acc.rv4 <dbl>, forehead.fw1 <dbl>, forehead.fw3 <dbl>,
    ## #   forehead.rv2 <dbl>, forehead.rv4 <dbl>, eyes.fw1 <dbl>,
    ## #   eyes.fw3 <dbl>, eyes.rv2 <dbl>, eyes.rv4 <dbl>, mouth.fw1 <dbl>,
    ## #   mouth.fw3 <dbl>, mouth.rv2 <dbl>, mouth.rv4 <dbl>, chin.fw1 <dbl>,
    ## #   chin.fw3 <dbl>, chin.rv2 <dbl>, chin.rv4 <dbl>, upperchest.fw1 <dbl>,
    ## #   upperchest.fw3 <dbl>, upperchest.rv2 <dbl>, upperchest.rv4 <dbl>,
    ## #   midchest.fw1 <dbl>, midchest.fw3 <dbl>, midchest.rv2 <dbl>,
    ## #   midchest.rv4 <dbl>, lowerchest.fw1 <dbl>, lowerchest.fw3 <dbl>,
    ## #   lowerchest.rv2 <dbl>, lowerchest.rv4 <dbl>, belly.fw1 <dbl>,
    ## #   belly.fw3 <dbl>, belly.rv2 <dbl>, belly.rv4 <dbl>, left.fw1 <dbl>,
    ## #   left.fw3 <dbl>, left.rv2 <dbl>, left.rv4 <dbl>, right.fw1 <dbl>,
    ## #   right.fw3 <dbl>, right.rv2 <dbl>, right.rv4 <dbl>

Now we'll reshape the data. Based on Rain's UNM talk, this is what Group 1 & 2 saw:

| No. | Group1              | Group2              |
|:----|:--------------------|:--------------------|
| 1   | Red Riding Hood Fwd | Goldilocks Fwd      |
| 2   | King Midas Rev      | Cinderella Rev      |
| 3   | Cinderella Fwd      | King Mias Fwd       |
| 4   | Goldilocks Rev      | Red Riding Hood Rev |

Let's add that information to our data, too.

``` r
# I tried writing a function to do this using column names as arguments. 
# But after hours of googling I couldn't figure it out! So just copying/pasting code here.
data.acc <- data %>%
  select(id,acc.fw1:acc.rv4) %>%
  gather(video,acc,acc.fw1:acc.rv4, factor_key=TRUE) %>%
  mutate(video = str_sub(video,-3,-1))
data.forehead <- data %>%
  select(id,forehead.fw1:forehead.rv4) %>%
  gather(video,forehead,forehead.fw1:forehead.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.eyes <- data %>%
  select(id,eyes.fw1:eyes.rv4) %>%
  gather(video,eyes,eyes.fw1:eyes.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.mouth <- data %>%
  select(id,mouth.fw1:mouth.rv4) %>%
  gather(video,mouth,mouth.fw1:mouth.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.chin <- data %>%
  select(id,chin.fw1:chin.rv4) %>%
  gather(video,chin,chin.fw1:chin.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.upperchest <- data %>%
  select(id,upperchest.fw1:upperchest.rv4) %>%
  gather(video,upperchest,upperchest.fw1:upperchest.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.midchest <- data %>%
  select(id,midchest.fw1:midchest.rv4) %>%
  gather(video,midchest,midchest.fw1:midchest.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.lowerchest <- data %>%
  select(id,lowerchest.fw1:lowerchest.rv4) %>%
  gather(video,lowerchest,lowerchest.fw1:lowerchest.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.belly <- data %>%
  select(id,belly.fw1:belly.rv4) %>%
  gather(video,belly,belly.fw1:belly.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.left <- data %>%
  select(id,left.fw1:left.rv4) %>%
  gather(video,left,left.fw1:left.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.right <- data %>%
  select(id,right.fw1:right.rv4) %>%
  gather(video,right,right.fw1:right.rv4) %>%
  mutate(video = str_sub(video,-3,-1))
data.header <- data[1:11]

# Join them all back together. inner_join is smart, it'll join by id AND video.
newdata <- data.header %>%
  inner_join(data.acc,by="id") %>%
  inner_join(data.forehead) %>%
  inner_join(data.eyes) %>%
  inner_join(data.mouth) %>%
  inner_join(data.chin) %>%
  inner_join(data.upperchest) %>%
  inner_join(data.midchest) %>%
  inner_join(data.lowerchest) %>%
  inner_join(data.belly) %>%
  inner_join(data.left) %>%
  inner_join(data.right)

# Let's change the names
oldata <- data
data <- newdata

# Add story and direction variables and split data into videogroups
data$story <- NA
data$direction <- NA
group1 <- filter(data,videogroup=="Group 1")
group2 <- filter(data,videogroup=="Group 2")

# Now define levels for story and direction based on that table above
group1 <- mutate(group1,story = ifelse(video == "fw1","RedRidingHood",
                                       ifelse(video == "rv2","KingMidas",
                                              ifelse(video =="fw3","Cinderella","Goldilocks"))))
group1 <- mutate(group1,direction = ifelse(video == "fw1","forward",
                                       ifelse(video == "rv2","reversed",
                                              ifelse(video == "fw3","forward","reversed"))))
group2 <- mutate(group2,story = ifelse(video == "fw1","Goldilocks",
                                       ifelse(video == "rv2","Cinderella",
                                              ifelse(video=="fw3","KingMidas","RedRidingHood"))))
group2 <- mutate(group2,direction = ifelse(video == "fw1","forward",
                                           ifelse(video == "rv2","reversed",
                                                  ifelse(video == "fw3","forward","reversed"))))

# Join groups back together and view
data <- rbind(group1,group2)
data <- arrange(data,id,video)

# Convert some columns to factors
data <- data %>%
  mutate(hearing = as.factor(hearing)) %>%
  mutate(videogroup = as.factor(videogroup)) %>%
  mutate(aoagroup = as.factor(aoagroup)) %>%
  mutate(languagegroup = as.factor(languagegroup)) %>%
  mutate(maingroup = as.factor(maingroup)) %>%
  mutate(video = as.factor(video)) %>%
  mutate(story = as.factor(story)) %>%
  mutate(direction = as.factor(direction)) %>%
  select(id,participant,hearing,videogroup,aoagroup,languagegroup,maingroup,video,story,
         direction,age,selfrate,signyrs,aoasl,acc,forehead,eyes,mouth,chin,upperchest,
         midchest,lowerchest,belly,left,right)
data
```

    ## # A tibble: 196 x 25
    ##       id  participant hearing videogroup aoagroup languagegroup
    ##    <int>        <chr>  <fctr>     <fctr>   <fctr>        <fctr>
    ##  1     1      Jessika    Deaf    Group 1    Early      EarlyASL
    ##  2     1      Jessika    Deaf    Group 1    Early      EarlyASL
    ##  3     1      Jessika    Deaf    Group 1    Early      EarlyASL
    ##  4     1      Jessika    Deaf    Group 1    Early      EarlyASL
    ##  5     2        Derek    Deaf    Group 1    Early      EarlyASL
    ##  6     2        Derek    Deaf    Group 1    Early      EarlyASL
    ##  7     2        Derek    Deaf    Group 1    Early      EarlyASL
    ##  8     2        Derek    Deaf    Group 1    Early      EarlyASL
    ##  9     3 Vanessa_Deaf    Deaf    Group 2    Early      EarlyASL
    ## 10     3 Vanessa_Deaf    Deaf    Group 2    Early      EarlyASL
    ## # ... with 186 more rows, and 19 more variables: maingroup <fctr>,
    ## #   video <fctr>, story <fctr>, direction <fctr>, age <dbl>,
    ## #   selfrate <dbl>, signyrs <dbl>, aoasl <int>, acc <dbl>, forehead <dbl>,
    ## #   eyes <dbl>, mouth <dbl>, chin <dbl>, upperchest <dbl>, midchest <dbl>,
    ## #   lowerchest <dbl>, belly <dbl>, left <dbl>, right <dbl>

Participant Demographics
------------------------

Now we can easily get group means. Let's see...

``` r
groupmeans <- data %>%
  group_by(maingroup) %>%
  summarize(n = n()/4,
            age = mean(age),
            selfrate = mean(selfrate),
            signyrs = mean(signyrs),
            aoasl = mean(aoasl))
groupmeans
```

    ## # A tibble: 5 x 6
    ##          maingroup     n      age selfrate   signyrs    aoasl
    ##             <fctr> <dbl>    <dbl>    <dbl>     <dbl>    <dbl>
    ## 1     DeafEarlyASL     8 35.37500 5.000000 29.750000  5.62500
    ## 2      DeafLateASL     6 37.16667 5.000000 23.666667 12.83333
    ## 3   HearingLateASL    12 28.91667 4.625000 11.750000 17.25000
    ## 4 HearingNoviceASL    11 20.25000 3.045455  2.440909 17.63636
    ## 5       NativeDeaf    12 32.83333 5.000000 32.416667  0.25000

Accuracy Data Analysis
----------------------

And accuracy boxplots and error bar charts for forward vs. backward stories.

``` r
# Summarizing means and SDs
accdata <- data %>%
  group_by(maingroup,direction) %>%
  summarize(acc.mean = mean(acc, na.rm=TRUE),
            acc.sd = sd(acc, na.rm=TRUE))

#Boxplot
ggplot(data,aes(maingroup,acc,fill=direction)) + 
  geom_boxplot() +
  scale_y_continuous(limits=c(0,1))
```

    ## Warning: Removed 4 rows containing non-finite values (stat_boxplot).

![](datanotebook_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

``` r
# Error bar chart
ggplot(accdata,aes(maingroup,acc.mean,color=direction)) + 
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=acc.mean-acc.sd,ymax=acc.mean+acc.sd),width=0.1,position=position_dodge(0.5)) +
  scale_y_continuous(limits=c(0,1))
```

![](datanotebook_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

Let's test for statistical significance. A simple ANOVA tell us there is a main effect of group and direction, but no interactions. Tukey's HSD posthoc tells us HearingNoviceASL and DeafLateASL are significantly different, as well as HearingNoviceASL and HearingLateASL, and importantly, NativeDeaf is different from HearingNoviceASL!

``` r
# Let's set Native Deaf as the reference level to compare all other
data$maingroup <- relevel(data$maingroup, ref="NativeDeaf")
# Run the ANOVA
acc.anova <- aov(data=data,acc ~ maingroup*direction)
summary(acc.anova)
```

    ##                      Df Sum Sq Mean Sq F value  Pr(>F)    
    ## maingroup             4 0.2255  0.0564   4.540 0.00161 ** 
    ## direction             1 0.9511  0.9511  76.602 1.4e-15 ***
    ## maingroup:direction   4 0.0562  0.0141   1.132 0.34280    
    ## Residuals           182 2.2597  0.0124                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 4 observations deleted due to missingness

``` r
# Run the posthoc on main group
TukeyHSD(acc.anova,'maingroup',conf.level = 0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = acc ~ maingroup * direction, data = data)
    ## 
    ## $maingroup
    ##                                        diff         lwr           upr
    ## DeafEarlyASL-NativeDeaf         -0.04133152 -0.11200470  2.934166e-02
    ## DeafLateASL-NativeDeaf          -0.01528834 -0.09487228  6.429560e-02
    ## HearingLateASL-NativeDeaf       -0.03070652 -0.09405346  3.264042e-02
    ## HearingNoviceASL-NativeDeaf     -0.09536561 -0.16010629 -3.062493e-02
    ## DeafLateASL-DeafEarlyASL         0.02604318 -0.05898674  1.110731e-01
    ## HearingLateASL-DeafEarlyASL      0.01062500 -0.05944153  8.069153e-02
    ## HearingNoviceASL-DeafEarlyASL   -0.05403409 -0.12536319  1.729500e-02
    ## HearingLateASL-DeafLateASL      -0.01541818 -0.09446389  6.362753e-02
    ## HearingNoviceASL-DeafLateASL    -0.08007727 -0.16024426  8.971011e-05
    ## HearingNoviceASL-HearingLateASL -0.06465909 -0.12873698 -5.812003e-04
    ##                                     p adj
    ## DeafEarlyASL-NativeDeaf         0.4920600
    ## DeafLateASL-NativeDeaf          0.9842462
    ## HearingLateASL-NativeDeaf       0.6693767
    ## HearingNoviceASL-NativeDeaf     0.0006921
    ## DeafLateASL-DeafEarlyASL        0.9164747
    ## HearingLateASL-DeafEarlyASL     0.9935733
    ## HearingNoviceASL-DeafEarlyASL   0.2300599
    ## HearingLateASL-DeafLateASL      0.9833233
    ## HearingNoviceASL-DeafLateASL    0.0504109
    ## HearingNoviceASL-HearingLateASL 0.0467731

Group coefficients are here. Remember our reference level ("control") is Native Deaf. Their forward accuracy is 85% with a reversal effect of -9%. ASL, Forward. Mean accuracy for that is 84%, and its reversal effect is -14%. All the other values are to be added to these coefficients. Hearing Novice ASL's forward accuracy was 78%, with a reversal effect of -6%!

``` r
#Coefficients
acc.anova$coefficients
```

    ##                                 (Intercept) 
    ##                                  0.85000000 
    ##                       maingroupDeafEarlyASL 
    ##                                 -0.01250000 
    ##                        maingroupDeafLateASL 
    ##                                  0.02272727 
    ##                     maingroupHearingLateASL 
    ##                                  0.01666667 
    ##                   maingroupHearingNoviceASL 
    ##                                 -0.06772727 
    ##                           directionreversed 
    ##                                 -0.08608696 
    ##     maingroupDeafEarlyASL:directionreversed 
    ##                                 -0.05766304 
    ##      maingroupDeafLateASL:directionreversed 
    ##                                 -0.07603123 
    ##   maingroupHearingLateASL:directionreversed 
    ##                                 -0.09474638 
    ## maingroupHearingNoviceASL:directionreversed 
    ##                                 -0.05527668

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
  facet_wrap("story")
```

    ## Warning: Removed 2 rows containing missing values (geom_errorbar).

![](datanotebook_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

Boy! Seems King Midas had a strong reversal effect, while Red Riding Hood had a weak reversal effect. Maybe we should put those in as random effects variables in a mixed model, along with participants too. With mixed models, you define predictor variables (what we're interested in; aka, fixed effects) and grouping (what we're not interested in, aka, random effects). This is overkill for simple accuracy data but this will help set us up for eye tracking analysis and besides reviewers may ask us about item-level effects given we have just 4 stories.

So here, we have fixed effects of group and direction, and random effects of story and id, with varying slopes for direction.

The output tells us now that Hearing Novice ASL is significantly different from Deaf Native ASL, that we still have a main effect of direction, with a significant interaction of direction and Hearing Late ASL. We probably don't need all this in the paper, though, the ANOVA will do and accuracy is not our main point.

``` r
acc.lm <- lmer(data=data, acc ~ maingroup*direction + (direction|id) + (1|story))
summary(acc.lm)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: acc ~ maingroup * direction + (direction | id) + (1 | story)
    ##    Data: data
    ## 
    ## REML criterion at convergence: -282
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.17717 -0.61072  0.06207  0.62080  2.33306 
    ## 
    ## Random effects:
    ##  Groups   Name              Variance Std.Dev. Corr
    ##  id       (Intercept)       0.001917 0.04378      
    ##           directionreversed 0.003765 0.06136  0.14
    ##  story    (Intercept)       0.001340 0.03661      
    ##  Residual                   0.007464 0.08640      
    ## Number of obs: 192, groups:  id, 49; story, 4
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error t value
    ## (Intercept)                                  0.84986    0.02868  29.634
    ## maingroupDeafEarlyASL                       -0.01058    0.03463  -0.305
    ## maingroupDeafLateASL                         0.01717    0.03865   0.444
    ## maingroupHearingLateASL                      0.01561    0.03094   0.505
    ## maingroupHearingNoviceASL                   -0.06824    0.03163  -2.158
    ## directionreversed                           -0.08647    0.03116  -2.775
    ## maingroupDeafEarlyASL:directionreversed     -0.06084    0.04898  -1.242
    ## maingroupDeafLateASL:directionreversed      -0.06936    0.05452  -1.272
    ## maingroupHearingLateASL:directionreversed   -0.09198    0.04363  -2.108
    ## maingroupHearingNoviceASL:directionreversed -0.05359    0.04460  -1.202
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mnDEASL mnDLASL mnHLASL mnHNASL drctnr mDEASL: mDLASL:
    ## mngrpDfEASL -0.492                                                       
    ## mngrpDfLASL -0.439  0.364                                                
    ## mngrpHrLASL -0.548  0.453   0.407                                        
    ## mngrpHrNASL -0.537  0.445   0.398   0.497                                
    ## dirctnrvrsd -0.330  0.276   0.244   0.303   0.298                        
    ## mngrpDEASL:  0.212 -0.427  -0.156  -0.192  -0.190  -0.640                
    ## mngrpDLASL:  0.188 -0.156  -0.432  -0.174  -0.170  -0.571  0.364         
    ## mngrpHLASL:  0.234 -0.193  -0.174  -0.424  -0.212  -0.711  0.452   0.407 
    ## mngrpHNASL:  0.229 -0.191  -0.170  -0.212  -0.423  -0.697  0.444   0.398 
    ##             mHLASL:
    ## mngrpDfEASL        
    ## mngrpDfLASL        
    ## mngrpHrLASL        
    ## mngrpHrNASL        
    ## dirctnrvrsd        
    ## mngrpDEASL:        
    ## mngrpDLASL:        
    ## mngrpHLASL:        
    ## mngrpHNASL:  0.497

Eye Data
--------

To come!
