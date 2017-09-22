Eye Gaze Analysis (study1adults)
================
Adam Stone, PhD
09-22-2017

-   [Re-Initializing](#re-initializing)
-   [AOIs](#aois)
-   [Data Cleaning](#data-cleaning)
    -   [Percentage Data and Viz](#percentage-data-and-viz)
-   [Big Five AOIs](#big-five-aois)
    -   [Group ANOVA](#group-anova)
    -   [Age of ASL & Hearing Status ANCOVA](#age-of-asl-hearing-status-ancova)
-   [3 Face AOIs Only](#face-aois-only)
    -   [Visualizations](#visualizations)
    -   [Group ANOVA](#group-anova-1)
    -   [Age of ASL & Hearing Status ANCOVA](#age-of-asl-hearing-status-ancova-1)
-   [Left/Right Analysis](#leftright-analysis)
-   [Assorted/older stuff pushed to the bottom](#assortedolder-stuff-pushed-to-the-bottom)

Re-Initializing
===============

This assumes you've already done [01dataimportclean](01dataimportclean.nb.html) and so there'll be a nice new .csv file to re-import here. Also we gotta import all the libraries again. This shouldn't depend on anything we did for 02 Lexical Recall Analysis.

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
library(viridis)
```

    ## Loading required package: viridisLite

``` r
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
                        right = col_double(),
                        total = col_double()
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

AOIs
====

These are our current AOIs.

1.  Forehead (above eyes)
2.  Eyes
3.  Mouth
4.  Chin (below chin)
5.  Upper Chest
6.  Middle Chest
7.  Lower Chest
8.  Belly
9.  Left
10. Right

It's possible to do a secondary analysis combining some of these AOIs (in particular, maybe 5-6 and 7-8 can be combined into Torso Upper Half and Torso Lower Half). Anyway, the face AOIs are important, and the division of them into 4 areas is theoretically motivated and also previously seen in the literature.

*Why 4 AOIs on Face?* Emmorey et al. (2008) did this same setup. We generally know people fixate on the face across all conditions and langauge experiences, but **where** on the face is important for us to know. So these 4 AOIs. *Write one sentence here about predictions for language experience on face-looking.*

*Why 4 AOIs for Torso?* Past papers tend to just classify the body as “body” with no further breakdown, or two-part breakdown. In our study, we have higher resolution to break this down into four AOI’s, defined as upper chest, middle chest, lower chest, and belly. We know that hands start and end at the belly and the hands spend the majority of the time in these four areas in front of the torso (and the hands spend very little time overlapping the face). If an observer (child or adult) glances at the hands (or if the hands have any “gravity” upon gaze behavior), then gaze samples will fall within these four torso areas. Although we expect that all observers do spend the most amount of time on the face, we also predict that the number of gazes towards the hands (by way of “torso”) might be impacted by language experience. *(make a footnote here: As a future project, we will analyze the data using dynamic “hand” AOIs, for each hand, in which we document, on each frame, where the left and right hands are in viewing space.)*

*Why 2 AOIs for left vs right?* People have talked how sign language impacts left vs right visual field asymmetries, as related to hemispheric laterality for language processing, so it is worth checking this. If we do find an asymmetry, we will then just touch upon this literature in the discussion, but also acknowledge that it could be the signer’s hand dominance that drives a lateral asymmetry too, not just a hemispheric asymmetry. Meaning, if the signer is right handed, her dominant hand might have some “gravity” in the viewer’s left visual field. (And we can check this with the future analysis of dynamic hand AOIs.)

Data Cleaning
=============

This is my process of documenting how I'm weeding through data and making sure all's good. \[Change 18 Sep 2017\] I deleted half of these...just cutting out the fat.

Let's sum up *all* AOIs across each story for each participant...back to the big dataset, and we'll do histograms

``` r
# data2 <- data %>%
#   group_by(id,story) %>%
#   mutate(total = sum(forehead,eyes,mouth,chin,upperchest,
#                      midchest,lowerchest,belly,left,right,na.rm=TRUE))
ggplot(data,aes(x=total)) +
  geom_histogram(binwidth=1) +
  facet_wrap("story") +
  xlab("secs") +
  ggtitle("Sum of ALL AOIs for each participant for each story")
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png) The tall bars are near the end of the story, right? So we see two issues: 1. Some barely watched the story at all. (Those are the ones with bars at or near zero). We should remove those. We need a rule for it. 1. A few people's AOI data has total seconds higher than the video itself! (Those are the ones with very short bars to the right of the very tall bars.) Those should be investigated, something went wrong in the data.

I'll highlight those rows that's for \#2 and send to Rain to look at.

But maybe a good way for diagnosing problem trials is to look at each AOI for each story, instead of sums of AOIs. Any outliers can be easily seen in the histograms. Let's reshape the data again and generate histograms.

``` r
data.reshape <- data %>% gather(aoi,looking,forehead:total)
ggplot(data.reshape,aes(x=looking)) +
  geom_histogram(binwidth=1) +
  facet_grid(aoi ~ story) +
  xlab("secs") +
  ggtitle("Looking times of each AOI for each participant for each story")
```

    ## Warning: Removed 813 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Need to mark this up.

Cool. Now I want to know how many people have really low looking times for each story. We can do this easily. Here's a table for how many have looking time sums that are less than 25% of the story, or less than 50% of the story.

``` r
# Split into 4 datasets (1 each story)
data.cinderella <- filter(data,story == "Cinderella")
data.goldilocks <- filter(data,story == "Goldilocks")
data.kingmidas <- filter(data,story == "KingMidas")
data.redridinghood <- filter(data,story== "RedRidingHood")

# FALSE = less than quarter or total story length
data.cinderella$quarter <- data.cinderella$total >= 5.5
data.cinderella$half <- data.cinderella$total >= 11
data.goldilocks$quarter <- data.goldilocks$total >=  5.25
data.goldilocks$half <- data.goldilocks$total >= 10.5
data.kingmidas$quarter <- data.kingmidas$total >= 9.25
data.kingmidas$half <- data.kingmidas$total >= 18.5
data.redridinghood$quarter <- data.redridinghood$total >= 4.5
data.redridinghood$half <- data.redridinghood$total >= 9

# Put it back together
data <- bind_rows(data.cinderella,data.goldilocks,data.kingmidas,data.redridinghood)

# Calculations
lowlooking <- data %>%
  group_by(story) %>%
  dplyr::summarize(lessthan25 = sum(!quarter),
            lessthan50 = sum(!half),
            total = sum(!quarter,quarter))
lowlooking
```

    ## # A tibble: 4 x 4
    ##           story lessthan25 lessthan50 total
    ##          <fctr>      <int>      <int> <int>
    ## 1    Cinderella          4          7    46
    ## 2    Goldilocks          2          3    46
    ## 3     KingMidas          3          5    46
    ## 4 RedRidingHood          3          4    46

``` r
lowlookingid <- filter(data,quarter==FALSE) %>% 
  ungroup() %>%
  select(id,participant,hearing,videogroup,story,direction,total) %>%
  arrange(participant)
write.csv(lowlookingid, file="lessthan25.csv")
select(lowlookingid,-participant)
```

    ## # A tibble: 12 x 6
    ##       id hearing videogroup         story direction total
    ##    <int>  <fctr>     <fctr>        <fctr>    <fctr> <dbl>
    ##  1    10    Deaf    Group 1    Cinderella  reversed  4.56
    ##  2    10    Deaf    Group 1     KingMidas   forward  2.67
    ##  3    32 Hearing    Group 2    Goldilocks  reversed  4.08
    ##  4    31 Hearing    Group 2    Cinderella   forward  4.73
    ##  5     6    Deaf    Group 1    Cinderella  reversed  4.43
    ##  6     6    Deaf    Group 1 RedRidingHood  reversed  3.80
    ##  7     5    Deaf    Group 1     KingMidas   forward  2.91
    ##  8     5    Deaf    Group 1 RedRidingHood  reversed  1.96
    ##  9    25    Deaf    Group 2    Goldilocks  reversed  4.62
    ## 10     7    Deaf    Group 1    Cinderella  reversed  0.81
    ## 11     7    Deaf    Group 1     KingMidas   forward  6.84
    ## 12    17    Deaf    Group 1 RedRidingHood  reversed  0.52

``` r
#lowlookingid
```

So I will filter out the data with &lt;25% looking time. Maybe the threshold should be higher, but we'll revisit that later.

``` r
originalrows <- nrow(data)
data <- filter(data,quarter==TRUE)
difference <- originalrows - nrow(data)
```

So 12 stories were dropped from the previous total of 184 stories for a new total of 172 stories.

Percentage Data and Viz
-----------------------

We need to work with percentages, because of participants' idiosyntractic eye behavior. Some blink a lot, some don't, so automatically the maximum number of eye gaze data points each participant is able to contribute is different. For that reason we work with percent of total data points on a per-participant basis. That's also why we took out stories with &lt;25% looking data.

I also want to have Face and Chest AOIs defined, such that: - Face = eyes + mouth + chin - Chest = upperchest + midchest + lowerchest - And a Face-Chest ratio, defined by (F-Chest)/(F+Chest)

And I will save this as `cleanpercentdata.csv.` Now here's the boxplots for each AOI.

``` r
# data2 uses percentage data!
data2 <- data %>%
  mutate_at(vars(forehead:right), funs(./total)) %>%
  select(-total, -quarter, -half) %>%
  group_by(participant,story) %>%
  mutate(face = sum(eyes,mouth,chin,na.rm=TRUE),
         chest = sum(upperchest,midchest,lowerchest,na.rm=TRUE),
         facechest = (face-chest)/(face+chest)) %>%
  gather(aoi,percent,forehead:facechest)

write.csv(data2,"cleanpercentdata.csv",row.names=FALSE)

ggplot(data2, aes(x=aoi,y=percent)) +
  geom_boxplot() 
```

    ## Warning: Removed 754 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

But we should also look at histograms of percentage data. Those should have more normal-like distributions for the high-hitting AOIs, unlike the actual looking data in seconds which has a upper limit.

``` r
ggplot(data2,aes(x=percent)) +
  geom_histogram() +
  facet_grid(aoi ~ story) +
  xlab("percent") +
  ggtitle("Looking percentages of each AOI for each participant for each story - or index for FaceChest")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 754 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

### Problem people are Sara G for the reasons below:

-   **Sara G. What is happening...her story sums are still over the limit (one story has 41 s, and no story is more than 37 s). There isn't another Sara G recording hiding somewhere or maybe her scenes are wrong? I'm attaching what I have for her, and from the histograms it's her forehead in Cinderella and eyes in King Midas that seem way out of the ordinary (I highlighted both cells in yellow). **

Big Five AOIs
=============

> You’ll have to decide how to put the AOIs in an ANOVA. All of them together is too many. And you cannot put ALL the AOIs in. If they all sum to 100% (which they currently do), then the observations are not independent. Also, you can’t put AOIs that have near-zero values in with AOIs that have super high values, you’ll get whopping significance that is too obvious to reveal anything meaningful.

Based on the boxplot there are five AOIs that got hit the most: forehead, eyes, mouth, chin, and upperchest. **But this is really important...I think there is one or two outliers in forehead. And maybe it's better to get rid of forehead and upper chest for the big ANOVAs to keep things simple. Neither of them touch 50%...but then again they are not "non-significant." I am going ahead with all 5 for now.**

*Important reference levels* - AOI reference level is eyes - MainGroup reference level is DeafNative

Creating the `data.big5` thing here.

``` r
# Make Big5 df with reference levels
data.big5 <- filter(data2,aoi == "forehead" | aoi == "eyes" 
                    | aoi == "mouth" | aoi == "chin" | aoi == "upperchest") %>%
  mutate(aoi = as.factor(aoi))
data.big5$aoi <- factor(data.big5$aoi, levels=c("upperchest","chin","mouth","eyes","forehead"))
data.big5$aoi <- relevel(data.big5$aoi, ref="eyes")
data.big5$maingroup <- relevel(data.big5$maingroup, ref="DeafNative")
```

Group ANOVA
-----------

Because we're doing ANOVAs, that means we need subject-level data, not trial-level data. Let's bump the `data.big5` up one level.

``` r
data.big5.item <- data.big5 # save item-level data for later

# Pull out and save subject info
data.big5.subjectinfo <- data.big5 %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()
```

    ## Adding missing grouping variables: `story`

``` r
# Now collapse data.big5 to subject-level 
data.big5 <- data.big5 %>%
  group_by(participant,direction,aoi) %>%
  summarize(percent = mean(percent,na.rm=TRUE))
data.big5[data.big5=="NaN"] <- NA


# Join subject info with data.big5 that's now subject-level
data.big5 <- left_join(data.big5,data.big5.subjectinfo, by=c("participant","direction"))
```

Now we can do the ANOVAs.

> First, check the most important stats: Subject Groups, Video Condition, and some AOIs that are on the central body like: eyes, nose, mouth, neck, chest, below chest. You can combine some. If you see that no one ever looked at the forehead, you can dispense of that, and say that in the paper when you rationalize your AOIs used for stats. There might be a significant group main effect, OR a group main effect for reversed and not for forward, and furthermore, maybe only the late AoA groups show a reversal effect.

First let's do groups only (no continuous variables). First the viz, then the stats.

``` r
ggplot(data.big5) + 
  geom_boxplot(aes(x=maingroup,y=percent,color=direction)) +
  facet_wrap("aoi") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 120 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

``` r
data.big5.viz <- data.big5 %>%
  group_by(maingroup,direction,aoi) %>%
  summarize(mean = mean(percent,na.rm=TRUE), sd = sd(percent,na.rm=TRUE))
ggplot(data.big5.viz, aes(x=maingroup,y=mean,color=direction)) +
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.1,position=position_dodge(0.5)) +
  facet_wrap("aoi") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png)

Or a heat map!

``` r
data.big5.reduce <- data.big5 %>%
  group_by(maingroup,aoi,direction) %>%
  summarize(meanlooking = mean(percent, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(aoi = factor(aoi,levels=c("upperchest","chin","mouth","eyes","forehead")))
ggplot(data.big5.reduce, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=meanlooking),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "inferno") +
  facet_wrap("direction") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)

What if we faceted this heat map by group instead of direction:

``` r
ggplot(data.big5.reduce, aes(x = direction, y = aoi)) +
  geom_tile(aes(fill=meanlooking),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "inferno") +
  facet_wrap("maingroup") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)

The ANOVA below tells us there's a significant effect of AOI, and significant interactions of AOI x Direction and AOI x MainGroup.

``` r
group.anova <- aov(data=data.big5,percent ~ aoi * direction * maingroup)
anova(group.anova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                          Df Sum Sq Mean Sq  F value    Pr(>F)    
    ## aoi                       4 33.113  8.2782 230.8034 < 2.2e-16 ***
    ## direction                 1  0.001  0.0005   0.0146  0.903924    
    ## maingroup                 4  0.024  0.0060   0.1661  0.955567    
    ## aoi:direction             4  0.570  0.1424   3.9697  0.003417 ** 
    ## aoi:maingroup            16  2.395  0.1497   4.1732 9.431e-08 ***
    ## direction:maingroup       4  0.001  0.0003   0.0077  0.999884    
    ## aoi:direction:maingroup  16  0.579  0.0362   1.0081  0.445678    
    ## Residuals               690 24.748  0.0359                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Here's the posthoc (Tukey's HSD) for AOI, which tells us most AOIs are different from each other, except: 1. chin vs. eyes 1. forehead vs. upper chest

Forget doing posthocs for all the interactions, way too many.

``` r
group.anova.posthoc <- TukeyHSD(group.anova,'aoi',conf.level = 0.95) 
group.anova.posthoc
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = percent ~ aoi * direction * maingroup, data = data.big5)
    ## 
    ## $aoi
    ##                             diff         lwr          upr     p adj
    ## upperchest-eyes     -0.177434155 -0.23541758 -0.119450731 0.0000000
    ## chin-eyes           -0.048468448 -0.10473528  0.007798383 0.1288895
    ## mouth-eyes           0.390360091  0.33417438  0.446545804 0.0000000
    ## forehead-eyes       -0.168941662 -0.24022505 -0.097658272 0.0000000
    ## chin-upperchest      0.128965707  0.07122439  0.186707028 0.0000000
    ## mouth-upperchest     0.567794246  0.51013197  0.625456524 0.0000000
    ## forehead-upperchest  0.008492493 -0.06396043  0.080945413 0.9977127
    ## mouth-chin           0.438828539  0.38289271  0.494764369 0.0000000
    ## forehead-chin       -0.120473214 -0.19155981 -0.049386616 0.0000419
    ## forehead-mouth      -0.559301753 -0.63032416 -0.488279345 0.0000000

Age of ASL & Hearing Status ANCOVA
----------------------------------

MainGroup is actually two different variables combined together: deaf and hearing, and native/early/late learners. What if we separated those out and regressed AoASL as a continuous variable, and added deaf/hearing as a factor. Again, first the viz, then the stats.

``` r
# Draw it
ggplot(data.big5, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 120 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 120 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

Let's move to ANOVAs. This is technically an ANCOVA, and AoASL is the covariate. The output tells us there is a significant main effect of AOI, and significant interactions of AOI x Direction and AOI x Hearing. A marginally significant interaction of AOI x Hearing X AoASL. So really it's very similar to what we got with the group ANOVA.

``` r
continuous.anova <- aov(data=data.big5, percent ~ aoi * direction * hearing * aoasl)
anova(continuous.anova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                              Df Sum Sq Mean Sq  F value    Pr(>F)    
    ## aoi                           4 33.113  8.2782 231.0147 < 2.2e-16 ***
    ## direction                     1  0.001  0.0005   0.0146  0.903879    
    ## hearing                       1  0.012  0.0125   0.3479  0.555476    
    ## aoasl                         1  0.002  0.0024   0.0678  0.794577    
    ## aoi:direction                 4  0.569  0.1423   3.9716  0.003402 ** 
    ## aoi:hearing                   4  1.331  0.3328   9.2859 2.579e-07 ***
    ## direction:hearing             1  0.000  0.0002   0.0054  0.941335    
    ## aoi:aoasl                     4  0.277  0.0692   1.9322  0.103320    
    ## direction:aoasl               1  0.000  0.0004   0.0109  0.917012    
    ## hearing:aoasl                 1  0.006  0.0065   0.1806  0.670990    
    ## aoi:direction:hearing         4  0.146  0.0365   1.0180  0.397150    
    ## aoi:direction:aoasl           4  0.035  0.0088   0.2464  0.911855    
    ## aoi:hearing:aoasl             4  0.800  0.1999   5.5778  0.000201 ***
    ## direction:hearing:aoasl       1  0.001  0.0012   0.0341  0.853604    
    ## aoi:direction:hearing:aoasl   4  0.051  0.0128   0.3559  0.839971    
    ## Residuals                   700 25.084  0.0358                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Maybe it's a problem that all the hearing people have late AoASL while most deaf have early AoASL. And I don't like the forehead outliers throwing off some of those lines from the graph above so that needs to be looked at. What if we tried sign-years instead of AoASL.

``` r
ggplot(data.big5, aes(x=signyrs,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 120 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 120 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-1.png)

Look interesting and you can sort of compare the deaf/hearing lines better, although we should get rid of that person who's been signing for 60 years if we do a years-of-signing analysis. Here's the ANCOVA.

Let's move to ANOVAs. This is technically an ANCOVA, and AoASL is the covariate. The output tells us there is a significant main effect of AOI, and significant interactions of AOI x Direction and AOI x Hearing and AOI x SignYrs. Again a rather similar thing result.

``` r
continuous.anova.sy <- aov(data=data.big5, percent ~ aoi * direction * hearing * signyrs)
anova(continuous.anova.sy)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                                Df Sum Sq Mean Sq  F value    Pr(>F)    
    ## aoi                             4 33.113  8.2782 228.9540 < 2.2e-16 ***
    ## direction                       1  0.001  0.0005   0.0145  0.904307    
    ## hearing                         1  0.012  0.0125   0.3448  0.557244    
    ## signyrs                         1  0.000  0.0002   0.0042  0.948646    
    ## aoi:direction                   4  0.569  0.1422   3.9325  0.003641 ** 
    ## aoi:hearing                     4  1.332  0.3329   9.2068 2.974e-07 ***
    ## direction:hearing               1  0.000  0.0002   0.0047  0.945407    
    ## aoi:signyrs                     4  0.539  0.1347   3.7252  0.005213 ** 
    ## direction:signyrs               1  0.001  0.0006   0.0171  0.895867    
    ## hearing:signyrs                 1  0.006  0.0062   0.1714  0.679008    
    ## aoi:direction:hearing           4  0.151  0.0378   1.0448  0.383147    
    ## aoi:direction:signyrs           4  0.058  0.0145   0.4001  0.808645    
    ## aoi:hearing:signyrs             4  0.227  0.0566   1.5663  0.181489    
    ## direction:hearing:signyrs       1  0.000  0.0002   0.0047  0.945130    
    ## aoi:direction:hearing:signyrs   4  0.112  0.0280   0.7747  0.541827    
    ## Residuals                     700 25.310  0.0362                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

3 Face AOIs Only
================

I'm giving this a shot. Because the boxplot way up there tells us there wasn't much looking to the forehead or the upper chest, compared with eyes, mouth, and chin. **So I'm going to take out forehead and upper chest AOIs and see what we get.**

``` r
# Make face3 df with reference levels
data.face3 <- filter(data2, aoi == "eyes" | aoi == "mouth" | aoi == "chin") %>%
  mutate(aoi = as.factor(aoi))
data.face3$aoi <- factor(data.face3$aoi, levels=c("eyes","mouth","chin"))
#data.face3$aoi <- relevel(data.face3$aoi, ref="eyes")
data.face3$maingroup <- relevel(data.face3$maingroup, ref="DeafNative")

data.face3.item <- data.face3 # save item-level data for later

# Pull out and save subject info
data.face3.subjectinfo <- data.face3 %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()
```

    ## Adding missing grouping variables: `story`

``` r
# Now collapse data.big5 to subject-level 
data.face3 <- data.face3 %>%
  group_by(participant,direction,aoi) %>%
  summarize(percent = mean(percent,na.rm=TRUE))
data.face3[data.face3=="NaN"] <- NA

# Join subject info with data.big5 that's now subject-level
data.face3 <- left_join(data.face3,data.face3.subjectinfo, by=c("participant","direction"))
```

Visualizations
--------------

Let's start with the visualizations.

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=direction)) +
  facet_grid(direction ~ aoi) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-22-1.png)

But we have less levels so maybe another way of looking at the boxplots:

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_grid(direction~.) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-23-1.png)

Or another way even

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=aoi,y=percent,fill=direction)) +
  facet_grid(direction~maingroup) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-24-1.png)

Group ANOVA
-----------

Anyway. Onto the ANOVAs. It gives us virtually identical results as the `big5` ANOVAs.

``` r
group.anova.face3 <- aov(data=data.face3,percent ~ aoi * direction * maingroup)
anova(group.anova.face3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                          Df  Sum Sq Mean Sq  F value    Pr(>F)    
    ## aoi                       2 19.8328  9.9164 197.5779 < 2.2e-16 ***
    ## direction                 1  0.0136  0.0136   0.2714  0.602654    
    ## maingroup                 4  0.0759  0.0190   0.3778  0.824502    
    ## aoi:direction             2  0.5329  0.2665   5.3090  0.005241 ** 
    ## aoi:maingroup             8  2.2393  0.2799   5.5770 9.055e-07 ***
    ## direction:maingroup       4  0.0034  0.0009   0.0170  0.999431    
    ## aoi:direction:maingroup   8  0.5022  0.0628   1.2508  0.267492    
    ## Residuals               481 24.1413  0.0502                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The posthocs are easier to interpret, too. No difference between chin and eyes. It's the mouth vs. chin and the mouth vs. eyes contrasts that drive the differences.

``` r
group.anova.face3.posthoc <- TukeyHSD(group.anova.face3,'aoi',conf.level = 0.95) 
group.anova.face3.posthoc
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = percent ~ aoi * direction * maingroup, data = data.face3)
    ## 
    ## $aoi
    ##                   diff        lwr          upr     p adj
    ## mouth-eyes  0.39036009  0.3332281  0.447492087 0.0000000
    ## chin-eyes  -0.04846845 -0.1056829  0.008746032 0.1153484
    ## chin-mouth -0.43882854 -0.4957064 -0.381950635 0.0000000

And posthoc for aoi:direction.

``` r
group.anova.face3.posthoc2 <- TukeyHSD(group.anova.face3,'aoi:direction',conf.level = 0.95) 
group.anova.face3.posthoc2
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = percent ~ aoi * direction * maingroup, data = data.face3)
    ## 
    ## $`aoi:direction`
    ##                                     diff         lwr          upr
    ## mouth:forward-eyes:forward    0.45094902  0.35431347  0.547584562
    ## chin:forward-eyes:forward    -0.05909055 -0.15572610  0.037544992
    ## eyes:reversed-eyes:forward    0.02449052 -0.07453145  0.123512482
    ## mouth:reversed-eyes:forward   0.35076562  0.25298642  0.448544819
    ## chin:reversed-eyes:forward   -0.01317962 -0.11125972  0.084900484
    ## chin:forward-mouth:forward   -0.51003957 -0.60667511 -0.413404024
    ## eyes:reversed-mouth:forward  -0.42645850 -0.52548047 -0.327436534
    ## mouth:reversed-mouth:forward -0.10018340 -0.19796260 -0.002404198
    ## chin:reversed-mouth:forward  -0.46412863 -0.56220874 -0.366048532
    ## eyes:reversed-chin:forward    0.08358107 -0.01544090  0.182603035
    ## mouth:reversed-chin:forward   0.40985617  0.31207697  0.507635372
    ## chin:reversed-chin:forward    0.04591093 -0.05216917  0.143991037
    ## mouth:reversed-eyes:reversed  0.32627510  0.22613673  0.426413475
    ## chin:reversed-eyes:reversed  -0.03767013 -0.13810234  0.062762072
    ## chin:reversed-mouth:reversed -0.36394524 -0.46315234 -0.264738130
    ##                                  p adj
    ## mouth:forward-eyes:forward   0.0000000
    ## chin:forward-eyes:forward    0.4996212
    ## eyes:reversed-eyes:forward   0.9809970
    ## mouth:reversed-eyes:forward  0.0000000
    ## chin:reversed-eyes:forward   0.9989083
    ## chin:forward-mouth:forward   0.0000000
    ## eyes:reversed-mouth:forward  0.0000000
    ## mouth:reversed-mouth:forward 0.0409974
    ## chin:reversed-mouth:forward  0.0000000
    ## eyes:reversed-chin:forward   0.1530280
    ## mouth:reversed-chin:forward  0.0000000
    ## chin:reversed-chin:forward   0.7628974
    ## mouth:reversed-eyes:reversed 0.0000000
    ## chin:reversed-eyes:reversed  0.8918864
    ## chin:reversed-mouth:reversed 0.0000000

And posthoc for aoi:maingroup.

``` r
group.anova.face3.posthoc3 <- TukeyHSD(group.anova.face3,'aoi:maingroup',conf.level = 0.95) 
group.anova.face3.posthoc3
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = percent ~ aoi * direction * maingroup, data = data.face3)
    ## 
    ## $`aoi:maingroup`
    ##                                                       diff           lwr
    ## mouth:DeafNative-eyes:DeafNative              0.4978246142  0.3301470854
    ## chin:DeafNative-eyes:DeafNative               0.0788369943 -0.0888405345
    ## eyes:DeafEarlyASL-eyes:DeafNative             0.0099172820 -0.1950321227
    ## mouth:DeafEarlyASL-eyes:DeafNative            0.5490944958  0.3472488440
    ## chin:DeafEarlyASL-eyes:DeafNative             0.0201824288 -0.1847669759
    ## eyes:DeafLateASL-eyes:DeafNative              0.0801971290 -0.1216485228
    ## mouth:DeafLateASL-eyes:DeafNative             0.5851102237  0.3832645720
    ## chin:DeafLateASL-eyes:DeafNative             -0.1023749205 -0.3042205722
    ## eyes:HearingLateASL-eyes:DeafNative           0.1698345428  0.0000989281
    ## mouth:HearingLateASL-eyes:DeafNative          0.3670868377  0.1994093089
    ## chin:HearingLateASL-eyes:DeafNative           0.0005423013 -0.1671352275
    ## eyes:HearingNoviceASL-eyes:DeafNative         0.0564747067 -0.1093096653
    ## mouth:HearingNoviceASL-eyes:DeafNative        0.3950431173  0.2292587453
    ## chin:HearingNoviceASL-eyes:DeafNative         0.0376854349 -0.1280989371
    ## chin:DeafNative-mouth:DeafNative             -0.4189876198 -0.5856519821
    ## eyes:DeafEarlyASL-mouth:DeafNative           -0.4879073322 -0.6920286551
    ## mouth:DeafEarlyASL-mouth:DeafNative           0.0512698816 -0.1497349022
    ## chin:DeafEarlyASL-mouth:DeafNative           -0.4776421854 -0.6817635083
    ## eyes:DeafLateASL-mouth:DeafNative            -0.4176274851 -0.6186322690
    ## mouth:DeafLateASL-mouth:DeafNative            0.0872856096 -0.1137191743
    ## chin:DeafLateASL-mouth:DeafNative            -0.6001995346 -0.8012043184
    ## eyes:HearingLateASL-mouth:DeafNative         -0.3279900713 -0.4967248777
    ## mouth:HearingLateASL-mouth:DeafNative        -0.1307377764 -0.2974021387
    ## chin:HearingLateASL-mouth:DeafNative         -0.4972823129 -0.6639466751
    ## eyes:HearingNoviceASL-mouth:DeafNative       -0.4413499075 -0.6061094716
    ## mouth:HearingNoviceASL-mouth:DeafNative      -0.1027814969 -0.2675410610
    ## chin:HearingNoviceASL-mouth:DeafNative       -0.4601391793 -0.6248987434
    ## eyes:DeafEarlyASL-chin:DeafNative            -0.0689197123 -0.2730410352
    ## mouth:DeafEarlyASL-chin:DeafNative            0.4702575014  0.2692527176
    ## chin:DeafEarlyASL-chin:DeafNative            -0.0586545656 -0.2627758885
    ## eyes:DeafLateASL-chin:DeafNative              0.0013601347 -0.1996446491
    ## mouth:DeafLateASL-chin:DeafNative             0.5062732294  0.3052684456
    ## chin:DeafLateASL-chin:DeafNative             -0.1812119148 -0.3822166986
    ## eyes:HearingLateASL-chin:DeafNative           0.0909975485 -0.0777372579
    ## mouth:HearingLateASL-chin:DeafNative          0.2882498434  0.1215854812
    ## chin:HearingLateASL-chin:DeafNative          -0.0782946930 -0.2449590553
    ## eyes:HearingNoviceASL-chin:DeafNative        -0.0223622876 -0.1871218518
    ## mouth:HearingNoviceASL-chin:DeafNative        0.3162061229  0.1514465588
    ## chin:HearingNoviceASL-chin:DeafNative        -0.0411515594 -0.2059111235
    ## mouth:DeafEarlyASL-eyes:DeafEarlyASL          0.5391772138  0.3061720036
    ## chin:DeafEarlyASL-eyes:DeafEarlyASL           0.0102651468 -0.2254338547
    ## eyes:DeafLateASL-eyes:DeafEarlyASL            0.0702798470 -0.1627253631
    ## mouth:DeafLateASL-eyes:DeafEarlyASL           0.5751929417  0.3421877316
    ## chin:DeafLateASL-eyes:DeafEarlyASL           -0.1122922025 -0.3452974126
    ## eyes:HearingLateASL-eyes:DeafEarlyASL         0.1599172608 -0.0458980440
    ## mouth:HearingLateASL-eyes:DeafEarlyASL        0.3571695557  0.1530482328
    ## chin:HearingLateASL-eyes:DeafEarlyASL        -0.0093749807 -0.2134963036
    ## eyes:HearingNoviceASL-eyes:DeafEarlyASL       0.0465574247 -0.1560116223
    ## mouth:HearingNoviceASL-eyes:DeafEarlyASL      0.3851258353  0.1825567883
    ## chin:HearingNoviceASL-eyes:DeafEarlyASL       0.0277681529 -0.1748008941
    ## chin:DeafEarlyASL-mouth:DeafEarlyASL         -0.5289120670 -0.7619172771
    ## eyes:DeafLateASL-mouth:DeafEarlyASL          -0.4688973667 -0.6991772760
    ## mouth:DeafLateASL-mouth:DeafEarlyASL          0.0360157280 -0.1942641813
    ## chin:DeafLateASL-mouth:DeafEarlyASL          -0.6514694162 -0.8817493255
    ## eyes:HearingLateASL-mouth:DeafEarlyASL       -0.3792599529 -0.5819847624
    ## mouth:HearingLateASL-mouth:DeafEarlyASL      -0.1820076580 -0.3830124419
    ## chin:HearingLateASL-mouth:DeafEarlyASL       -0.5485521945 -0.7495569783
    ## eyes:HearingNoviceASL-mouth:DeafEarlyASL     -0.4926197891 -0.6920480405
    ## mouth:HearingNoviceASL-mouth:DeafEarlyASL    -0.1540513785 -0.3534796299
    ## chin:HearingNoviceASL-mouth:DeafEarlyASL     -0.5114090609 -0.7108373122
    ## eyes:DeafLateASL-chin:DeafEarlyASL            0.0600147003 -0.1729905099
    ## mouth:DeafLateASL-chin:DeafEarlyASL           0.5649277950  0.3319225848
    ## chin:DeafLateASL-chin:DeafEarlyASL           -0.1225573492 -0.3555625593
    ## eyes:HearingLateASL-chin:DeafEarlyASL         0.1496521141 -0.0561631908
    ## mouth:HearingLateASL-chin:DeafEarlyASL        0.3469044090  0.1427830861
    ## chin:HearingLateASL-chin:DeafEarlyASL        -0.0196401275 -0.2237614504
    ## eyes:HearingNoviceASL-chin:DeafEarlyASL       0.0362922779 -0.1662767691
    ## mouth:HearingNoviceASL-chin:DeafEarlyASL      0.3748606885  0.1722916415
    ## chin:HearingNoviceASL-chin:DeafEarlyASL       0.0175030061 -0.1850660408
    ## mouth:DeafLateASL-eyes:DeafLateASL            0.5049130947  0.2746331855
    ## chin:DeafLateASL-eyes:DeafLateASL            -0.1825720495 -0.4128519587
    ## eyes:HearingLateASL-eyes:DeafLateASL          0.0896374138 -0.1130873956
    ## mouth:HearingLateASL-eyes:DeafLateASL         0.2868897087  0.0858849249
    ## chin:HearingLateASL-eyes:DeafLateASL         -0.0796548277 -0.2806596116
    ## eyes:HearingNoviceASL-eyes:DeafLateASL       -0.0237224223 -0.2231506737
    ## mouth:HearingNoviceASL-eyes:DeafLateASL       0.3148459882  0.1154177369
    ## chin:HearingNoviceASL-eyes:DeafLateASL       -0.0425116941 -0.2419399455
    ## chin:DeafLateASL-mouth:DeafLateASL           -0.6874851442 -0.9177650534
    ## eyes:HearingLateASL-mouth:DeafLateASL        -0.4152756809 -0.6180004903
    ## mouth:HearingLateASL-mouth:DeafLateASL       -0.2180233860 -0.4190281698
    ## chin:HearingLateASL-mouth:DeafLateASL        -0.5845679224 -0.7855727063
    ## eyes:HearingNoviceASL-mouth:DeafLateASL      -0.5286355170 -0.7280637684
    ## mouth:HearingNoviceASL-mouth:DeafLateASL     -0.1900671065 -0.3894953578
    ## chin:HearingNoviceASL-mouth:DeafLateASL      -0.5474247888 -0.7468530402
    ## eyes:HearingLateASL-chin:DeafLateASL          0.2722094633  0.0694846538
    ## mouth:HearingLateASL-chin:DeafLateASL         0.4694617582  0.2684569744
    ## chin:HearingLateASL-chin:DeafLateASL          0.1029172217 -0.0980875621
    ## eyes:HearingNoviceASL-chin:DeafLateASL        0.1588496271 -0.0405786242
    ## mouth:HearingNoviceASL-chin:DeafLateASL       0.4974180377  0.2979897863
    ## chin:HearingNoviceASL-chin:DeafLateASL        0.1400603554 -0.0593678960
    ## mouth:HearingLateASL-eyes:HearingLateASL      0.1972522949  0.0285174885
    ## chin:HearingLateASL-eyes:HearingLateASL      -0.1692922415 -0.3380270479
    ## eyes:HearingNoviceASL-eyes:HearingLateASL    -0.1133598361 -0.2802134822
    ## mouth:HearingNoviceASL-eyes:HearingLateASL    0.2252085744  0.0583549284
    ## chin:HearingNoviceASL-eyes:HearingLateASL    -0.1321491079 -0.2990027540
    ## chin:HearingLateASL-mouth:HearingLateASL     -0.3665445365 -0.5332088987
    ## eyes:HearingNoviceASL-mouth:HearingLateASL   -0.3106121311 -0.4753716952
    ## mouth:HearingNoviceASL-mouth:HearingLateASL   0.0279562795 -0.1368032846
    ## chin:HearingNoviceASL-mouth:HearingLateASL   -0.3294014028 -0.4941609670
    ## eyes:HearingNoviceASL-chin:HearingLateASL     0.0559324054 -0.1088271587
    ## mouth:HearingNoviceASL-chin:HearingLateASL    0.3945008160  0.2297412519
    ## chin:HearingNoviceASL-chin:HearingLateASL     0.0371431336 -0.1276164305
    ## mouth:HearingNoviceASL-eyes:HearingNoviceASL  0.3385684106  0.1757359252
    ## chin:HearingNoviceASL-eyes:HearingNoviceASL  -0.0187892718 -0.1816217572
    ## chin:HearingNoviceASL-mouth:HearingNoviceASL -0.3573576824 -0.5201901677
    ##                                                        upr     p adj
    ## mouth:DeafNative-eyes:DeafNative              0.6655021430 0.0000000
    ## chin:DeafNative-eyes:DeafNative               0.2465145231 0.9575806
    ## eyes:DeafEarlyASL-eyes:DeafNative             0.2148666867 1.0000000
    ## mouth:DeafEarlyASL-eyes:DeafNative            0.7509401475 0.0000000
    ## chin:DeafEarlyASL-eyes:DeafNative             0.2251318335 1.0000000
    ## eyes:DeafLateASL-eyes:DeafNative              0.2820427808 0.9905700
    ## mouth:DeafLateASL-eyes:DeafNative             0.7869558755 0.0000000
    ## chin:DeafLateASL-eyes:DeafNative              0.0994707313 0.9232661
    ## eyes:HearingLateASL-eyes:DeafNative           0.3395701576 0.0496931
    ## mouth:HearingLateASL-eyes:DeafNative          0.5347643665 0.0000000
    ## chin:HearingLateASL-eyes:DeafNative           0.1682198301 1.0000000
    ## eyes:HearingNoviceASL-eyes:DeafNative         0.2222590787 0.9980268
    ## mouth:HearingNoviceASL-eyes:DeafNative        0.5608274892 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafNative         0.2034698069 0.9999823
    ## chin:DeafNative-mouth:DeafNative             -0.2523232576 0.0000000
    ## eyes:DeafEarlyASL-mouth:DeafNative           -0.2837860092 0.0000000
    ## mouth:DeafEarlyASL-mouth:DeafNative           0.2522746654 0.9999274
    ## chin:DeafEarlyASL-mouth:DeafNative           -0.2735208625 0.0000000
    ## eyes:DeafLateASL-mouth:DeafNative            -0.2166227013 0.0000000
    ## mouth:DeafLateASL-mouth:DeafNative            0.2882903934 0.9785046
    ## chin:DeafLateASL-mouth:DeafNative            -0.3991947508 0.0000000
    ## eyes:HearingLateASL-mouth:DeafNative         -0.1592552650 0.0000000
    ## mouth:HearingLateASL-mouth:DeafNative         0.0359265858 0.3240151
    ## chin:HearingLateASL-mouth:DeafNative         -0.3306179506 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafNative       -0.2765903434 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafNative       0.0619780672 0.7163978
    ## chin:HearingNoviceASL-mouth:DeafNative       -0.2953796151 0.0000000
    ## eyes:DeafEarlyASL-chin:DeafNative             0.1352016106 0.9982055
    ## mouth:DeafEarlyASL-chin:DeafNative            0.6712622853 0.0000000
    ## chin:DeafEarlyASL-chin:DeafNative             0.1454667574 0.9997033
    ## eyes:DeafLateASL-chin:DeafNative              0.2023649185 1.0000000
    ## mouth:DeafLateASL-chin:DeafNative             0.7072780132 0.0000000
    ## chin:DeafLateASL-chin:DeafNative              0.0197928690 0.1304048
    ## eyes:HearingLateASL-chin:DeafNative           0.2597323549 0.8812175
    ## mouth:HearingLateASL-chin:DeafNative          0.4549142057 0.0000007
    ## chin:HearingLateASL-chin:DeafNative           0.0883696692 0.9578721
    ## eyes:HearingNoviceASL-chin:DeafNative         0.1423972765 1.0000000
    ## mouth:HearingNoviceASL-chin:DeafNative        0.4809656871 0.0000000
    ## chin:HearingNoviceASL-chin:DeafNative         0.1236080047 0.9999437
    ## mouth:DeafEarlyASL-eyes:DeafEarlyASL          0.7721824239 0.0000000
    ## chin:DeafEarlyASL-eyes:DeafEarlyASL           0.2459641482 1.0000000
    ## eyes:DeafLateASL-eyes:DeafEarlyASL            0.3032850571 0.9994832
    ## mouth:DeafLateASL-eyes:DeafEarlyASL           0.8081981518 0.0000000
    ## chin:DeafLateASL-eyes:DeafEarlyASL            0.1207130077 0.9482212
    ## eyes:HearingLateASL-eyes:DeafEarlyASL         0.3657325657 0.3401901
    ## mouth:HearingLateASL-eyes:DeafEarlyASL        0.5612908786 0.0000005
    ## chin:HearingLateASL-eyes:DeafEarlyASL         0.1947463422 1.0000000
    ## eyes:HearingNoviceASL-eyes:DeafEarlyASL       0.2491264716 0.9999797
    ## mouth:HearingNoviceASL-eyes:DeafEarlyASL      0.5876948822 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafEarlyASL       0.2303371999 1.0000000
    ## chin:DeafEarlyASL-mouth:DeafEarlyASL         -0.2959068569 0.0000000
    ## eyes:DeafLateASL-mouth:DeafEarlyASL          -0.2386174575 0.0000000
    ## mouth:DeafLateASL-mouth:DeafEarlyASL          0.2662956372 0.9999999
    ## chin:DeafLateASL-mouth:DeafEarlyASL          -0.4211895070 0.0000000
    ## eyes:HearingLateASL-mouth:DeafEarlyASL       -0.1765351435 0.0000000
    ## mouth:HearingLateASL-mouth:DeafEarlyASL       0.0189971258 0.1258829
    ## chin:HearingLateASL-mouth:DeafEarlyASL       -0.3475474107 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafEarlyASL     -0.2931915377 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafEarlyASL     0.0453768729 0.3502292
    ## chin:HearingNoviceASL-mouth:DeafEarlyASL     -0.3119808095 0.0000000
    ## eyes:DeafLateASL-chin:DeafEarlyASL            0.2930199104 0.9999184
    ## mouth:DeafLateASL-chin:DeafEarlyASL           0.7979330051 0.0000000
    ## chin:DeafLateASL-chin:DeafEarlyASL            0.1104478609 0.9000981
    ## eyes:HearingLateASL-chin:DeafEarlyASL         0.3554674189 0.4577366
    ## mouth:HearingLateASL-chin:DeafEarlyASL        0.5510257319 0.0000013
    ## chin:HearingLateASL-chin:DeafEarlyASL         0.1844811954 1.0000000
    ## eyes:HearingNoviceASL-chin:DeafEarlyASL       0.2388613249 0.9999991
    ## mouth:HearingNoviceASL-chin:DeafEarlyASL      0.5774297355 0.0000001
    ## chin:HearingNoviceASL-chin:DeafEarlyASL       0.2200720531 1.0000000
    ## mouth:DeafLateASL-eyes:DeafLateASL            0.7351930039 0.0000000
    ## chin:DeafLateASL-eyes:DeafLateASL             0.0477078598 0.3062897
    ## eyes:HearingLateASL-eyes:DeafLateASL          0.2923622232 0.9747883
    ## mouth:HearingLateASL-eyes:DeafLateASL         0.4878944925 0.0001540
    ## chin:HearingLateASL-eyes:DeafLateASL          0.1213499561 0.9908041
    ## eyes:HearingNoviceASL-eyes:DeafLateASL        0.1757058290 1.0000000
    ## mouth:HearingNoviceASL-eyes:DeafLateASL       0.5142742396 0.0000118
    ## chin:HearingNoviceASL-eyes:DeafLateASL        0.1569165573 0.9999921
    ## chin:DeafLateASL-mouth:DeafLateASL           -0.4572052350 0.0000000
    ## eyes:HearingLateASL-mouth:DeafLateASL        -0.2125508715 0.0000000
    ## mouth:HearingLateASL-mouth:DeafLateASL       -0.0170186022 0.0193409
    ## chin:HearingLateASL-mouth:DeafLateASL        -0.3835631386 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafLateASL      -0.3292072657 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafLateASL      0.0093611449 0.0806080
    ## chin:HearingNoviceASL-mouth:DeafLateASL      -0.3479965374 0.0000000
    ## eyes:HearingLateASL-chin:DeafLateASL          0.4749342727 0.0005794
    ## mouth:HearingLateASL-chin:DeafLateASL         0.6704665420 0.0000000
    ## chin:HearingLateASL-chin:DeafLateASL          0.3039220056 0.9177090
    ## eyes:HearingNoviceASL-chin:DeafLateASL        0.3582778785 0.2986508
    ## mouth:HearingNoviceASL-chin:DeafLateASL       0.6968462891 0.0000000
    ## chin:HearingNoviceASL-chin:DeafLateASL        0.3394886067 0.5203187
    ## mouth:HearingLateASL-eyes:HearingLateASL      0.3659871013 0.0067648
    ## chin:HearingLateASL-eyes:HearingLateASL      -0.0005574352 0.0482817
    ## eyes:HearingNoviceASL-eyes:HearingLateASL     0.0534938099 0.5789391
    ## mouth:HearingNoviceASL-eyes:HearingLateASL    0.3920622205 0.0005210
    ## chin:HearingNoviceASL-eyes:HearingLateASL     0.0347045382 0.3079992
    ## chin:HearingLateASL-mouth:HearingLateASL     -0.1998801742 0.0000000
    ## eyes:HearingNoviceASL-mouth:HearingLateASL   -0.1458525669 0.0000000
    ## mouth:HearingNoviceASL-mouth:HearingLateASL   0.1927158437 0.9999996
    ## chin:HearingNoviceASL-mouth:HearingLateASL   -0.1646418387 0.0000000
    ## eyes:HearingNoviceASL-chin:HearingLateASL     0.2206919695 0.9980981
    ## mouth:HearingNoviceASL-chin:HearingLateASL    0.5592603801 0.0000000
    ## chin:HearingNoviceASL-chin:HearingLateASL     0.2019026978 0.9999840
    ## mouth:HearingNoviceASL-eyes:HearingNoviceASL  0.5014008960 0.0000000
    ## chin:HearingNoviceASL-eyes:HearingNoviceASL   0.1440432136 1.0000000
    ## chin:HearingNoviceASL-mouth:HearingNoviceASL -0.1945251970 0.0000000

Age of ASL & Hearing Status ANCOVA
----------------------------------

Now we're using AoASL as a covariate and putting in hearing as a factor. Let's visualize that...what we see here is that AoASL isn't having much of an effect, but being deaf or hearing does.

``` r
# Draw it
ggplot(data.face3, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 5 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-1.png) and the ANCOVA itself...which gives us almost identical results as the `big5` stats. So maybe it's easier overall to just drop all AOIs except eye, mouth, chin when trying to look for AoA, group effects, etc? We can present summary stats overall for all AOIs, then when it gets down to the dirty stats work, we keep it simple and show ... that whatever we found.

``` r
continuous.anova.face3 <- aov(data=data.face3, percent ~ aoi * direction * hearing * aoasl)
anova(continuous.anova.face3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                              Df  Sum Sq Mean Sq  F value    Pr(>F)    
    ## aoi                           2 19.8328  9.9164 197.9785 < 2.2e-16 ***
    ## direction                     1  0.0136  0.0136   0.2719 0.6022840    
    ## hearing                       1  0.0609  0.0609   1.2157 0.2707591    
    ## aoasl                         1  0.0003  0.0003   0.0051 0.9430351    
    ## aoi:direction                 2  0.5321  0.2661   5.3120 0.0052217 ** 
    ## aoi:hearing                   2  1.2499  0.6249  12.4768 5.196e-06 ***
    ## direction:hearing             1  0.0014  0.0014   0.0285 0.8660281    
    ## aoi:aoasl                     2  0.2554  0.1277   2.5493 0.0791819 .  
    ## direction:aoasl               1  0.0020  0.0020   0.0400 0.8416163    
    ## hearing:aoasl                 1  0.0137  0.0137   0.2732 0.6014152    
    ## aoi:direction:hearing         2  0.1360  0.0680   1.3571 0.2583683    
    ## aoi:direction:aoasl           2  0.0229  0.0114   0.2285 0.7958139    
    ## aoi:hearing:aoasl             2  0.7914  0.3957   7.9002 0.0004202 ***
    ## direction:hearing:aoasl       1  0.0000  0.0000   0.0008 0.9780817    
    ## aoi:direction:hearing:aoasl   2  0.0360  0.0180   0.3596 0.6981247    
    ## Residuals                   487 24.3930  0.0501                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We have to figure out what the interactions mean. That's where simple linear models can be helpful here. Let me try it here. Okay so the NEW cleaned data tells us there were noe ffects except AOI where mouth is looked at more than anything. I think the linear model is punishing us for the high collinearity between AoA and age, whereas ANCOVA doesn't (but why the difference, I don't know).

*Old text from before data was further cleaned: Okay, so the results are slightly different. ANOVAs in R default to Type I sums of squares, while regressions use more of a Type III sum of squares approach, I believe. But we can interpret the results here a bit more easily. Almost all the interactions have to do with being hearing vs. deaf, and there seems to be no effect of Age of ASL acquisition. So that's interesting.*

``` r
lm.face3 <- lm(data=data.face3, percent ~ aoi * direction * aoasl * hearing)
summary(lm.face3)
```

    ## 
    ## Call:
    ## lm(formula = percent ~ aoi * direction * aoasl * hearing, data = data.face3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.60681 -0.14413 -0.03767  0.12972  0.68225 
    ## 
    ## Coefficients:
    ##                                                   Estimate Std. Error
    ## (Intercept)                                      0.1258486  0.0441226
    ## aoimouth                                         0.5585142  0.0623988
    ## aoichin                                          0.0694468  0.0623988
    ## directionreversed                                0.0293253  0.0644201
    ## aoasl                                            0.0054129  0.0063075
    ## hearingHearing                                   0.5543713  0.2394616
    ## aoimouth:directionreversed                      -0.1176315  0.0905765
    ## aoichin:directionreversed                       -0.0005815  0.0907249
    ## aoimouth:aoasl                                  -0.0028463  0.0089202
    ## aoichin:aoasl                                   -0.0155953  0.0089202
    ## directionreversed:aoasl                         -0.0018257  0.0091060
    ## aoimouth:hearingHearing                         -0.8700700  0.3386499
    ## aoichin:hearingHearing                          -0.9620198  0.3386499
    ## directionreversed:hearingHearing                -0.1110421  0.3495215
    ## aoasl:hearingHearing                            -0.0305579  0.0146234
    ## aoimouth:directionreversed:aoasl                 0.0098416  0.0128212
    ## aoichin:directionreversed:aoasl                 -0.0019191  0.0128214
    ## aoimouth:directionreversed:hearingHearing        0.1738280  0.4868363
    ## aoichin:directionreversed:hearingHearing         0.1126658  0.4868639
    ## aoimouth:aoasl:hearingHearing                    0.0404265  0.0206806
    ## aoichin:aoasl:hearingHearing                     0.0594227  0.0206806
    ## directionreversed:aoasl:hearingHearing           0.0075538  0.0214212
    ## aoimouth:directionreversed:aoasl:hearingHearing -0.0221489  0.0298068
    ## aoichin:directionreversed:aoasl:hearingHearing  -0.0010066  0.0298069
    ##                                                 t value Pr(>|t|)    
    ## (Intercept)                                       2.852  0.00453 ** 
    ## aoimouth                                          8.951  < 2e-16 ***
    ## aoichin                                           1.113  0.26628    
    ## directionreversed                                 0.455  0.64915    
    ## aoasl                                             0.858  0.39122    
    ## hearingHearing                                    2.315  0.02102 *  
    ## aoimouth:directionreversed                       -1.299  0.19466    
    ## aoichin:directionreversed                        -0.006  0.99489    
    ## aoimouth:aoasl                                   -0.319  0.74980    
    ## aoichin:aoasl                                    -1.748  0.08104 .  
    ## directionreversed:aoasl                          -0.200  0.84118    
    ## aoimouth:hearingHearing                          -2.569  0.01049 *  
    ## aoichin:hearingHearing                           -2.841  0.00469 ** 
    ## directionreversed:hearingHearing                 -0.318  0.75085    
    ## aoasl:hearingHearing                             -2.090  0.03717 *  
    ## aoimouth:directionreversed:aoasl                  0.768  0.44310    
    ## aoichin:directionreversed:aoasl                  -0.150  0.88108    
    ## aoimouth:directionreversed:hearingHearing         0.357  0.72120    
    ## aoichin:directionreversed:hearingHearing          0.231  0.81709    
    ## aoimouth:aoasl:hearingHearing                     1.955  0.05118 .  
    ## aoichin:aoasl:hearingHearing                      2.873  0.00424 ** 
    ## directionreversed:aoasl:hearingHearing            0.353  0.72452    
    ## aoimouth:directionreversed:aoasl:hearingHearing  -0.743  0.45779    
    ## aoichin:directionreversed:aoasl:hearingHearing   -0.034  0.97307    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2238 on 487 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.4847, Adjusted R-squared:  0.4604 
    ## F-statistic: 19.92 on 23 and 487 DF,  p-value: < 2.2e-16

Left/Right Analysis
===================

> Then, how about a section on Percent Looking for AOI’s examines whether there are side biases by doing an ANOVA with the entire Left vs Right side as 2 AOI levels, with Subject Groups, and Forward and Reversed. If there are side biases, hopefully it is for the later AoA groups, then this means they are being drawn to the hands more than native signers. It’s possible that this is driven by where the signer puts her dominant hand or by a hemispheric bias.

> Maybe even the native signers have a little side bias too for other reasons, but I doubt it. This is kind of the hand-gravity idea, because there is nothing ever in the sides but arms and hands.

> If nothing is significant with side biases, you still write all this up in a section in the paper and in the Discussion revisit that saying there were no side biases found.

Let's go for it. Creating the `data.lr` thing here. And again because we're still doing ANOVAs and not hierarchical linear models we need to bump `data.lr` up one level.

``` r
# Make LR df with reference levels
data.lr <- filter(data2,aoi == "left" | aoi == "right") %>%
  mutate(aoi = as.factor(aoi))
data.lr$maingroup <- relevel(data.lr$maingroup, ref="DeafNative")

data.lr.item <- data.lr # save item-level data for later

# Pull out and save subject info
data.lr.subjectinfo <- data.lr %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()
```

    ## Adding missing grouping variables: `story`

``` r
# Now collapse data.big5 to subject-level 
data.lr <- data.lr %>%
  group_by(participant,direction,aoi) %>%
  summarize(percent = mean(percent,na.rm=TRUE))
data.lr[data.lr=="NaN"] <- NA

# Join subject info with data.big5 that's now subject-level
data.lr <- left_join(data.lr,data.lr.subjectinfo, by=c("participant","direction"))
```

Problem is, I can already tell this dataset is rather sparse. There are 213 empty cells out of 344. Soooo. Let's give this a shot anyway but probably not a good idea? The graph below, I changed the colors so they map on left/right AOI, and each facet is direction. So we can directly compare L/R biases.

``` r
ggplot(data.lr) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_wrap("direction") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 213 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-33-1.png)

Let's try the group ANOVA and the AoASL ANCOVAs. Group ANOVA first...nothing significant here.

``` r
group.lranova <- aov(data=data.lr,percent ~ aoi * direction * maingroup)
anova(group.lranova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                          Df   Sum Sq    Mean Sq F value   Pr(>F)   
    ## aoi                       1 0.002752 0.00275197  9.2870 0.002879 **
    ## direction                 1 0.000058 0.00005800  0.1957 0.659034   
    ## maingroup                 4 0.002458 0.00061452  2.0738 0.088935 . 
    ## aoi:direction             1 0.002553 0.00255277  8.6148 0.004047 **
    ## aoi:maingroup             4 0.001600 0.00039997  1.3498 0.256035   
    ## direction:maingroup       4 0.000647 0.00016182  0.5461 0.702229   
    ## aoi:direction:maingroup   3 0.000232 0.00007731  0.2609 0.853427   
    ## Residuals               112 0.033188 0.00029632                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

But the ANCOVA here shows some almost significant effects of hearing, and significant AOI:Hearing and AOASL:Hearing interactions.

``` r
continuous.lranova <- aov(data=data.lr,percent ~ aoi * direction * aoasl * hearing)
anova(continuous.lranova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                              Df   Sum Sq    Mean Sq F value   Pr(>F)   
    ## aoi                           1 0.002752 0.00275197  9.7384 0.002282 **
    ## direction                     1 0.000058 0.00005800  0.2053 0.651367   
    ## aoasl                         1 0.002189 0.00218926  7.7471 0.006291 **
    ## hearing                       1 0.000245 0.00024479  0.8662 0.353953   
    ## aoi:direction                 1 0.002710 0.00270951  9.5881 0.002461 **
    ## aoi:aoasl                     1 0.000054 0.00005368  0.1900 0.663768   
    ## direction:aoasl               1 0.000097 0.00009704  0.3434 0.559021   
    ## aoi:hearing                   1 0.000573 0.00057321  2.0284 0.157091   
    ## direction:hearing             1 0.000456 0.00045564  1.6124 0.206723   
    ## aoasl:hearing                 1 0.000154 0.00015370  0.5439 0.462328   
    ## aoi:direction:aoasl           1 0.000515 0.00051453  1.8208 0.179874   
    ## aoi:direction:hearing         1 0.000022 0.00002222  0.0786 0.779665   
    ## aoi:aoasl:hearing             1 0.000826 0.00082593  2.9227 0.090038 . 
    ## direction:aoasl:hearing       1 0.000341 0.00034070  1.2056 0.274494   
    ## aoi:direction:aoasl:hearing   1 0.000000 0.00000010  0.0004 0.984866   
    ## Residuals                   115 0.032498 0.00028259                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

So that's interesting. Let's plot those out. Again, pay attention to the legend. And ahh, there's one huge outlier point with 0.30% (forward, right AOI, hearing person who learned ASL around age 12) which could be throwing off the entire stats too. Still, it's also interesting the deaf group has an increasing bias to the left the later they learn ASL. And I'm suspecting there is no general bias for hearing signers (once we fix that outlier).

``` r
ggplot(data.lr, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=aoi,shape=hearing)) +
  geom_smooth(aes(color=aoi,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("direction")
```

    ## Warning: Removed 213 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 213 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-36-1.png)

Assorted/older stuff pushed to the bottom
=========================================

> Then when you do a multiple regression analysis, which will be looking at whether gaze behavior can be used to predict accuracy on lexical recall, this will have hearing status, AoA, lexical recall accuracy …. For reversed and not forward? You can’t put both in. And a few of the AOI measures, maybe just one. Maybe a looking-ratio. Maybe a measure of scatter? I don't know. That's where viewing space comes in, and that's saved for later. If we end up saving this for later, that's fine.

This will go into a separate data notebook (04).

Let's jump straight to a big linear mixed model for the Big 5. We'll try both groups and regressing on AoA. Here are the ANOVA tables in order: 1. Linear model (no random terms) with MainGroups 1. Linear mixed model with MainGroups 1. Linear model (no random terms) with AoASL and Hearing 1. Linear mixed model with AoASL and Hearing

But that can be complicated because of so many possible interactions (groups x aois x direction x hearing) in the posthoc analyses. We'll try separating for direction. Because we think there is no difference among groups for forward, but there should be for reverse.

What if we use AoA as linear and then deaf/hearing
