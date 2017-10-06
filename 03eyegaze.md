Eye Gaze Analysis (study1adults)
================
Adam Stone, PhD
10-06-2017

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

    ## Warning: Removed 889 rows containing non-finite values (stat_bin).

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
    ## 1    Cinderella          4          7    51
    ## 2    Goldilocks          3          4    51
    ## 3     KingMidas          4          6    51
    ## 4 RedRidingHood          4          5    51

``` r
lowlookingid <- filter(data,quarter==FALSE) %>% 
  ungroup() %>%
  select(id,participant,hearing,videogroup,story,direction,total) %>%
  arrange(participant)
write.csv(lowlookingid, file="lessthan25.csv")
select(lowlookingid,-participant)
```

    ## # A tibble: 15 x 6
    ##       id hearing videogroup         story direction total
    ##    <int>  <fctr>     <fctr>        <fctr>    <fctr> <dbl>
    ##  1    10    Deaf    Group 1    Cinderella  reversed  4.56
    ##  2    10    Deaf    Group 1     KingMidas   forward  2.67
    ##  3    32 Hearing    Group 2    Goldilocks  reversed  4.08
    ##  4    31 Hearing    Group 2    Cinderella   forward  4.73
    ##  5    54    Deaf    Group 1     KingMidas   forward  7.17
    ##  6    54    Deaf    Group 1 RedRidingHood  reversed  0.56
    ##  7     6    Deaf    Group 1    Cinderella  reversed  4.43
    ##  8     6    Deaf    Group 1 RedRidingHood  reversed  3.80
    ##  9     5    Deaf    Group 1     KingMidas   forward  2.91
    ## 10     5    Deaf    Group 1 RedRidingHood  reversed  1.96
    ## 11    25    Deaf    Group 2    Goldilocks  reversed  4.62
    ## 12     7    Deaf    Group 1    Cinderella  reversed  0.81
    ## 13     7    Deaf    Group 1     KingMidas   forward  6.84
    ## 14    30 Hearing    Group 1    Goldilocks   forward  2.56
    ## 15    17    Deaf    Group 1 RedRidingHood  reversed  0.52

``` r
#lowlookingid
```

So I will filter out the data with &lt;25% looking time. Maybe the threshold should be higher, but we'll revisit that later.

``` r
originalrows <- nrow(data)
data <- filter(data,quarter==TRUE)
difference <- originalrows - nrow(data)
```

So 15 stories were dropped from the previous total of 204 stories for a new total of 189 stories.

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

    ## Warning: Removed 818 rows containing non-finite values (stat_boxplot).

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

    ## Warning: Removed 818 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

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
  dplyr::summarize(percent = mean(percent,na.rm=TRUE))
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

    ## Warning: Removed 128 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

``` r
data.big5.viz <- data.big5 %>%
  group_by(maingroup,direction,aoi) %>%
  dplyr::summarize(mean = mean(percent,na.rm=TRUE), sd = sd(percent,na.rm=TRUE))
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
  dplyr::summarize(meanlooking = mean(percent, na.rm=TRUE)) %>%
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
    ## aoi                       4 37.269  9.3173 275.0786 < 2.2e-16 ***
    ## direction                 1  0.001  0.0012   0.0341  0.853633    
    ## maingroup                 4  0.023  0.0058   0.1700  0.953717    
    ## aoi:direction             4  0.547  0.1369   4.0405  0.002999 ** 
    ## aoi:maingroup            16  3.134  0.1959   5.7823 5.505e-12 ***
    ## direction:maingroup       4  0.000  0.0001   0.0031  0.999981    
    ## aoi:direction:maingroup  16  0.505  0.0315   0.9311  0.532794    
    ## Residuals               767 25.979  0.0339                       
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
    ## upperchest-eyes     -0.182234423 -0.23611908 -0.128349765 0.0000000
    ## chin-eyes           -0.061924534 -0.11403783 -0.009811235 0.0105843
    ## mouth-eyes           0.389084353  0.33703948  0.441129229 0.0000000
    ## forehead-eyes       -0.175476128 -0.24014841 -0.110803848 0.0000000
    ## chin-upperchest      0.120309889  0.06662829  0.173991482 0.0000000
    ## mouth-upperchest     0.571318775  0.51770360  0.624933947 0.0000000
    ## forehead-upperchest  0.006758295 -0.05918427  0.072700856 0.9986497
    ## mouth-chin           0.451008887  0.39917428  0.502843492 0.0000000
    ## forehead-chin       -0.113551594 -0.17805478 -0.049048408 0.0000176
    ## forehead-mouth      -0.564560480 -0.62900840 -0.500112563 0.0000000

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

    ## Warning: Removed 128 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 128 rows containing missing values (geom_point).

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
    ## aoi                           4 37.269  9.3173 276.8708 < 2.2e-16 ***
    ## direction                     1  0.001  0.0012   0.0343 0.8531613    
    ## hearing                       1  0.011  0.0113   0.3354 0.5626733    
    ## aoasl                         1  0.002  0.0016   0.0479 0.8267947    
    ## aoi:direction                 4  0.547  0.1368   4.0664 0.0028644 ** 
    ## aoi:hearing                   4  1.705  0.4264  12.6698 5.413e-10 ***
    ## direction:hearing             1  0.000  0.0001   0.0040 0.9496962    
    ## aoi:aoasl                     4  0.655  0.1637   4.8636 0.0007052 ***
    ## direction:aoasl               1  0.000  0.0003   0.0083 0.9276291    
    ## hearing:aoasl                 1  0.008  0.0075   0.2236 0.6364747    
    ## aoi:direction:hearing         4  0.146  0.0365   1.0855 0.3625017    
    ## aoi:direction:aoasl           4  0.036  0.0090   0.2671 0.8991300    
    ## aoi:hearing:aoasl             4  0.872  0.2179   6.4746 3.974e-05 ***
    ## direction:hearing:aoasl       1  0.002  0.0017   0.0492 0.8245446    
    ## aoi:direction:hearing:aoasl   4  0.057  0.0143   0.4249 0.7907154    
    ## Residuals                   777 26.148  0.0337                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Maybe it's a problem that all the hearing people have late AoASL while most deaf have early AoASL. And I don't like the forehead outliers throwing off some of those lines from the graph above so that needs to be looked at. What if we tried sign-years instead of AoASL.

``` r
ggplot(data.big5, aes(x=signyrs,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 128 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 128 rows containing missing values (geom_point).

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
    ## aoi                             4 37.269  9.3173 269.5036 < 2.2e-16 ***
    ## direction                       1  0.001  0.0012   0.0334  0.855106    
    ## hearing                         1  0.011  0.0113   0.3265  0.567916    
    ## signyrs                         1  0.000  0.0000   0.0000  0.998253    
    ## aoi:direction                   4  0.547  0.1368   3.9566  0.003469 ** 
    ## aoi:hearing                     4  1.706  0.4265  12.3351 9.909e-10 ***
    ## direction:hearing               1  0.000  0.0001   0.0035  0.953061    
    ## aoi:signyrs                     4  0.400  0.0999   2.8904  0.021566 *  
    ## direction:signyrs               1  0.001  0.0007   0.0194  0.889194    
    ## hearing:signyrs                 1  0.003  0.0026   0.0759  0.783037    
    ## aoi:direction:hearing           4  0.159  0.0398   1.1504  0.331541    
    ## aoi:direction:signyrs           4  0.078  0.0194   0.5612  0.690933    
    ## aoi:hearing:signyrs             4  0.344  0.0861   2.4895  0.042019 *  
    ## direction:hearing:signyrs       1  0.000  0.0000   0.0000  0.999538    
    ## aoi:direction:hearing:signyrs   4  0.078  0.0194   0.5620  0.690307    
    ## Residuals                     777 26.862  0.0346                       
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
  dplyr::summarize(percent = mean(percent,na.rm=TRUE))
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
    ## aoi                       2 22.5162 11.2581 236.2141 < 2.2e-16 ***
    ## direction                 1  0.0179  0.0179   0.3752  0.540428    
    ## maingroup                 4  0.0730  0.0182   0.3828  0.821003    
    ## aoi:direction             2  0.5087  0.2544   5.3370  0.005071 ** 
    ## aoi:maingroup             8  2.9807  0.3726   7.8176 5.949e-10 ***
    ## direction:maingroup       4  0.0028  0.0007   0.0144  0.999590    
    ## aoi:direction:maingroup   8  0.4243  0.0530   1.1127  0.352729    
    ## Residuals               532 25.3555  0.0477                       
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
    ## mouth-eyes  0.38908435  0.3360178  0.442150858 0.0000000
    ## chin-eyes  -0.06192453 -0.1150608 -0.008788263 0.0174817
    ## chin-mouth -0.45100889 -0.5038610 -0.398156780 0.0000000

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
    ##                                     diff           lwr          upr
    ## mouth:forward-eyes:forward    0.44304506  0.3529198345  0.533170282
    ## chin:forward-eyes:forward    -0.07644847 -0.1665736963  0.013676751
    ## eyes:reversed-eyes:forward    0.01588832 -0.0759919368  0.107768585
    ## mouth:reversed-eyes:forward   0.34891669  0.2580675571  0.439765823
    ## chin:reversed-eyes:forward   -0.03114965 -0.1222492262  0.059949931
    ## chin:forward-mouth:forward   -0.51949353 -0.6096187545 -0.429368307
    ## eyes:reversed-mouth:forward  -0.42715673 -0.5190369950 -0.335276474
    ## mouth:reversed-mouth:forward -0.09412837 -0.1849775011 -0.003279236
    ## chin:reversed-mouth:forward  -0.47419471 -0.5652942844 -0.383095127
    ## eyes:reversed-chin:forward    0.09233680  0.0004565357  0.184217057
    ## mouth:reversed-chin:forward   0.42536516  0.3345160297  0.516214295
    ## chin:reversed-chin:forward    0.04529883 -0.0458007537  0.136398404
    ## mouth:reversed-eyes:reversed  0.33302837  0.2404379168  0.425618815
    ## chin:reversed-eyes:reversed  -0.04703797 -0.1398741691  0.045798226
    ## chin:reversed-mouth:reversed -0.38006634 -0.4718821433 -0.288250531
    ##                                  p adj
    ## mouth:forward-eyes:forward   0.0000000
    ## chin:forward-eyes:forward    0.1491032
    ## eyes:reversed-eyes:forward   0.9963573
    ## mouth:reversed-eyes:forward  0.0000000
    ## chin:reversed-eyes:forward   0.9248427
    ## chin:forward-mouth:forward   0.0000000
    ## eyes:reversed-mouth:forward  0.0000000
    ## mouth:reversed-mouth:forward 0.0372745
    ## chin:reversed-mouth:forward  0.0000000
    ## eyes:reversed-chin:forward   0.0480547
    ## mouth:reversed-chin:forward  0.0000000
    ## chin:reversed-chin:forward   0.7135245
    ## mouth:reversed-eyes:reversed 0.0000000
    ## chin:reversed-eyes:reversed  0.6967604
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
    ##                                                       diff          lwr
    ## mouth:DeafNative-eyes:DeafNative              0.4975815540  0.334265869
    ## chin:DeafNative-eyes:DeafNative               0.0790435693 -0.084272116
    ## eyes:DeafEarlyASL-eyes:DeafNative             0.0522013056 -0.132166918
    ## mouth:DeafEarlyASL-eyes:DeafNative            0.5063157924  0.323943429
    ## chin:DeafEarlyASL-eyes:DeafNative             0.0009453231 -0.183422900
    ## eyes:DeafLateASL-eyes:DeafNative              0.0431999662 -0.135523691
    ## mouth:DeafLateASL-eyes:DeafNative             0.6241060179  0.445382361
    ## chin:DeafLateASL-eyes:DeafNative             -0.0983372981 -0.277060955
    ## eyes:HearingLateASL-eyes:DeafNative           0.1863604435  0.023985538
    ## mouth:HearingLateASL-eyes:DeafNative          0.3528236840  0.192219644
    ## chin:HearingLateASL-eyes:DeafNative          -0.0060655050 -0.166669545
    ## eyes:HearingNoviceASL-eyes:DeafNative         0.0567544382 -0.104717337
    ## mouth:HearingNoviceASL-eyes:DeafNative        0.3946207178  0.233148942
    ## chin:HearingNoviceASL-eyes:DeafNative         0.0378714717 -0.123600304
    ## chin:DeafNative-mouth:DeafNative             -0.4185379847 -0.580866859
    ## eyes:DeafEarlyASL-mouth:DeafNative           -0.4453802484 -0.628874914
    ## mouth:DeafEarlyASL-mouth:DeafNative           0.0087342384 -0.172754960
    ## chin:DeafEarlyASL-mouth:DeafNative           -0.4966362309 -0.680130896
    ## eyes:DeafLateASL-mouth:DeafNative            -0.4543815878 -0.632203960
    ## mouth:DeafLateASL-mouth:DeafNative            0.1265244640 -0.051297908
    ## chin:DeafLateASL-mouth:DeafNative            -0.5959188520 -0.773741224
    ## eyes:HearingLateASL-mouth:DeafNative         -0.3112211104 -0.472603453
    ## mouth:HearingLateASL-mouth:DeafNative        -0.1447578700 -0.304358334
    ## chin:HearingLateASL-mouth:DeafNative         -0.5036470590 -0.663247523
    ## eyes:HearingNoviceASL-mouth:DeafNative       -0.4408271158 -0.601300742
    ## mouth:HearingNoviceASL-mouth:DeafNative      -0.1029608362 -0.263434462
    ## chin:HearingNoviceASL-mouth:DeafNative       -0.4597100822 -0.620183709
    ## eyes:DeafEarlyASL-chin:DeafNative            -0.0268422637 -0.210336929
    ## mouth:DeafEarlyASL-chin:DeafNative            0.4272722231  0.245783024
    ## chin:DeafEarlyASL-chin:DeafNative            -0.0780982462 -0.261592912
    ## eyes:DeafLateASL-chin:DeafNative             -0.0358436032 -0.213665976
    ## mouth:DeafLateASL-chin:DeafNative             0.5450624486  0.367240076
    ## chin:DeafLateASL-chin:DeafNative             -0.1773808674 -0.355203240
    ## eyes:HearingLateASL-chin:DeafNative           0.1073168742 -0.054065468
    ## mouth:HearingLateASL-chin:DeafNative          0.2737801147  0.114179651
    ## chin:HearingLateASL-chin:DeafNative          -0.0851090744 -0.244709538
    ## eyes:HearingNoviceASL-chin:DeafNative        -0.0222891311 -0.182762757
    ## mouth:HearingNoviceASL-chin:DeafNative        0.3155771485  0.155103522
    ## chin:HearingNoviceASL-chin:DeafNative        -0.0411720976 -0.201645724
    ## mouth:DeafEarlyASL-eyes:DeafEarlyASL          0.4541144868  0.253470628
    ## chin:DeafEarlyASL-eyes:DeafEarlyASL          -0.0512559825 -0.253715660
    ## eyes:DeafLateASL-eyes:DeafEarlyASL           -0.0090013394 -0.206334623
    ## mouth:DeafLateASL-eyes:DeafEarlyASL           0.5719047123  0.374571429
    ## chin:DeafLateASL-eyes:DeafEarlyASL           -0.1505386037 -0.347871887
    ## eyes:HearingLateASL-eyes:DeafEarlyASL         0.1341591380 -0.048498709
    ## mouth:HearingLateASL-eyes:DeafEarlyASL        0.3006223784  0.119536938
    ## chin:HearingLateASL-eyes:DeafEarlyASL        -0.0582668106 -0.239352251
    ## eyes:HearingNoviceASL-eyes:DeafEarlyASL       0.0045531326 -0.177302342
    ## mouth:HearingNoviceASL-eyes:DeafEarlyASL      0.3424194122  0.160563938
    ## chin:HearingNoviceASL-eyes:DeafEarlyASL      -0.0143298339 -0.196185308
    ## chin:DeafEarlyASL-mouth:DeafEarlyASL         -0.5053704693 -0.706014328
    ## eyes:DeafLateASL-mouth:DeafEarlyASL          -0.4631158263 -0.658585676
    ## mouth:DeafLateASL-mouth:DeafEarlyASL          0.1177902255 -0.077679624
    ## chin:DeafLateASL-mouth:DeafEarlyASL          -0.6046530905 -0.800122940
    ## eyes:HearingLateASL-mouth:DeafEarlyASL       -0.3199553489 -0.500598440
    ## mouth:HearingLateASL-mouth:DeafEarlyASL      -0.1534921084 -0.332545100
    ## chin:HearingLateASL-mouth:DeafEarlyASL       -0.5123812975 -0.691434289
    ## eyes:HearingNoviceASL-mouth:DeafEarlyASL     -0.4495613542 -0.629393083
    ## mouth:HearingNoviceASL-mouth:DeafEarlyASL    -0.1116950746 -0.291526803
    ## chin:HearingNoviceASL-mouth:DeafEarlyASL     -0.4684443207 -0.648276049
    ## eyes:DeafLateASL-chin:DeafEarlyASL            0.0422546431 -0.155078641
    ## mouth:DeafLateASL-chin:DeafEarlyASL           0.6231606948  0.425827411
    ## chin:DeafLateASL-chin:DeafEarlyASL           -0.0992826212 -0.296615905
    ## eyes:HearingLateASL-chin:DeafEarlyASL         0.1854151205  0.002757273
    ## mouth:HearingLateASL-chin:DeafEarlyASL        0.3518783609  0.170792920
    ## chin:HearingLateASL-chin:DeafEarlyASL        -0.0070108281 -0.188096269
    ## eyes:HearingNoviceASL-chin:DeafEarlyASL       0.0558091151 -0.126046359
    ## mouth:HearingNoviceASL-chin:DeafEarlyASL      0.3936753947  0.211819921
    ## chin:HearingNoviceASL-chin:DeafEarlyASL       0.0369261486 -0.144929325
    ## mouth:DeafLateASL-eyes:DeafLateASL            0.5809060518  0.388835937
    ## chin:DeafLateASL-eyes:DeafLateASL            -0.1415372642 -0.333607379
    ## eyes:HearingLateASL-eyes:DeafLateASL          0.1431604774 -0.033798255
    ## mouth:HearingLateASL-eyes:DeafLateASL         0.3096237178  0.134288494
    ## chin:HearingLateASL-eyes:DeafLateASL         -0.0492654712 -0.224600695
    ## eyes:HearingNoviceASL-eyes:DeafLateASL        0.0135544720 -0.162575927
    ## mouth:HearingNoviceASL-eyes:DeafLateASL       0.3514207516  0.175290353
    ## chin:HearingNoviceASL-eyes:DeafLateASL       -0.0053284944 -0.181458893
    ## chin:DeafLateASL-mouth:DeafLateASL           -0.7224433160 -0.914513430
    ## eyes:HearingLateASL-mouth:DeafLateASL        -0.4377455744 -0.614704307
    ## mouth:HearingLateASL-mouth:DeafLateASL       -0.2712823340 -0.446617558
    ## chin:HearingLateASL-mouth:DeafLateASL        -0.6301715230 -0.805506747
    ## eyes:HearingNoviceASL-mouth:DeafLateASL      -0.5673515797 -0.743481979
    ## mouth:HearingNoviceASL-mouth:DeafLateASL     -0.2294853001 -0.405615699
    ## chin:HearingNoviceASL-mouth:DeafLateASL      -0.5862345462 -0.762364945
    ## eyes:HearingLateASL-chin:DeafLateASL          0.2846977416  0.107739009
    ## mouth:HearingLateASL-chin:DeafLateASL         0.4511609821  0.275825758
    ## chin:HearingLateASL-chin:DeafLateASL          0.0922717930 -0.083063431
    ## eyes:HearingNoviceASL-chin:DeafLateASL        0.1550917363 -0.021038663
    ## mouth:HearingNoviceASL-chin:DeafLateASL       0.4929580159  0.316827617
    ## chin:HearingNoviceASL-chin:DeafLateASL        0.1362087698 -0.039921629
    ## mouth:HearingLateASL-eyes:HearingLateASL      0.1664632404  0.007825587
    ## chin:HearingLateASL-eyes:HearingLateASL      -0.1924259486 -0.351063602
    ## eyes:HearingNoviceASL-eyes:HearingLateASL    -0.1296060053 -0.289122091
    ## mouth:HearingNoviceASL-eyes:HearingLateASL    0.2082602742  0.048744188
    ## chin:HearingNoviceASL-eyes:HearingLateASL    -0.1484889718 -0.308005058
    ## chin:HearingLateASL-mouth:HearingLateASL     -0.3588891890 -0.515713781
    ## eyes:HearingNoviceASL-mouth:HearingLateASL   -0.2960692458 -0.453782369
    ## mouth:HearingNoviceASL-mouth:HearingLateASL   0.0417970338 -0.115916090
    ## chin:HearingNoviceASL-mouth:HearingLateASL   -0.3149522122 -0.472665336
    ## eyes:HearingNoviceASL-chin:HearingLateASL     0.0628199432 -0.094893180
    ## mouth:HearingNoviceASL-chin:HearingLateASL    0.4006862228  0.242973099
    ## chin:HearingNoviceASL-chin:HearingLateASL     0.0439369768 -0.113776147
    ## mouth:HearingNoviceASL-eyes:HearingNoviceASL  0.3378662796  0.179269602
    ## chin:HearingNoviceASL-eyes:HearingNoviceASL  -0.0188829665 -0.177479644
    ## chin:HearingNoviceASL-mouth:HearingNoviceASL -0.3567492461 -0.515345923
    ##                                                        upr     p adj
    ## mouth:DeafNative-eyes:DeafNative              0.6608972391 0.0000000
    ## chin:DeafNative-eyes:DeafNative               0.2423592544 0.9467489
    ## eyes:DeafEarlyASL-eyes:DeafNative             0.2365695288 0.9997528
    ## mouth:DeafEarlyASL-eyes:DeafNative            0.6886881556 0.0000000
    ## chin:DeafEarlyASL-eyes:DeafNative             0.1853135463 1.0000000
    ## eyes:DeafLateASL-eyes:DeafNative              0.2219236235 0.9999627
    ## mouth:DeafLateASL-eyes:DeafNative             0.8028296753 0.0000000
    ## chin:DeafLateASL-eyes:DeafNative              0.0803863593 0.8647538
    ## eyes:HearingLateASL-eyes:DeafNative           0.3487353491 0.0088725
    ## mouth:HearingLateASL-eyes:DeafNative          0.5134277236 0.0000000
    ## chin:HearingLateASL-eyes:DeafNative           0.1545385346 1.0000000
    ## eyes:HearingNoviceASL-eyes:DeafNative         0.2182262137 0.9972785
    ## mouth:HearingNoviceASL-eyes:DeafNative        0.5560924933 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafNative         0.1993432472 0.9999742
    ## chin:DeafNative-mouth:DeafNative             -0.2562091103 0.0000000
    ## eyes:DeafEarlyASL-mouth:DeafNative           -0.2618855831 0.0000000
    ## mouth:DeafEarlyASL-mouth:DeafNative           0.1902234373 1.0000000
    ## chin:DeafEarlyASL-mouth:DeafNative           -0.3131415656 0.0000000
    ## eyes:DeafLateASL-mouth:DeafNative            -0.2765592154 0.0000000
    ## mouth:DeafLateASL-mouth:DeafNative            0.3043468364 0.4976579
    ## chin:DeafLateASL-mouth:DeafNative            -0.4180964796 0.0000000
    ## eyes:HearingLateASL-mouth:DeafNative         -0.1498387682 0.0000000
    ## mouth:HearingLateASL-mouth:DeafNative         0.0148425937 0.1243274
    ## chin:HearingLateASL-mouth:DeafNative         -0.3440465953 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafNative       -0.2803534895 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafNative       0.0575127901 0.6747035
    ## chin:HearingNoviceASL-mouth:DeafNative       -0.2992364559 0.0000000
    ## eyes:DeafEarlyASL-chin:DeafNative             0.1566524016 0.9999999
    ## mouth:DeafEarlyASL-chin:DeafNative            0.6087614220 0.0000000
    ## chin:DeafEarlyASL-chin:DeafNative             0.1053964191 0.9821854
    ## eyes:DeafLateASL-chin:DeafNative              0.1419787693 0.9999962
    ## mouth:DeafLateASL-chin:DeafNative             0.7228848211 0.0000000
    ## chin:DeafLateASL-chin:DeafNative              0.0004415051 0.0513276
    ## eyes:HearingLateASL-chin:DeafNative           0.2686992165 0.6164554
    ## mouth:HearingLateASL-chin:DeafNative          0.4333805783 0.0000009
    ## chin:HearingLateASL-chin:DeafNative           0.0744913893 0.8905388
    ## eyes:HearingNoviceASL-chin:DeafNative         0.1381844952 1.0000000
    ## mouth:HearingNoviceASL-chin:DeafNative        0.4760507748 0.0000000
    ## chin:HearingNoviceASL-chin:DeafNative         0.1193015287 0.9999230
    ## mouth:DeafEarlyASL-eyes:DeafEarlyASL          0.6547583456 0.0000000
    ## chin:DeafEarlyASL-eyes:DeafEarlyASL           0.1512036948 0.9999345
    ## eyes:DeafLateASL-eyes:DeafEarlyASL            0.1883319443 1.0000000
    ## mouth:DeafLateASL-eyes:DeafEarlyASL           0.7692379961 0.0000000
    ## chin:DeafLateASL-eyes:DeafEarlyASL            0.0467946801 0.3725917
    ## eyes:HearingLateASL-eyes:DeafEarlyASL         0.3168169851 0.4402503
    ## mouth:HearingLateASL-eyes:DeafEarlyASL        0.4817078188 0.0000026
    ## chin:HearingLateASL-eyes:DeafEarlyASL         0.1228186298 0.9989471
    ## eyes:HearingNoviceASL-eyes:DeafEarlyASL       0.1864086067 1.0000000
    ## mouth:HearingNoviceASL-eyes:DeafEarlyASL      0.5242748863 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafEarlyASL       0.1675256403 1.0000000
    ## chin:DeafEarlyASL-mouth:DeafEarlyASL         -0.3047266106 0.0000000
    ## eyes:DeafLateASL-mouth:DeafEarlyASL          -0.2676459769 0.0000000
    ## mouth:DeafLateASL-mouth:DeafEarlyASL          0.3132600749 0.7650169
    ## chin:DeafLateASL-mouth:DeafEarlyASL          -0.4091832411 0.0000000
    ## eyes:HearingLateASL-mouth:DeafEarlyASL       -0.1393122581 0.0000003
    ## mouth:HearingLateASL-mouth:DeafEarlyASL       0.0255608833 0.1900496
    ## chin:HearingLateASL-mouth:DeafEarlyASL       -0.3333283057 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafEarlyASL     -0.2697296256 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafEarlyASL     0.0681366539 0.7235129
    ## chin:HearingNoviceASL-mouth:DeafEarlyASL     -0.2886125921 0.0000000
    ## eyes:DeafLateASL-chin:DeafEarlyASL            0.2395879268 0.9999917
    ## mouth:DeafLateASL-chin:DeafEarlyASL           0.8204939786 0.0000000
    ## chin:DeafLateASL-chin:DeafEarlyASL            0.0980506626 0.9281444
    ## eyes:HearingLateASL-chin:DeafEarlyASL         0.3680729676 0.0425452
    ## mouth:HearingLateASL-chin:DeafEarlyASL        0.5329638013 0.0000000
    ## chin:HearingLateASL-chin:DeafEarlyASL         0.1740746123 1.0000000
    ## eyes:HearingNoviceASL-chin:DeafEarlyASL       0.2376645893 0.9993784
    ## mouth:HearingNoviceASL-chin:DeafEarlyASL      0.5755308688 0.0000000
    ## chin:HearingNoviceASL-chin:DeafEarlyASL       0.2187816228 0.9999958
    ## mouth:DeafLateASL-eyes:DeafLateASL            0.7729761661 0.0000000
    ## chin:DeafLateASL-eyes:DeafLateASL             0.0505328501 0.4343153
    ## eyes:HearingLateASL-eyes:DeafLateASL          0.3201192101 0.2741404
    ## mouth:HearingLateASL-eyes:DeafLateASL         0.4849589416 0.0000003
    ## chin:HearingLateASL-eyes:DeafLateASL          0.1260697526 0.9997739
    ## eyes:HearingNoviceASL-eyes:DeafLateASL        0.1896848710 1.0000000
    ## mouth:HearingNoviceASL-eyes:DeafLateASL       0.5275511506 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafLateASL        0.1708019045 1.0000000
    ## chin:DeafLateASL-mouth:DeafLateASL           -0.5303732016 0.0000000
    ## eyes:HearingLateASL-mouth:DeafLateASL        -0.2607868417 0.0000000
    ## mouth:HearingLateASL-mouth:DeafLateASL       -0.0959471102 0.0000201
    ## chin:HearingLateASL-mouth:DeafLateASL        -0.4548362992 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafLateASL      -0.3912211808 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafLateASL     -0.0533549012 0.0010421
    ## chin:HearingNoviceASL-mouth:DeafLateASL      -0.4101041473 0.0000000
    ## eyes:HearingLateASL-chin:DeafLateASL          0.4616564743 0.0000067
    ## mouth:HearingLateASL-chin:DeafLateASL         0.6264962058 0.0000000
    ## chin:HearingLateASL-chin:DeafLateASL          0.2676070168 0.9002118
    ## eyes:HearingNoviceASL-chin:DeafLateASL        0.3312221352 0.1566844
    ## mouth:HearingNoviceASL-chin:DeafLateASL       0.6690884148 0.0000000
    ## chin:HearingNoviceASL-chin:DeafLateASL        0.3123391687 0.3488519
    ## mouth:HearingLateASL-eyes:HearingLateASL      0.3251008934 0.0291099
    ## chin:HearingLateASL-eyes:HearingLateASL      -0.0337882956 0.0037439
    ## eyes:HearingNoviceASL-eyes:HearingLateASL     0.0299100806 0.2673809
    ## mouth:HearingNoviceASL-eyes:HearingLateASL    0.3677763602 0.0010021
    ## chin:HearingNoviceASL-eyes:HearingLateASL     0.0110271141 0.0998165
    ## chin:HearingLateASL-mouth:HearingLateASL     -0.2020645973 0.0000000
    ## eyes:HearingNoviceASL-mouth:HearingLateASL   -0.1383561224 0.0000000
    ## mouth:HearingNoviceASL-mouth:HearingLateASL   0.1995101572 0.9998865
    ## chin:HearingNoviceASL-mouth:HearingLateASL   -0.1572390889 0.0000000
    ## eyes:HearingNoviceASL-chin:HearingLateASL     0.2205330666 0.9904174
    ## mouth:HearingNoviceASL-chin:HearingLateASL    0.5583993462 0.0000000
    ## chin:HearingNoviceASL-chin:HearingLateASL     0.2016501002 0.9997954
    ## mouth:HearingNoviceASL-eyes:HearingNoviceASL  0.4964629568 0.0000000
    ## chin:HearingNoviceASL-eyes:HearingNoviceASL   0.1397137107 1.0000000
    ## chin:HearingNoviceASL-mouth:HearingNoviceASL -0.1981525689 0.0000000

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
    ## aoi                           2 22.5162 11.2581 238.1007 < 2.2e-16 ***
    ## direction                     1  0.0179  0.0179   0.3782 0.5388128    
    ## hearing                       1  0.0603  0.0603   1.2745 0.2594306    
    ## aoasl                         1  0.0005  0.0005   0.0096 0.9218057    
    ## aoi:direction                 2  0.5086  0.2543   5.3782 0.0048677 ** 
    ## aoi:hearing                   2  1.6221  0.8111  17.1534 6.001e-08 ***
    ## direction:hearing             1  0.0012  0.0012   0.0251 0.8741720    
    ## aoi:aoasl                     2  0.6330  0.3165   6.6941 0.0013439 ** 
    ## direction:aoasl               1  0.0021  0.0021   0.0451 0.8318093    
    ## hearing:aoasl                 1  0.0147  0.0147   0.3116 0.5769187    
    ## aoi:direction:hearing         2  0.1385  0.0693   1.4650 0.2319880    
    ## aoi:direction:aoasl           2  0.0203  0.0101   0.2145 0.8070181    
    ## aoi:hearing:aoasl             2  0.8639  0.4319   9.1351 0.0001255 ***
    ## direction:hearing:aoasl       1  0.0000  0.0000   0.0000 0.9982782    
    ## aoi:direction:hearing:aoasl   2  0.0414  0.0207   0.4379 0.6455974    
    ## Residuals                   538 25.4383  0.0473                       
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
    ## -0.59995 -0.13886 -0.04123  0.11828  0.69537 
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error
    ## (Intercept)                                      0.150564   0.040972
    ## aoimouth                                         0.511763   0.057943
    ## aoichin                                          0.037960   0.057943
    ## directionreversed                                0.024749   0.059773
    ## aoasl                                            0.001983   0.004857
    ## hearingHearing                                   0.617478   0.226428
    ## aoimouth:directionreversed                      -0.106240   0.084063
    ## aoichin:directionreversed                       -0.002355   0.084219
    ## aoimouth:aoasl                                   0.004335   0.006868
    ## aoichin:aoasl                                   -0.011781   0.006868
    ## directionreversed:aoasl                         -0.003286   0.006967
    ## aoimouth:hearingHearing                         -0.988190   0.320218
    ## aoichin:hearingHearing                          -1.027949   0.320218
    ## directionreversed:hearingHearing                -0.130779   0.326226
    ## aoasl:hearingHearing                            -0.031636   0.013445
    ## aoimouth:directionreversed:aoasl                 0.009222   0.009825
    ## aoichin:directionreversed:aoasl                  0.002548   0.009825
    ## aoimouth:directionreversed:hearingHearing        0.215226   0.454673
    ## aoichin:directionreversed:hearingHearing         0.123007   0.454702
    ## aoimouth:aoasl:hearingHearing                    0.041707   0.019015
    ## aoichin:aoasl:hearingHearing                     0.060608   0.019015
    ## directionreversed:aoasl:hearingHearing           0.010213   0.019505
    ## aoimouth:directionreversed:aoasl:hearingHearing -0.024191   0.027140
    ## aoichin:directionreversed:aoasl:hearingHearing  -0.005865   0.027140
    ##                                                 t value Pr(>|t|)    
    ## (Intercept)                                       3.675 0.000262 ***
    ## aoimouth                                          8.832  < 2e-16 ***
    ## aoichin                                           0.655 0.512670    
    ## directionreversed                                 0.414 0.679004    
    ## aoasl                                             0.408 0.683158    
    ## hearingHearing                                    2.727 0.006599 ** 
    ## aoimouth:directionreversed                       -1.264 0.206842    
    ## aoichin:directionreversed                        -0.028 0.977705    
    ## aoimouth:aoasl                                    0.631 0.528217    
    ## aoichin:aoasl                                    -1.715 0.086869 .  
    ## directionreversed:aoasl                          -0.472 0.637433    
    ## aoimouth:hearingHearing                          -3.086 0.002133 ** 
    ## aoichin:hearingHearing                           -3.210 0.001406 ** 
    ## directionreversed:hearingHearing                 -0.401 0.688664    
    ## aoasl:hearingHearing                             -2.353 0.018987 *  
    ## aoimouth:directionreversed:aoasl                  0.939 0.348294    
    ## aoichin:directionreversed:aoasl                   0.259 0.795450    
    ## aoimouth:directionreversed:hearingHearing         0.473 0.636146    
    ## aoichin:directionreversed:hearingHearing          0.271 0.786862    
    ## aoimouth:aoasl:hearingHearing                     2.193 0.028705 *  
    ## aoichin:aoasl:hearingHearing                      3.187 0.001519 ** 
    ## directionreversed:aoasl:hearingHearing            0.524 0.600761    
    ## aoimouth:directionreversed:aoasl:hearingHearing  -0.891 0.373144    
    ## aoichin:directionreversed:aoasl:hearingHearing   -0.216 0.828980    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2174 on 538 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.5097, Adjusted R-squared:  0.4887 
    ## F-statistic: 24.31 on 23 and 538 DF,  p-value: < 2.2e-16

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
  dplyr::summarize(percent = mean(percent,na.rm=TRUE))
data.lr[data.lr=="NaN"] <- NA

# Join subject info with data.big5 that's now subject-level
data.lr <- left_join(data.lr,data.lr.subjectinfo, by=c("participant","direction"))
```

Problem is, I can already tell this dataset is rather sparse. There are 231 empty cells out of 378. Soooo. Let's give this a shot anyway but probably not a good idea? The graph below, I changed the colors so they map on left/right AOI, and each facet is direction. So we can directly compare L/R biases.

``` r
ggplot(data.lr) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_wrap("direction") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 231 rows containing non-finite values (stat_boxplot).

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
    ## aoi                       1 0.002795 0.00279532  9.0395 0.003185 **
    ## direction                 1 0.000380 0.00038047  1.2304 0.269435   
    ## maingroup                 4 0.002836 0.00070907  2.2930 0.063004 . 
    ## aoi:direction             1 0.002297 0.00229669  7.4270 0.007332 **
    ## aoi:maingroup             4 0.002050 0.00051239  1.6569 0.164093   
    ## direction:maingroup       4 0.000724 0.00018104  0.5854 0.673751   
    ## aoi:direction:maingroup   4 0.001760 0.00044002  1.4229 0.230114   
    ## Residuals               127 0.039273 0.00030924                    
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
    ## aoi                           1 0.002795 0.00279532  9.3996 0.002637 **
    ## direction                     1 0.000380 0.00038047  1.2794 0.260083   
    ## aoasl                         1 0.002221 0.00222092  7.4681 0.007148 **
    ## hearing                       1 0.000562 0.00056227  1.8907 0.171468   
    ## aoi:direction                 1 0.002299 0.00229874  7.7298 0.006232 **
    ## aoi:aoasl                     1 0.000023 0.00002317  0.0779 0.780609   
    ## direction:aoasl               1 0.000220 0.00022006  0.7400 0.391235   
    ## aoi:hearing                   1 0.001632 0.00163157  5.4864 0.020673 * 
    ## direction:hearing             1 0.000338 0.00033758  1.1352 0.288635   
    ## aoasl:hearing                 1 0.000451 0.00045136  1.5177 0.220167   
    ## aoi:direction:aoasl           1 0.001170 0.00117031  3.9353 0.049373 * 
    ## aoi:direction:hearing         1 0.000314 0.00031432  1.0569 0.305810   
    ## aoi:aoasl:hearing             1 0.000633 0.00063301  2.1286 0.146969   
    ## direction:aoasl:hearing       1 0.000080 0.00007999  0.2690 0.604902   
    ## aoi:direction:aoasl:hearing   1 0.000039 0.00003858  0.1297 0.719275   
    ## Residuals                   131 0.038958 0.00029739                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

So that's interesting. Let's plot those out. Again, pay attention to the legend. And ahh, there's one huge outlier point with 0.30% (forward, right AOI, hearing person who learned ASL around age 12) which could be throwing off the entire stats too. Still, it's also interesting the deaf group has an increasing bias to the left the later they learn ASL. And I'm suspecting there is no general bias for hearing signers (once we fix that outlier).

``` r
ggplot(data.lr, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=aoi,shape=hearing)) +
  geom_smooth(aes(color=aoi,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("direction")
```

    ## Warning: Removed 231 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 231 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-36-1.png)

Assorted/older stuff pushed to the bottom
=========================================

> Then when you do a multiple regression analysis, which will be looking at whether gaze behavior can be used to predict accuracy on lexical recall, this will have hearing status, AoA, lexical recall accuracy …. For reversed and not forward? You can’t put both in. And a few of the AOI measures, maybe just one. Maybe a looking-ratio. Maybe a measure of scatter? I don't know. That's where viewing space comes in, and that's saved for later. If we end up saving this for later, that's fine.

This will go into a separate data notebook (04).

Let's jump straight to a big linear mixed model for the Big 5. We'll try both groups and regressing on AoA. Here are the ANOVA tables in order: 1. Linear model (no random terms) with MainGroups 1. Linear mixed model with MainGroups 1. Linear model (no random terms) with AoASL and Hearing 1. Linear mixed model with AoASL and Hearing

But that can be complicated because of so many possible interactions (groups x aois x direction x hearing) in the posthoc analyses. We'll try separating for direction. Because we think there is no difference among groups for forward, but there should be for reverse.

What if we use AoA as linear and then deaf/hearing
