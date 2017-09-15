Eye Gaze Analysis (study1adults)
================
Adam Stone, PhD
09-15-2017

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

These are our current AOIs. 1. Forehead (above eyes) 2. Eyes 3. Mouth 4. Chin (below chin) 5. Upper Chest 6. Middle Chest 7. Lower Chest 8. Belly 9. Left 10. Right

It's possible to do a secondary analysis combining some of these AOIs (in particular, maybe 5-6 and 7-8 can be combined into Torso Upper Half and Torso Lower Half). Anyway, the face AOIs are important, and the division of them into 4 areas is theoretically motivated and also previously seen in the literature.

*Why 4 AOIs on Face?* Emmorey et al. (2008) did this same setup. We generally know people fixate on the face across all conditions and langauge experiences, but **where** on the face is important for us to know. So these 4 AOIs. *Write one sentence here about predictions for language experience on face-looking.*

*Why 4 AOIs for Torso?* Past papers tend to just classify the body as “body” with no further breakdown, or two-part breakdown. In our study, we have higher resolution to break this down into four AOI’s, defined as upper chest, middle chest, lower chest, and belly. We know that hands start and end at the belly and the hands spend the majority of the time in these four areas in front of the torso (and the hands spend very little time overlapping the face). If an observer (child or adult) glances at the hands (or if the hands have any “gravity” upon gaze behavior), then gaze samples will fall within these four torso areas. Although we expect that all observers do spend the most amount of time on the face, we also predict that the number of gazes towards the hands (by way of “torso”) might be impacted by language experience. *(make a footnote here: As a future project, we will analyze the data using dynamic “hand” AOIs, for each hand, in which we document, on each frame, where the left and right hands are in viewing space.)*

*Why 2 AOIs for left vs right?* People have talked how sign language impacts left vs right visual field asymmetries, as related to hemispheric laterality for language processing, so it is worth checking this. If we do find an asymmetry, we will then just touch upon this literature in the discussion, but also acknowledge that it could be the signer’s hand dominance that drives a lateral asymmetry too, not just a hemispheric asymmetry. Meaning, if the signer is right handed, her dominant hand might have some “gravity” in the viewer’s left visual field. (And we can check this with the future analysis of dynamic hand AOIs.)

Data Cleaning
=============

This is my process of documenting how I'm weeding through data and making sure all's good. Let's visualize first of all.

``` r
# Reduce dataset to face AOIs only
data.face <- select(data,-upperchest,-midchest,-lowerchest,-belly,-left,-right)
# Reshape data so we can easily facet our charts based on face AOIs
data.face <- data.face %>% gather(aoi,looking,forehead:chin)
# Graph!
ggplot(data.face,aes(x=maingroup,y=looking,fill=direction)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  facet_wrap("aoi")
```

    ## Warning: Removed 156 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png) Okay, right away I see some issues - I want to check for outliers but I'm not sure what could count as an outlier. All 4 stories are different lengths - a data point at 30 seconds would be fine for King Midas (0:37) but impossible for Red Riding Hood (0:18) so outliers need to be *relative* to the story length itself. Let's back up and do histograms for each story.

``` r
ggplot(data.face,aes(x=looking)) +
  geom_histogram(binwidth=1) +
  facet_wrap("story") +
  xlab("secs") +
  ggtitle("Face AOI sums for each story for each participant")
```

    ## Warning: Removed 156 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png) Loooks good but I see weird outliers for Red Riding Hood (before Cinderalla too, which I fixed) - those single data points are past the video length (and that's just the face AOIs!). Let's sum up *all* AOIs across each story for each participant...back to the big dataset, and we'll do histograms again.

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

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png) The tall bars are near the end of the story, right? So we see two issues: 1. Some barely watched the story at all. (Those are the ones with bars at or near zero). We should remove those. We need a rule for it. 1. A few people's AOI data has total seconds higher than the video itself! (Those are the ones with very short bars to the right of the very tall bars.) Those should be investigated, something went wrong in the data.

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

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

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
  summarize(lessthan25 = sum(!quarter),
            lessthan50 = sum(!half),
            total = sum(!quarter,quarter))
lowlooking
```

    ## # A tibble: 4 x 4
    ##           story lessthan25 lessthan50 total
    ##          <fctr>      <int>      <int> <int>
    ## 1    Cinderella          4          7    46
    ## 2    Goldilocks          3          4    46
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

    ## # A tibble: 13 x 6
    ##       id hearing videogroup         story direction total
    ##    <int>  <fctr>     <fctr>        <fctr>    <fctr> <dbl>
    ##  1    10    Deaf    Group 1    Cinderella  reversed  4.60
    ##  2    10    Deaf    Group 1     KingMidas   forward  2.66
    ##  3    32 Hearing    Group 2    Goldilocks  reversed  4.08
    ##  4    31 Hearing    Group 2    Cinderella   forward  4.73
    ##  5     6    Deaf    Group 1    Cinderella  reversed  4.43
    ##  6     6    Deaf    Group 1 RedRidingHood  reversed  3.80
    ##  7     5    Deaf    Group 1     KingMidas   forward  2.91
    ##  8     5    Deaf    Group 1 RedRidingHood  reversed  1.96
    ##  9    25    Deaf    Group 2    Goldilocks  reversed  4.62
    ## 10     7    Deaf    Group 1    Cinderella  reversed  0.81
    ## 11     7    Deaf    Group 1     KingMidas   forward  6.84
    ## 12    30 Hearing    Group 1    Goldilocks   forward  2.56
    ## 13    17    Deaf    Group 1 RedRidingHood  reversed  0.52

``` r
#lowlookingid
```

So I will filter out the data with &lt;25% looking time. Maybe the threshold should be higher, but we'll revisit that later.

``` r
originalrows <- nrow(data)
data <- filter(data,quarter==TRUE)
difference <- originalrows - nrow(data)
```

So 13 stories were dropped from the previous total of 184 stories for a new total of 171 stories.

**I'm also dropping Sara and ChrissyG's data too until we fix them.**

``` r
data <- filter(data,participant!="Sara")
data <- filter(data,participant!="ChrissyG")
```

Percentage Data and Viz
-----------------------

We need to work with percentages, because of participants' idiosyntractic eye behavior. Some blink a lot, some don't, so automatically the maximum number of eye gaze data points each participant is able to contribute is different. For that reason we work with percent of total data points on a per-participant basis. That's also why we took out stories with &lt;25% looking data. Now here's the boxplots for each AOI.

``` r
# data2 uses percentage data!
data2 <- data %>%
  mutate_at(vars(forehead:right), funs(./total)) %>%
  select(-total, -quarter, -half) %>%
  gather(aoi,percent,forehead:right)

ggplot(data2, aes(x=aoi,y=percent)) +
  geom_boxplot() 
```

    ## Warning: Removed 737 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

But we should also look at histograms of percentage data. Those should have more normal-like distributions for the high-hitting AOIs, unlike the actual looking data in seconds which has a upper limit.

``` r
ggplot(data2,aes(x=percent)) +
  geom_histogram() +
  facet_grid(aoi ~ story) +
  xlab("secs") +
  ggtitle("Looking times of each AOI for each participant for each story")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 737 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

Big Five AOIs
=============

> You’ll have to decide how to put the AOIs in an ANOVA. All of them together is too many. And you cannot put ALL the AOIs in. If they all sum to 100% (which they currently do), then the observations are not independent. Also, you can’t put AOIs that have near-zero values in with AOIs that have super high values, you’ll get whopping significance that is too obvious to reveal anything meaningful.

Based on the boxplot there are five AOIs that got hit the most: forehead, eyes, mouth, chin, and upperchest. **But this is really important...I think there is one or two outliers in forehead. And maybe it's better to get rid of forehead and upper chest for the big ANOVAs to keep things simple. Neither of them touch 50%...but then again they are not "non-significant." I am going ahead with all 5 for now.**

*Important reference levels* - AOI reference level is eyes - MainGroup reference level is NativeDeaf

Creating the `data.big5` thing here.

``` r
# Make Big5 df with reference levels
data.big5 <- filter(data2,aoi == "forehead" | aoi == "eyes" 
                    | aoi == "mouth" | aoi == "chin" | aoi == "upperchest") %>%
  mutate(aoi = as.factor(aoi))
data.big5$aoi <- factor(data.big5$aoi, levels=c("upperchest","chin","mouth","eyes","forehead"))
data.big5$aoi <- relevel(data.big5$aoi, ref="eyes")
data.big5$maingroup <- relevel(data.big5$maingroup, ref="NativeDeaf")
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

    ## Warning: Removed 68 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)

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

    ## Warning: Removed 1 rows containing missing values (geom_errorbar).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-1.png)

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

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

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

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

The ANOVA below tells us there's a significant effect of AOI, and significant interactions of AOI x Direction and AOI x MainGroup.

``` r
group.anova <- aov(data=data.big5,percent ~ aoi * direction * maingroup)
anova(group.anova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                          Df  Sum Sq Mean Sq F value  Pr(>F)    
    ## aoi                       4 15.3862  3.8465 89.6975 < 2e-16 ***
    ## direction                 1  0.0002  0.0002  0.0040 0.94940    
    ## maingroup                 4  0.0172  0.0043  0.1001 0.98235    
    ## aoi:direction             4  0.5361  0.1340  3.1252 0.01526 *  
    ## aoi:maingroup            16  1.2087  0.0755  1.7616 0.03536 *  
    ## direction:maingroup       4  0.0027  0.0007  0.0160 0.99950    
    ## aoi:direction:maingroup  16  0.2120  0.0133  0.3090 0.99568    
    ## Residuals               317 13.5941  0.0429                    
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
    ##                             diff        lwr         upr     p adj
    ## upperchest-eyes     -0.163117532 -0.2535807 -0.07265439 0.0000121
    ## chin-eyes           -0.015583269 -0.1032743  0.07210779 0.9884749
    ## mouth-eyes           0.386468735  0.2990240  0.47391346 0.0000000
    ## forehead-eyes       -0.157965393 -0.2715553 -0.04437551 0.0015271
    ## chin-upperchest      0.147534264  0.0580888  0.23697973 0.0000832
    ## mouth-upperchest     0.549586267  0.4603823  0.63879024 0.0000000
    ## forehead-upperchest  0.005152139 -0.1097976  0.12010183 0.9999482
    ## mouth-chin           0.402052003  0.3156605  0.48844351 0.0000000
    ## forehead-chin       -0.142382125 -0.2551632 -0.02960104 0.0054472
    ## forehead-mouth      -0.544434128 -0.6570238 -0.43184446 0.0000000

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

    ## Warning: Removed 68 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 68 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-20-1.png)

Let's move to ANOVAs. This is technically an ANCOVA, and AoASL is the covariate. The output tells us there is a significant main effect of AOI, and significant interactions of AOI x Direction and AOI x Hearing. A marginally significant interaction of AOI x Hearing X AoASL. So really it's very similar to what we got with the group ANOVA.

``` r
continuous.anova <- aov(data=data.big5, percent ~ aoi * direction * hearing * aoasl)
anova(continuous.anova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                              Df  Sum Sq Mean Sq F value  Pr(>F)    
    ## aoi                           4 15.3862  3.8465 90.6448 < 2e-16 ***
    ## direction                     1  0.0002  0.0002  0.0041 0.94914    
    ## hearing                       1  0.0126  0.0126  0.2968 0.58626    
    ## aoasl                         1  0.0001  0.0001  0.0032 0.95465    
    ## aoi:direction                 4  0.5356  0.1339  3.1555 0.01447 *  
    ## aoi:hearing                   4  0.5026  0.1257  2.9611 0.01998 *  
    ## direction:hearing             1  0.0013  0.0013  0.0313 0.85977    
    ## aoi:aoasl                     4  0.2383  0.0596  1.4038 0.23240    
    ## direction:aoasl               1  0.0001  0.0001  0.0014 0.96970    
    ## hearing:aoasl                 1  0.0070  0.0070  0.1644 0.68540    
    ## aoi:direction:hearing         4  0.0251  0.0063  0.1479 0.96386    
    ## aoi:direction:aoasl           4  0.0104  0.0026  0.0613 0.99303    
    ## aoi:hearing:aoasl             4  0.3434  0.0858  2.0229 0.09094 .  
    ## direction:hearing:aoasl       1  0.0001  0.0001  0.0028 0.95789    
    ## aoi:direction:hearing:aoasl   4  0.0178  0.0044  0.1048 0.98081    
    ## Residuals                   327 13.8763  0.0424                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Maybe it's a problem that all the hearing people have late AoASL while most deaf have early AoASL. And I don't like the forehead outliers throwing off some of those lines from the graph above so that needs to be looked at. What if we tried sign-years instead of AoASL.

``` r
ggplot(data.big5, aes(x=signyrs,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 68 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 68 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-22-1.png)

Look interesting and you can sort of compare the deaf/hearing lines better, although we should get rid of that person who's been signing for 60 years if we do a years-of-signing analysis. Here's the ANCOVA.

Let's move to ANOVAs. This is technically an ANCOVA, and AoASL is the covariate. The output tells us there is a significant main effect of AOI, and significant interactions of AOI x Direction and AOI x Hearing and AOI x SignYrs. Again a rather similar thing result.

``` r
continuous.anova.sy <- aov(data=data.big5, percent ~ aoi * direction * hearing * signyrs)
anova(continuous.anova.sy)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                                Df  Sum Sq Mean Sq F value  Pr(>F)    
    ## aoi                             4 15.3862  3.8465 90.6853 < 2e-16 ***
    ## direction                       1  0.0002  0.0002  0.0041 0.94913    
    ## hearing                         1  0.0126  0.0126  0.2970 0.58617    
    ## signyrs                         1  0.0004  0.0004  0.0085 0.92640    
    ## aoi:direction                   4  0.5354  0.1339  3.1558 0.01447 *  
    ## aoi:hearing                     4  0.5026  0.1256  2.9621 0.01995 *  
    ## direction:hearing               1  0.0013  0.0013  0.0313 0.85962    
    ## aoi:signyrs                     4  0.4606  0.1151  2.7146 0.02997 *  
    ## direction:signyrs               1  0.0001  0.0001  0.0019 0.96493    
    ## hearing:signyrs                 1  0.0024  0.0024  0.0570 0.81143    
    ## aoi:direction:hearing           4  0.0233  0.0058  0.1372 0.96843    
    ## aoi:direction:signyrs           4  0.0261  0.0065  0.1539 0.96117    
    ## aoi:hearing:signyrs             4  0.0511  0.0128  0.3012 0.87707    
    ## direction:hearing:signyrs       1  0.0003  0.0003  0.0075 0.93098    
    ## aoi:direction:hearing:signyrs   4  0.0844  0.0211  0.4977 0.73744    
    ## Residuals                     327 13.8701  0.0424                    
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
data.face3$maingroup <- relevel(data.face3$maingroup, ref="NativeDeaf")

data.face3.item <- data.face3 # save item-level data for later

# Pull out and save subject info
data.face3.subjectinfo <- data.face3 %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()

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

    ## Warning: Removed 6 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-25-1.png)

But we have less levels so maybe another way of looking at the boxplots:

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_grid(direction~.) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 6 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-26-1.png)

Or another way even

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=aoi,y=percent,fill=direction)) +
  facet_grid(direction~maingroup) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 6 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-27-1.png)

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
    ##                          Df  Sum Sq Mean Sq F value  Pr(>F)    
    ## aoi                       2  8.9281  4.4641 76.1553 < 2e-16 ***
    ## direction                 1  0.0047  0.0047  0.0805 0.77685    
    ## maingroup                 4  0.0405  0.0101  0.1727 0.95219    
    ## aoi:direction             2  0.5153  0.2577  4.3958 0.01341 *  
    ## aoi:maingroup             8  1.1403  0.1425  2.4317 0.01538 *  
    ## direction:maingroup       4  0.0036  0.0009  0.0154 0.99953    
    ## aoi:direction:maingroup   8  0.1891  0.0236  0.4033 0.91797    
    ## Residuals               225 13.1890  0.0586                    
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
    ##                   diff        lwr         upr     p adj
    ## mouth-eyes  0.38646873  0.2985503  0.47438721 0.0000000
    ## chin-eyes  -0.01558327 -0.1037494  0.07258287 0.9086476
    ## chin-mouth -0.40205200 -0.4889115 -0.31519246 0.0000000

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

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-30-1.png) and the ANCOVA itself...which gives us almost identical results as the `big5` stats. So maybe it's easier overall to just drop all AOIs except eye, mouth, chin when trying to look for AoA, group effects, etc? We can present summary stats overall for all AOIs, then when it gets down to the dirty stats work, we keep it simple and show ... that whatever we found.

``` r
continuous.anova.face3 <- aov(data=data.face3, percent ~ aoi * direction * hearing * aoasl)
anova(continuous.anova.face3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                              Df  Sum Sq Mean Sq F value  Pr(>F)    
    ## aoi                           2  8.9281  4.4641 76.7486 < 2e-16 ***
    ## direction                     1  0.0047  0.0047  0.0812 0.77600    
    ## hearing                       1  0.0335  0.0335  0.5761 0.44861    
    ## aoasl                         1  0.0014  0.0014  0.0241 0.87676    
    ## aoi:direction                 2  0.5146  0.2573  4.4233 0.01303 *  
    ## aoi:hearing                   2  0.4743  0.2372  4.0773 0.01819 *  
    ## direction:hearing             1  0.0011  0.0011  0.0188 0.89107    
    ## aoi:aoasl                     2  0.2237  0.1118  1.9229 0.14851    
    ## direction:aoasl               1  0.0004  0.0004  0.0072 0.93235    
    ## hearing:aoasl                 1  0.0076  0.0076  0.1312 0.71750    
    ## aoi:direction:hearing         2  0.0225  0.0112  0.1931 0.82454    
    ## aoi:direction:aoasl           2  0.0094  0.0047  0.0812 0.92201    
    ## aoi:hearing:aoasl             2  0.3407  0.1704  2.9289 0.05544 .  
    ## direction:hearing:aoasl       1  0.0003  0.0003  0.0056 0.94028    
    ## aoi:direction:hearing:aoasl   2  0.0123  0.0062  0.1060 0.89948    
    ## Residuals                   231 13.4360  0.0582                    
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
    ##     Min      1Q  Median      3Q     Max 
    ## -0.5546 -0.1502 -0.0427  0.1484  0.6468 
    ## 
    ## Coefficients:
    ##                                                   Estimate Std. Error
    ## (Intercept)                                      0.1054104  0.0703357
    ## aoimouth                                         0.5542274  0.0975009
    ## aoichin                                          0.1225277  0.0975009
    ## directionreversed                                0.0669363  0.1018497
    ## aoasl                                            0.0015688  0.0110102
    ## hearingHearing                                   0.4649091  0.3602292
    ## aoimouth:directionreversed                      -0.2120001  0.1396535
    ## aoichin:directionreversed                       -0.0075729  0.1399773
    ## aoimouth:aoasl                                   0.0077295  0.0154359
    ## aoichin:aoasl                                   -0.0133529  0.0154359
    ## directionreversed:aoasl                          0.0005422  0.0160750
    ## aoimouth:hearingHearing                         -0.7034286  0.5090603
    ## aoichin:hearingHearing                          -0.9148085  0.5090603
    ## directionreversed:hearingHearing                -0.1158637  0.5257094
    ## aoasl:hearingHearing                            -0.0210483  0.0227167
    ## aoimouth:directionreversed:aoasl                 0.0065929  0.0222818
    ## aoichin:directionreversed:aoasl                 -0.0057121  0.0222896
    ## aoimouth:directionreversed:hearingHearing        0.2256911  0.7315299
    ## aoichin:directionreversed:hearingHearing         0.1269789  0.7315918
    ## aoimouth:aoasl:hearingHearing                    0.0216536  0.0320611
    ## aoichin:aoasl:hearingHearing                     0.0519979  0.0320611
    ## directionreversed:aoasl:hearingHearing           0.0034674  0.0333375
    ## aoimouth:directionreversed:aoasl:hearingHearing -0.0167627  0.0462505
    ## aoichin:directionreversed:aoasl:hearingHearing   0.0025121  0.0462543
    ##                                                 t value Pr(>|t|)    
    ## (Intercept)                                       1.499   0.1353    
    ## aoimouth                                          5.684 3.94e-08 ***
    ## aoichin                                           1.257   0.2101    
    ## directionreversed                                 0.657   0.5117    
    ## aoasl                                             0.142   0.8868    
    ## hearingHearing                                    1.291   0.1981    
    ## aoimouth:directionreversed                       -1.518   0.1304    
    ## aoichin:directionreversed                        -0.054   0.9569    
    ## aoimouth:aoasl                                    0.501   0.6170    
    ## aoichin:aoasl                                    -0.865   0.3879    
    ## directionreversed:aoasl                           0.034   0.9731    
    ## aoimouth:hearingHearing                          -1.382   0.1684    
    ## aoichin:hearingHearing                           -1.797   0.0736 .  
    ## directionreversed:hearingHearing                 -0.220   0.8258    
    ## aoasl:hearingHearing                             -0.927   0.3551    
    ## aoimouth:directionreversed:aoasl                  0.296   0.7676    
    ## aoichin:directionreversed:aoasl                  -0.256   0.7980    
    ## aoimouth:directionreversed:hearingHearing         0.309   0.7580    
    ## aoichin:directionreversed:hearingHearing          0.174   0.8624    
    ## aoimouth:aoasl:hearingHearing                     0.675   0.5001    
    ## aoichin:aoasl:hearingHearing                      1.622   0.1062    
    ## directionreversed:aoasl:hearingHearing            0.104   0.9173    
    ## aoimouth:directionreversed:aoasl:hearingHearing  -0.362   0.7174    
    ## aoichin:directionreversed:aoasl:hearingHearing    0.054   0.9567    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2412 on 231 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.4404, Adjusted R-squared:  0.3847 
    ## F-statistic: 7.905 on 23 and 231 DF,  p-value: < 2.2e-16

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
data.lr$maingroup <- relevel(data.lr$maingroup, ref="NativeDeaf")

data.lr.item <- data.lr # save item-level data for later

# Pull out and save subject info
data.lr.subjectinfo <- data.lr %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()

# Now collapse data.big5 to subject-level 
data.lr <- data.lr %>%
  group_by(participant,direction,aoi) %>%
  summarize(percent = mean(percent,na.rm=TRUE))
data.lr[data.lr=="NaN"] <- NA

# Join subject info with data.big5 that's now subject-level
data.lr <- left_join(data.lr,data.lr.subjectinfo, by=c("participant","direction"))
```

Problem is, I can already tell this dataset is rather sparse. There are 110 empty cells out of 174. Soooo. Let's give this a shot anyway but probably not a good idea? The graph below, I changed the colors so they map on left/right AOI, and each facet is direction. So we can directly compare L/R biases.

``` r
ggplot(data.lr) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_wrap("direction") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 110 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-34-1.png)

Let's try the group ANOVA and the AoASL ANCOVAs. Group ANOVA first...nothing significant here.

``` r
group.lranova <- aov(data=data.lr,percent ~ aoi * direction * maingroup)
anova(group.lranova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                         Df   Sum Sq    Mean Sq F value Pr(>F)
    ## aoi                      1 0.000028 0.00002808  0.0138 0.9070
    ## direction                1 0.000515 0.00051538  0.2533 0.6172
    ## maingroup                4 0.009258 0.00231447  1.1375 0.3511
    ## aoi:direction            1 0.000070 0.00006976  0.0343 0.8539
    ## aoi:maingroup            4 0.006482 0.00162044  0.7964 0.5338
    ## direction:maingroup      4 0.004085 0.00102124  0.5019 0.7344
    ## aoi:direction:maingroup  3 0.004902 0.00163408  0.8031 0.4987
    ## Residuals               45 0.091564 0.00203476

But the ANCOVA here shows some almost significant effects of hearing, and significant AOI:Hearing and AOASL:Hearing interactions.

``` r
continuous.lranova <- aov(data=data.lr,percent ~ aoi * direction * aoasl * hearing)
anova(continuous.lranova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                             Df   Sum Sq   Mean Sq F value  Pr(>F)  
    ## aoi                          1 0.000028 0.0000281  0.0183 0.89297  
    ## direction                    1 0.000515 0.0005154  0.3358 0.56498  
    ## aoasl                        1 0.001121 0.0011211  0.7304 0.39699  
    ## hearing                      1 0.005802 0.0058019  3.7801 0.05774 .
    ## aoi:direction                1 0.000067 0.0000667  0.0435 0.83576  
    ## aoi:aoasl                    1 0.000077 0.0000765  0.0499 0.82423  
    ## direction:aoasl              1 0.000023 0.0000229  0.0149 0.90339  
    ## aoi:hearing                  1 0.008868 0.0088681  5.7778 0.02014 *
    ## direction:hearing            1 0.000849 0.0008490  0.5532 0.46065  
    ## aoasl:hearing                1 0.010935 0.0109350  7.1245 0.01034 *
    ## aoi:direction:aoasl          1 0.000422 0.0004216  0.2747 0.60263  
    ## aoi:direction:hearing        1 0.002502 0.0025021  1.6302 0.20782  
    ## aoi:aoasl:hearing            1 0.002966 0.0029658  1.9323 0.17092  
    ## direction:aoasl:hearing      1 0.003656 0.0036563  2.3822 0.12929  
    ## aoi:direction:aoasl:hearing  1 0.005401 0.0054009  3.5189 0.06676 .
    ## Residuals                   48 0.073673 0.0015348                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

So that's interesting. Let's plot those out. Again, pay attention to the legend. And ahh, there's one huge outlier point with 0.30% (forward, right AOI, hearing person who learned ASL around age 12) which could be throwing off the entire stats too. Still, it's also interesting the deaf group has an increasing bias to the left the later they learn ASL. And I'm suspecting there is no general bias for hearing signers (once we fix that outlier).

``` r
ggplot(data.lr, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=aoi,shape=hearing)) +
  geom_smooth(aes(color=aoi,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("direction")
```

    ## Warning: Removed 110 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 110 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-37-1.png)

Assorted/older stuff pushed to the bottom
=========================================

> Then when you do a multiple regression analysis, which will be looking at whether gaze behavior can be used to predict accuracy on lexical recall, this will have hearing status, AoA, lexical recall accuracy …. For reversed and not forward? You can’t put both in. And a few of the AOI measures, maybe just one. Maybe a looking-ratio. Maybe a measure of scatter? I don't know. That's where viewing space comes in, and that's saved for later. If we end up saving this for later, that's fine.

This will go into a separate data notebook (04).

Let's jump straight to a big linear mixed model for the Big 5. We'll try both groups and regressing on AoA. Here are the ANOVA tables in order: 1. Linear model (no random terms) with MainGroups 1. Linear mixed model with MainGroups 1. Linear model (no random terms) with AoASL and Hearing 1. Linear mixed model with AoASL and Hearing

But that can be complicated because of so many possible interactions (groups x aois x direction x hearing) in the posthoc analyses. We'll try separating for direction. Because we think there is no difference among groups for forward, but there should be for reverse.

What if we use AoA as linear and then deaf/hearing
