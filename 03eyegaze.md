Eye Gaze Analysis (study1adults)
================
Adam Stone, PhD
10-16-2017

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

    ## Warning: Removed 906 rows containing non-finite values (stat_bin).

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
    ## 1    Cinderella          4          7    52
    ## 2    Goldilocks          3          4    52
    ## 3     KingMidas          4          6    52
    ## 4 RedRidingHood          4          5    52

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
    ##  5    55    Deaf    Group 1     KingMidas   forward  7.17
    ##  6    55    Deaf    Group 1 RedRidingHood  reversed  0.56
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

So 15 stories were dropped from the previous total of 208 stories for a new total of 193 stories.

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

    ## Warning: Removed 835 rows containing non-finite values (stat_boxplot).

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

    ## Warning: Removed 835 rows containing non-finite values (stat_bin).

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

    ## Warning: Removed 130 rows containing non-finite values (stat_boxplot).

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
    ## aoi                       4 37.650  9.4124 277.8487 < 2.2e-16 ***
    ## direction                 1  0.001  0.0010   0.0290  0.864801    
    ## maingroup                 4  0.022  0.0054   0.1598  0.958550    
    ## aoi:direction             4  0.610  0.1525   4.5009  0.001337 ** 
    ## aoi:maingroup            16  3.128  0.1955   5.7715 5.653e-12 ***
    ## direction:maingroup       4  0.001  0.0002   0.0049  0.999951    
    ## aoi:direction:maingroup  16  0.532  0.0332   0.9810  0.475517    
    ## Residuals               785 26.593  0.0339                       
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
    ## upperchest-eyes     -0.183156836 -0.23643303 -0.129880643 0.0000000
    ## chin-eyes           -0.059431532 -0.11099557 -0.007867497 0.0145279
    ## mouth-eyes           0.385833474  0.33433575  0.437331200 0.0000000
    ## forehead-eyes       -0.176672876 -0.24064552 -0.112700236 0.0000000
    ## chin-upperchest      0.123725305  0.07064596  0.176804650 0.0000000
    ## mouth-upperchest     0.568990311  0.51597538  0.622005242 0.0000000
    ## forehead-upperchest  0.006483961 -0.05871624  0.071684158 0.9988007
    ## mouth-chin           0.445265006  0.39397095  0.496559060 0.0000000
    ## forehead-chin       -0.117241344 -0.18105014 -0.053432545 0.0000062
    ## forehead-mouth      -0.562506350 -0.62626158 -0.498751124 0.0000000

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

    ## Warning: Removed 130 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 130 rows containing missing values (geom_point).

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
    ## aoi                           4 37.650  9.4124 279.4510 < 2.2e-16 ***
    ## direction                     1  0.001  0.0010   0.0292 0.8644150    
    ## hearing                       1  0.011  0.0111   0.3287 0.5665660    
    ## aoasl                         1  0.001  0.0014   0.0405 0.8406232    
    ## aoi:direction                 4  0.610  0.1524   4.5260 0.0012773 ** 
    ## aoi:hearing                   4  1.588  0.3970  11.7868 2.634e-09 ***
    ## direction:hearing             1  0.000  0.0002   0.0058 0.9393747    
    ## aoi:aoasl                     4  0.779  0.1947   5.7792 0.0001378 ***
    ## direction:aoasl               1  0.000  0.0002   0.0047 0.9455099    
    ## hearing:aoasl                 1  0.007  0.0071   0.2104 0.6466018    
    ## aoi:direction:hearing         4  0.127  0.0318   0.9441 0.4376885    
    ## aoi:direction:aoasl           4  0.044  0.0111   0.3289 0.8586560    
    ## aoi:hearing:aoasl             4  0.881  0.2203   6.5394 3.523e-05 ***
    ## direction:hearing:aoasl       1  0.002  0.0018   0.0547 0.8150637    
    ## aoi:direction:hearing:aoasl   4  0.057  0.0143   0.4246 0.7909800    
    ## Residuals                   795 26.777  0.0337                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Maybe it's a problem that all the hearing people have late AoASL while most deaf have early AoASL. And I don't like the forehead outliers throwing off some of those lines from the graph above so that needs to be looked at. What if we tried sign-years instead of AoASL.

``` r
ggplot(data.big5, aes(x=signyrs,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 130 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 130 rows containing missing values (geom_point).

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
    ## aoi                             4 37.650  9.4124 270.4292 < 2.2e-16 ***
    ## direction                       1  0.001  0.0010   0.0282  0.866601    
    ## hearing                         1  0.011  0.0111   0.3181  0.572896    
    ## signyrs                         1  0.000  0.0000   0.0000  0.998426    
    ## aoi:direction                   4  0.609  0.1524   4.3775  0.001659 ** 
    ## aoi:hearing                     4  1.588  0.3971  11.4084 5.229e-09 ***
    ## direction:hearing               1  0.000  0.0002   0.0051  0.943132    
    ## aoi:signyrs                     4  0.360  0.0900   2.5851  0.035864 *  
    ## direction:signyrs               1  0.001  0.0007   0.0209  0.885031    
    ## hearing:signyrs                 1  0.003  0.0026   0.0747  0.784714    
    ## aoi:direction:hearing           4  0.137  0.0342   0.9823  0.416347    
    ## aoi:direction:signyrs           4  0.049  0.0122   0.3504  0.843838    
    ## aoi:hearing:signyrs             4  0.352  0.0880   2.5290  0.039347 *  
    ## direction:hearing:signyrs       1  0.000  0.0000   0.0000  0.996890    
    ## aoi:direction:hearing:signyrs   4  0.105  0.0262   0.7520  0.556784    
    ## Residuals                     795 27.670  0.0348                       
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
    ## aoi                       2 22.4829 11.2414 235.4935 < 2.2e-16 ***
    ## direction                 1  0.0177  0.0177   0.3708  0.542813    
    ## maingroup                 4  0.0745  0.0186   0.3904  0.815562    
    ## aoi:direction             2  0.5693  0.2846   5.9626  0.002744 ** 
    ## aoi:maingroup             8  2.9754  0.3719   7.7914 6.296e-10 ***
    ## direction:maingroup       4  0.0027  0.0007   0.0142  0.999603    
    ## aoi:direction:maingroup   8  0.4479  0.0560   1.1728  0.313342    
    ## Residuals               544 25.9682  0.0477                       
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
    ## mouth-eyes  0.38583347  0.3332877  0.438379268 0.0000000
    ## chin-eyes  -0.05943153 -0.1120450 -0.006818079 0.0222245
    ## chin-mouth -0.44526501 -0.4976030 -0.392927030 0.0000000

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
    ##                                      diff           lwr          upr
    ## mouth:forward-eyes:forward    0.435352506  0.3460887380  0.524616274
    ## chin:forward-eyes:forward    -0.084112698 -0.1733764664  0.005151070
    ## eyes:reversed-eyes:forward    0.005940244 -0.0850239393  0.096904427
    ## mouth:reversed-eyes:forward   0.340561238  0.2505955158  0.430526960
    ## chin:reversed-eyes:forward   -0.027858161 -0.1180665454  0.062350223
    ## chin:forward-mouth:forward   -0.519465204 -0.6087289724 -0.430201436
    ## eyes:reversed-mouth:forward  -0.429412262 -0.5203764452 -0.338448079
    ## mouth:reversed-mouth:forward -0.094791268 -0.1847569901 -0.004825546
    ## chin:reversed-mouth:forward  -0.463210667 -0.5534190513 -0.373002282
    ## eyes:reversed-chin:forward    0.090052942 -0.0009112408  0.181017125
    ## mouth:reversed-chin:forward   0.424673936  0.3347082143  0.514639658
    ## chin:reversed-chin:forward    0.056254537 -0.0339538469  0.146462922
    ## mouth:reversed-eyes:reversed  0.334620994  0.2429678793  0.426274109
    ## chin:reversed-eyes:reversed  -0.033798405 -0.1256897260  0.058092917
    ## chin:reversed-mouth:reversed -0.368419399 -0.4593224430 -0.277516354
    ##                                  p adj
    ## mouth:forward-eyes:forward   0.0000000
    ## chin:forward-eyes:forward    0.0778777
    ## eyes:reversed-eyes:forward   0.9999686
    ## mouth:reversed-eyes:forward  0.0000000
    ## chin:reversed-eyes:forward   0.9504686
    ## chin:forward-mouth:forward   0.0000000
    ## eyes:reversed-mouth:forward  0.0000000
    ## mouth:reversed-mouth:forward 0.0321675
    ## chin:reversed-mouth:forward  0.0000000
    ## eyes:reversed-chin:forward   0.0541257
    ## mouth:reversed-chin:forward  0.0000000
    ## chin:reversed-chin:forward   0.4773258
    ## mouth:reversed-eyes:reversed 0.0000000
    ## chin:reversed-eyes:reversed  0.8999180
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
    ##                                                      diff          lwr
    ## mouth:DeafNative-eyes:DeafNative              0.474574908  0.318498712
    ## chin:DeafNative-eyes:DeafNative               0.077299413 -0.078776783
    ## eyes:DeafEarlyASL-eyes:DeafNative             0.043526942 -0.137681847
    ## mouth:DeafEarlyASL-eyes:DeafNative            0.497969051  0.318794096
    ## chin:DeafEarlyASL-eyes:DeafNative            -0.006862945 -0.188071734
    ## eyes:DeafLateASL-eyes:DeafNative              0.035079618 -0.140375037
    ## mouth:DeafLateASL-eyes:DeafNative             0.615784253  0.440329598
    ## chin:DeafLateASL-eyes:DeafNative             -0.106752942 -0.282207597
    ## eyes:HearingLateASL-eyes:DeafNative           0.178202385  0.019456674
    ## mouth:HearingLateASL-eyes:DeafNative          0.344587247  0.187655832
    ## chin:HearingLateASL-eyes:DeafNative          -0.014524920 -0.171456334
    ## eyes:HearingNoviceASL-eyes:DeafNative         0.048634090 -0.109186461
    ## mouth:HearingNoviceASL-eyes:DeafNative        0.386298953  0.228478401
    ## chin:HearingNoviceASL-eyes:DeafNative         0.029455828 -0.128364724
    ## chin:DeafNative-mouth:DeafNative             -0.397275495 -0.552491761
    ## eyes:DeafEarlyASL-mouth:DeafNative           -0.431047966 -0.611516621
    ## mouth:DeafEarlyASL-mouth:DeafNative           0.023394143 -0.155032242
    ## chin:DeafEarlyASL-mouth:DeafNative           -0.481437853 -0.661906508
    ## eyes:DeafLateASL-mouth:DeafNative            -0.439495290 -0.614185433
    ## mouth:DeafLateASL-mouth:DeafNative            0.141209345 -0.033480799
    ## chin:DeafLateASL-mouth:DeafNative            -0.581327850 -0.756017994
    ## eyes:HearingLateASL-mouth:DeafNative         -0.296372523 -0.454272842
    ## mouth:HearingLateASL-mouth:DeafNative        -0.129987661 -0.286063857
    ## chin:HearingLateASL-mouth:DeafNative         -0.489099828 -0.645176024
    ## eyes:HearingNoviceASL-mouth:DeafNative       -0.425940818 -0.582910995
    ## mouth:HearingNoviceASL-mouth:DeafNative      -0.088275955 -0.245246133
    ## chin:HearingNoviceASL-mouth:DeafNative       -0.445119080 -0.602089258
    ## eyes:DeafEarlyASL-chin:DeafNative            -0.033772471 -0.214241126
    ## mouth:DeafEarlyASL-chin:DeafNative            0.420669638  0.242243253
    ## chin:DeafEarlyASL-chin:DeafNative            -0.084162358 -0.264631013
    ## eyes:DeafLateASL-chin:DeafNative             -0.042219795 -0.216909938
    ## mouth:DeafLateASL-chin:DeafNative             0.538484840  0.363794696
    ## chin:DeafLateASL-chin:DeafNative             -0.184052355 -0.358742499
    ## eyes:HearingLateASL-chin:DeafNative           0.100902972 -0.056997347
    ## mouth:HearingLateASL-chin:DeafNative          0.267287834  0.111211638
    ## chin:HearingLateASL-chin:DeafNative          -0.091824333 -0.247900529
    ## eyes:HearingNoviceASL-chin:DeafNative        -0.028665323 -0.185635500
    ## mouth:HearingNoviceASL-chin:DeafNative        0.308999540  0.152029362
    ## chin:HearingNoviceASL-chin:DeafNative        -0.047843585 -0.204813763
    ## mouth:DeafEarlyASL-eyes:DeafEarlyASL          0.454442109  0.253661580
    ## chin:DeafEarlyASL-eyes:DeafEarlyASL          -0.050389887 -0.252987472
    ## eyes:DeafLateASL-eyes:DeafEarlyASL           -0.008447324 -0.205915023
    ## mouth:DeafLateASL-eyes:DeafEarlyASL           0.572257311  0.374789611
    ## chin:DeafLateASL-eyes:DeafEarlyASL           -0.150279884 -0.347747584
    ## eyes:HearingLateASL-eyes:DeafEarlyASL         0.134675443 -0.048106824
    ## mouth:HearingLateASL-eyes:DeafEarlyASL        0.301060305  0.119851516
    ## chin:HearingLateASL-eyes:DeafEarlyASL        -0.058051862 -0.239260651
    ## eyes:HearingNoviceASL-eyes:DeafEarlyASL       0.005107148 -0.176872199
    ## mouth:HearingNoviceASL-eyes:DeafEarlyASL      0.342772011  0.160792664
    ## chin:HearingNoviceASL-eyes:DeafEarlyASL      -0.014071114 -0.196050462
    ## chin:DeafEarlyASL-mouth:DeafEarlyASL         -0.504831996 -0.705612526
    ## eyes:DeafLateASL-mouth:DeafEarlyASL          -0.462889433 -0.658492429
    ## mouth:DeafLateASL-mouth:DeafEarlyASL          0.117815202 -0.077787794
    ## chin:DeafLateASL-mouth:DeafEarlyASL          -0.604721993 -0.800324989
    ## eyes:HearingLateASL-mouth:DeafEarlyASL       -0.319766666 -0.500532804
    ## mouth:HearingLateASL-mouth:DeafEarlyASL      -0.153381804 -0.332556760
    ## chin:HearingLateASL-mouth:DeafEarlyASL       -0.512493971 -0.691668927
    ## eyes:HearingNoviceASL-mouth:DeafEarlyASL     -0.449334961 -0.629289184
    ## mouth:HearingNoviceASL-mouth:DeafEarlyASL    -0.111670099 -0.291624322
    ## chin:HearingNoviceASL-mouth:DeafEarlyASL     -0.468513224 -0.648467447
    ## eyes:DeafLateASL-chin:DeafEarlyASL            0.041942563 -0.155525136
    ## mouth:DeafLateASL-chin:DeafEarlyASL           0.622647198  0.425179498
    ## chin:DeafLateASL-chin:DeafEarlyASL           -0.099889997 -0.297357697
    ## eyes:HearingLateASL-chin:DeafEarlyASL         0.185065330  0.002283063
    ## mouth:HearingLateASL-chin:DeafEarlyASL        0.351450192  0.170241403
    ## chin:HearingLateASL-chin:DeafEarlyASL        -0.007661975 -0.188870764
    ## eyes:HearingNoviceASL-chin:DeafEarlyASL       0.055497035 -0.126482312
    ## mouth:HearingNoviceASL-chin:DeafEarlyASL      0.393161898  0.211182551
    ## chin:HearingNoviceASL-chin:DeafEarlyASL       0.036318773 -0.145660574
    ## mouth:DeafLateASL-eyes:DeafLateASL            0.580704635  0.388503689
    ## chin:DeafLateASL-eyes:DeafLateASL            -0.141832560 -0.334033506
    ## eyes:HearingLateASL-eyes:DeafLateASL          0.143122767 -0.033956504
    ## mouth:HearingLateASL-eyes:DeafLateASL         0.309507629  0.134052973
    ## chin:HearingLateASL-eyes:DeafLateASL         -0.049604538 -0.225059193
    ## eyes:HearingNoviceASL-eyes:DeafLateASL        0.013554472 -0.162695900
    ## mouth:HearingNoviceASL-eyes:DeafLateASL       0.351219334  0.174968962
    ## chin:HearingNoviceASL-eyes:DeafLateASL       -0.005623791 -0.181874163
    ## chin:DeafLateASL-mouth:DeafLateASL           -0.722537195 -0.914738140
    ## eyes:HearingLateASL-mouth:DeafLateASL        -0.437581868 -0.614661138
    ## mouth:HearingLateASL-mouth:DeafLateASL       -0.271197006 -0.446651662
    ## chin:HearingLateASL-mouth:DeafLateASL        -0.630309173 -0.805763828
    ## eyes:HearingNoviceASL-mouth:DeafLateASL      -0.567150163 -0.743400535
    ## mouth:HearingNoviceASL-mouth:DeafLateASL     -0.229485300 -0.405735672
    ## chin:HearingNoviceASL-mouth:DeafLateASL      -0.586328425 -0.762578798
    ## eyes:HearingLateASL-chin:DeafLateASL          0.284955327  0.107876057
    ## mouth:HearingLateASL-chin:DeafLateASL         0.451340189  0.275885533
    ## chin:HearingLateASL-chin:DeafLateASL          0.092228022 -0.083226633
    ## eyes:HearingNoviceASL-chin:DeafLateASL        0.155387032 -0.020863340
    ## mouth:HearingNoviceASL-chin:DeafLateASL       0.493051895  0.316801523
    ## chin:HearingNoviceASL-chin:DeafLateASL        0.136208770 -0.040041602
    ## mouth:HearingLateASL-eyes:HearingLateASL      0.166384862  0.007639151
    ## chin:HearingLateASL-eyes:HearingLateASL      -0.192727305 -0.351473016
    ## eyes:HearingNoviceASL-eyes:HearingLateASL    -0.129568295 -0.289193037
    ## mouth:HearingNoviceASL-eyes:HearingLateASL    0.208096568  0.048471826
    ## chin:HearingNoviceASL-eyes:HearingLateASL    -0.148746557 -0.308371300
    ## chin:HearingLateASL-mouth:HearingLateASL     -0.359112167 -0.516043581
    ## eyes:HearingNoviceASL-mouth:HearingLateASL   -0.295953157 -0.453773708
    ## mouth:HearingNoviceASL-mouth:HearingLateASL   0.041711706 -0.116108846
    ## chin:HearingNoviceASL-mouth:HearingLateASL   -0.315131419 -0.472951971
    ## eyes:HearingNoviceASL-chin:HearingLateASL     0.063159010 -0.094661542
    ## mouth:HearingNoviceASL-chin:HearingLateASL    0.400823872  0.243003321
    ## chin:HearingNoviceASL-chin:HearingLateASL     0.043980747 -0.113839804
    ## mouth:HearingNoviceASL-eyes:HearingNoviceASL  0.337664862  0.178960155
    ## chin:HearingNoviceASL-eyes:HearingNoviceASL  -0.019178263 -0.177882970
    ## chin:HearingNoviceASL-mouth:HearingNoviceASL -0.356843125 -0.515547832
    ##                                                       upr     p adj
    ## mouth:DeafNative-eyes:DeafNative              0.630651104 0.0000000
    ## chin:DeafNative-eyes:DeafNative               0.233375609 0.9363322
    ## eyes:DeafEarlyASL-eyes:DeafNative             0.224735731 0.9999655
    ## mouth:DeafEarlyASL-eyes:DeafNative            0.677144007 0.0000000
    ## chin:DeafEarlyASL-eyes:DeafNative             0.174345844 1.0000000
    ## eyes:DeafLateASL-eyes:DeafNative              0.210534274 0.9999965
    ## mouth:DeafLateASL-eyes:DeafNative             0.791238909 0.0000000
    ## chin:DeafLateASL-eyes:DeafNative              0.068701714 0.7523917
    ## eyes:HearingLateASL-eyes:DeafNative           0.336948096 0.0121752
    ## mouth:HearingLateASL-eyes:DeafNative          0.501518662 0.0000000
    ## chin:HearingLateASL-eyes:DeafNative           0.142406495 1.0000000
    ## eyes:HearingNoviceASL-eyes:DeafNative         0.206454642 0.9993502
    ## mouth:HearingNoviceASL-eyes:DeafNative        0.544119505 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafNative         0.187276379 0.9999986
    ## chin:DeafNative-mouth:DeafNative             -0.242059229 0.0000000
    ## eyes:DeafEarlyASL-mouth:DeafNative           -0.250579311 0.0000000
    ## mouth:DeafEarlyASL-mouth:DeafNative           0.201820529 1.0000000
    ## chin:DeafEarlyASL-mouth:DeafNative           -0.300969198 0.0000000
    ## eyes:DeafLateASL-mouth:DeafNative            -0.264805146 0.0000000
    ## mouth:DeafLateASL-mouth:DeafNative            0.315899488 0.2755293
    ## chin:DeafLateASL-mouth:DeafNative            -0.406637707 0.0000000
    ## eyes:HearingLateASL-mouth:DeafNative         -0.138472204 0.0000000
    ## mouth:HearingLateASL-mouth:DeafNative         0.026088535 0.2301160
    ## chin:HearingLateASL-mouth:DeafNative         -0.333023631 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafNative       -0.268970640 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafNative       0.068694222 0.8443631
    ## chin:HearingNoviceASL-mouth:DeafNative       -0.288148903 0.0000000
    ## eyes:DeafEarlyASL-chin:DeafNative             0.146696184 0.9999985
    ## mouth:DeafEarlyASL-chin:DeafNative            0.599096024 0.0000000
    ## chin:DeafEarlyASL-chin:DeafNative             0.096306297 0.9606310
    ## eyes:DeafLateASL-chin:DeafNative              0.132470349 0.9999628
    ## mouth:DeafLateASL-chin:DeafNative             0.713174984 0.0000000
    ## chin:DeafLateASL-chin:DeafNative             -0.009362211 0.0277255
    ## eyes:HearingLateASL-chin:DeafNative           0.258803291 0.6811456
    ## mouth:HearingLateASL-chin:DeafNative          0.423364030 0.0000010
    ## chin:HearingLateASL-chin:DeafNative           0.064251864 0.7951331
    ## eyes:HearingNoviceASL-chin:DeafNative         0.128304855 0.9999989
    ## mouth:HearingNoviceASL-chin:DeafNative        0.465969717 0.0000000
    ## chin:HearingNoviceASL-chin:DeafNative         0.109126592 0.9994258
    ## mouth:DeafEarlyASL-eyes:DeafEarlyASL          0.655222639 0.0000000
    ## chin:DeafEarlyASL-eyes:DeafEarlyASL           0.152207698 0.9999473
    ## eyes:DeafLateASL-eyes:DeafEarlyASL            0.189020376 1.0000000
    ## mouth:DeafLateASL-eyes:DeafEarlyASL           0.769725011 0.0000000
    ## chin:DeafLateASL-eyes:DeafEarlyASL            0.047187816 0.3769409
    ## eyes:HearingLateASL-eyes:DeafEarlyASL         0.317457710 0.4346954
    ## mouth:HearingLateASL-eyes:DeafEarlyASL        0.482269094 0.0000025
    ## chin:HearingLateASL-eyes:DeafEarlyASL         0.123156927 0.9989988
    ## eyes:HearingNoviceASL-eyes:DeafEarlyASL       0.187086496 1.0000000
    ## mouth:HearingNoviceASL-eyes:DeafEarlyASL      0.524751358 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafEarlyASL       0.167908233 1.0000000
    ## chin:DeafEarlyASL-mouth:DeafEarlyASL         -0.304051467 0.0000000
    ## eyes:DeafLateASL-mouth:DeafEarlyASL          -0.267286437 0.0000000
    ## mouth:DeafLateASL-mouth:DeafEarlyASL          0.313418198 0.7657850
    ## chin:DeafLateASL-mouth:DeafEarlyASL          -0.409118997 0.0000000
    ## eyes:HearingLateASL-mouth:DeafEarlyASL       -0.139000528 0.0000003
    ## mouth:HearingLateASL-mouth:DeafEarlyASL       0.025793151 0.1919518
    ## chin:HearingLateASL-mouth:DeafEarlyASL       -0.333319015 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafEarlyASL     -0.269380738 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafEarlyASL     0.068284125 0.7249762
    ## chin:HearingNoviceASL-mouth:DeafEarlyASL     -0.288559000 0.0000000
    ## eyes:DeafLateASL-chin:DeafEarlyASL            0.239410263 0.9999926
    ## mouth:DeafLateASL-chin:DeafEarlyASL           0.820114898 0.0000000
    ## chin:DeafLateASL-chin:DeafEarlyASL            0.097577703 0.9252436
    ## eyes:HearingLateASL-chin:DeafEarlyASL         0.367847597 0.0437567
    ## mouth:HearingLateASL-chin:DeafEarlyASL        0.532658981 0.0000000
    ## chin:HearingLateASL-chin:DeafEarlyASL         0.173546814 1.0000000
    ## eyes:HearingNoviceASL-chin:DeafEarlyASL       0.237476383 0.9994222
    ## mouth:HearingNoviceASL-chin:DeafEarlyASL      0.575141245 0.0000000
    ## chin:HearingNoviceASL-chin:DeafEarlyASL       0.218298120 0.9999966
    ## mouth:DeafLateASL-eyes:DeafLateASL            0.772905580 0.0000000
    ## chin:DeafLateASL-eyes:DeafLateASL             0.050368385 0.4319309
    ## eyes:HearingLateASL-eyes:DeafLateASL          0.320202037 0.2757263
    ## mouth:HearingLateASL-eyes:DeafLateASL         0.484962284 0.0000004
    ## chin:HearingLateASL-eyes:DeafLateASL          0.125850117 0.9997576
    ## eyes:HearingNoviceASL-eyes:DeafLateASL        0.189804844 1.0000000
    ## mouth:HearingNoviceASL-eyes:DeafLateASL       0.527469707 0.0000000
    ## chin:HearingNoviceASL-eyes:DeafLateASL        0.170626582 1.0000000
    ## chin:DeafLateASL-mouth:DeafLateASL           -0.530336250 0.0000000
    ## eyes:HearingLateASL-mouth:DeafLateASL        -0.260502598 0.0000000
    ## mouth:HearingLateASL-mouth:DeafLateASL       -0.095742351 0.0000205
    ## chin:HearingLateASL-mouth:DeafLateASL        -0.454854517 0.0000000
    ## eyes:HearingNoviceASL-mouth:DeafLateASL      -0.390899790 0.0000000
    ## mouth:HearingNoviceASL-mouth:DeafLateASL     -0.053234928 0.0010539
    ## chin:HearingNoviceASL-mouth:DeafLateASL      -0.410078053 0.0000000
    ## eyes:HearingLateASL-chin:DeafLateASL          0.462034597 0.0000066
    ## mouth:HearingLateASL-chin:DeafLateASL         0.626794844 0.0000000
    ## chin:HearingLateASL-chin:DeafLateASL          0.267682678 0.9011236
    ## eyes:HearingNoviceASL-chin:DeafLateASL        0.331637405 0.1552984
    ## mouth:HearingNoviceASL-chin:DeafLateASL       0.669302267 0.0000000
    ## chin:HearingNoviceASL-chin:DeafLateASL        0.312459142 0.3501470
    ## mouth:HearingLateASL-eyes:HearingLateASL      0.325130573 0.0295062
    ## chin:HearingLateASL-eyes:HearingLateASL      -0.033981594 0.0036853
    ## eyes:HearingNoviceASL-eyes:HearingLateASL     0.030056448 0.2689911
    ## mouth:HearingNoviceASL-eyes:HearingLateASL    0.367721310 0.0010289
    ## chin:HearingNoviceASL-eyes:HearingLateASL     0.010878185 0.0989176
    ## chin:HearingLateASL-mouth:HearingLateASL     -0.202180752 0.0000000
    ## eyes:HearingNoviceASL-mouth:HearingLateASL   -0.138132605 0.0000000
    ## mouth:HearingNoviceASL-mouth:HearingLateASL   0.199532258 0.9998904
    ## chin:HearingNoviceASL-mouth:HearingLateASL   -0.157310868 0.0000000
    ## eyes:HearingNoviceASL-chin:HearingLateASL     0.220979562 0.9899918
    ## mouth:HearingNoviceASL-chin:HearingLateASL    0.558644424 0.0000000
    ## chin:HearingNoviceASL-chin:HearingLateASL     0.201801299 0.9997951
    ## mouth:HearingNoviceASL-eyes:HearingNoviceASL  0.496369570 0.0000000
    ## chin:HearingNoviceASL-eyes:HearingNoviceASL   0.139526445 1.0000000
    ## chin:HearingNoviceASL-mouth:HearingNoviceASL -0.198138418 0.0000000

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
    ## aoi                           2 22.4829 11.2414 237.1858 < 2.2e-16 ***
    ## direction                     1  0.0177  0.0177   0.3735 0.5413648    
    ## hearing                       1  0.0618  0.0618   1.3046 0.2538754    
    ## aoasl                         1  0.0004  0.0004   0.0086 0.9262453    
    ## aoi:direction                 2  0.5693  0.2846   6.0057 0.0026293 ** 
    ## aoi:hearing                   2  1.5014  0.7507  15.8390 2.051e-07 ***
    ## direction:hearing             1  0.0013  0.0013   0.0280 0.8672355    
    ## aoi:aoasl                     2  0.7587  0.3793   8.0035 0.0003748 ***
    ## direction:aoasl               1  0.0019  0.0019   0.0408 0.8400208    
    ## hearing:aoasl                 1  0.0145  0.0145   0.3070 0.5797585    
    ## aoi:direction:hearing         2  0.1192  0.0596   1.2577 0.2851267    
    ## aoi:direction:aoasl           2  0.0277  0.0138   0.2921 0.7468383    
    ## aoi:hearing:aoasl             2  0.8731  0.4365   9.2105 0.0001163 ***
    ## direction:hearing:aoasl       1  0.0000  0.0000   0.0000 0.9952022    
    ## aoi:direction:hearing:aoasl   2  0.0414  0.0207   0.4368 0.6463417    
    ## Residuals                   550 26.0673  0.0474                       
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
    ## -0.59227 -0.14324 -0.04125  0.11932  0.66925 
    ## 
    ## Coefficients:
    ##                                                   Estimate Std. Error
    ## (Intercept)                                      0.1710373  0.0396375
    ## aoimouth                                         0.4822345  0.0560559
    ## aoichin                                          0.0054908  0.0560559
    ## directionreversed                               -0.0083965  0.0576969
    ## aoasl                                            0.0003404  0.0047879
    ## hearingHearing                                   0.5970053  0.2264499
    ## aoimouth:directionreversed                      -0.0766807  0.0811766
    ## aoichin:directionreversed                        0.0689111  0.0813165
    ## aoimouth:aoasl                                   0.0067044  0.0067711
    ## aoichin:aoasl                                   -0.0091754  0.0067711
    ## directionreversed:aoasl                         -0.0006589  0.0068624
    ## aoimouth:hearingHearing                         -0.9586619  0.3202485
    ## aoichin:hearingHearing                          -0.9954796  0.3202485
    ## directionreversed:hearingHearing                -0.0976334  0.3262254
    ## aoasl:hearingHearing                            -0.0299928  0.0134346
    ## aoimouth:directionreversed:aoasl                 0.0068601  0.0096782
    ## aoichin:directionreversed:aoasl                 -0.0030665  0.0096784
    ## aoimouth:directionreversed:hearingHearing        0.1856661  0.4546684
    ## aoichin:directionreversed:hearingHearing         0.0517414  0.4546933
    ## aoimouth:aoasl:hearingHearing                    0.0393376  0.0189995
    ## aoichin:aoasl:hearingHearing                     0.0580025  0.0189995
    ## directionreversed:aoasl:hearingHearing           0.0075864  0.0194878
    ## aoimouth:directionreversed:aoasl:hearingHearing -0.0218282  0.0271149
    ## aoichin:directionreversed:aoasl:hearingHearing  -0.0002505  0.0271150
    ##                                                 t value Pr(>|t|)    
    ## (Intercept)                                       4.315 1.89e-05 ***
    ## aoimouth                                          8.603  < 2e-16 ***
    ## aoichin                                           0.098  0.92201    
    ## directionreversed                                -0.146  0.88435    
    ## aoasl                                             0.071  0.94335    
    ## hearingHearing                                    2.636  0.00862 ** 
    ## aoimouth:directionreversed                       -0.945  0.34527    
    ## aoichin:directionreversed                         0.847  0.39712    
    ## aoimouth:aoasl                                    0.990  0.32253    
    ## aoichin:aoasl                                    -1.355  0.17595    
    ## directionreversed:aoasl                          -0.096  0.92355    
    ## aoimouth:hearingHearing                          -2.993  0.00288 ** 
    ## aoichin:hearingHearing                           -3.108  0.00198 ** 
    ## directionreversed:hearingHearing                 -0.299  0.76484    
    ## aoasl:hearingHearing                             -2.232  0.02598 *  
    ## aoimouth:directionreversed:aoasl                  0.709  0.47874    
    ## aoichin:directionreversed:aoasl                  -0.317  0.75149    
    ## aoimouth:directionreversed:hearingHearing         0.408  0.68317    
    ## aoichin:directionreversed:hearingHearing          0.114  0.90944    
    ## aoimouth:aoasl:hearingHearing                     2.070  0.03888 *  
    ## aoichin:aoasl:hearingHearing                      3.053  0.00238 ** 
    ## directionreversed:aoasl:hearingHearing            0.389  0.69721    
    ## aoimouth:directionreversed:aoasl:hearingHearing  -0.805  0.42115    
    ## aoichin:directionreversed:aoasl:hearingHearing   -0.009  0.99263    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2177 on 550 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.5038, Adjusted R-squared:  0.4831 
    ## F-statistic: 24.28 on 23 and 550 DF,  p-value: < 2.2e-16

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

Problem is, I can already tell this dataset is rather sparse. There are 239 empty cells out of 386. Soooo. Let's give this a shot anyway but probably not a good idea? The graph below, I changed the colors so they map on left/right AOI, and each facet is direction. So we can directly compare L/R biases.

``` r
ggplot(data.lr) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_wrap("direction") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 239 rows containing non-finite values (stat_boxplot).

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

    ## Warning: Removed 239 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 239 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-36-1.png)

Assorted/older stuff pushed to the bottom
=========================================

> Then when you do a multiple regression analysis, which will be looking at whether gaze behavior can be used to predict accuracy on lexical recall, this will have hearing status, AoA, lexical recall accuracy …. For reversed and not forward? You can’t put both in. And a few of the AOI measures, maybe just one. Maybe a looking-ratio. Maybe a measure of scatter? I don't know. That's where viewing space comes in, and that's saved for later. If we end up saving this for later, that's fine.

This will go into a separate data notebook (04).

Let's jump straight to a big linear mixed model for the Big 5. We'll try both groups and regressing on AoA. Here are the ANOVA tables in order: 1. Linear model (no random terms) with MainGroups 1. Linear mixed model with MainGroups 1. Linear model (no random terms) with AoASL and Hearing 1. Linear mixed model with AoASL and Hearing

But that can be complicated because of so many possible interactions (groups x aois x direction x hearing) in the posthoc analyses. We'll try separating for direction. Because we think there is no difference among groups for forward, but there should be for reverse.

What if we use AoA as linear and then deaf/hearing
