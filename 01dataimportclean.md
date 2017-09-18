Data Import & Cleaning (study1adults)
================
Adam Stone, PhD
09-18-2017

-   [Importing and Reshaping Data](#importing-and-reshaping-data)
-   [Participant Demographics](#participant-demographics)
-   [Save the dataset for further analysis](#save-the-dataset-for-further-analysis)

Importing and Reshaping Data
============================

Here we're going to import the data, remove dropped participants, and reshape the data so story and direction are grouping variables (and the dataset will be more tall than wide). Let's see ALL of our data first (scroll horizontally).

    ## # A tibble: 52 x 54
    ##       id hearing videogroup aoagroup languagegroup    maingroup selfrate
    ##    <int>   <chr>      <chr>    <chr>         <chr>        <chr>    <dbl>
    ##  1     1    Deaf    Group 1    Early      EarlyASL DeafEarlyASL        5
    ##  2     2    Deaf    Group 1    Early      EarlyASL DeafEarlyASL        5
    ##  3     3    Deaf    Group 2    Early      EarlyASL DeafEarlyASL        5
    ##  4     4    Deaf    Group 2    Early      EarlyASL DeafEarlyASL        5
    ##  5     5    Deaf    Group 1    Early      EarlyASL DeafEarlyASL        5
    ##  6     6    Deaf    Group 1    Early      EarlyASL DeafEarlyASL        5
    ##  7     7    Deaf    Group 1    Early      EarlyASL DeafEarlyASL        5
    ##  8     8    Deaf    Group 2    Early      EarlyASL DeafEarlyASL        5
    ##  9     9    Deaf    Group 2    Early       LateASL  DeafLateASL        5
    ## 10    10    Deaf    Group 1    Early       LateASL  DeafLateASL        5
    ## # ... with 42 more rows, and 47 more variables: age <dbl>, signyrs <dbl>,
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

Remove dropped participants (they have no main group name, or no data, or are bad). I'm also removing Sara G for now due to her weird data file. These are who we dropped.

``` r
# Removes those with no maingroup name
nodata <- filter(data, is.na(maingroup)==TRUE) 
# Removes those with no data
manual <- filter(data,participant=="Lucinda" | participant=="Joe" | participant=="Cathy" | participant=="ChrissyG" | participant=="Sara") 
# Make table of all dropped participants
dropped <- rbind(nodata,manual) %>% arrange(id)
data <- data %>%
  anti_join(dropped)
#kable(select(dropped,participant,maingroup,age)) %>% kable_styling(bootstrap_options = c("striped", "hover","condensed"), full_width = F, position = "left")
#select(dropped,-participant)
dropped
```

    ## # A tibble: 8 x 55
    ##      id participant hearing videogroup aoagroup languagegroup
    ##   <int>       <chr>   <chr>      <chr>    <chr>         <chr>
    ## 1     8       Cathy    Deaf    Group 2    Early      EarlyASL
    ## 2    10    ChrissyG    Deaf    Group 1    Early       LateASL
    ## 3    14         Joe    Deaf    Group 2    Early       LateASL
    ## 4    23     Lucinda    Deaf    Group 2   Native        Native
    ## 5    27       Megan Hearing    Group 2     Late      EarlyASL
    ## 6    30        Sara Hearing    Group 1     Late       LateASL
    ## 7    40      Dustin Hearing    Group 2   Native        Native
    ## 8    41         Dan Hearing    Group 1   Native        Native
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

Now we'll reshape the data. Based on Rain's UNM talk, this is what Group 1 & 2 saw: **Note, I've switched the groups around from Rain's UNM talk**

| No. | Group1              | Group2              |
|:----|:--------------------|:--------------------|
| 1   | Goldilocks Fwd      | Red Riding Hood Fwd |
| 2   | Cinderella Rev      | King Midas Rev      |
| 3   | King Midas Fwd      | Cinderella Fwd      |
| 4   | Red Riding Hood Rev | Goldilocks Rev      |

Let's add that information to our data when we reshape it. You can look at the code below if you want.

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
group1 <- mutate(group1,story = ifelse(video == "fw1","Goldilocks",
                                       ifelse(video == "rv2","Cinderella",
                                            ifelse(video=="fw3","KingMidas","RedRidingHood"))))
group1 <- mutate(group1,direction = ifelse(video == "fw1","forward",
                                           ifelse(video == "rv2","reversed",
                                                  ifelse(video == "fw3","forward","reversed"))))
group2 <- mutate(group2,story = ifelse(video == "fw1","RedRidingHood",
                                       ifelse(video == "rv2","KingMidas",
                                              ifelse(video =="fw3","Cinderella","Goldilocks"))))
group2 <- mutate(group2,direction = ifelse(video == "fw1","forward",
                                       ifelse(video == "rv2","reversed",
                                              ifelse(video == "fw3","forward","reversed"))))



# Join groups back together and view
data <- rbind(group1,group2)
data <- arrange(data,id,video)

# Convert some columns to factors, reorder columns, and calculate totals
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
         midchest,lowerchest,belly,left,right) %>%
  group_by(id,story) %>%
  mutate(total = sum(forehead,eyes,mouth,chin,upperchest,
                     midchest,lowerchest,belly,left,right,na.rm=TRUE)) %>%
  ungroup()
```

Here's the final, "cleaned-up" dataset that we're going to use for all further analysis. Just the first 10 rows. Notice that there are now story and direction columns, and AOI-only columns, and each participant has 4 observation rows (one observation per story). So, more rows, less columns.

``` r
#kable(head(data, n=10)) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
select(data,-participant)
```

    ## # A tibble: 176 x 25
    ##       id hearing videogroup aoagroup languagegroup    maingroup  video
    ##    <int>  <fctr>     <fctr>   <fctr>        <fctr>       <fctr> <fctr>
    ##  1     1    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    fw1
    ##  2     1    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    fw3
    ##  3     1    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    rv2
    ##  4     1    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    rv4
    ##  5     2    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    fw1
    ##  6     2    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    fw3
    ##  7     2    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    rv2
    ##  8     2    Deaf    Group 1    Early      EarlyASL DeafEarlyASL    rv4
    ##  9     3    Deaf    Group 2    Early      EarlyASL DeafEarlyASL    fw1
    ## 10     3    Deaf    Group 2    Early      EarlyASL DeafEarlyASL    fw3
    ## # ... with 166 more rows, and 18 more variables: story <fctr>,
    ## #   direction <fctr>, age <dbl>, selfrate <dbl>, signyrs <dbl>,
    ## #   aoasl <int>, acc <dbl>, forehead <dbl>, eyes <dbl>, mouth <dbl>,
    ## #   chin <dbl>, upperchest <dbl>, midchest <dbl>, lowerchest <dbl>,
    ## #   belly <dbl>, left <dbl>, right <dbl>, total <dbl>

Participant Demographics
========================

Now we can easily get group means.

``` r
groupmeans <- data %>%
  group_by(maingroup) %>%
  summarize(n = n()/4,
            age = mean(age),
            selfrate = mean(selfrate),
            signyrs = mean(signyrs),
            aoasl = mean(aoasl))
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
age
</th>
<th style="text-align:right;">
selfrate
</th>
<th style="text-align:right;">
signyrs
</th>
<th style="text-align:right;">
aoasl
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
DeafEarlyASL
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
34.9
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
29.7
</td>
<td style="text-align:right;">
5.1
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafLateASL
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
34.0
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
21.5
</td>
<td style="text-align:right;">
12.5
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafNative
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
32.9
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
32.5
</td>
<td style="text-align:right;">
0.3
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingLateASL
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
28.9
</td>
<td style="text-align:right;">
4.7
</td>
<td style="text-align:right;">
11.5
</td>
<td style="text-align:right;">
17.5
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingNoviceASL
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
20.2
</td>
<td style="text-align:right;">
3.0
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
17.6
</td>
</tr>
</tbody>
</table>
``` r
#groupmeans
```

Save the dataset for further analysis
=====================================

We'll save this nice clean dataset now and call it `cleandata.csv`.

``` r
write.csv(data,"cleandata.csv",row.names=FALSE)
```

Happy? You can proceed to [Lexical Recall Analysis](02lexicalrecall.nb.html) or [Eye Gaze Analysis](03eyegaze.nb.html).
