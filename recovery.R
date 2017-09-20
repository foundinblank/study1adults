accdiff <- dataoriginal %>%
  select(participant,maingroup,video,acc) %>%
  distinct() %>%
  spread(video,acc) %>%
  mutate(fw = fw3-fw1,
         rv = rv4-rv2) %>%
  select(participant,maingroup,fw,rv) %>%
  gather(direction,diff,fw:rv)

accdiff$maingroup = factor(accdiff$maingroup,levels=c("DeafNative","DeafEarlyASL","DeafLateASL","HearingLateASL","HearingNoviceASL"))

ggplot(accdiff,aes(x=maingroup,y=diff,fill=direction)) + geom_boxplot() + ylab("diff: 2nd rv story - 1st rv story")
