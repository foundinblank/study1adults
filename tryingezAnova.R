library(ez)
data.face.nona <- data.face3 %>%
  ungroup() %>%
  select(id,percent,aoi,direction,maingroup)
data.face.nona <- na.omit(data.face.nona)
data_anova <- ezANOVA(
  data = data.face.nona,
  dv = percent,
  wid = id,
  within = .(aoi,direction),
  between = maingroup
)
