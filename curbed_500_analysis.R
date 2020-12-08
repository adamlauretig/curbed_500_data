rm(list = ls())
library(data.table)
library(ggplot2)
library(stringr)
# load data -----
d <- fread('curbed_500.csv', sep = '\t', header = FALSE)
d[, rn := seq.int(1, 1000, 1)]  
d2 <- data.table(biz = d[d$rn %% 2 == 1 ]$V1, details = d[d$rn %% 2 == 0 ]$V1)
expanded_details <- d2[, tstrsplit(details, split = ', ')]

# get locations -----
locations <- expanded_details[!(str_detect(V2, pattern = '[0-9]+'))]$V2
locations <- c(locations, expanded_details[
  !(str_detect(V3, pattern = '[0-9]+'))]$V3)
locations <- c(locations, expanded_details[
  !(str_detect(V4, pattern = '[0-9]+'))]$V4)
locations <- c(locations, expanded_details[
  !(str_detect(V5, pattern = '[0-9]+'))]$V5)
locations_dt <- as.data.table(locations)
locations_dt_count <- locations_dt[,.N, by = .(locations)]

# create factors for ordering -----
locations_dt_count$location_fact <- factor(locations_dt_count$locations, 
  levels = locations_dt_count$locations[
    order(locations_dt_count$N, decreasing = FALSE)])

ggplot(locations_dt_count, aes(x = location_fact, y = N)) + 
  geom_bar(stat = "identity", color = "white", fill = "black") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    text = element_text(family = "Palatino"), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
#    axis.line = element_blank(),
    legend.key = element_blank(), 
    legend.title=element_blank()) + 
  labs(title = "Most Common Neighborhoods in Curbed 500", x = "", y = "Neighborhood Count") + 
  scale_color_manual(values=c("grey20", "grey60")) +  
  coord_flip() 

# get top 20 -----
locations_dt_count2 <- locations_dt_count[order(
  locations_dt_count$N, decreasing = TRUE)][1:20]
ggplot(locations_dt_count2, aes(x = location_fact, y = N)) + 
  geom_bar(stat = "identity", color = "white", fill = "black") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    text = element_text(family = "Palatino"), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
#    axis.line = element_blank(),
    legend.key = element_blank(), 
    legend.title=element_blank()) + 
  labs(title = "Most Common Neighborhoods in Curbed 500", x = "", y = "Neighborhood Count") + 
  scale_color_manual(values=c("grey20", "grey60")) +  
  coord_flip() 
