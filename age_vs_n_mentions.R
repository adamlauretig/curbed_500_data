# Code by Adam Lauretig, created 12/8/2020
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
expanded_details2 <- expanded_details[, 2:5]
expanded_details2[, V6 :=  as.numeric(str_extract_all(V3, pattern = '[0-9]+', simplify = TRUE))]
expanded_details2 <- expanded_details2[, age := 2020 - V6][!(is.na(age))]
avg_age <- expanded_details2[,. (mean(age)), by = .(V2)]


# create factors for ordering -----
avg_age$location_fact <- factor(avg_age$V2, 
  levels = avg_age$V2[
    order(avg_age$V1, decreasing = FALSE)])

ggplot(avg_age, aes(x = location_fact, y = V1)) + 
  geom_bar(stat = "identity", color = "white", fill = "black") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    text = element_text(family = "Palatino"), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
#    axis.line = element_blank(),
    legend.key = element_blank(), 
    legend.title=element_blank()) + 
  labs(title = "Avg Age of Closed Venues in Curbed 500", x = "", y = "Avg Age") + 
  scale_color_manual(values=c("grey20", "grey60")) +  
  coord_flip() 


### now get frequency mentioned -----


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

age_counts <- merge(locations_dt_count, avg_age[, c('V2', 'V1')], by.x = 'locations', by.y = 'V2')
ggplot(age_counts, aes(x = N, y = V1)) + 
  geom_point(color = "black", fill = "black") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    text = element_text(family = "Palatino"), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
#    axis.line = element_blank(),
    legend.key = element_blank(), 
    legend.title=element_blank()) + 
  labs(title = "Frequency of Neighborhood vs\nAge of Institutions Closed", x = "Number of mentions", y = "Avg Age") + 
