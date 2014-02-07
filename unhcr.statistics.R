#### Analyzing UNHCR data from http://data.unhcr.org/syrianrefugees/regional.php

library(ggplot2)
library(scales)

egypt <- read.csv('data/unhcr-egypt.csv', sep = ";")
jordan <- read.csv('data/unhcr-jordan.csv', sep = ";")
lebanon <- read.csv('data/unhcr-lebanon.csv', sep = ";")
turkey <- read.csv('data/unhcr-turkey.csv', sep = ";")
iraq <- read.csv('data/unhcr-iraq.csv', sep = ";")

colnames(egypt)[1] <- "date"
colnames(egypt)[2] <- "refugees"
egypt$country <- "egypt"

colnames(jordan)[1] <- "date"
colnames(jordan)[2] <- "refugees"
jordan$country <- "jordan"

colnames(lebanon)[1] <- "date"
colnames(lebanon)[2] <- "refugees"
lebanon$country <- "lebanon"

colnames(turkey)[1] <- "date"
colnames(turkey)[2] <- "refugees"
turkey$country <- "turkey"

colnames(iraq)[1] <- "date"
colnames(iraq)[2] <- "refugees"
iraq$country <- "iraq"


# Making a single data.frame
refugee.data <- rbind(egypt, jordan, lebanon, turkey, iraq)



#### Plotting #### 

# Number of reports using facets.
ggplot(refugee.data, aes(as.Date(date))) + theme_bw() +
  geom_line(aes(y = refugees), stat = "identity", size = 1.3) + 
  ylab('Number of Refugees') + xlab('Date') + 
  facet_wrap(~ country, scales = "fixed") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.border = element_rect( colour = "white"),
        panel.border = element_rect(fill = NA, colour = NA),
        strip.background =  element_rect(fill = "#d3d3d3", colour = NA),
        axis.text.x = element_text(angle = 90, hjust = 1)
  )


