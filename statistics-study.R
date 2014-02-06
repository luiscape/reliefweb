#### Working with resulting data from Relief Web. ####

# setwd("~/Documents/Programming/reliefweb-study")

library(ggplot2)
library(lubridate)
library(scales) # for using scale_x_continuous by dividing it in each month, etc.

# Loading data.
car <- read.csv('data/Central African Republic.csv')
south.sudan <- read.csv('data/mali.csv')
syria <- read.csv('data/syria.csv')
phil <- read.csv('data/philippines.csv')


# Creating a single data.frame
car$country <- 'Central African Republic'
south.sudan$country <- 'South Sudan'
syria$country <- 'Syria'
philippines$country <- 'Philippines'
  data <- rbind(car, south.sudan, syria, philippines)

data$date.created <- as.Date(data$date.created)

drc.jobs$type <- c('jobs')
drc$type <- c('report')
drc <- rbind(drc.jobs, drc)

#### Plotting #### 

# Number of reports using facets.
ggplot(drc, aes(as.Date(date.created))) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
#   geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Year') +
  scale_x_date(limits = as.Date(c('2000-01-01','2014-01-01')), 
               breaks = date_breaks(width = "1 year")) + 
  facet_wrap(~ type, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.border = element_rect( colour = "white"),
        panel.border = element_rect(fill = NA, colour = NA),
        strip.background =  element_rect(fill = "#d3d3d3", colour = NA),
        axis.text.x = element_text(angle = 90, hjust = 1)
        )

# calculating correlations
drc$date.created <- as.Date(drc$date.created)
drc.jobs$date.created <- as.Date(drc.jobs$date.created)

drc$month <- month(drc$date.created)
drc$year <- year(drc$date.created)
drc$time <- paste(drc$month, "-", drc$year, sep = "")


drc.jobs$month <- month(drc.jobs$date.created)
drc.jobs$year <- year(drc.jobs$date.created)
drc.jobs$time <- paste(drc.jobs$month, "-", drc.jobs$year, sep = "")

drc.m <- as.data.frame(count(drc$time))
colnames(drc.m)[1] <- "date"
drc.jobs.m <- as.data.frame(count(drc.jobs$time))
colnames(drc.jobs.m)[1] <- "date"


cor.data <- merge(drc.m, drc.jobs.m, by = "date")

cor(cor.data$freq.x, cor.data$freq.y, use = "pairwise")

ggplot(cor.data, aes(freq.x, freq.y)) + geom_point() + stat_smooth()


# Number of reports using facets.
ggplot(drc.jobs, aes(as.Date(date.created))) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  #   geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Year') +
  scale_x_date(breaks = date_breaks(width = "1 year")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.border = element_rect( colour = "white")
  )


  facet_wrap(~ country) + 
  theme( panel.border = element_rect(fill = NA, colour = NA),
         strip.background =  element_rect(fill = "#d3d3d3", colour = NA),
         axis.text.x = element_text(angle = 90, hjust = 1)
  )



# Reports for CAR
ggplot(car, aes(date.created)) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Month') +
  scale_x_date(limits = as.Date(c('2013-01-01','2014-02-28')), 
               breaks = date_breaks(width = "1 month")) + 
  scale_y_continuous(limits = c(0,300))

# Reports for Mali
ggplot(mali, aes(date.created)) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Year') + 
  scale_x_date(limits = as.Date(c('2013-01-01','2014-02-28')), 
               breaks = date_breaks(width = "1 month")) + 
  scale_y_continuous(limits = c(0,300))

# Reports for Syria
ggplot(syria, aes(date.created)) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Year') +
  scale_x_date(limits = as.Date(c('2013-01-01','2014-02-28')), 
               breaks = date_breaks(width = "1 month")) + 
  scale_y_continuous(limits = c(0,300))

# Reports for Philippines
ggplot(phil, aes(date.created)) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Year') +
  scale_x_date(limits = as.Date(c('2013-01-01','2014-02-28')), 
               breaks = date_breaks(width = "1 month")) + 
  scale_y_continuous(limits = c(0,300))


drc$duplicated <- duplicated(drc)
drc <- subset(drc, drc$duplicated == FALSE)
