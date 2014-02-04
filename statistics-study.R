#### Working with resulting data from Relief Web. ####

# setwd("~/Documents/Programming/reliefweb-study")

library(ggplot2)
library(lubridate)
library(scales) # for using scale_x_continuous by dividing it in each month.

# Loading data.
car <- read.csv('data/Central African Republic.csv')
mali <- read.csv('data/mali.csv')
syria <- read.csv('data/syria.csv')
phil <- read.csv('data/philippines.csv')

#
# # Function to clean the 1970 dates -- check the Epoch converter. 
# fix.dates <- function (df = NULL) { 
#   df$date.created <- as.Date(df$date.created)
#   x <- year(df$date.created) > year('1970-01-01') 
#   y <- cbind(df, x)
#   z <- subset(y, y$x == "TRUE")
#   z[5] <- NULL
#   return(z)
# }

# car <- fix.dates(car)
# mali <- fix.dates(mali)
# syria <- fix.dates(syria)
# phil <- fix.dates(phil)

# Creating a single data.frame
car$country <- 'Central African Republic'
mali$country <- 'Mali'
syria$country <- 'Syria'
phil$country <- 'Philippines'
  data <- rbind(car, mali, syria, phil)

data$date.created <- as.Date(data$date.created)

#### Plotting #### 

# Number of reports using facets.
ggplot(syria, aes(as.Date(date.created))) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Month') +
  scale_x_date(limits = as.Date(c('2000-01-01','2014-01-01')), 
               breaks = date_breaks(width = "1 year")) 



  facet_wrap(~ country) + 
  theme( panel.border = element_rect(fill = NA, colour = NA),
         strip.background =  element_rect(fill = "#d3d3d3", colour = NA)
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






