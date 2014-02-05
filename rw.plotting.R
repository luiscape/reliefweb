## Plotting ##
# Create plotting functions with ggplot2. 
# Final plotting function with ggplot2.

library(ggplot2)
library(scales) # for working with scale_x_continuous and divide the breaks in months, etc.

# Subsetting from 2010 unti now. I haven't been able to use scale_x_continuous.
dez <- subset(test, ymd(test$created) > ymd('2010-01-01')) 

# Reports from 1970 until now. 
ggplot(test, aes(date.created)) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Year')

# Reports from 2010 until now. 
ggplot(dez, aes(created)) + theme_bw() +
  geom_hline(yintercept = 36, colour = "#cccccc", linetype = "longdash") +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) + 
  ylab('Number of Reports') + xlab('Year')


# Snippets. 

geom_vline(xintercept = 1989, colour = "#cccccc", linetype = "longdash") +
scale_x_continuous(limits = c(1960,2014), breaks = c(1960,1970,1980,1990,2000,2010)) + 
  


