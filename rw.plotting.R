## Plotting ##
# Create plotting functions with ggplot2. 
# Final plotting function with ggplot2.

library(ggplot2)
library(scales) # for working with scale_x_continuous and divide the breaks in months, etc.


#### Plotting #### 

# Number of reports using facets.
ggplot(haiti.reports, aes(date.created)) + theme_bw() +
  geom_line(stat = 'bin', colour = '#0988bb', size = 1.3) + 
  #   geom_area(stat = 'bin', fill = '#0988bb', alpha = .3) +
  ylab('Number of Reports') + xlab('Year') +
  scale_x_date(limits = as.Date(c('2000-01-01','2013-01-01')), 
               breaks = date_breaks(width = "1 year")) + 
  facet_wrap(~ country) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.border = element_rect( colour = "white"),
        panel.border = element_rect(fill = NA, colour = NA),
        strip.background =  element_rect(fill = "#d3d3d3", colour = NA),
        axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Snippets. 
geom_vline(xintercept = 1989, colour = "#cccccc", linetype = "longdash") +
scale_x_continuous(limits = c(1960,2014), breaks = c(1960,1970,1980,1990,2000,2010)) + 
  


