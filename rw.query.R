## This code has been written with th objective of exploring if the data coming from ReliefWeb could be used
# as an indicator of activity in the Humanitarian Sector. 
#  
#  Luis Capelo | capelo@un.org | @luiscape
# 

## Code Structure 
# The documentation for the API is available here: http://apidoc.rwlabs.org/
# The type can be: report, job, training, country, disaster. 


library(lubridate)
library(countrycode)

# URL with an example query of Syria data.
url <- c("http://api.rwlabs.org/v0/report/list?fields[include][0]=date.created&fields[include][1]=url&fields[include][2]=title&query[value]=primary_country:Syria")

test <- data.frame(fromJSON(getURLContent(url)))

rw.query <- function() { 
  require(rjson) # for reading the resulting JSON file.
  require(RCurl) # for making HTTP requests.

      final <- data.frame() 
      
      url <- paste("http://api.rwlabs.org/v0/", type, sep="")
  
          line <- tryCatch( # Error handler.
            data.frame(fromJSON(getURLContent(url))), 
            error = function(e) e
            )
          if(inherits(line, "error")) line <- c(NA)
          final <- rbind(final, line)
        }
      return(final)
    }
}



json_data <- fromJSON(paste(readLines(url), collapse=""))

