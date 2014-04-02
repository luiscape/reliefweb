ReliefWeb R Package
===================

This package is designed to query ReliefWeb's upcoming API (still in __alpha__) for statistical analysis. It allows you to fetch all the data from a certain query, effectively overriding the 1000-results limitation. The result is an R `data.frame` ready for analysis.


Installation
------------

First, install and load Hadley Wickham's `devtools` (https://github.com/hadley/devtools) by typing: 

```r
    install.packages('devtools')
    library(devtools)
```

Then use `devtools` function `install_github` pulling the code from this GitHub repo: 

```r 
    devtools::install_github('luiscape/reliefweb')
```

If everything works well the package should be installed in your system without problems.


Usage
-----

At this point, there is only one querying function:

```r
    rw.query()
```

It acceps the following paramenters:


    `entity` = One of the five ReliefWeb entities: 'report', 'job', 'disaster', 'training', and 'country'. 
    `limit` = The number or results you want back: a number between 1 and 1000. If you want more than 1000 use 'all'.
    `text.query` = Use this field to make text queries to the API.
    `query.field` = Add the fields that you want to query.
    `query.field.value` = Submit a value for the field above. 
    `add.fields` = The fields you want back. 
    `csv` = In case you want the final output to be stored in a CSV file automatically. 

There is also a debugging function `debug = FALSE`, but you can ignore that. 

**Note:** There is a known error with fields that return multiple results for a single ReliefWeb entry. If you try to query one of these fields you will get an error. The next version (`0.1.6`) will add that feature.


Example
-------
Let's think that you want to get all the metadata about the reports available from Syria on ReliefWeb's history. You will then ask for `'report'` as an `entity` and type `'all'` in the `limit` parameter. Those two parameters simply say that you want all the reports about something. You need to specify what: I want reports about a `'county'` (using the `query.field`); I want reports about `'Syria'` (using the `query.field.value`). Finally, you specify what output you would like to have back: `add.fields` = `'id'`, `'title'`, `'primary_country.iso3'`, and `'date.created'`.

Here is the code: 

```r
    syria <- rw.query(entity = 'report', limit = 'all', query.field = 'country', query.field.value = 'Syria', 
        add.field = c('id', 'title', 'primary_country.iso3', 'date.created'))
```
The function will then run (it may take a few minutes to complete, especially if you are downloading hundreds of thousands of entries): 

![Asking for all the reports about Syria.](https://raw.githubusercontent.com/luiscape/reliefweb/master/readme/rw.query-syria.gif)



The result will be a `data.frame` with the fields you specified:

![Resulting data.frame](https://raw.githubusercontent.com/luiscape/reliefweb/master/readme/view-syria.gif)


Which you can plot using `ggplot2` into:

```r
    library(lubridate)
    library(ggplot2)
    
    ggplot(syria) + theme_bw() + 
      geom_area(aes(created), stat = 'bin', fill = "#0988bb", alpha = 0.3) + 
      geom_line(aes(created), stat = 'bin', color = "#0988bb", size = 1.3) 
```

![Plotting the number of reports for Syria.](https://raw.github.com/luiscape/reliefweb-study/master/readme/syria-plot.png)



Roadmap
-------
1. ~~Release first compiled version (0.1) using devtools.~~
2. Create a `codebook` for the API.
3. Finish the first version of the `rw.plot` function. 
4. Host and run the package on ScraperWiki.
5. Write the article `Introducing Reliefweb as a real-time source of data`. 
6. Create an User Interface for the querier. 


License
-------
This work is licensed under [General Public License v3](https://www.gnu.org/copyleft/gpl.html).

