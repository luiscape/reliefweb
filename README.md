ReliefWeb R Package
===================

This _yet-to-be-package_ is designed to query ReliefWeb's upcoming API (still in __alpha__) for statistical analysis. It allows you to fetch all the data from a certain query, effectively overriding the 1000-results limitation. The result is an R `data.frame` ready for analysis.


Usage
-----

At this point, the basic querying function is the following:

```r
rw.query()
```

It acceps the following paramenters:

```r
type = "report", "job" (Other types will be added soon.)
limit = 1 to 1000, and "all"
country = "Full name of the country of interest."
field1 to 5 = up to 5 fields using any fields available in the API. 
```

The function currently works well with up to three fields.


Example
-------
For getting all the reports available in ReliefWeb that have Syria as their primary country (clarify this!) you can do:

```r
syria <- rw.query(type = 'report', limit = 'all', country = 'Syria', field1 = 'url', field2 = 'title', field3 = 'date.created', field4 = 'body-html')
```

The result will be:

```r
View(syria)
```

![Resulting data.frame](https://raw.github.com/luiscape/reliefweb-study/master/readme/dataframe.png)

Which you can plot using `ggplot2` into:

```r
ggplot(syria) + 
    theme_bw() + 
    geom_line(aes(created), stat = 'bin', color = "#0988bb", size = 1.5)
```

![Plotting the number of reports for Syria.](https://raw.github.com/luiscape/reliefweb-study/master/readme/reports-syria.png)



Roadmap
-------
1. Release first compiled version (0.1) using `dev-tools`.
2. Create a `codebook` for the API.
3. Finish the first version of the `rw.plot` function. 
4. Write the article `Introducing Reliefweb as a real-time source of data`. 
5. Host and run the package on a web server. 
6. Create a User Interface for the querier. 


License
-------
This work is licensed under [General Public License v3](https://www.gnu.org/copyleft/gpl.html).

