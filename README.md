ReliefWeb R Package
===================

This _yet-to-be-package_ is designed to query ReliefWeb's upcoming API (still in __alpha__) for statistical analysis. It allows you to fetch all the data from a certain query, effectively overriding the 1000-results limitation. The result is an R `data.frame` ready for analysis.


Usage
-----

At this point, the basic querying function is the following:

```
rw.query()
```

It acceps the following paramenters:

```
type = "report", "job" (Other types will be added soon.)
limit = 1 to 1000 (Not really that relevant in this version.)
country = "Full name of the country of interest."
field1 to 3 = "date.created", "title", "url" (More fields will be added soon.)
```

The function currently works well with up to three fields.


Example
-------
For getting all the reports available in ReliefWeb that have Syria as their primary country (clarify this!) you can do:

```
syria <- rw.query(type = "report", limit = 1000, country = "Syria", field1 = "date.created", field2 = "title", field3 = "url")
```

The result will be:

```
View(syria)
```

![Resulting data.frame](https://raw.github.com/luiscape/reliefweb-study/master/readme/dataframe.png)

Which you can plot using `ggplot2` into: 

```
[ggplot2 plotting code here]
```

![Plotting the number of reports for DR of the Congo. ](https://raw.github.com/luiscape/reliefweb-study/master/graphics/reports-dr-congo.png)


License
-------
This work is licensed under [General Public License v3](https://www.gnu.org/copyleft/gpl.html).

