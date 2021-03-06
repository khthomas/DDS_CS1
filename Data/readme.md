# Files Required for this analysis

The three datasets below are required to recreate this anaylsis.
All data related to beer and breweries were supplied by the client.

U.S.A. population data was taken from the the US Census Bureau. The raw population data can be found here (https://www.census.gov/data/tables/2016/demo/popest/state-total.html)

## Data Descriptions
1. "Beers.csv" - this CSV file contains relatvent information regarding the beers (ABV, IBU, beer name, beer style, and serving size)
2. "Breweries.csv" - this CSV file contains relavent information regarding the breweries (brewery name, state, and city of the brewery)
3. "uspop.csv" - this CSV file contains US population data from the US Census. Population data from 2010 to 2016 is available.


## Process for creating tidy data

### The final dataframe upon which all analysis is conducted is called beerandbrew (this dataframe is subset and sliced to create sub dataframe for quick analysis)

First read in the files and make the changes needed to the column names.
```{r}
beers <- read.csv(file = "Beers.csv", sep=",", header=TRUE)
breweries <- read.csv(file = "Breweries.csv", sep=",", header=TRUE)
uspop <- read.csv("uspop.csv")
colnames(uspop) <- c("State_Name", "State", "pop2016")
colnames(breweries)[1] <- "Brewery_id"

```

Next The beer and brewery datasets are be merged. Column names are also changed.
```{r}
beerandbrew <- merge(breweries, beers, by=c("Brewery_id"), all= FALSE)
colnames(beerandbrew)[2] <- "Brewery"
colnames(beerandbrew)[5] <- "Beer"
```

Finally, US population data is merged with the beerandbrew dataframe
```{r}
beerandbrew = merge(beerandbrew, uspop, by = "State")
```
