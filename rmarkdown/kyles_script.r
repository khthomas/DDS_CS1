#Read in the data
dfBeers = read.csv("Beers.csv")
dfBrew = read.csv("Breweries.csv")

#Read in needed libraries
library(dplyr)
library(plyr)
library(ggplot2)


#Question 1 - How many breweries are present in each state?
states = data.frame(plyr::count(dfBrew$State))

#Question 2 - Merge the datasets and print the first and last 6 observations
#Need to merge on variable "brewery_id"
colnames(dfBrew) = c("Brewery_id", "Brewery_name", "City", "State")
dfMerge = merge(dfBeers, dfBrew, by = "Brewery_id")
head(dfMerge,6)
tail(dfMerge,6)

#Question 3 - Report number of NAs in each column
naData = sapply(dfMerge, function(x) length(which(is.na(x))))

#Question 4 - Compute median ABV and IBU for each state
outputdb = data.frame("State" = character(), "Median_ABV" = double(), "Median_IBU" = double())
for(var in unique(dfMerge$State)) {
      outputdb = add_row(outputdb, State = var, 
              Median_ABV = median(dfMerge$ABV[dfMerge$State == var], na.rm = TRUE),
              Median_IBU = median(dfMerge$IBU[dfMerge$State == var], na.rm = TRUE)
              )
  }

#Question 4B - Plot the median values
#Using Base Graphics
ibuChart = barplot(outputdb$Median_IBU, names = outputdb$State,
                   ylab = "Median IBU Content", xlab = "State",
                   main = "Median IBU Content by State",
                   col = 4) #need to work on scaling
abvChart = barplot(outputdb$Median_ABV * 100, names = outputdb$State,
                   ylab = "Median ABV Content (%)", xlab = "State",
                   main = "Median ABV Content by State",
                   col = 4) #need to work on scaling
#Using ggplot2
#try to do by descending ABV and IBU
# get state population data
ggIBU = (ggplot(outputdb, aes(y = Median_IBU, x = State)) + geom_col(color="blue") +
           labs(title="Median IBU Content by State", y="Median IBU") + 
           theme(plot.title = element_text(hjust = 0.5)))

ggABV = (ggplot(outputdb, aes(y = Median_ABV * 100, x = State)) + geom_col() +
                  labs(title="Median ABV Content by State", y="Median ABV (%)") + 
                  theme(plot.title = element_text(hjust=0.5)))
 


#Question 5 - Which states have the highest ABV and IBUs?
highABV = dfMerge[which.max(dfMerge$ABV),]["State"]
highIBU = dfMerge[which.max(dfMerge$IBV),]["State"]

#Question 6 - Summary Stats for the ABV variable
summary(dfMerge$ABV)

#Question 7 - Is there a relationship between bitterness and ABV
#there does appear to be a positive relationship, which makes sense with IPAs being a predominate
#Style
scatter.smooth(dfMerge$ABV, dfMerge$IBU, ylab = "IBU",
               xlab = "ABV", col= 4, main = "ABV vs IBU")


ggScatter = ( ggplot(dfMerge, aes(x=IBU, y=ABV)) + geom_point() + 
                labs(title="IBU vs ABV") + theme(plot.title = element_text(hjust = 0.5)) +
                geom_smooth())


# What states are over staturated with high IBU beer, which are not
## need to define what is high IBU what is over staturated
## high IBU 

