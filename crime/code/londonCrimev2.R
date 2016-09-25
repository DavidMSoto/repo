

rm(list = ls())

setwd("~/dataScience/projects/repo/crime/code/")

outcomes = read.csv("../input/outcomes20152016.csv", header=T, sep=",", stringsAsFactors=F) 
street = read.csv("../input/street20152016.csv", header=T, sep=",", stringsAsFactors=F) 

names(street)
names(outcomes)


names(street) <- c( "CrimeID"  , "Month"  ,  "Reportedby"    ,  "Fallswithin" ,  
 "Longitude"   ,   "Latitude"     ,      "Location"    ,    "LSOAcode"     ,       
 "LSOAname"     ,        "Crimetype"   ,         "Lastoutcomecategory", "Context" )

names(outcomes)<-c( "CrimeID"  ,   "Month"     ,   "ReportedBy"  ,"FallsWithin" ,"Longitude" ,   "Latitude" ,  
                   "Location"  ,   "LSOACode"   , "LSOAName"   , "OutcomeType"  )    

tail(street)


#data <- merge(x = outcomes, y = street, by = "Crime.ID", all = TRUE)

library(sqldf)
crimes <-  sqldf("SELECT 
          --count(*)
          s.Month ,s.Crimetype ,s.Lastoutcomecategory,s.Context ,  o.Longitude, o.Latitude 
             FROM street s, outcomes o
             where s.CrimeID = o.CrimeID 
            and s.rowid <= 2000
      --   and s.Crimetype like '%sexual%'
          " )


library(tidyr)
# specify the new column names:
vars <- c("year", "monthy")
crimes <- separate(crimes, Month, into = vars, sep = "-")
crimes$monthy<- as.factor(crimes$monthy )

crimes = as.data.frame(crimes)

crimes$monthy = factor(crimes$monthy, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", ""));
class(crimes$monthy)

### Analysis by year

```{r, tidy=T}
t_total_count_by_year_and_offense = tb %>%
  group_by(Occurrence.Year, Offense) %>% summarise(count=n())

ggplot(data=t_total_count_by_year_and_offense, aes(x=as.numeric(Occurrence.Year), y=count)) +
  ggtitle("Number of offense insidents over the last 10 years (2006-2015)") +
  xlab("Year from 2006 to 2015") +
  ylab("Number of incidents") + 
  geom_point(aes(color=Offense), size=5.0) + 
  geom_line(aes(color=Offense), size=2.0)
```
