

rm(list = ls())

require(XML)

caso <- "C:/HIP/XML/dlogi/PRO/0000051211_76.xml"

doc = xmlTreeParse(caso, useInternal = TRUE)
top = xmlRoot(doc)

names(top)

names(xmlRoot(top))

top[["SCBUKApp"]] 

SCBUKApp =  top[["SCBUKApp"]] 
names(SCBUKApp)


Applicant = top[["SCBUKApp"]]  [["Applicant"]]

names(Applicant)

xmlSApply(top[["SCBUKApp"]], xmlValue)

names(Exposure)





# notas -------------------------------------------------------------------
  
require(XML)
weather <- xmlParse("http://forecast.weather.gov/MapClick.php?lat=29.803&lon=-82.411&FcstType=digitalDWML")

xml_weather <- xmlToList(weather)

location <- as.list(xml_weather[["data"]][["location"]][["point"]])

start_time <- unlist(xml_data[["data"]][["time-layout"]][
  names(xml_data[["data"]][["time-layout"]]) == "start-valid-time"])

temps <- xml_data[["data"]][["parameters"]]
temps <- temps[names(temps) == "temperature"]
temps <- temps[sapply(temps, function(x) any(unlist(x) == "hourly"))]
temps <- unlist(temps[[1]][sapply(temps, names) == "value"])

out <- data.frame(
  as.list(location),
  "start_valid_time" = start_time,
  "hourly_temperature" = temps)

head(out)
