install.packages("openssl" ,dependencies = TRUE )
install.packages('plyr', configure.args='--disable-cxx11')
library(rvest)
library(devtools)
install.packages(c("rj", "rj.gd"), repos="http://download.walware.de/rj-2.0")

lego_movie <- html("http://www.imdb.com/title/tt0000017/")


lego_movie %>% 
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()



lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()



lego_movie %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()