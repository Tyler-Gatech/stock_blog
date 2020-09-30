

install.packages("quantmod")
library("quantmod")


prices <- getSymbols("AAPL", from="2018–01–01", to=Sys.Date(), auto.assign=FALSE)

prices <- getSymbols("AAPL", from="2018-01-01", to = "2020-09-28", auto.assign = F)


#Bring in data an libraries
library(RSelenium)
library(stringr)
library(rvest)
library(stringr)

df <- readRDS('./data/asr_validate.rds')

#check current chrome version, and input in chomever below
binman::list_versions("chromedriver")

driver <- rsDriver(browser=c("chrome"), port = 4588L, chromever = "84.0.4147.30")
rd <- driver[["client"]]

stock_list <- c ('TSLA', 'SQ', 'LVGO', 'BABA', 'DOCU', 'SE', 'VRM', 'OSTK','ZM', 'ROKU',
                 'AYX', 'PTON', 'CRWD', 'DAO', 'DDOG', 'ETSY', 'FSLY', 'MELI', 'TWLO',
                 'STNE', 'GDRX', 'PINS', 'FROG', 'CWH', 'SNOW')
  
for (i in stock_list){
  print(i)
  rd$navigate(paste0("https://ycharts.com/companies/",i,"/revenues_ttm"))
  Sys.sleep(sample(seq(1, 2, by=0.01), 1))
}

rd$navigate(paste0("https://ycharts.com/companies/",i,"/revenues_ttm"))

rd$navigate("https://ycharts.com/companies/TSLA/revenues_ttm")

table <- rd$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()

table[[2]]

str_extract(table[[2]][1,], ".*[:alpha:]*")
str_extract(table[[2]][1,], "^[:digit:]*")

month <- str_extract(table[[2]][1,][1], "[:alpha:]{3,5}")
day <- str_extract(table[[2]][1,][1], "[0-9]{2}")
