#overwatch profile file extract

library(RJSONIO)
library(dplyr)
library(jsonlite)
library(plyr)
library(RMySQL)

userlist <-c("RandomTH-3179","머거본놈-31139","ksa-3943","wlsrud531-3918","yangrae-3284","틴팬강원래-3198","농촌계모임-3391","마키아울프-3387","쉑쉑감자-3831","더네임-3971")
df <- data.frame(NULL)

for(i in 1:length(userlist)){
  print(i)
  userlist[i] <- iconv(userlist[i],from="cp949", to="UTF-8")
  user <- URLencode(userlist[i])
  url <- paste0("https://owapi.net/api/v3/u/",user,"/stats")
 
  raw.data <- readLines(url, warn="F", encoding = "UTF-8")
  dat <- fromJSON(raw.data) #데이터 변환(list)
  datatrans <- as.data.frame(dat$kr$stats$competitive$average_stats) #list -> data.frame
  df <- rbind.fill(df,datatrans)
}
df <- cbind(userlist,df)
names(df)[1]<- c('battletag')
battletag <- as.character(df$battletag)
df$battletag <-battletag
con <- dbConnect(MySQL(), user="scott", password="tiger", dbname="overwatch", host="127.0.0.1")
print(df)
getwd()
write.csv(df, "db_data.csv", row.names = F, quote = F)
da_data <- read.csv("db_data.csv", header = T, encoding = 'utf-8')
da_data
dbWriteTable(con, "user_competitive_stat", da_data, append=TRUE)



