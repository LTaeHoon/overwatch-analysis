#overwatch competitive stats data collection script

library(RJSONIO)
library(dplyr)
library(jsonlite)
library(plyr)
library(RMySQL)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111') 
library(RJDBC)
library(DBI)
library(rJava)

userlist <-c("RandomTH-3179","머거본놈-31139","ksa-3943","wlsrud531-3918","yangrae-3284","틴팬강원래-3198","농촌계모임-3391","마키아울프-3387","쉑쉑감자-3831","더네임-3971","HotDokk-1561")
df_avg <- data.frame(NULL) #경쟁전 평균 스탯
df_game <- data.frame(NULL) # 경쟁전 게임 스탯
df_over <- data.frame(NULL) # 경쟁전 누적 스탯
df_comp <- data.frame(NULL)
userlist <-iconv(userlist, from="cp949", to="UTF-8") 
for(i in 1:length(userlist)){
  print(i)
 
  user <- URLencode(userlist[i])
  url <- paste0("https://owapi.net/api/v3/u/",user,"/stats")
 
  raw.data <- readLines(url, warn="F", encoding = "UTF-8")
  dat <- fromJSON(raw.data) #데이터 변환(list)
  
  #경쟁전 avg 데이터 추출
  data_avg <- as.data.frame(dat$kr$stats$competitive$average_stats) #list -> data.frame
  #경쟁전 game_stats 데이터 추출
  data_game <- as.data.frame(dat$kr$stats$competitive$game_stats) #list -> data.frame
  #경쟁전 overall_stats 데이터 추출
  data_over <- as.data.frame(dat$kr$stats$competitive$overall_stats) #list -> data.frame
  
  #row 누적 각 데이터가 있는 경
  if(nrow(data_avg)){
  df_avg <- rbind.fill(df_avg,data_avg)
  }
  if(nrow(data_game)){
  df_game <- rbind.fill(df_game,data_game)
  }
  if(nrow(data_over)){
  df_over <- rbind.fill(df_over,data_over)
  }
  
}
df_avg <- cbind(userlist,df_avg)
df_game <- cbind(userlist,df_game)
df_over <- cbind(userlist,df_over)

names(df_avg)[1]<- c('battletag')
names(df_game)[1]<- c('battletag')
names(df_over)[1]<- c('battletag')


#db 연결 과정
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="C:\\NCS\\python\\util\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40-bin.jar")
con <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/overwatch", "scott", "tiger")

#테이블 저장
dbWriteTable(con, "user_compe_avg", df_avg, append=TRUE, row.names =F)
dbWriteTable(con, "user_compe_game", df_game, append=TRUE, row.names =F)
dbWriteTable(con, "user_compe_over", df_over, append=TRUE, row.names =F)


# query = "select * from user_compe_avg_stat"
# result <- dbGetQuery(con, query)
# result

dbDisconnect(con)

