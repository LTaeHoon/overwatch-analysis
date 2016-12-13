#overwatch_eda 탐색적 데이터 분석
#goals
#1. 경쟁전 랭킹 시각화 및 각종 데이터 이해(scatter plot)를 위한 plot 그리기
#2. kmeans clustering 을 이용한 그룹 분류 (avg 스탯 사용), heatmap 만들
#3. 상관관계 테이블 생성(설명변수, 반응변수:comprank)

#library
library(DBI) #db interface
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
library(rJava)
library(RJDBC) # rJava에 의존적이다.

library(ggplot2)
library(plyr)
library(reshape2)

#read data
#db 연결
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="C:\\NCS\\python\\util\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40-bin.jar")
conn <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/overwatch", "scott", "tiger")

data_overall <- dbGetQuery(conn,"select * from user_compe_over")
head(data_overall)

# pal <- colorRamp(c("red","blue"))
# palcol<-pal(seq(0,1,len=10))

ggplot(data=data_overall,aes(x=battletag,y=comprank,fill=battletag))+
  geom_bar(stat="identity", position="identity", colour="white", width=1)+
  ggtitle("경쟁전 랭킹")


