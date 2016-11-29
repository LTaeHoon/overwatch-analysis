data_processing<-function(){
#set directory
# setwd(paste(getwd(),"overwatch_analysis",sep="\\"))
#road the necessary library 

library(RCurl)
library(RJSONIO)
library(httr)
library(dplyr)
library(jsonlite)
library(plyr)

#user id vector with battle tag 
user <-c("RandomTH-3179","%EB%A8%B8%EA%B1%B0%EB%B3%B8%EB%86%88-31139","ksa-3943","wlsrud531-3918","yangrae-3284","DarkBarBar-3330","%EB%86%8D%EC%B4%8C%EA%B3%84%EB%AA%A8%EC%9E%84-3391","%EB%A7%88%ED%82%A4%EC%95%84%EC%9A%B8%ED%94%84-3387","%EC%89%91%EC%89%91%EA%B0%90%EC%9E%90-3831","%EB%8D%94%EB%84%A4%EC%9E%84-3971")
userlist <-c("RandomTH-3179","머거본놈-31139","ksa-3943","wlsrud531-3918","yangrae-3284","DarkBarBar-3330","농촌계모?-3391","마키아울-3387","쉑쉑감자-3831","더네임-3971")
#heroes name vector
heroes <-c("Genji","Mccree","Pharah","Reaper","Soldier76","Tracer","Bastion","Hanzo","Junkrat","Mei","Torbjoern","Widowmaker","D.va","Reinhardt","Roadhog","Winston","Zarya","Ana","Lucio","Mercy","Symmetra","Zenyatta")
# #user & heroes list
# ov_list <-list(user=user,heroes=heroes) 

#url list API URL 
url_list <- list(url_profile=NULL, url_allHeroes_competitive=NULL, url_allHeroes_quick=NULL, url_overall_herostat_quick=NULL,url_overall_herostat_competitive=NULL)
df_url_multiple_heroes_quick =NULL
df_url_multiple_heroes_competitive =NULL

#data.frame for storing data 

list.profile = NULL
df.list.profile = NULL
df.allHeroes_competitive = NULL
df.allHeroes_quick = NULL
df.multiple_heroes_quick = NULL
df.multiple_heroes_competitive = NULL
df.overall_herostat_competitive = NULL
df.overall_herostat_quick = NULL


#get all url
for(i in 1:10){

  url_list$url_profile[i] <- paste("https://api.lootbox.eu/pc/kr/",user[i],"/profile",sep="")
  url_list$url_allHeroes_competitive[i] <- paste("https://api.lootbox.eu/pc/kr/",user[i],"/competitive-play/allHeroes/",sep = "")
  url_list$url_allHeroes_quick[i] <- paste("https://api.lootbox.eu/pc/kr/",user[i],"/quick-play/allHeroes/",sep= "")
  url_list$url_overall_herostat_competitive[i] <- paste("https://api.lootbox.eu/pc/kr/",user[i],"/competitive-play/heroes",sep = "")
  url_list$url_overall_herostat_quick[i] <- paste("https://api.lootbox.eu/pc/kr/",user[i],"/quick-play/heroes",sep= "")
  
  
  
  
  list.profile[i] = fromJSON(url_list$url_profile[i])
  df.list.profile.data1 = as.data.frame(t(unlist(list.profile[i])),stringsAsFactors = FALSE)
  df.list.profile <- rbind.fill(df.list.profile,df.list.profile.data1)
  

  df.allHeroes_competitive.data1 <- as.data.frame(t(fromJSON(url_list$url_allHeroes_competitive[i])), stringsAsFactors = FALSE)
    if(!(nrow(df.allHeroes_competitive.data1)==0)){
    df.allHeroes_competitive.data1<-cbind(user=userlist[i],df.allHeroes_competitive.data1)
    df.allHeroes_competitive <-rbind.fill(df.allHeroes_competitive,df.allHeroes_competitive.data1)
    }
  
 
  df.allHeroes_quick.data1 <- as.data.frame(t(fromJSON(url_list$url_allHeroes_quick[i])), stringsAsFactors = FALSE)
  if(!(nrow(df.allHeroes_quick.data1)==0)){
    df.allHeroes_quick.data1<-cbind(user=userlist[i],df.allHeroes_quick.data1)
    df.allHeroes_quick <-rbind.fill(df.allHeroes_quick,df.allHeroes_quick.data1)
  }
  
  df.overall_herostat_competitive.data1 <- as.data.frame(fromJSON(url_list$url_overall_herostat_competitive[i]), stringsAsFactors = FALSE)
  if(!(nrow(df.overall_herostat_competitive.data1)==0)){
    df.overall_herostat_competitive.data1<-cbind(user=userlist[i],df.overall_herostat_competitive.data1)
    df.overall_herostat_competitive <-rbind.fill(df.overall_herostat_competitive,df.overall_herostat_competitive.data1)
  }
  ?
  df.overall_herostat_quick.data1 <- as.data.frame(fromJSON(url_list$url_overall_herostat_quick[i]), stringsAsFactors = FALSE)
  if(!(nrow(df.overall_herostat_quick.data1)==0)){
    df.overall_herostat_quick.data1<-cbind(user=userlist[i],df.overall_herostat_quick.data1)
    df.overall_herostat_quick <-rbind.fill(df.overall_herostat_quick,df.overall_herostat_quick.data1)
  }
  
     for(j in 1:22)
     {
       url_multiple_heroes_quick <- paste("https://api.lootbox.eu/pc/kr/",user[i],"/quick-play/hero/",heroes[j],"/",sep="")
       df.multiple_heroes_quick.data1 <- as.data.frame(t(fromJSON(url_multiple_heroes_quick)[[1]]),stringsAsFactors = FALSE)
       if(!(nrow(df.multiple_heroes_quick.data1)==0)){
       df.multiple_heroes_quick.data1 <- cbind(user[i],heroes[j],df.multiple_heroes_quick.data1)
       class(df.multiple_heroes_quick)
       class(df.multiple_heroes_quick.data1)
       df.multiple_heroes_quick <- rbind.fill(df.multiple_heroes_quick,df.multiple_heroes_quick.data1)
       }
       
       url_multiple_heroes_competitive <- paste("https://api.lootbox.eu/pc/kr/",user[i],"/competitive-play/hero/",heroes[j],"/",sep="")
       df.multiple_heroes_competitive.data1 <- as.data.frame(t(fromJSON(url_multiple_heroes_competitive)[[1]]),stringsAsFactors = FALSE)
       if(!(nrow(df.multiple_heroes_competitive.data1)==0)){
       df.multiple_heroes_competitive.data1 <- cbind(user[i],heroes[j],df.multiple_heroes_competitive.data1)
       df.multiple_heroes_competitive <- rbind.fill(df.multiple_heroes_competitive,df.multiple_heroes_competitive.data1)
       }
     }
}
# names(df_url_multiple_heroes_quick) <- c("id","heroes","url")
# names(df_url_multiple_heroes_competitive) <- c("id","heroes","url")

# #use api
# achieve = getURL(url_achieve)
# profile = getURL(url_profile)
 
# #converting data from JSON to data.frame
# data = fromJSON(page)
# achieve <- as.data.frame(data, stringsAsFactors = FALSE)
 
write.csv(df.list.profile, file="./overwatch_analysis/profile.csv")
write.csv(df.allHeroes_competitive,file="./overwatch_analysis/allheroes_com.csv")
write.csv(df.allHeroes_quick,file="./overwatch_analysis/allheroes_quick.csv")
write.csv(df.multiple_heroes_quick, file="./overwatch_analysis/multiple_heroes_quick.csv")
write.csv(df.multiple_heroes_competitive, file="./overwatch_analysis/multiple_heroes_competitive.csv")
write.csv(df.overall_herostat_competitive, file="./overwatch_analysis/overall_herostat_competitive.csv")
write.csv(df.overall_herostat_quick, file="./overwatch_analysis/overall_herostat_quick.csv")

}