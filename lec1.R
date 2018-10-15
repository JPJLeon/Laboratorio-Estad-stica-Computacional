#Including library
library(ggplot2)
library(labdsv)
library(rmarkdown)
setwd("C:/Users/dagui/Desktop/laboratorio-1-estadistica-computacional")
#Import csv into frame
data<-read.csv("USvideosWithCategories.csv", stringsAsFactors=FALSE)
cuenta = 0
mayor = 0
for(i in 2:18){
  for(j in 2:18){
    if(typeof(data[[i]][1]) == 'integer' && typeof(data[[j]][1]) == 'integer'){
      #print("numeric")
      #print(cor(data[i], data[j], method=c("pearson")))
      if(mayor < cor(data[i], data[j]) && data[i] != data[j] ){
        mayor = cor(data[i], data[j], method=c("pearson"))
      }
    }  
  }
  
}
print(mayor)
ggplot(data, aes(x=log(data$likes), y=log(data$views)))+geom_point(aes(color = factor(data$category),shape = factor(data$dislikes>data$likes)))+stat_smooth(method = "glm",col = "#C42128",se = FALSE,size = 1)+labs(title = "Laboratorio de ESTACA 1",x="Likes",y="Vistas")
ggplot(data, aes(x=log(data$views), y=log(data$likes/(data$dislikes + 1))))+geom_point(aes(shape=factor(data$comment_count>1000 %in% c(FALSE) ) , color = factor(data$category)))+stat_smooth(geom = "smooth", method = "glm",col = "#C42128",se = FALSE,size = 1)+labs(title = "Laboratorio de ESTACA 1",x="Likes",y="Ratio")


