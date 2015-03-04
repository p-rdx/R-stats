library(xlsx)
library(stats)
require(graphics)
fname <- "periods"
setwd("D:/R/Shreider")
data <- read.table("vdm1.txt")
ages <- read.table("changes.txt",)
vdm <- matrix(nrow=0,ncol = 2)
i <- 1
j<-1
res <- matrix(nrow = 0, ncol = 2)
per <- matrix(nrow = 0, ncol = 6)
aver <- matrix(nrow = 0, ncol = 7)
colnames(aver)<-c("start","finish","a_age","a_vdm","count","D_age", "D_vdm")
while (i <= length(ages[,2]))
{
  
  a <- 1/(ages[i,2] - ages[i,1])
  temp <- data[data[,1] >= ages[i,1] & data[,1] < ages[i,2],]
  temp[,2] <- j*temp[,2]
  vdm <- rbind(vdm, temp)
  temp[,1] <- a*(temp[,1]-ages[i,1]) + i - 1
  
  res <-rbind(res, temp)
  #aver <- rbind(aver, c(ages[i,1], ages[i,2], mean(temp[,1]), mean(temp[,2]), length(temp[,2]), sd(temp[,1]), sd(temp[,2])))
  for(w in 1:19)
  {
    wind <- c(w, w+1)/20 +i-1
    temp1 <- temp[temp[,1] >= wind[1] & temp[,1] < wind[2],]
    per <- rbind(per, c(wind[2], mean(temp1[,1]), mean(temp1[,2]), sd(temp1[,1]), sd(temp1[,2]), length(temp1[,1])))
  }
  
  i <- i + 1
  j <- 1*j
  
}
per1<-per[per[,6]>0,]
length(per[,1])
length(per1[,1])
#write.xlsx(res, "periods2.xlsx", sheetName='data', row.names=FALSE)
#write.xlsx(aver, "periods2.xlsx", sheetName='averages', row.names=FALSE,append = TRUE)
#write.xlsx(per, "periods2.xlsx", sheetName='windows', row.names=FALSE,append = TRUE)
head(per)
per2 <- as.data.frame(approx(per[,2],per[,3], per[,1]))
head(per2)
#X.k <- fft(aver[,3:4])
#spectrum(X.k)
timetry <- ts(per2[,2],start = 0, frequency = 20)
#plot(na.approx(timetry))
ttr <- decompose(timetry)
plot(ttr)
vdm1 <- as.data.frame(approx(vdm[,1],vdm[,2], (0:402)/2))

freq <-21
vdm2 <- head(vdm1,freq*(length(vdm1[,2]) %/% freq))
plot(decompose(ts(vdm2[,2],start = 0, freq = 21)))

