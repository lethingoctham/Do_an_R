setwd("C:\\Users\\acer\\Downloads\\COVID-19-master\\COVID-19-master\\csse_covid_19_data\\csse_covid_19_daily_reports_us")
names(data)
str(data)

#Thu vien
install.packages("ggplot2", lib="C:\\Users\\acer\\Downloads\\R\\win-library\\3.3")
update.packages("ggplot2")
library(tidyverse)
library(plotly)
library(dplyr)
library(data.table)

#Doc tat ca cac file vao mot dataframe
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill=TRUE)


NY <- data[Province_State=="New York"]
WST <- data[Province_State=="Washington"]
HW <- data[Province_State=="Hawaii"]
library("ggplot2", lib.loc="~/R/win-library/4.0")
#tai My ngay 01/02/2021
df1 <- read.table("01-02-2021.csv", 
                  header = TRUE,
                  sep = ",")
names(df1)
#Chuyen sang so thap phan co 2 chu so sau dau phay
df1$Case_Fatality_Ratio <- format(round(df1$Case_Fatality_Ratio, 2), nsmall = 2)
df1$Case_Fatality_Ratio
#tai My ngay 04/06/2021
df <- read.table("04-06-2021.csv", 
                 header = TRUE,
                 sep = ",")
names(df)
View(df)
df$Mortality_Rate <- format(round(df$Mortality_Rate, 2), nsmall = 2)

#1- Bieu do diem-so luong ca nhiem covid 19 tai Newyork
p = ggplot(data=WST, aes(x=Confirmed, y=Last_Update))
p+
  geom_point(aes(colour = Confirmed), colour = "yellow") +
  xlab("SO NGUOI NHiEM")+
  ylab("NGAY")+
  labs(title="Do thi the hien so luong ca nhiem covid 19 duoc xac nhan tai Newyork")

####################################
#2- Bieu do cot-so luong ca tu vong covid 19 tai Newyork
ggplot(NY, aes(x=Deaths, y=Last_Update))  + 
  geom_col(aes(color=Deaths,fill =Last_Update)) + 
  xlab("Deaths")+
  ylab("Last_Update")+
  labs(title="Do thi the hien so ca tu vong do covid 19 du???c xac nhan tai New York ")

###################################
# 3- Bieu do duong - So ca hoi phuc tai Newyord
p = ggplot(NY, aes(Last_Update, Recovered))
p+
  geom_line(aes(color = "Recovered",size = 1),colour="blue")+
  ylab("SO NGUOI HOI PHUC")+
  xlab("NGAY")+
  labs(title="Do thi the hien so ca hoi phuc tai NEWYORK")

#######################################
# 4-Bieu do diem - So ca nhiem benh tai WASHINTON
p = ggplot(data=WST, aes(x=Confirmed, y=Last_Update))
p+
  geom_point(aes(colour = Confirmed), colour = "orange") +
  xlab("SO NGUOI NHiEM")+
  ylab("NGAY")+
  labs(title="Do thi the hien so luong ca nhiem covid 19 du???c xac nhan tai Washington")

#############################################
# 5-Bieu do cot-so ca tu vong tai Washington
ggplot(NY, aes(x=Deaths, y=Last_Update,fill= Deaths )) + 
  geom_col(aes(color=Deaths,fill =Last_Update)) + 
  labs(title="Do thi the hien so luong ca tu vong covid 19 du???c xac nhan tai Washington ")

##################################################
#6 -Bieu do tron - So ca nhiem benh các khu vuc o My
fig <- plot_ly(data,
               type='pie',
               labels = ~Province_State,
               values = ~Confirmed,
               text = ~Country_Region,
               textposition = 'inside')

fig <- fig %>% layout(title = "So ca nhiem covid 19 cac khu vuc tai MY ",uniformtext=list(minsize=8, mode='hide'))
fig

##################################
#7 -Bieu do duong - So sanh ti le tu vong giua NewYork và Washington
ny_rec <- c(NY$Case_Fatality_Ratio)
wst_rec <- c(WST$Case_Fatality_Ratio)

plot(ny_rec,type = "o",col = "red",xlab = "NGAY",main = "So sanh ti le tu vong giua WASHITON VA NEW YORK ")
par(new=TRUE)
plot(wst_rec,type = "o",col = "green",xlab = "NGAY")
legend("topright", legend=c("New York Case Fatality Ratio", "Washinton Case Fatality Ratio"),
       col=c("red", "green"), lty=1:2, cex=0.8)

####################################
#8 Bieu do diem - So ca nhiem benh tai HAWAII
ggplot(HW, aes(x=Confirmed, y=Last_Update))+
  geom_point(aes(colour = Confirmed),
             colour = "skyblue")+
  xlab("SO NGUOI NHIEM")+
  ylab("NGAY")+
  labs(title=" Do thi the hien so luong ca nhiem covid 19 duoc xac nhan tai HAWAII")

################################################
#9-Bieu do cot - so nguoi tu vong cac khu vuc tai HAWAII
ggplot(HW, aes(x=Last_Update,
               color=Deaths)) +
  geom_col(aes(x=Last_Update,
               y=Deaths,
               fill =Deaths)) + 
  theme_grey() +
  labs(title="Do thi the hien so nguoi tu vong cac khu vuc tai HAWAII")

##############################
# 10 -Bieu do diem - so sanh ca nhiem benh giua NEW YORK va HAWAII
ny_con <- c(NY$Confirmed)
hw_con <- c(HW$Confirmed)

plot(ny_con,type = "o",col = "red",xlab = "NGAY ",main = "So sanh ti le nhiem benh giua HAWAII VA NEW YORK ")
par(new=TRUE)
plot(hw_con,type = "o",col = "green",xlab = "NGAY")
legend("topright", legend=c("New York Confirmed", "Hawaii Confirmed"),
       col=c("red", "green"), lty=1:1, cex=0.5)


#11- Bieu do tron- ca nhiem covide 19 t???i mot so bang/thanh pho o My 04-06-2021
ggplot(df[20: 26,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Do thi the hien so luong ca nhiem covid 19 duocc xac nhan o mot so bang/thanh pho tai MY
              trong ngay 04-06-2021") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 

#########################################
#12- Bieu do duong- Ti le tu vong tai NewYord t??? 12/2020-07/2021
ggplot(NY) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  labs(title="Bieu do the hien ti le tu vong gay ra boi covid duoc xac nhan o NewYord-MY
              t??? 12/2020 d???n 07/2021", x="Last Update", y="Case Fatality Ratio")

########################################
#13- Bieu do tron- so luong ca nhiem benh tai cac bang/thanh pho tai My 10-01-2020
fig2 <- plot_ly(data,
               type='pie',
               labels = ~Province_State,
               values = ~Confirmed,
               text = ~Country_Region,
               textposition = 'inside')
fig2 <- fig2 %>% layout(title = "Do thi the hien so luong ca nhiem benh boi covid 19 duoc xac nhan o cac bang/thanh pho
                         tai MY trong ngay 10-01-2020 ",uniformtext=list(minsize=8, mode='hide'))
fig2

################################################
#14- Bieu do tron- so ca tu vong tai cac bang/thanh pho tai My 10-01-2020
fig3 <- plot_ly(data,
                type='pie',
                labels = ~Province_State,
                values = ~Deaths,
                text = ~Country_Region,
                textposition = 'inside')

fig3 <- fig3 %>% layout(title = " Do thi the hien so luong ca tu vong boi covid 19 duoc xac nhan o mot so bang/thanh pho
                               tai MY trong ngay 10/01/2020 ",uniformtext=list(minsize=8, mode='hide'))
fig3

#####################################
#15- GEO_PLOT - So ca nhiem - Tu vong - Hoi phuc cua khu vuc MY 01-09-2021
fig4 <- df %>% 
  plot_geo(lat = ~Lat, lon = ~Long_) %>% 
  add_markers(
    text = ~paste("KHU VUC: ",Province_State,
                  "NUOC: ", Country_Region,
                  "NHIEM BENH: ",Confirmed,
                  "SO NGUOI TU VONG: ",Deaths,
                  "HOI PHUC: ",Recovered),
    color = ~Confirmed,
  )
fig4 <- fig4 %>% layout(title = " Do thi the hien so luong ca nhiem- tu vong-hoi phuc cua 
                           khu vuc MY 01-09-2021 ",uniformtext=list(minsize=8, mode='hide'))
fig4 

#####################################
#16 Bieu do diem -so luong ca tu vong t???i cac bang/thanh pho tai My trong ngày 04/03/2021 
ggplot(df, aes(x=Deaths, color=Province_State)) +
  geom_point(aes(x=Deaths, y=Province_State, fill =Province_State)) + 
  theme_grey() +
  labs(title="Do thi the hien so luong ca tu vong boi covid 19 duoc xac nhan o cac bang/thanh pho tai MY
              trong ngay 04-03-2021")

#################################
#17 GEO_PLOT - So ca nhiem - Tu vong - Hoi phuc cua khu vuc tren the gioi 15-02-2021
fig5 <- data %>% 
  plot_geo(lat = ~Lat, lon = ~Long_) %>% 
  add_markers(
    text = ~paste("KHU VUC: ",Province_State,
                  "NUOC: ", Country_Region,
                  "NHIEM BENH: ",Confirmed,
                  "SO NGUOI TU VONG: ",Deaths,
                  "HOI PHUC: ",Recovered),
    color = ~Confirmed,
  )
fig5 <- fig5 %>% layout(title = " Do thi the hien so luong ca nhiem- tu vong-hoi phuc 
    cua khu vuc tai MY trong 15-02 2021  ",uniformtext=list(minsize=8, mode='hide'))
fig5

#####################################
#18- bieu do cot- So luong ca tu vong tai mot so bang/thanh pho tai MY trong 01-01-2020  
ggplot(df1[1:20,], aes(x=Deaths, y=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill = Province_State)) + 
  labs(title=" so luong ca tu vong do covid 19 duoc xac nhan o mot so bang/thanh pho tai MY
                            trong ngay 01-01-2020",x = "Deaths", y="Province")

#########################
#19-  Bieu do tron- so luong ca hoi phuc ??? mot so bang tai My trong ngay 10-01-2021
ggplot(df1[2:15,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="So luong ca hoi phuc sau khi nhiem covid 19 o mot so bang/thanh pho tai MY
                                                        trong ngay 2021-01-10") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 

###########################
#20 -Bieu do diem- So luong ca tu vong tai NewYork tai MY t??? 01/2020 d???n 07/2021
ggplot(NY, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Deaths), colour = "red") + 
  labs(title="So luong ca tu vong boi covid 19 duoc xac nhan o NewYork tai MY
              tu 01/2020 d???n 07/2021")
