
#https://r4ds.had.co.nz/data-visualisation.html
# Rscript name_file.R

install.packages('ggplot2')

install.packages('ggpubr')

install.packages('readxl')

install.packages('dplyr')

setwd("~/Google Drive/III.ComponenteProfesional/20/MTPE/Lab/Outputs/Entregable_5/5.1.DiagnósticoSistemPerfiles")

library(ggplot2)

library(ggpubr)

library(tidyverse)

library(readxl)

library(dplyr)


# Loading data

df <-read_excel('./Perfil_Encuestas_DISEL_pool.xlsx',sheet="df")

tible_df <-as_tibble(df)%>% drop_na()

glimpse(tible_df)


# Transforming variables

tible_df$Unidad 			<-factor(tible_df$Unidad) 
tible_df$Profesión			<-factor(tible_df$Profesión) 


tible_df$años_MTPE  		<- as.numeric(tible_df$años_MTPE)  

tible_df$años_ED_ML   	  <- as.numeric(tible_df$años_ED_ML)  
tible_df$años_EI_ML   	   <- as.numeric(tible_df$años_EI_ML)  
tible_df$años_ED_IPRE   	<- as.numeric(tible_df$años_ED_IPRE)  


# seting elements

# Graph 1

df_pie1 	<-as.data.frame(table (tible_df$Unidad)) 
labels_pie1 <- paste0(df_pie1$Var1, " = ", round(100 * df_pie1$Freq/sum(df_pie1$Freq), 2), "%")

# Graph 2

df_pie2 	<-as.data.frame(table (tible_df$Profesión)) 
labels_pie2 <- paste0(df_pie2$Var1, " = ", round(100 * df_pie2$Freq/sum(df_pie2$Freq), 2), "%")

detach(package:plyr)

# Graph 3

df_bar_1 <- filter(tible_df,Unidad!='Red OSEL') %>% 
  group_by(Unidad) %>%
  summarize(mean=mean(años_MTPE, na.rm = TRUE)) %>% ungroup()

----------------------------------------------------------------------
# Graph 4

names<-c('Descriptiva-Mercado Laboral','Inferencial-Mercado Laboral','Descriptiva-IPRE','Inferencial-IPRE')
c <-c(mean(tible_df$años_ED_ML), mean(tible_df$años_EI_ML), mean(tible_df$años_ED_IPRE), mean(tible_df$años_E_I_IPRE))

df <-data.frame(x=names , y=c)

------------------------------------------------------------
# Graph 5

df_bar_2 <- filter(tible_df,Unidad!='Red OSEL') %>% 
  group_by(Unidad) %>%
  summarize(mean=mean(años_ED_ML, na.rm = TRUE)) %>% ungroup()

df_bar_3 <- filter(tible_df,Unidad!='Red OSEL')  %>%
  group_by(Unidad) %>%
  summarize(mean=mean(años_EI_ML, na.rm = TRUE)) %>% ungroup()

df_bar_4 <- filter(tible_df,Unidad!='Red OSEL') %>%
  group_by(Unidad) %>%
  summarize(mean=mean(años_ED_IPRE , na.rm = TRUE)) %>% ungroup()

df_bar_5 <- filter(tible_df,Unidad!='Red OSEL')  %>%
  group_by(Unidad) %>%
  summarize(mean=mean(años_E_I_IPRE , na.rm = TRUE)) %>% ungroup()

------------------------------------------------------------
# Graph 6
names_6<-c('Stata','R','Python','SQL','Github','AWSoAzure')
d_6 <-c(mean(tible_df$Stata), mean(tible_df$R), mean(tible_df$Python), mean(tible_df$SQL),mean(tible_df$Github),mean(tible_df$AWSoAzure))

df_6 <-data.frame(x=names_6 , y=d_6)
------------------------------------------------------------
#Graph 7
------------------------------------------------------------

df_g71 <- filter(tible_df,Unidad!='Red OSEL') %>% 
  group_by(Unidad) %>%
  summarize(mean=mean(Stata, na.rm = TRUE)) %>% ungroup()

df_g72 <- filter(tible_df,Unidad!='Red OSEL')  %>%
  group_by(Unidad) %>%
  summarize(mean=mean(R, na.rm = TRUE)) %>% ungroup()

df_g73 <- filter(tible_df,Unidad!='Red OSEL') %>%
  group_by(Unidad) %>%
  summarize(mean=mean(SQL , na.rm = TRUE)) %>% ungroup()

df_g74 <- filter(tible_df,Unidad!='Red OSEL')  %>%
  group_by(Unidad) %>%
  summarize(mean=mean(Python , na.rm = TRUE)) %>% ungroup()


df_g75 <- filter(tible_df,Unidad!='Red OSEL')  %>%
  group_by(Unidad) %>%
  summarize(mean=mean(Github , na.rm = TRUE)) %>% ungroup()


df_g76 <- filter(tible_df,Unidad!='Red OSEL')  %>%
  group_by(Unidad) %>%
  summarize(mean=mean(AWSoAzure, na.rm = TRUE)) %>% ungroup()


# integration + execution of elements

------------------------------------------------------------
 # Graph 1

pie(df_pie1$Freq, labels = labels_pie1, main="Gráfico 1: Distribución de personal DISEL, según Unidad" )
------------------------------------------------------------
 # Graph 2

pie(df_pie2$Freq, labels = labels_pie2, main="Gráfico 2: Distribución de personal DISEL, según Profesión",cex=0.5)

------------------------------------------------------------
 # Graph 3

ggplot(df_bar_1, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity",width=0.4) +
  ggtitle("Gráfico 3: Experiencia profesional, según Unidad") +
  labs(y="Promedio de años", x="Unidad")

------------------------------------------------------------
# Graph 4

ggplot(df,aes(x=reorder(names,y),y=y))+
  geom_bar(sta="identity")+ 
  ggtitle("Gráfico 4: Experiencia según estratos temáticos")+
  labs(y="Promedio de años",x="Estratos")


------------------------------------------------------------
# Graph 5


plot_1<-ggplot(df_bar_2, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("Descriptiva-Mercado Laboral") +
  labs(y="Promedio de años", x="Unidad")


plot_2<- ggplot(df_bar_3, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("Inferencial-Mercado Laboral") +
  labs(y="Promedio de años", x="Unidad")


plot3<- ggplot(df_bar_4, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("Descriptiva-IPRE") +
  labs(y="Promedio de años", x="Unidad")

plot4<-ggplot(df_bar_5, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("Inferencial-IPRE") +
  labs(y="Promedio de años", x="Unidad")

(figure <-ggarrange(plot_1,plot_2,plot3,plot4,ncol=2,nrow=2))

------------------------------------------------------------
# Graph 6

ggplot(df_6,aes(x=reorder(names_6,y),y=y))+
  geom_bar(sta="identity")+ 
  ggtitle("Gráfico 6: Conocimiento de Softwares estadísticos")+
  labs(y="Promedio de score [0-10]",x="Softwares")

------------------------------------------------------------
# Graph 7

plot_g71<-ggplot(df_g71, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("Stata") +
  labs(y="Promedio de score [0-10]", x="Unidad")


plot_g72<- ggplot(df_g72, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("R") +
  labs(y="Promedio de score [0-10]", x="Unidad")


plot_g73<- ggplot(df_g73, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("SQL") +
  labs(y="Promedio de score [0-10]", x="Unidad")

plot_g74<-ggplot(df_g74, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("Python") +
  labs(y="Promedio de score [0-10]", x="Unidad")

plot_g75<-ggplot(df_g75, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("Github") +
  labs(y="Promedio de score [0-10]", x="Unidad")

plot_g76<-ggplot(df_g76, aes(x=reorder(Unidad,mean), y=mean)) + 
  geom_bar(stat = "identity", width=0.4) +
  ggtitle("AWSoAzure") +
  labs(y="Promedio de score [0-10]", x="Unidad")


(figure <-ggarrange(plot_g71,plot_g72,plot_g73,plot_g74,plot_g75,plot_g76,ncol=3,nrow=3))


