#.............................................
#            Oceanografic Model
#.............................................
#
# Author : Pamela Massiel Chiroque Solano
# Date   : 27/05/2020
# Email  : pchiroque@gmail.com
#        : pamela@dme.ufrj.br
# Local  : SAGE - Lab de biodiversidade
# Project: Rede Abrolhos        
#          Rio de Janeiro, Ilha do Governador
#
#.............................................


#### Read DATA:: PERFIL - FUNDEI ####
rm(list = ls())
require(dplyr)
require(ggplot2)
require(tidyr)

# names <- c(
# "time",
# "Camp",
# "Site",
# "Longitude",
# "Latitude",
# "CO",
# "O",
# "Temperatura",
# "Profundidade",
# "Condutividade",
# "Salinidade",
# "Densidade",
# "PAR",
# "Turbidez",
# "Clorofila",
# "PH",
# "MOD")

setwd("~/Documents/DADOS SEAGUARD/Analysis")

SEA1.F <- read.csv2(file = "FASE_1_PLANILHA_SEAGUARD_FUNDEIOS.csv",dec = ",")[-c(18)]
SEA1.F%>%summary()

n.Site <- SEA1.F%>%dplyr::select(Site)%>%table%>%names

SEA <- SEA1.F[SEA1.F$Site==n.Site[1],]

SEA[which((SEA$time=="N/A")),]
SEA[which((SEA$Camp=="N/A")),]
SEA[which((SEA$Longitude=="N/A")),]
SEA[which((SEA$Latitude=="N/A")),]
SEA[which((SEA$CO2=="N/A")),]
SEA[which((SEA$O=="N/A")),]
SEA[which((SEA$Temperatura=="N/A")),]
SEA[which((SEA$Profundidade=="N/A")),] # reportar
SEA[which((SEA$Condutividade=="N/A")),]
SEA[which((SEA$Salinidade=="N/A")),]
SEA[which((SEA$Densidade=="N/A")),]
SEA[which((SEA$PAR=="N/A")),]
SEA[which((SEA$Turbidez=="N/A")),]
SEA[which((SEA$Clorofila=="N/A")),]
SEA[which((SEA$PH=="N/A")),] # reportar
SEA[which((SEA$MOD=="N/A")),] # reportar



SEA1.P.CO2 <- read.csv2(file = "FASE_1_PLANILHA_SEAGUARD_PERFIS_CO2.csv",header = TRUE,dec = ".")
SEA1.P.CO2%>%summary

SEA1.P <- read.csv2(file = "FASE_1_PLANILHA_SEAGUARD_PERFIS.csv",header = TRUE,dec = ",")

SEA <- SEA1.P
SEA%>%dim
SEA%>%str 

SEA%>%summary


SEA[which((SEA$CO2=="N/A")),]
SEA[which((SEA$O2=="N/A")),]
SEA[which((SEA$Temperatura=="N/A")),]
SEA[which((SEA$Profundidade=="N/A")),]
SEA[which((SEA$Condutividade=="N/A")),]
SEA[which((SEA$Salinidade=="N/A")),]


SEA[which((SEA$Densidade=="N/A")),]
SEA[which((SEA$PAR=="N/A")),]
SEA[which((SEA$Turbidez=="N/A")),]
SEA[which((SEA$Clorofila=="N/A")),]
SEA[which((SEA$PH=="N/A")),]
SEA[which((SEA$MOD=="N/A")),]

SEA2.F <- read.csv2(file = "FASE_2_PLANILHA_SEAGUARD_FUNDEIOS.csv",dec = ",")[-c(18)]
SEA <- SEA2.F
SEA%>%dim
SEA%>%str 

SEA%>%summary


SEA[which((SEA$CO2=="N/A")),]

SEA2.P <- read.csv2(file = "FASE_2_PLANILHA_SEAGUARD_PERFIS.csv",header = TRUE,dec = ",")

SEA <- SEA2.P
SEA%>%dim
SEA%>%str 

SEA%>%summary


SEA[which((SEA$CO2=="N/A")),]


##### Exploration: SEA1.P.CO2 ####
rm(list = ls())
require(dplyr)
require(ggplot2)
require(tidyr)

setwd("~/Documents/DADOS SEAGUARD/Analysis")


SEA1.P.CO2 <- read.csv2(file = "FASE_1_PLANILHA_SEAGUARD_PERFIS_CO2.csv",header = TRUE,dec = ".")
SEA1.P.CO2%>%summary

SEA1.P.CO2$time

require(stringi)

date <- stri_sub(SEA1.P.CO2$time,1,10)
today <- as.Date(date, format = "%Y-%m-%d")
SEA1.P.CO2$Date <- format(today, format="%d %b %y")

today%>%table
SEA1.P.CO2$Date%>%table
letter <- stri_sub(SEA1.P.CO2$time,11,11)

letter%>%table()

hour <- stri_sub(SEA1.P.CO2$time,12,19)
SEA1.P.CO2$Hour <- format(hour, format="%H:%M:%S")
hour%>%table()

SEA1.P.CO2%>%
  ggplot()+
  geom_point(aes(x=Hour,y=CO2,color=Site))

SEA1.P.CO2%>%ggplot()+
  geom_point(aes(x=time,y=Pressure,color=Site))

SEA1.P.CO2%>%ggplot()+
  geom_point(aes(x=CO2,y=Pressure,color=Site))





library(corrplot)

n.Site <- SEA1.P.CO2%>%dplyr::select(Site)%>%table()%>%names()
  
for(i in 1:length(n.Site)){
  SEA1.P.CO21 <- SEA1.P.CO2%>%dplyr::filter(Site == n.Site[i])  
matcor <- round(cor(SEA1.P.CO21[c("CO2","Pressure")]),2)
print(matcor)
print(ggcorrplot(matcor))
  invisible(readline(prompt="Press [enter] to continue"))

}
require(ggcorrplot)


df %>% drop_na()

##### Exploration: SEA2.P ####
rm(list = ls())
require(dplyr)
require(ggplot2)
require(tidyr)

setwd("~/Documents/DADOS SEAGUARD/Analysis")

SEA2.P <- read.csv2(file = "FASE_2_PLANILHA_SEAGUARD_PERFIS.csv",header = TRUE,dec = ",")

SEA <- SEA2.P
SEA%>%dim
SEA%>%str 
SEA%>%colnames()
SEA%>%summary

SEA$Prof <- -SEA$Profundidade


var <- c("O2","Temperatura","Condutividade","Densidade",  
"PAR","Turbidez","Clorofila","PH","MOD")

seq.N.Depth <- seq(-max(SEA$Profundidade),0,length = 5)
seq.Depth <- as.character( 
round(  seq(max(SEA$Profundidade),min(SEA$Profundidade),length = 5),2)
  )

for( i in 1:length(var)){
q <- SEA%>%ggplot()+
  geom_point(aes(x=Salinidade,y=Prof,color=eval(parse(text=var[i]))))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12,angle=0,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=20),
        strip.text =  element_text(size=18))+
  scale_y_continuous(breaks = seq.N.Depth,
                     labels = seq.Depth) +
  labs(x="Salinidade",
       y="Profundidade", 
       color=var[i])
q

ggsave(filename = paste("",var[i],".pdf",sep = ""),
       plot = q,device = "pdf",width = 5,height = 5)

}

