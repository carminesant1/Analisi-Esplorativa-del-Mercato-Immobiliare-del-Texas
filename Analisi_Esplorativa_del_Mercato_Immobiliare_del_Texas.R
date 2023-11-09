#1) Scarica il dataset realestate_textas.csv da qui e importalo con R
#Working directory
setwd("C:/Users/csant/Desktop/ProfessionAI/Statistica Descrittiva")

#Dataset
library(readr)
texas <- read.csv("realestate_texas.csv")
View(texas)
attach(texas)

#2) Indica il tipo di variabili contenute nel dataset
str(texas)

#3) Calcola Indici di posizione, variabilità e forma per tutte le variabili per le quali ha senso farlo, per le altre crea una distribuzione di frequenza. Commenta tutto brevemente.
library(moments)

#Variabile "city"
ni.city<-table(city)
fi.city<-table(city)/dim(texas)[1]
Tabcity<-cbind(ni.city,fi.city)
Tabcity

#Variabile "year"
ni.year<-table(year)
fi.year<-table(year)/dim(texas)[1]
Ni.year<-cumsum(table(year))
Fi.year<-cumsum(table(year))/dim(texas)[1]
Tabyear<-cbind(ni.year,fi.year,Ni.year,Fi.year)
Tabyear

#Variabile "month"
ni.month<-table(month)
fi.month<-table(month)/dim(texas)[1]
Ni.month<-cumsum(table(month))
Fi.month<-cumsum(table(month))/dim(texas)[1]
Tabmonth<-cbind(ni.month,fi.month,Ni.month,Fi.month)
Tabmonth

#Altre variabili
quant_var <- colnames(texas[4:8])
lista_tab <- c()
for (i in quant_var) {
  #Indici di posizione
  Mean<-mean(texas[[i]]) #media
  Mode<-as.numeric(names(which.max(table(texas[[i]])))) #moda
  Median<-median(texas[[i]]) #mediana
  Min<-min(texas[[i]])
  Max<-max(texas[[i]])
  #Indici di variabilità
  Range<-Max-Min
  IQR<-IQR(texas[[i]])
  SD<-sd(texas[[i]]) #varianza
  #Variance.sales<-(var(sales)*(length(sales)-1))/length(sales) in alternativa
  Variance<-SD^2
  CV<-SD/Mean
  #Indici di forma
  Kurtosis<-kurtosis(texas[[i]])-3
  Skewness<-skewness(texas[[i]])
  Tab<-cbind(Mean,Mode,Median,Min,Max,Range,IQR,Variance,SD,CV,Skewness,Kurtosis)
  
  lista_tab[[i]] <- Tab
}
Tabcompl<-rbind(lista_tab[[(1)]], lista_tab[[(2)]], lista_tab[[(3)]], lista_tab[[(4)]], lista_tab[[(5)]])
Tabcompl<-as.data.frame(Tabcompl)
rownames(Tabcompl)<-colnames(texas[4:8])
Tabcompl

#4) Qual è la variabile con variabilità più elevata? Come ci sei arrivato? E quale quella più asimmetrica?
rownames(Tabcompl[which.max(Tabcompl$CV),])#la variabile con indice 2, ovvero volume, ha CV più alto, quindi variabilità più elevata
rownames(Tabcompl[which.max(abs(Tabcompl$Skewness)),])#la variabile con indice 2 ha indice skewness più elevato in valore assoluto, quindi è più asimmetrica

#5) Dividi una delle variabili quantitative in classi, scegli tu quale e come, costruisci la distribuzione di frequenze, il grafico a barre corrispondente e infine calcola l'indice di Gini.
#Per il numero di classi utilizzo la formula di Sturges
S<-round(1+10/3*log(length(sales)))
W<-ceiling((max(sales)-min(sales))/S)
class.sales<-cut(sales,breaks=c(seq(min(sales)-1,max(sales),W),max(sales)))
ni.class.sales<-table(class.sales)
Ni.class.sales<-cumsum(table(class.sales))
fi.class.sales<-table(class.sales)/length(sales)
Fi.class.sales<-cumsum(table(class.sales)/length(sales))
Tabclass.sales<-cbind(ni.class.sales,Ni.class.sales,fi.class.sales,Fi.class.sales)
Tabclass.sales
barplot(table(class.sales),ylim = c(0,30),xlab = "Classi di sales",ylab = "Freq. Ass.")

library(ineq)
ineq(class.sales,type = "Gini")

#6) Indovina l'indice di gini per la variabile city.
gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  gini = 1 - sum(fi2)
  gini.normalizzato = gini/((J-1)/J)
  return(gini.normalizzato)}

gini.index(city)

#7) Qual è la probabilità che presa una riga a caso di questo dataset essa riporti la città "Beaumont"? E la probabilità che riporti il mese di Luglio? E la probabilità che riporti il mese di dicembre 2012?
sum(city=="Beaumont")/nrow(texas)
sum(month==7)/nrow(texas)
sum(month==12 & year==2012)/nrow(texas)

#8) Esiste una colonna col prezzo mediano, creane una che indica invece il prezzo medio, utilizzando le altre variabili che hai a disposizione.
texas$mean_price<-(volume/sales)*1000000

#9) Prova a creare un'altra colonna che dia un'idea di "efficacia" degli annunci di vendita. Riesci a fare qualche considerazione?
texas$efflistings<-(sales/listings)*100
library(ggplot2)
texas$date <- as.Date(paste(texas$year,texas$month,"01",sep = "-"))
ggplot(data = texas)+
  geom_line(aes(x=date,y=efflistings, colour=city))+
  labs(title="Serie storica dell'efficacia degli annunci per città",
       x="Periodo",
       y="Percentuale di vendita sugli annunci attivi")+
  scale_x_date(breaks=seq(as.Date("2010-01-01"),as.Date("2014-12-01"), by="6 months"),date_labels = "%b %Y")+
  theme_classic()

#10) Prova a creare dei summary(), o semplicemente media e deviazione standard, di alcune variabili a tua scelta, condizionatamente alla città, agli anni e ai mesi.
library(dplyr)
texas %>%
  group_by(year,city) %>%
  summarise(media.di.sales=mean(sales),
            sd.di.volume=sd(volume))



#1) Utilizza i boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città. Commenta il risultato
ggplot(data=texas)+
  geom_boxplot(aes(y=median_price,x=city))+
  labs(title = "BoxPlot prezzo mediano tra città",
       x="Città",
       y="Prezzo mediano")+
  theme_classic()


#2) Utilizza i boxplot o qualche variante per confrontare la distribuzione del valore totale delle vendite tra le varie città ma anche tra i vari anni. Qualche considerazione da fare?
ggplot(data=texas)+
  geom_boxplot(aes(y=volume,x=city,fill=as.factor(year)))+
  labs(title = "BoxPlot valore totale vendite per città",
       x="Città",
       y="Valore totale vendite",
       fill="Anno")+
  theme_classic()

#3) Usa un grafico a barre sovrapposte per ogni anno, per confrontare il totale delle vendite nei vari mesi, sempre considerando le città. Prova a commentare ciò che viene fuori. Già che ci sei prova anche il grafico a barre normalizzato. 
library(gridExtra)
grafici <- c()
for (i in 2010:2014) {
  grafico<-ggplot(data=subset(texas,year==i))+
    geom_col(aes(x=month,y=sales,
                 fill=city),
             position = "stack")+
    labs(title = paste("Totale vendite per mese e città nell'anno" ,i),
         x="Mese",
         y="Totale delle vendite",
         fill="Città")+
    scale_x_continuous(breaks=seq(1,12,1))+
    scale_y_continuous(breaks = seq(100,1200,100))+
    theme_classic()+
    theme(plot.title = element_text(size = 12))
  grafici[[i - 2010 + 1]] <- grafico
}
grid.arrange(grobs = grafici, ncol = 3)

grafici1 <- c()
for (i in 2010:2014) {
  grafico1<-ggplot(data=subset(texas,year==i))+
    geom_col(aes(x=month,y=sales,
                 fill=city),
             position = "fill")+
    labs(title = paste("Percentuale totale vendite per mese e città nell'anno" ,i),
         x="Mese",
         y="Percentuale delle vendite",
         fill="Città")+
    scale_x_continuous(breaks=seq(1,12,1))+
    scale_y_continuous(breaks=seq(0,1,0.05))+
    theme_classic()+
    theme(plot.title = element_text(size = 10))
  grafici1[[i - 2010 + 1]] <- grafico1
}
grid.arrange(grobs = grafici1, ncol = 3)

#4) Crea un line chart di una variabile a tua scelta per fare confronti commentati fra città e periodi storici.
#per ogni anno
ggplot(data = texas)+
  geom_line(aes(x=date,y=mean_price,colour=city))+
  labs(title="Serie storica del prezzo medio tra le città",
       x="Periodo",
       y="Prezzo medio",
       colour="Città")+
  scale_x_date(breaks = seq(as.Date("2010-01-01"),as.Date("2014-12-01"), by="6 months"),date_labels = "%b %Y")+
  theme_classic()

