library(eurostat)
library(zoo)
library(tidyverse)
#Laver for-loop (Definerer lande)
lande <- c("DK") 
liste <- list()

for (i in 1:length(lande)) {
#Til brug for at clear cache
clean_eurostat_cache()

#Laver query/filter for ønskede data
Euindikatorfilter <- list(s_adj = "SA",
                          geo = lande[i],
                          sinceTimePeriod = "2000-01")

euindikator <- get_eurostat(id = "EI_BSCO_M", filters = Euindikatorfilter)

#Inddrager udelukkende EU-lande på baggrund af deres landkode
euindikator2 <- euindikator[nchar(euindikator$geo) == 2,]

#Fjerner 3 unødvendige kolonner 
euindikator2 <- euindikator2[,-c(1,3,4)]

#Transposerer dataframe
euindikator3 <- pivot_wider(euindikator2,
                        id_cols = time,
                        names_from = (indic),
                        values_from = values)

#Navngiver kolonnerne
colnames(euindikator3) <- c("Dato","Familiens økonomiske situation seneste 12 mdr",
                            "Familiens økonomiske situation situation næste 12 mdr",
                            "General økonomisk situation seneste 12 mdr.",
                            "General økonomisk situation næste 12 mdr",
                            "Priser i dag sammenlignet med for et år siden", 
                            "Priser om et år, sammenlignet med i dag", 
                            "Arbejdsløshedsforventninger om et år sammenlignet med i dag",
                            "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                            "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.",
                            "Anser man det som fornuftigt at spare op i den nuværende økonomiske situation",
                            "Regner med at kunne spare op i de kommende 12 mdr.", 
                            "Familiens nuværende økonomiske situation", 
                            "Forbrugertillidsindikator")

#Fra måneder til kvartaler (Under udarbejdelse)
euindikatorkvar <- euindikator3 %>% 
  group_by(Dato = format(as.yearqtr(Dato, "%Y-%m"), "%Y Q%q")) %>%
  summarise_all(mean)
  euindikatorkvar2 <- round(euindikatorkvar[,c(2:ncol(euindikatorkvar))],2)
  euindikatorkvar2 <- cbind(euindikatorkvar$Dato, euindikatorkvar2)
  colnames(euindikatorkvar2)[1] = "Dato"
  
#Gemmer mine lande datasæt i liste
liste[[lande[i]]] <- euindikatorkvar2
euindikatorkvar2 <- euindikatorkvar[-96,]

#Opdeler spørgsmål i mikro og makrøøkonomi
#Mikroøkonomi
eumcci <- euindikatorkvar2[,-c(4:8)]
#Makroøkonomi
eumacci <- euindikatorkvar2[,c(1,4:8)]

#Lav top 5 summary i et loop
}



