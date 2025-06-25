library(ggplot2)
library(readxl)
library(dplyr)
library(DescTools)

#load project_data
df_orig <- read_excel("C:/Users/maxwo/OneDrive/HWZ Applied Data Science/00_Leistungsausweis Zertifikatsarbeit/rohdaten/Anonymisierte Projektliste.xlsx")
df_leistung <- read_excel("C:/Users/maxwo/OneDrive/HWZ Applied Data Science/00_Leistungsausweis Zertifikatsarbeit/rohdaten/Anonymisierte Leistungsdaten.xlsx")
str(df_orig)
str(df_leistung)

df_light <- df_orig[c("projekt","kunde", "initiales_budget","aufwand_in_chf","aufwand_in_h","Produktgruppe","zustaendig","archiviert_am","erste_leistungsbuchung","letzte_leistungsbuchung","erfasst_am","anzahl_projekte_auf_kunde","Delta")] 
df_light["projektdauer"] <- as.numeric(difftime(df_light$letzte_leistungsbuchung, df_light$erste_leistungsbuchung, units = "days"))
df_light["under_budget"] <- df_orig$Delta < 0
str(df_light)


kennzahlenBerechner <- function(variable, kennzahl_name) {
  cat(paste0(
    "Kennzahl: ", kennzahl_name, " | ",
    "Mittel = ", round(mean(variable, na.rm = TRUE), 2), " | ",
    "SD = ", round(sd(variable, na.rm = TRUE), 2), " | ",
    "Median = ", round(median(variable, na.rm = TRUE), 2), " | ",
    "Min = ", round(min(variable, na.rm = TRUE), 2), " | ",
    "Max = ", round(max(variable, na.rm = TRUE), 2), "\n"
  ))
}



# Anzahl Projekte auf Kunde
kennzahlenBerechner(df_light$anzahl_projekte_auf_kunde, "Anzahl Projekte pro Kunde")
mean_anz_proj <- round(mean(df_light$anzahl_projekte_auf_kunde, na.rm = TRUE),2)
  
ggplot(data=df_light, aes(x= anzahl_projekte_auf_kunde))+
  geom_histogram(binwidth = 1, fill="steelblue", color= "black")+
  geom_vline(aes(xintercept = mean_anz_proj), color="red",linetype="dashed", size=1)+
  annotate("text", x = mean_anz_proj+0.4, y=40,label=paste0("mean: ",round(mean_anz_proj,2)),color="red",vjust=1.5, angle=0)+
  labs(title= "Verteilung der Anzahl Projekte pro Kunde", x= "Anzahl Projekte", y="Anzahl Kunden")
  

# Angefallene Stunden Aufwand pro Kunde
  subset_agg_h <- aggregate(aufwand_in_h ~ kunde, data = df_light, sum) #aggregierte Dienstleistungsstunden pro Kunden

  kennzahlenBerechner(subset_agg_h$aufwand_in_h, "Angefallene Dienstleistungsstunden pro Kunde")
  mean_auf_kunde <- round(mean(subset_agg_h$aufwand_in_h, na.rm = TRUE),2)

  ggplot(data=subset_agg_h, aes(x= aufwand_in_h))+
    geom_histogram(binwidth = 15, fill="steelblue", color= "black")+
    geom_vline(aes(xintercept = mean_auf_kunde), color="red",linetype="dashed", size=1)+
    annotate("text", x = mean_auf_kunde+25, y=7.5,label=paste0("mean: ",mean_auf_kunde),color="red",vjust=1.5, angle=0)+
    labs(title= "Verteilung der angefallenen Dienstleistungsstunden pro Kunde", x= "Anzahl geleistete Dienstleistungsstunden", y="Anzahl Kunden")

  # Delta über Projekte und Kunde
  subset_agg_delta_proj <- aggregate(Delta ~ projekt, data = df_light, sum) #aggregierte Delta pro Projekt
  subset_agg_delta_kunde <- aggregate(Delta ~ kunde, data = df_light, sum) #aggregierte Delta pro Kunden
  
  kennzahlenBerechner(subset_agg_delta_proj$Delta, "Delta über Projekte")
  kennzahlenBerechner(subset_agg_delta_kunde$Delta, "Delta über Kunde")
  mean_auf_kunde <- round(mean(subset_agg_h$aufwand_in_h, na.rm = TRUE),2)

  par(mfrow = (c(1,2)))
  boxplot(subset_agg_delta_proj$Delta,
          main = "Delta über Projekte",
          ylab = "CHF",
          col = "lightblue")
  boxplot(subset_agg_delta_kunde$Delta,
          main = "Delta über Kunde",
          ylab = "CHF",
          col = "lightblue")
  
# Angefallene Stunden pro Projekt
  kennzahlenBerechner(df_light$aufwand_in_h, "Angefallene Dienstleistungsstunden pro Projekt")
  q <- quantile(df_light$aufwand_in_h, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  obere_grenze <- q[2]+1.5*iqr
  obere_grenze
  
  par(mfrow = c(1, 2))  # 1 Zeile, 2 Spalten
  boxplot(df_light$aufwand_in_h,
          main = "Angefallene Stunden pro Projekt (mit Ausreisser)",
          ylab = "Aufwand in Stunden",
          col = "lightblue")
  
  subset_wO_Outliers <- df_light[df_light$aufwand_in_h <  obere_grenze,]
  boxplot(subset_wO_Outliers$aufwand_in_h,
          main = "Angefallene Stunden pro Projekt (ohne Ausreisser)",
          ylab = "Aufwand in Stunden",
          col = "lightblue")
  
  nrow(df_light[df_light$aufwand_in_h>=obere_grenze,])
  

# Projektdauer
  kennzahlenBerechner(df_light$projektdauer, "Dauer eines Projektes in Kalendertagen")
  
  boxplot(df_light$projektdauer,
          main = "Boxplot: Projektdauer in Tagen",
          ylab = "Kalendertage",
          col = "lightblue")
  
  
  # Temporale Analyse: Startmonat
  df_light$startMonat <- format(df_light$erste_leistungsbuchung, "%Y-%m")
  
  df_startMonate <- df_light %>%
    count(startMonat)
  
  str(df_startMonate)
  
  
  kennzahlenBerechner(df_startMonate$n, "Kennzahlen zu eröffente Projekte pro Monat")
  
  ggplot(df_startMonate, aes(x = startMonat, y = n)) +
    geom_col(fill = "steelblue") +
    labs(title = "Projektstarts nach Monat",
         x = "Startmonat", y = "Anzahl Projekte") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))             
  
  # Temporale Analyse: Abschlussmonat
  df_light$abschlussMonat <- format(df_light$letzte_leistungsbuchung, "%Y-%m")
  
  df_abschlussMonate <- df_light %>%
    count(abschlussMonat)
  
  str(df_abschlussMonate)
  
  kennzahlenBerechner(df_abschlussMonate$n, "Kennzahlen abschliessende Projekte pro Monat")
  
  ggplot(df_abschlussMonate, aes(x = abschlussMonat, y = n)) +
    geom_col(fill = "steelblue") +
    labs(title = "Projektabschluss nach Monat",
         x = "Abschlussmonat", y = "Anzahl Projekte") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))     
  
  
#Leistungsbuchungen
  # Temporale Analyse: Leistungsdaten
  df_leistung$Monat <- format(df_leistung$Datum, "%Y-%m")
  
  df_angefalleneStundenMonat <- df_leistung %>%
                                  group_by(Monat) %>%
                                  summarise(gesamtstunden = sum(Anzahl, na.rm = FALSE))
  
  kennzahlenBerechner(df_angefalleneStundenMonat$gesamtstunden, "Kennzahlen geleistete Projektstunden pro Monat")

  ggplot(df_angefalleneStundenMonat, aes(x = Monat, y = gesamtstunden)) +
    geom_col(fill = "steelblue") +
    labs(title = "Verteilung aller Leistungsbuchungen",
         x = "Monat", y = "Anzahl geleistete Projektstunden") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    