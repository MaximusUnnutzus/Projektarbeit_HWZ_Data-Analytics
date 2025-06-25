library(ggplot2)
library(readxl)
library(dplyr)
library(DescTools)

#load project_data
df_orig <- read_excel("C:/Users/maxwo/OneDrive/HWZ Applied Data Science/00_Leistungsausweis Zertifikatsarbeit/rohdaten/Anonymisierte Login.xlsx")
str(df_orig)
df_light <- df_orig[c("datum","kalenderwoche","Jahr","kunde", "user","anzahl_login_kunde","anzahl_login_user","login_kunde_ns.wow","login_kunde_ns.publish","anzahl_user")]



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


# Eindeutige Liste nur für Kunden
df_kunde_distinct <- df_light %>%
  distinct(kunde, .keep_all = TRUE) %>%
  select(kunde, anzahl_user,anzahl_login_kunde)

df_user_distinct <- df_light %>%
  distinct(user, .keep_all = TRUE) %>%
  select(user, anzahl_login_user)



# Anzahl Login pro User
kennzahlenBerechner(df_user_distinct$anzahl_login_user, "Anzahl Logins pro User")

par(mfrow = (c(1,2)))
boxplot(df_light$anzahl_login_user,
        main = "Anzahl Logins pro User (mit Ausreisser)",
        ylab = "Anzahl Logins",
        col = "lightblue")

q <- quantile(df_user_distinct$anzahl_login_user, probs = c(0.25, 0.75), na.rm = TRUE)
iqr <- q[2] - q[1]
untere_grenze <- q[1] - 1.5 * iqr
obere_grenze <- q[2]+1.5*iqr


subset_wO_Outliers <- df_user_distinct[df_user_distinct$anzahl_login_user <=  obere_grenze & df_user_distinct$anzahl_login_user >=  untere_grenze,]
boxplot(subset_wO_Outliers$anzahl_login_user,
        main = "Anzahl Logins pro User (ohne Ausreisser)",
        ylab = "Anzahl Logins",
        col = "lightblue")


# Anzahl Login pro Kunde
kennzahlenBerechner(df_kunde_distinct$anzahl_login_kunde, "Anzahl Logins pro Kunde")
mean_anz_login_kunde <- round(mean(df_kunde_distinct$anzahl_login_kunde, na.rm = TRUE),2)

par(mfrow = (c(1,2)))
boxplot(df_kunde_distinct$anzahl_login_kunde,
        main = "Anzahl Logins pro Kunde (mit Ausreisser)",
        ylab = "Anzahl Logins",
        col = "lightblue")

q <- quantile(df_kunde_distinct$anzahl_login_kunde, probs = c(0.25, 0.75), na.rm = TRUE)
iqr <- q[2] - q[1]
untere_grenze <- q[1] - 1.5 * iqr
obere_grenze <- q[2]+1.5*iqr


subset_wO_Outliers <- df_kunde_distinct[df_kunde_distinct$anzahl_login_kunde <=  obere_grenze & df_kunde_distinct$anzahl_login_kunde >=  untere_grenze,]
boxplot(subset_wO_Outliers$anzahl_login_kunde,
        main = "Anzahl Logins pro Kunde (ohne Ausreisser)",
        ylab = "Anzahl Logins",
        col = "lightblue")

ggplot(data=df_kunde_distinct, aes(x= anzahl_login_kunde))+
  geom_histogram(binwidth = 100, fill="steelblue", color= "black")+
  geom_vline(aes(xintercept = mean_anz_login_kunde), color="red",linetype="dashed", size=1)+
  annotate("text", x = mean_anz_login_kunde+150, y=12,label=paste0("mean: ",mean_anz_login_kunde),color="red",vjust=1.5, angle=0)+
  labs(title= "Anzahl der summierten User-Logins pro Kunde", x= "Anzahl Logins", y="Anzahl Kunden")+
  scale_x_continuous(breaks = seq(0, max(df_kunde_distinct$anzahl_login_kunde, na.rm = TRUE), by = 250))


# Anzahl User pro Kunde
kennzahlenBerechner(df_kunde_distinct$anzahl_user, "Anzahl User pro Kunde")
mean_anz_user <- round(mean(df_kunde_distinct$anzahl_user, na.rm = TRUE),2)

ggplot(data=df_kunde_distinct, aes(x= anzahl_user))+
  geom_histogram(binwidth = 1, fill="steelblue", color= "black")+
  geom_vline(aes(xintercept = mean_anz_user), color="red",linetype="dashed", size=1)+
  annotate("text", x = mean_anz_user+3, y=7.5,label=paste0("mean: ",mean_anz_user),color="red",vjust=1.5, angle=0)+
  labs(title= "Verteilung der Anzahl User pro Kunde", x= "Anzahl User", y="Anzahl Kunden")+
  scale_x_continuous(breaks = seq(0, max(df_kunde_distinct$anzahl_user, na.rm = TRUE), by = 5))


# Temporale Analyse
df_light$monat <- format(df_light$datum, "%Y-%m")

df_monatslogins <- df_light %>%
  count(monat)

kennzahlenBerechner(df_monatslogins$n, "Anzahl Logins pro Monat")

ggplot(df_monatslogins, aes(x = monat, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Loginaktivitäten pro Monat (alle Kunden)",
       x = "Monat", y = "Anzahl Logins") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))      
