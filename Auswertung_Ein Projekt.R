library(ggplot2)
library(readxl)
library(dplyr)

df_log <- read_excel("C:/Users/maxwo/OneDrive/HWZ Applied Data Science/00_Leistungsausweis Zertifikatsarbeit/rohdaten/Anonymisierte Login.xlsx")
df_proj <- read_excel("C:/Users/maxwo/OneDrive/HWZ Applied Data Science/00_Leistungsausweis Zertifikatsarbeit/rohdaten/Anonymisierte Projektliste.xlsx")

# Datenvorbereitung
df_1_proj <- df_proj %>%
            filter(anzahl_projekte_auf_kunde == 1)

df_log_distinct <- df_log %>%
  distinct(kunde, .keep_all = TRUE) %>%
  select(kunde, anzahl_login_kunde, anzahl_user,login_erste_30_tage,vertrag)

df <- left_join(df_1_proj, df_log_distinct, by="kunde")


#nach interne Abklärung: Diese Projekte müssen entfernt werden, da Ausreisser aufgrund interner Fehler        
df <- df %>%
        filter(aufwand_in_h >= 10)
df <- df %>%
  filter(login_erste_30_tage <= 75)


#Funktion für Residual-Plots
plot_residuals <- function(model, x_var, log) {
  if (log){
    value_forX = log(df[[x_var]]+1)
    label_forX = paste("log(", x_var, " + 1)", sep = "")
  }else{
    value_forX = df[[x_var]]
    label_forX = x_var
  }
  
  df_resid <- data.frame(
      x = value_forX,
      residuals = resid(model)
  )
  
  ggplot(df_resid, aes(x = x, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residualplot:", label_forX),
         x = x_var, y = "Residuen") +
    scale_y_continuous(breaks = seq(-100, 200, by = 50))+
    theme_minimal()

}

# Betrachtung Abhängige Variable & Prädiktor Login erste 30 Tage
    
  ggplot(df, aes(x = login_erste_30_tage, y = aufwand_in_h, color = vertrag))+
    geom_point() +
    scale_color_manual(values = c("ns.publish"="blue","ns.wow"="orange","Beide Systeme"="grey"))+
    geom_smooth(method="lm", se=TRUE,color="red",linewidth=1)+
    labs(title = "Anzahl Logins vs. Angefallene Dienstleistungsstunden auf Projekt", x="Anzahl Logins in den ersten 30 Tagen", y="Aufwand in h", color ="Publishingsystem")

  # Test auf Normalverteilung
  shapiro.test(df$aufwand_in_h)
  shapiro.test(df$login_erste_30_tage)


  # Spearman Korrelationskoeffizent
  cor.test(df$aufwand_in_h, df$login_erste_30_tage, method = "spearman")

  # Regressionsanalyse

  #Linear
  m1 <- lm(aufwand_in_h ~ login_erste_30_tage, data = df)
  summary(m1)
  plot_residuals(m1, "login_erste_30_tage",FALSE)
  #log-transformation
  m1_log <- lm(log(aufwand_in_h+1) ~ log(login_erste_30_tage+1), data = df)
  summary(m1_log)
  plot_residuals(m1_log, "login_erste_30_tage",TRUE)
  

  #Polynomial
  m_poly<- lm(aufwand_in_h ~ poly(login_erste_30_tage, 2), data = df)
  summary(m_poly)
  
  ggplot(df, aes(x = login_erste_30_tage, y = aufwand_in_h)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +
    labs(title = "Polynomiale Regression (2. Grades)", x = "Anzahl Login in den ersten 30 Tagen", y = "Aufwand in h")



# Betrachtung Abhängige Variable & Prädiktor Aufwand in h (nicht relevant für Arbeit - aber interessant)
  # Anzahl User vs Aufwand in h
  ggplot(df, aes(x = anzahl_user, y = aufwand_in_h, color = vertrag))+
    geom_point() +
    scale_color_manual(values = c("ns.publish"="blue","ns.wow"="orange","Beide Systeme"="grey"))+
    geom_smooth(method="lm", se=TRUE,color="red",linewidth=1)+
    labs(title = "Anzahl User vs. Angefallene Dienstleistungsstunden auf Projekt", x="Anzahl User", y="Aufwand in h", color ="Publishingsystem")
  
  # Anzahl Logins vs Aufwand in h
  ggplot(df, aes(x = anzahl_login_kunde, y = aufwand_in_h, color = vertrag))+
    geom_point() +
    scale_color_manual(values = c("ns.publish"="blue","ns.wow"="orange","Beide Systeme"="grey"))+
    geom_smooth(method="lm", se=TRUE,color="red",linewidth=1)+
    labs(title = "Anzahl Logins vs. Angefallene Dienstleistungsstunden auf Projekt", x="Anzahl Logins", y="Aufwand in h", color ="Publishingsystem")
  
  
  # Test auf Normalverteilung
  ## Signifikanztest: Hypothese prüfen, dass Grundgesamtheit einer Stichprobe normalverteilt ist
  ### h0: ist Normalverteilt
  shapiro.test(df$aufwand_in_h)
  shapiro.test(df$anzahl_user)
  shapiro.test(df$anzahl_login_kunde)
  
  
  # Spearman Rangkorrelation
  cor.test(df$aufwand_in_h, df$anzahl_user, method = "spearman")
  cor.test(df$aufwand_in_h, df$anzahl_login_kunde, method = "spearman")
  
  
  # Lineares Regressionsmodell
  m1 <- lm(aufwand_in_h ~ anzahl_user + anzahl_login_kunde, data = df)
  summary(m1)
  