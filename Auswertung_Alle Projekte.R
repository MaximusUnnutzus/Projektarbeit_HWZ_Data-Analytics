#R-Script zur Auswertung des gesamten Datensets
library(ggplot2)
library(readxl)
library(dplyr)
library(viridis)

df_log <- read_excel("C:/Users/maxwo/OneDrive/HWZ Applied Data Science/00_Leistungsausweis Zertifikatsarbeit/rohdaten/Anonymisierte Login.xlsx")
df_proj <- read_excel("C:/Users/maxwo/OneDrive/HWZ Applied Data Science/00_Leistungsausweis Zertifikatsarbeit/rohdaten/Anonymisierte Projektliste.xlsx")

# Datenvorbereitung
df_log_distinct <- df_log %>%
  distinct(kunde, .keep_all = TRUE) %>%
  dplyr::select(kunde, anzahl_login_kunde, anzahl_user, vertrag)


df <- left_join(df_proj, df_log_distinct, by="kunde")

#Z-Transformation des Deltas für Anonymisierung
df$delta_z <- scale(df$Delta)


#Funktionen für Wiederverwendung
plotFunction <- function(x,y, title, x_title, y_title){
  ggplot(df, aes(x={{x}}, y={{y}}, color=vertrag))+
    geom_point() +
    scale_color_manual(values = c("ns.publish"="blue","ns.wow"="orange","Beide Systeme"="grey"))+
    geom_smooth(method="lm", se=TRUE,color="red",linewidth=1)+
    labs(title = title, x=x_title, y=y_title, color ="Publishingsystem")
}


shapiroTest <- function(listOfVariables, varNames){
  for (i in seq_along(listOfVariables)) {
    cat("\nShapiro-Wilk Test für:", varNames[i], "\n")
    print(shapiro.test(listOfVariables[[i]]))
  }
}

plot_residuals <- function(model, x_var,log) {
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
    labs(title = paste("Residualplot:", x_var),
         x = x_var, y = "Residuen") +
    theme_minimal()
}

#Analyse für Aufwand in h
  # Anzahl User vs Aufwand in h
    plotFunction(df$anzahl_user,df$aufwand_in_h, 
                 "Anzahl User vs. Angefallene Dienstleistungsstunden (Aggregiert nach Kunden)","Anzahl User","Aufwand in h")
  
  # Anzahl Logins vs Aufwand in h
    plotFunction(df$anzahl_login_kunde,df$aufwand_in_h, 
                 "Anzahl Login vs. Angefallene Dienstleistungsstunden (Aggregiert nach Kunden)","Anzahl Logins","Aufwand in h")
  
  
  # Test auf Normalverteilung
    shapiroTest(
      list(df$aufwand_in_h, df$anzahl_user, df$anzahl_login_kunde), c("aufwand_in_h", "anzahl_user", "anzahl_login_kunde")
    )

  # Spearman Korrelationskoeffizent
    cor.test(df$aufwand_in_h, df$anzahl_user, method = "spearman")
    cor.test(df$aufwand_in_h, df$anzahl_login_kunde, method = "spearman")
    

    # Lineares Regressionsmodell
    lm_univ_aufwand_user <- lm(aufwand_in_h ~ anzahl_user, data = df)
    lm_univ_aufwand_login <- lm(aufwand_in_h ~ anzahl_login_kunde, data = df)
    lm_multi_aufwand <- lm(aufwand_in_h ~ anzahl_user + anzahl_login_kunde, data = df)
    summary(lm_univ_aufwand_user)
    summary(lm_univ_aufwand_login)
    summary(lm_multi_aufwand)
    
    #Residuals Plotten
    #für multivariates Modell
    model_data <- data.frame(
      fitted = fitted(lm_multi_aufwand),
      residuals = residuals(lm_multi_aufwand)
    )
    ggplot(model_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(se = FALSE, color = "blue") +
      labs(title = "Residuals vs Fitted Values",
           x = "Fitted Values", y = "Residuals")
    
    plot_residuals(lm_univ_aufwand_user, "anzahl_user",FALSE)
    plot_residuals(lm_univ_aufwand_login, "anzahl_login_kunde",FALSE)
    plot_residuals(lm_multi_aufwand, "anzahl_user",FALSE)
    plot_residuals(lm_multi_aufwand, "anzahl_login_kunde",FALSE)
    
    
    
    #Nächster Anlauf: log-transformation
    lm_multi_aufwand_log <- lm(log(aufwand_in_h+1) ~ log(anzahl_user+1) + log(anzahl_login_kunde+1), data = df)
    plot_residuals(lm_multi_aufwand_log, "anzahl_login_kunde", TRUE)
    summary(lm_multi_aufwand_log)
    
    # Polynomiales Regressionsmodell
    m_poly_combined <- lm(aufwand_in_h ~ poly(anzahl_user, 2) + poly(anzahl_login_kunde, 2), data = df)
    summary(m_poly_combined)
    
    ggplot(df, aes(x = anzahl_user, y = aufwand_in_h)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +
      labs(title = "Polynomiale Regression (2. Grades)", x = "Anzahl User", y = "Aufwand in h")
    
    #Vergleich: Multivariat Lineares Regressionsmodell vs. Polynomiales Regressionsmodell
    AIC(lm_multi_aufwand, m_poly_combined)
    
    # Mit Interaktionsterm Publishingsystem 
    m_interaction_user <- lm(aufwand_in_h ~ anzahl_user * vertrag, data = df %>% filter(vertrag %in% c("ns.publish", "ns.wow")))
    summary(m_interaction_user)
    
    m_interaction_login <- lm(aufwand_in_h ~ anzahl_login_kunde * vertrag, data = df %>% filter(vertrag %in% c("ns.publish", "ns.wow")))
    summary(m_interaction_login)
    
    m_interaction <- lm(aufwand_in_h ~ anzahl_login_kunde * vertrag+anzahl_user * vertrag, data = df %>% filter(vertrag %in% c("ns.publish", "ns.wow")))
    summary(m_interaction)
    
    ggplot(df %>% filter(vertrag %in% c("ns.publish", "ns.wow")), 
           aes(x = anzahl_login_kunde, y = aufwand_in_h, color = vertrag)) +
      geom_point() +
      scale_color_manual(values = c("ns.publish"="blue", "ns.wow"="orange")) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Interaktion: Anzahl Logins vs. Angefallene Dienstleistungsstunden nach System (Aggregiert nach Kunden)", x="Anzahl Logins", y="Aufwand in h", color ="Publishingsystem")
    
    ggplot(df %>% filter(vertrag %in% c("ns.publish", "ns.wow")), 
           aes(x = anzahl_user, y = aufwand_in_h, color = vertrag)) +
      geom_point() +
      scale_color_manual(values = c("ns.publish"="blue", "ns.wow"="orange")) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Interaktion: Anzahl User vs. Angefallene Dienstleistungsstunden nach System (Aggregiert nach Kunden)", x="Anzahl Logins", y="Aufwand in h", color ="Publishingsystem")

    
#Analyse für Delta
  # Anzahl User vs Delta
      plotFunction(df$anzahl_user,df$delta_z, 
                   "Anzahl User vs. Delta (Aggregiert nach Kunden)","Anzahl User","Delta (Z-Transformation)")
  
  # Anzahl Logins vs Aufwand in h
    plotFunction(df$anzahl_login_kunde,df$delta_z, 
                 "Anzahl Logins vs. Delta (Aggregiert nach Kunden)","Anzahl Logins","Delta (Z-Transformation)")
  
  # Test auf Normalverteilung
    shapiroTest(
      list(df$Delta, df$anzahl_user, df$anzahl_login_kunde), c("Delta", "anzahl_user", "anzahl_login_kunde")
    )
  
  
  # Spearman Korrelationskoeffizent
  cor.test(df$Delta, df$anzahl_user, method = "spearman")
  cor.test(df$Delta, df$anzahl_login_kunde, method = "spearman")
  
  
  # Lineares Regressionsmodell
  lm_univ_delta_user <- lm(delta_z ~ anzahl_user , data = df)
  lm_univ_delta_login <- lm(delta_z ~ anzahl_login_kunde, data = df)
  lm_multi_delta <- lm(delta_z ~ anzahl_user + anzahl_login_kunde, data = df)
  summary(lm_univ_delta_user)
  summary(lm_univ_delta_login)
  summary(lm_multi_delta)
  
  #Residuals Plotten
  #für multivariates Modell
  model_data <- data.frame(
    fitted = fitted(lm_multi_delta),
    residuals = residuals(lm_multi_delta)
  )
  ggplot(model_data, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "blue") +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values", y = "Residuals")
  
  plot_residuals(lm_univ_delta_user, "anzahl_user",FALSE)
  plot_residuals(lm_univ_delta_login, "anzahl_login_kunde",FALSE)
  plot_residuals(lm_multi_delta, "anzahl_user",FALSE)
  plot_residuals(lm_multi_aufwand, "anzahl_login_kunde",FALSE)
  
  
  # Lineares Regressionsmodell unter Betracht von Interaktionsterm
  m2_interaction_combined <- lm(delta_z ~ anzahl_user * vertrag+anzahl_login_kunde * vertrag, data = df %>% filter(vertrag %in% c("ns.publish", "ns.wow")))
  summary(m2_interaction_combined)
  
  m2_interaction_user <- lm(delta_z ~ anzahl_user * vertrag, data = df %>% filter(vertrag %in% c("ns.publish", "ns.wow")))
  summary(m2_interaction_user)
  
  m2_interaction_login <- lm(delta_z ~ anzahl_login_kunde * vertrag, data = df %>% filter(vertrag %in% c("ns.publish", "ns.wow")))
  summary(m2_interaction_login)

  
  ggplot(df %>% filter(vertrag %in% c("ns.publish", "ns.wow")), 
         aes(x = anzahl_login_kunde, y = delta_z, color = vertrag)) +
    geom_point() +
    scale_color_manual(values = c("ns.publish"="blue", "ns.wow"="orange")) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Interaktion: Anzahl Logins vs. entstandenes Delta nach System (Aggregiert nach Kunden)", x="Anzahl Logins", y="Aufwand in h", color ="Publishingsystem")
  
  

# Sonstige Plots, die Einsicht in den Zusammenhang potentieller Prädiktoren geben

df['projektdauer'] <- as.numeric(df$letzte_leistungsbuchung - df$erste_leistungsbuchung)
df['under_budget'] <- (df$aufwand_in_chf -as.numeric(df$initiales_budget))<0

ggplot(df, aes(x = erste_leistungsbuchung, y = projektdauer, color = under_budget)) +
  geom_point() +
  scale_color_manual(values = c("TRUE"="red","FALSE"="blue"))+
  labs(title = "Total Angefallener Aufwand nach Projektdauer",
       x = "Erste Leistungsbuchung", y = "Projektdauer (in Tagen)", color = "Unter Budget") +
  scale_x_datetime(date_labels = "%m.%y", date_breaks = "1 month")


ggplot(df, aes(x = anzahl_login_kunde, y = aufwand_in_h,
               color = anzahl_user)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(
    title = "Aufwand vs. Logins; Users als Farbe/Größe",
    x = "Anzahl Logins", y = "Aufwand (h)",
    color = "Anzahl User", size = "Anzahl User"
  )
  
