
ggplot(df_light, aes(x = erste_leistungsbuchung, y = projektdauer, color = under_budget)) +
  geom_point() +
  scale_color_manual(values = c("TRUE"="red","FALSE"="blue"))+
  labs(title = "Total Angefallener Aufwand nach Projektdauer",
       x = "Erste Leistungsbuchung", y = "Projektdauer (in Tagen)", color = "Unter Budget") +
  scale_x_datetime(date_labels = "%m.%y", date_breaks = "1 month")

df_strange <- df_light[df_light$projektdauer < 5,]  
View(df_strange)