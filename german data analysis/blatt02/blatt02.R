install.packages('tidyverse')
library(tidyverse)

data <- read_csv("data/oktoberfestgesamt19852023.csv")

head(data, n=10)



#Year 
yearColumn <- data$jahr

typeOfYearColumn <- typeof(yearColumn)
classOfYearColumn <- class(yearColumn)
if (is.numeric(yearColumn)) {
  #Minimum Value
  min_value <- min(yearColumn, na.rm = TRUE)  # Use na.rm = TRUE to ignore missing values (NA)
  # Maximum value
  max_value <- max(yearColumn, na.rm = TRUE)
  
  cat("Jahr:",typeOfYearColumn,"(min:",min_value,", max:", max_value, ")")
} else {
  cat("Jahr:",typeOfYearColumn,"(min:",min_value,", max:", max_value, ")")
}

#Total Visitor 
visitorTotalColumn <- data$besucher_gesamt

typeOfVisitorTotalColumn <- typeof(visitorTotalColumn)
classOfVisitorTotalColumn <- class(visitorTotalColumn)
if (is.numeric(visitorTotalColumn)) {
  #Minimum Value
  min_value <- min(visitorTotalColumn, na.rm = TRUE)  # Use na.rm = TRUE to ignore missing values (NA)
  # Maximum value
  max_value <- max(visitorTotalColumn, na.rm = TRUE)
  
  cat("Gesamt Besucher:",typeOfVisitorTotalColumn,"(min:",min_value,", max:", max_value, ")")
} else {
  cat("Gesamtbesucher:",typeOfVisitorTotalColumn,"(min:",min_value,", max:", max_value, ")")
}


#Price of Beer
priceBeerColumn <- data$bier_preis

typeOfPriceBeerColumn <- typeof(priceBeerColumn)
classOfPriceBeerColumn <- class(priceBeerColumn)
if (is.numeric(priceBeerColumn)) {
  #Minimum Value
  min_value <- min(priceBeerColumn, na.rm = TRUE)  # Use na.rm = TRUE to ignore missing values (NA)
  # Maximum value
  max_value <- max(priceBeerColumn, na.rm = TRUE)
  
  cat("Bierpreis:",typeOfYearColumn,"(min:",min_value,", max:", max_value, ")")
} else {
  cat("Bierpreis:",typeOfYearColumn,"(min:",min_value,", max:", max_value, ")")
}


#Beer Consume
bierConsumeColumn <- data$bier_konsum

typeOfBierConsumeColumn <- typeof(bierConsumeColumn)
classOfBierConsumeColumn <- class(bierConsumeColumn)
if (is.numeric(bierConsumeColumn)) {
  #Minimum Value
  min_value <- min(bierConsumeColumn, na.rm = TRUE)  # Use na.rm = TRUE to ignore missing values (NA)
  # Maximum value
  max_value <- max(bierConsumeColumn, na.rm = TRUE)
  
  cat("Jahr:",typeOfBierConsumeColumn,"(min:",min_value,", max:", max_value, ")")
} else {
  cat("Jahr:",typeOfBierConsumeColumn,"(min:",min_value,", max:", max_value, ")")
}




esquisse::esquisser()
library(ggplot2)

ggplot(aktivBeschaeftigte) +
 aes(x = jahr, y = insgesamt) +
 geom_step(linewidth = 2L, colour = "#228B22") +
 labs(x = "Jahr", y = "Gesamtanzahl", title = "Aktiv Beschäftigte der Landeshauptstadt München", 
 subtitle = "Zwischen 2017 bis 2023", caption = "Open Data Port Munich (https://opendata.muenchen.de/id/dataset/aktiv-beschaftigte-der-landeshauptstadt-muenchen/resource/c49554db-becb-4bff-a16e-694bcda23376)") +
 theme_gray() +
 theme(plot.title = element_text(size = 16L, face = "bold"), plot.subtitle = element_text(size = 13L), 
 plot.caption = element_text(size = 8L))

library(ggplot2)







ggplot(dataKernbeschaeftigte) +
 aes(x = alter, y = vollzeit_weiblich) +
 geom_col(fill = "#8D5108") +
 theme_minimal() +
 facet_wrap(vars(jahr))


#Dauer über die Jahre 
  ggplot(
    data = oktoberfest,
    mapping = aes(
      x = dauer,
      fill = as.factor(dauer) # Färbung
    )
  ) +
  geom_bar() + 
  theme_linedraw() +
53-8235/-q978                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
typeof(data$jahr)


#Besucher über die Jahre
ggplot(data) +
  aes(x = jahr, y = besucher_gesamt) +
  geom_point(size = 5L, shape = "square", colour = "#228B22") +
  labs(x = "Jahr", y = "Anzahl der Besucher (in Millionen)", title = "Oktoberfest (1985-2023)", subtitle = "Anzahl der Besucher (in Millionen) über Jahren", 
       caption = "Open Data Portal Munich (https://opendata.muenchen.de/id/dataset/oktoberfest/resource/e0f664cf-6dd9-4743-bd2b-81a8b18bd1d2)") +
  theme_light() +
  theme(legend.justification = "top", plot.title = element_text(size = 18L, face = "bold"), 
        plot.subtitle = element_text(size = 14L), axis.title.y = element_text(size = 14L), axis.title.x = element_text(size = 14L), 
        legend.text = element_text(size = 12L), legend.title = element_text(size = 12L))


#Bier-Preise über die Jahren
ggplot(data) +
  aes(x = jahr, y = bier_preis) +
  geom_line(colour = "#112446") +
  labs(x = "Jahr ", y = "Bier-Preis", 
       title = "Oktoberfest (1985-2023)", subtitle = "Bier-Preise auf den Oktoberfest über die Jahren hinweg", 
       caption = "Open Data Portal Munich (https://opendata.muenchen.de/id/dataset/oktoberfest/resource/e0f664cf-6dd9-4743-bd2b-81a8b18bd1d2)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18L, face = "bold"), plot.subtitle = element_text(size = 14L))



#Bier-Konsum über die Jahren
ggplot(data) +
  aes(x = jahr, y = bier_konsum) +
  geom_line(colour = "#112446") +
  labs(x = "Jahr ", y = "Bier-Konsum", 
       title = "Oktoberfest (1985-2023)", subtitle = "Bier-Konsum auf den Oktoberfest über die Jahren hinweg", 
       caption = "Open Data Portal Munich (https://opendata.muenchen.de/id/dataset/oktoberfest/resource/e0f664cf-6dd9-4743-bd2b-81a8b18bd1d2)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18L, face = "bold"), plot.subtitle = element_text(size = 14L))

#Bier-Konsum und -Preis auf dem Oktoberfest.
ggplot(data) +
  aes(x = bier_preis, y = bier_konsum, size = besucher_tag) +
  geom_point(shape = "circle small", 
             colour = "#000000") +
  labs(x = "Bier-Konum (in Hektolitern)", y = "Bier-Preis (in EUR)", title = "Bier-Konsum und -Preis auf dem Oktoberfest", 
       subtitle = "Zwischen 1985 und 2023", caption = "Quelle: Open Data Platform der Stadt München") +
  theme_gray() +
  theme(plot.title = element_text(size = 18L, face = "bold"), plot.subtitle = element_text(size = 16L), 
        plot.caption = element_text(size = 13L, hjust = 0.5), axis.title.y = element_text(size = 16L), axis.title.x = element_text(size = 16L))

#Beschreibung 

#Ja, wei man sehen kann dass eine lineare Beziehung zwischen beiden gibt. 



#Gesamte Menge an Hendln und Hünchen
totalHuenchen <- sum(data$hendl_konsum)
print(totalHuenchen)


#Aufgabe 5

dataKernbeschaeftigte <- read_csv("data/lhm-personal-kernbeschaeftigte-alter-az.csv")
head(dataKernbeschaeftigte)

dataKernbeschaeftigte$jahr <- as.factor(dataKernbeschaeftigte$jahr)


ggplot(dataKernbeschaeftigte) +
  aes(x = alter, y = vollzeit_weiblich, fill = jahr) +
  geom_col() +
  scale_fill_brewer(palette = "Set2", direction = 1) +  # Use a vibrant Brewer palette
  labs(
    x = "Altergruppe", 
    y = "Anzahl der Vollzeitbeschäftigte", 
    title = "Weibliche Vollzeitbeschäftigte und Altersgruppe", 
    subtitle = "In den Jahren 2022 und 2023", 
    caption = "Open Data Portal of Munich", 
    fill = "Jahr"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18L, face = "bold"), 
    plot.subtitle = element_text(size = 16L), 
    axis.title.y = element_text(size = 14L), 
    axis.title.x = element_text(size = 14L)
  )


aktivBeschaeftigte <- read_csv("data/lhm-personal-aktiv-beschaeftigte.csv")


