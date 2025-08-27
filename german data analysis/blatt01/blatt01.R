#Name: Seyed Ramtin Hosseini
#Matrikelnummern: 1111111
#Hauptfach: Informatik
#Blatt: 01

############################################################################################

#Aufgabe 1
#Aufgabe 1b
install.packages("haven")
library(haven)


#Aufgabe 1c
allbus <- read_dta("data/ZA5280_v2-0-1.dta")

#Aufabe 1d
n_befragte1 <- nrow(allbus)
print(paste("Anzahl der Befragte in dem Datensatz allBuss ist: ", n_befragte1))

#alternative methode

n_befragte2 <- length(allbus$za_nr)
print(paste("(Alternativ) Anzahl der Befragte in dem Datensatz allBuss ist: ", n_befragte2))

#Aufgabe 1e
head(allbus, n=3)

#Aufgabe 1f
View(allbus)
#respid -> IDENTIFIKATIONSNUMMER DES BEFRAGTEN

########################################################################################################################################################################################

#Aufgabe 2


#Aufgabe 2b
#Auf die Seite 731 des Codebuches findet man die relevante Informationen. 


#Aufgabe 2c
# Leben in Eigentumswohnung (Wert: 6 im CodeBuch): 469 Personen
# Prozent in Untermiete (Wert: 1): 3,9% (210 Personen)
# Häufigste Art zu Wohnen ist im Eigenheim (Wert: 7): 2529 Personen

#Aufgabe 2d
#Luat Codbuch bedeutet der Wert -9 "Keine Abgabe" (insgesamt 156 keine Angabe)

#Aufgabe 2e
#table() Befehl wird hier genutzt
apartmentColumn <- table(allbus$aq01)
print(apartmentColumn)
#Ausgabe: 
#-42  -41  -33   -9    1    2    3    4    5    6    7    8 
#18    1   21  159  232   17  335 1430  187  420 2495   27 


eigentumswohnungAnzahl <- apartmentColumn["6"]
print(paste("Häufigkeit für Eigenwohnungen:", eigentumswohnungAnzahl))
#Ausgabe: Häufigkeit für Eigenwohnungen: 420

untermieteAnzahl <- apartmentColumn["1"]
print(paste("Häufigkeit für Untermiete:", untermieteAnzahl)) 
#Ausgabe: Häufigkeit für Untermiete: 232


eigenheimAnzahl <- apartmentColumn["7"]
print(paste("Häufigkeit für Eigenheim (Häufigste Art):", eigenheimAnzahl))
#Ausgabe: Häufigkeit für Eigenheim (Häufigste Art): 2495

#Die Werte stimmen nicht ganz mit dem gesuchten aus dem Code-Buch überein.
#Eigenwohnungen -> Codebuch:469, Datensatz: 420
#Untermiete     -> Codebuch:210, Datensatz: 232
#Eigenheim.     -> Codebuch:2529, Datensatz: 2495



#########################################################################################################################################################################################
#Aufgabe 3
#Aufgabe 3a
#Datentyp: Faktor 
#Grund: Diskrete Kategorien: Bundesländer sind nominale Kategorien, die eine begrenzte Anzahl eindeutiger Werte (z. B. "Bayern", "Berlin") haben. Faktoren modellieren solche Kategorien effizient.
#Grund: Speicheroptimierung: Faktoren speichern nur die eindeutigen Werte (Level) und ordnen jeder Kategorie einen internen numerischen Code zu, was Speicherplatz spart.
#Grund: Einfache Gruppierungen und Visualisierungen

#Extera: Man kann auch Charachter nutzen, aber nicht effizient (Faktor als Datentyp ist besser für Bundesländer). Falls der Datensatz hauptsächlich für textuelle Manipulationen oder flexible Bearbeitungen (z. B. Umbenennungen, Zusammenführungen) verwendet wird, kann die Spalte als Zeichenkette (character) belassen werden.



#Aufgabe 3b
typeof(allbus$land)   #>> double

# Der Typ ist hier double und dies ist falsch.
#warum ist double hier nicht gut: Ungeeignet, weil Zahlen keine Bedeutung für Bundesländer haben und keine sinnvollen Operationen ermöglichen.


#Aufgabe 3c

allbus$land <- as.factor(allbus$land)
is.factor(allbus$land) #>> TRUE

#Aufgabe 3d
#Zusammenfassung
summary(allbus$land)
#Ausgabe:
#10  20  30  40  50  60  70  80  90 100 111 112 120 130 140 150 160 
#169 107 426  27 921 313 200 538 699  62  97 174 316 188 514 308 283 
#Überblick
View(allbus$land)

#Die Zahl 10 ist ein codierter Wert, der ein bestimmtes Bundesland repräsentiert. Seine Bedeutung (z. B. Schleswig-Holstein) hängt von der Kodierung im Codebuch ab


#Aufgabe 3e

total_count <- nrow(allbus)

bundeslandcolumn <- table(allbus$land)

#Häufigkeit aus NRW
nrw_count <- bundeslandcolumn["50"]
#Häufigkeit aus Bayern
bayern_count <- bundeslandcolumn["90"]

#Aus NRW
# Anteil (Proportion) und Prozent mit 3 Dezimalstellen
proportionNRW <- round((nrw_count) / total_count, 3)
percentNRW <- proportionNRW * 100
print(paste("Anteil aus NRW:", proportionNRW )) # -> 0.172
print(paste("Prozent aus NRW:", percentNRW,"%")) # ->17.2%

#Aus Bayern
# Anteil (Proportion) und Prozent mit 3 Dezimalstellen
proportionBayern <- round((bayern_count) / total_count, 3) 
percentBayern <- proportionBayern * 100
print(paste("Anteil aus Bayern:", proportionBayern )) # -> 0.131
print(paste("Prozent aus Bayern:", percentBayern,"%")) # -> 13.1%

#Aus NRW oder Bayern
# Anteil (Proportion) und Prozent mit 3 Dezimalstellen
proportionNRWOrBayern <- round((nrw_count + bayern_count) / total_count, 3)
percentNRWOrBayern <- proportionNRWOrBayern * 100
print(paste("Anteil von NRW oder Bayern:", proportionNRWOrBayern ))  # ->0.303
print(paste("Prozent von NRW oder Bayern:", percentNRWOrBayern,"%")) # -> 30.3%


#Extra: Auf dem Codebuch steht für NRW: 21.5%, für Bayern: 16.3% und für Bayern + NRW: 37,8%





