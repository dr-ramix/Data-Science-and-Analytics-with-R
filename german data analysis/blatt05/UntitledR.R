install.packages("tidyverse")
library(tidyverse)


dataset_state <- as.data.frame(state.x77)


#Methods:
#Pearson (Default):
  
#Measures linear correlation between variables.
#Sensitive to outliers.
#Assumes data is continuous and normally distributed.

cor(dataset_state$Income, dataset_state$`HS Grad`)

cor(dataset_state$Illiteracy, dataset_state$Murder)


formula <- Income ~  Population + 
                               Illiteracy + 
                              `Life Exp` + 
                                Murder     + 
                              `HS Grad`  + 
                                Frost      + 
                                Area

linearmodel_income <-  lm(formula, data = dataset_state)

summary(linearmodel_income)

coef(linearmodel_income)

#BIP pro Kopf
gdp_percap <- function(population, gdp) {
  gdp_per_cap = gdp/population
  return(gdp_per_cap)
}

honkong_gdp <- read.csv("./data/honkong_gdp.csv")

honkong_gdp %>% mutate(gdp_percap= gdp_percap(pop))

#p: 7.536 million
#gdp: 382.1 billion USD


gdp_percap(7536000, 382100000000)

#50,696.59 USD (2023) per capita


variable_summary <- function(columnName) {
  # Check if the column is numeric
  if (is.numeric(columnName)) {
    summary(columnName)  # Summary for numeric columns
  } else {
    table(columnName)  # Frequency table for non-numeric columns (e.g., factors)
  }
}

# Test with the Species column (which is a factor in the iris dataset)
variable_summary(iris$Species)

# Test with a numeric column (e.g., Sepal.Length in the iris dataset)
variable_summary(iris$Sepal.Length)




sotu_dataset <- read_csv("data/sotu.csv")

sotu_dataset2 <- sotu_dataset;



name <- sotu_dataset %>% filter(str_detect(sotu_dataset$text, "the state of the Union is not good"))
name$text
name$name

#contains women
sotu_dataset$contains_women <- ifelse(str_detect(sotu_dataset$text, "women"), TRUE, FALSE)


# Locate the position of "god" in each row of the text column
position <- str_locate(sotu_dataset$text, "god")

# Extract the start positions (first column of the matrix)
start_positions <- position[, 1]

# Add a new column with characters till "god"
# Subtract 1 to exclude the starting character of "god"
sotu_dataset$characters_till_god <- start_positions - 1



#how many times does the word freedom exist in the text

sotu_dataset$count_freedom <- str_count(sotu_dataset$text, "freedom") 

sotu_dataset$count_justice <- str_count(sotu_dataset$text, "justice") 

sotu_dataset$speech_length <- str_length(sotu_dataset$text)


textAnalyser <- function(dataset) {
  if ("text" %in% colnames(dataset)) {
    #contains women
    dataset$contains_women <- ifelse(str_detect(dataset$text, "women"), TRUE, FALSE)
    
    
    # Locate the position of "god" in each row of the text column
    position <- str_locate(dataset$text, "god")
    
    # Extract the start positions (first column of the matrix)
    start_positions <- position[, 1]
    
    # Add a new column with characters till "god"
    # Subtract 1 to exclude the starting character of "god"
    dataset$characters_till_god <- start_positions - 1
    
    
    
    #how many times does the word freedom exist in the text
    
    dataset$count_freedom <- str_count(dataset$text, "freedom") 
    
    dataset$count_justice <- str_count(dataset$text, "justice") 
    
    dataset$speech_length <- str_length(dataset$text)

  }
  return(dataset)
}

textAnalyser(sotu_dataset2)



cor(sotu_dataset$count_justice, sotu_dataset$count_freedom)

esquisse::esquisser()

library(ggplot2)

ggplot(sotu_dataset) +
 aes(x = count_freedom, y = count_justice) +
 geom_jitter() +
 labs(x = "Anzahl des Wortes \"freedom\"", 
 y = "Anzahl des Wortes \"justice\"", title = "Beziehung zwischen den Wörter \"freedom\" und \"justice\"", 
 caption = "Datensatz der Rede ") +
 theme_gray() +
 theme(plot.title = element_text(size = 17L))



# Verwende pivot_longer, um die Spalten in lange Form zu bringen
long <- sotu_dataset %>%
  pivot_longer(cols = c(count_freedom, count_justice), 
               names_to = "variable",  # Die Spaltennamen werden in eine neue "variable"-Spalte verschoben
               values_to = "values")   # Die Werte der Spalten werden in eine neue "values"-Spalte verschoben

# Ausgabe des neuen "long" Datensatzes
print(long)



# Erstellung des Line-Plots
ggplot(long, aes(x = year, y = values, color = variable, group = variable)) +
  geom_line(size = 1.2) +  # Linien-Plot
  geom_point(size = 3) +    # Punkte an den Linien
  labs(title = "Häufigkeit der Wörter 'Freedom' und 'Justice' nach Jahr",
       x = "Jahr", 
       y = "Häufigkeit",
       color = "Begriff") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))  # Farben für die Linien


