install.packages("tidyverse")
library(tidyverse)


videos <- read_csv("data/yt-videos.csv")

amount_of_uploaded_videos <- nrow(videos)

most_viewed_video <- videos %>% slice_max(views, n = 1)


most_watched_video <- videos %>% slice_max(watch_time_hours, n = 1)
View(most_viewed_video)

total_hours_watched_video <- round(sum(videos$watch_time_hours))



give_answers <- function(dataset) {
  
  amount_of_uploaded_videos <- nrow(dataset)
  print(amount_of_uploaded_videos)
  
  most_viewed_video <- dataset %>% slice_max(views, n = 1)
  print(most_viewed_video)
  
  most_watched_video <- dataset %>% slice_max(watch_time_hours, n = 1)
  print(most_watched_video)
  
  total_hours_watched_video <- round(sum(dataset$watch_time_hours))
  print(total_hours_watched_video)
  
}

give_answers(videos)



statprog1_ids <- read_csv("data/yt-statprog1-ids.csv")

statprog1_videos <- inner_join(videos, statprog1_ids, by="content")



give_answers(statprog1_videos)


newdata <- anti_join(videos,statprog1_videos,by="content")

nrow(newdata)


daily_views <- read_csv("data/yt-daily-views.csv")



daily_views_of_most_viewed <- daily_views %>% filter(content == most_viewed_video$content)


library(esquisse)

esquisse::esquisser()

library(ggplot2)




# Identify the maximum view and corresponding date
max_point <- daily_views_of_most_viewed %>%
  filter(views == max(views))

# Enhanced Plot
ggplot(daily_views_of_most_viewed, aes(x = date, y = views)) +
  
  # Base line for daily views
  geom_line(color = "#112446", size = 1.2, alpha = 0.9) +
  
  
  # Highlight the max point
  geom_point(data = max_point, aes(x = date, y = views), 
             color = "red", size = 4, shape = 21, fill = "yellow", stroke = 1.5) +
  
  # Vertical dashed line for the max date
  geom_vline(xintercept = as.numeric(max_point$date), 
             linetype = "dashed", color = "gray40", size = 0.8) +
  
  # Horizontal dashed line for the max views
  geom_hline(yintercept = max_point$views, 
             linetype = "dotted", color = "gray40", size = 0.8) +
  
  # Annotation for the max point
  annotate("text", x = max_point$date, y = max_point$views + 10, 
           label = paste("Max Views:", max_point$views), 
           color = "red", fontface = "bold", vjust = -0.5, size = 4.5) +
  
  # Labels
  labs(
    title = "📈 Entwicklung der Views für das meistgesehene Video",
    subtitle = "Oktober 2024 – Januar 2025",
    x = "Datum",
    y = "Anzahl der Views",
    caption = "Datenquelle: Vorlesung Statistische Software"
  ) +
  
  # Theme adjustments
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#112446"),
    plot.subtitle = element_text(size = 14, face = "italic", color = "#444444"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "#333333"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    panel.grid.major = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.border = element_blank()
  ) +
  
  # Date formatting
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") 


daily_views_all <- daily_views %>% group_by(date) %>% summarise(views = sum(views))





