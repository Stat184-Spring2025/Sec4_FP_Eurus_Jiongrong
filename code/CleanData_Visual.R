install.packages("ggrepel")
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggrepel)

# import data
VGS_Dec2016 <- read_csv("your own path")
VG <- read_csv("your own path")
clean_VGS <- read_csv("your own path")

# clean data

clean_VGS_Dec2016 <- VGS_Dec2016 %>% select(-c(Platform, Other_Sales, Critic_Score, Critic_Count, 
                                               User_Score, User_Count, Developer, Rating))
clean_VG <- VG %>% select(-c(Platform, Other_Sales, Critic_Score, Critic_Count, 
                             User_Score, User_Count, Developer, Rating))

clean_VGS <- clean_VGS %>% select(-c(Rank, Platform, Other_Sales))


# draw graph
genre_sales <- clean_VGS %>%
  group_by(Genre) %>%
  summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Global_Sales))

ggplot(genre_sales, aes(x = reorder(Genre, Global_Sales), y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Top 12 Game Genres by Global Sales",
       x = "Game Genre",
       y = "Global Sales (in millions)") +
  theme_bw()+
  theme(legend.position = "right")

#Percentage：
genre_sales <- clean_VGS %>%
  group_by(Genre) %>%
  summarise(Total_Genre_Sales = sum(Global_Sales, na.rm = TRUE))

total_sales <- sum(clean_VGS$Global_Sales, na.rm = TRUE)

genre_sales <- genre_sales %>%
  mutate(Percentage = (Total_Genre_Sales / total_sales) * 100)

print(genre_sales)

graph_data <- clean_VGS %>%
  filter(!is.na(Year), !is.na(Genre), !is.na(Global_Sales)) %>%
  filter(Year >= 1980 & Year <= 2024)

GS_year <- graph_data %>%
  group_by(Year, Genre) %>%
  summarise(Total_Sales = sum(Global_Sales), 
            .groups = 'drop')

top5_genres <- GS_year %>%
  group_by(Genre) %>%
  summarise(Total = sum(Total_Sales)) %>%
  arrange(desc(Total)) %>%
  slice(1:5) %>%
  pull(Genre)

ggplot(GS_year %>% filter(Genre %in% top5_genres),
       aes(x = Year, y = Total_Sales, color = Genre)) +
  geom_line(size = 1.2) +
  labs(title = "Top 5 Genre within time",
       x = "Year", y = "Sales in Million）") +
  theme_bw()

top_GS <- GS_year %>%
  filter(Genre %in% top5_genres)

ggplot(top_GS, aes(x = Year, y = Total_Sales, color = Genre)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  labs(title = "Top Game Genres Sales Trend with Smoothed Line",
       x = "Year", y = "Sales (in millions)") +
  theme_bw() +
  theme(legend.position = "bottom")


region_comparison <- clean_VGS %>%
  mutate(Occident_Sales = NA_Sales + EU_Sales)%>%
  group_by(Genre) %>%
  summarise(Global = sum(Global_Sales, na.rm = TRUE),
            Occident = sum(Occident_Sales, na.rm = TRUE), ) %>%
  pivot_longer(cols = c(Global, Occident),
               names_to = "Region",
               values_to = "Sales")

ggplot(region_comparison, aes(x = reorder(Genre, -Sales), y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Occident VS Global",
       x = "Genre", y = "Sales") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
