# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
final_dataset_with_rankings <- read_csv("Desktop/Visualisation/final_dataset_with_rankings.csv")

# Create a data frame for team rankings
team_rankings <- data.frame(
  team = c("tottenham_hotspur", "everton", "wolverhampton_wanderers", "manchester_city", "aston_villa",
           "fulham", "reading", "stoke_city", "millwall", "bolton_wanderers", "lincoln_city",
           "peterborough_united", "scunthorpe_united", "plymouth_argyle", "arsenal", "brighton_and_hove_albion",
           "chelsea", "norwich_city", "west_ham_united", "birmingham_city", "cardiff_city", 
           "barnsley", "sheffield_wednesday", "tranmere_rovers", "doncaster_rovers", "wycombe_wanderers", 
           "coventry_city", "sunderland", "manchester_united", "swindon_town", "chesterfield", 
           "liverpool", "newcastle_united", "southampton", "gillingham", 
           "rochdale", "portsmouth", "colchester_united", "blackburn_rovers", 
           "nottingham_forest", "burnley"),
  ranking = c(8, 12, 12, NA, 3, 4, 4, 18, 8, 23, 16, 16, 19, 3, 1, 2, 5, 11, 16, 20, 
              5, 22, 16, 23, 6, 16, 1, 1, 12, 19, 16, 24, 9, 15, 7, NA, 17, NA, 10, 13, NA)
)

# Merge the team rankings into your dataset based on home_team
final_dataset_with_rankings <- final_dataset_with_rankings %>%
  left_join(team_rankings, by = c("home_team" = "team")) %>%
  rename(home_team_ranking_filtered = ranking)

# Merge the team rankings based on away_team
final_dataset_with_rankings <- final_dataset_with_rankings %>%
  left_join(team_rankings, by = c("away_team" = "team")) %>%
  rename(away_team_ranking_filtered = ranking)

# Merge the team rankings based on club_supported
final_dataset_with_rankings <- final_dataset_with_rankings %>%
  left_join(team_rankings, by = c("club_supported" = "team")) %>%
  rename(Arrested_team_ranking_filtered = ranking)

# Summarize arrests by league position
arrest_summary <- final_dataset_with_rankings %>%
  pivot_longer(cols = c(home_team_ranking, away_team_ranking),
               names_to = "team_type",
               values_to = "league_position") %>%
  group_by(league_position) %>%
  summarise(total_arrests = sum(number_of_arrests, na.rm = TRUE)) %>%
  arrange(league_position)

# Scatter plot of league position vs arrests
ggplot(arrest_summary, aes(x = league_position, y = total_arrests)) +
  geom_point(size = 3, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add trendline
  labs(
    title = "Relationship Between League Position and Arrest Numbers",
    x = "League Position (1 = Best)",
    y = "Total Number of Arrests"
  ) +
  theme_minimal()

# Correlation test
cor.test(arrest_summary$league_position, arrest_summary$total_arrests)

# Linear regression model
model <- lm(total_arrests ~ league_position, data = arrest_summary)
summary(model)

# Filter data to ensure only relevant rows (with non-NA values for team rankings and arrests)
filtered_data <- final_dataset_with_rankings %>%
  filter(!is.na(Arrested_team_ranking_filtered) & !is.na(number_of_arrests))

# Calculate the correlation between rankings and number of arrests
correlation <- cor(filtered_data$Arrested_team_ranking_filtered, filtered_data$number_of_arrests)

# Print the correlation result
print(paste("Correlation between Arrested Team Ranking and Number of Arrests: ", correlation))

# Scatterplot to visualize the relationship
ggplot(filtered_data, aes(x = Arrested_team_ranking_filtered, y = number_of_arrests)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  labs(title = "Correlation between club performance and number of arrests",
       x = "Team Ranking",
       y = "Number of Arrests") +
  theme_minimal()

# Fit a linear regression model
model <- lm(number_of_arrests ~ Arrested_team_ranking, data = filtered_data)

# Extract R-squared
r_squared <- summary(model)$r.squared
print(r_squared)

# Basic statistical summary of the dataset
summary(filtered_data)
