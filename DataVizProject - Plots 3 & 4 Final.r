# Load libraries
library(tidyverse) 
library(scales)  # For better number formatting
library(RColorBrewer)  # For enhanced color palettes

# optional - set the appropriate working directory
setwd("~/Downloads/")

data <- read.csv("final_dataset_with_rankings.csv")

# Create season factor to ensure proper ordering
data$season <- factor(data$season, levels = c("2019/20", "2020/21", "2021/22", "2023/24"))

# Plot 3 -  FootballArrest trends across seasons 2019/20 to 2023/24. 
# Public disorder is consistently the most common offense, with drug possession becoming 
# significant in 2023/24. 2019/20 had highest total arrests (81), while 2020/21 had 
# lowest (2), likely due to COVID-19 restrictions.

# Clean up offense types for better readability and annotation labels
data <- data %>%
  mutate(offence_type = str_to_title(str_replace_all(offence_type, "_", " ")))

# Group by season and offense_type to get counts
arrest_summary <- data %>%
  group_by(season, offence_type) %>%
  summarize(total_arrests = sum(number_of_arrests)) %>%
  ungroup() %>%
  # Calculate percentage for each offense type within season
  group_by(season) %>%
  mutate(
    percentage = total_arrests / sum(total_arrests) * 100,
    # Add labels for values over 5%
    label = ifelse(percentage > 5, 
                  paste0(round(total_arrests, 0)), 
                  "")
  ) %>%
  ungroup()

# Get total arrests per season for subtitle
season_totals <- arrest_summary %>%
  group_by(season) %>%
  summarize(total = sum(total_arrests)) %>%
  ungroup()

# Create enhanced stacked histogram
plot_11 <- ggplot(arrest_summary, aes(x = season, y = total_arrests, fill = reorder(offence_type, -total_arrests))) +
  geom_bar(stat = "identity", 
           position = "stack", 
           width = 0.7,
           alpha = 0.9) +  # Slightly transparent bars
  # Add value labels for sections with significant percentage
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  # Use a carefully selected color palette
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  # Format y-axis with comma separator
  scale_y_continuous(labels = comma_format(),
                    expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    # Enhanced overall appearance
    text = element_text(family = "sans"),
    plot.title = element_text(size = 14, 
                            face = "bold", 
                            margin = margin(b = 10),
                            hjust = 0),
    plot.subtitle = element_text(size = 11, 
                               color = "gray30", 
                               margin = margin(b = 20),
                               hjust = 0),
    
    # Axis formatting
    axis.title = element_text(size = 10, color = "gray30"),
    axis.text = element_text(size = 9, color = "gray30"),
    axis.text.x = element_text(angle = 0,  # Remove angle for better readability
                              margin = margin(t = 10)),
    
    # Grid modifications
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    
    # Legend improvements
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "cm"),
    
    # Overall spacing
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  labs(
    title = "Football-Related Arrests by Season",
    
    x = NULL,  # Remove x-axis label as it's self-explanatory
    y = "Number of Arrests",
    fill = "Offense Type"
  )
plot_11
# Save with better resolution
ggsave("DataVizProject-Plot3.png", plot = plot_11, width = 12, height = 8, dpi = 300)


# Plot 4 - The output shows average arrest numbers by offense type, 
# split between home and away supporters. Away supporters show notably higher arrests 
# for drug possession in stadiums (2.50 vs 1.00), while home supporters have slightly 
# higher averages for public disorder (1.86 vs 1.33).

# Function to wrap and clean offense types
wrap_offenses <- function(x) {
  # Capitalize first letter of each word and clean text
  x <- str_to_title(str_replace_all(x, "_", " "))
  str_wrap(x, width = 25)
}

# Create supporter type and calculate averages with improved text formatting
arrests_by_type <- data %>%
  # Create supporter_type column
  mutate(supporter_type = case_when(
    club_supported == home_team ~ "Home Team Supporter",
    club_supported == away_team ~ "Away Team Supporter",
    TRUE ~ "Neutral"
  )) %>%
  # Filter out neutral supporters
  filter(supporter_type != "Neutral") %>%
  # Calculate average arrests by offense type and supporter type
  group_by(offence_type, supporter_type) %>%
  summarise(avg_arrests = mean(number_of_arrests)) %>%
  ungroup() %>%
  # Calculate total arrests for sorting
  group_by(offence_type) %>%
  mutate(
    total_avg = sum(avg_arrests),
    # Clean up offense type names
    offence_type = wrap_offenses(offence_type)
  ) %>%
  ungroup() %>%
  # Order offense types by total arrests
  mutate(offence_type = reorder(offence_type, total_avg))

# Calculate overall totals for subtitle
total_stats <- arrests_by_type %>%
  group_by(supporter_type) %>%
  summarise(total = sum(avg_arrests)) %>%
  mutate(total = round(total, 1))

# Create enhanced plot with colorblind-friendly colors
ggplot(arrests_by_type, 
       aes(x = offence_type, y = avg_arrests, fill = supporter_type)) +
  # Add bars with reduced width
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.7,
           alpha = 0.9) +
  # Add value labels
  geom_text(aes(label = sprintf("%.2f", avg_arrests)),
            position = position_dodge(width = 0.8),
            hjust = -0.2,
            size = 3,
            color = "gray30") +
  coord_flip() +
  # Colorblind-friendly palette using ColorBrewer
  scale_fill_manual(values = c("Home Team Supporter" = "#1B9E77", 
                              "Away Team Supporter" = "#7570B3")) +
  # Rest of the code remains the same as previous version...
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2)),
    labels = number_format(accuracy = 0.01)
  ) +
  theme_minimal() +
  # Previous theme settings remain the same...
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 14, 
                            face = "bold", 
                            margin = margin(b = 10),
                            hjust = 0),
    plot.subtitle = element_text(size = 11, 
                               color = "gray30", 
                               margin = margin(b = 20),
                               hjust = 0),
    axis.title = element_text(size = 10, color = "gray30"),
    axis.text = element_text(size = 9, color = "gray30"),
    axis.text.y = element_text(margin = margin(r = 5)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = 20),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  labs(
    title = "Football-Related Arrests: Home vs Away Supporters",
    x = NULL,
    y = "Average Number of Arrests",
    fill = "Supporter Type"
  )

# Save with better resolution
ggsave("DataVizProject-Plot4.png", width = 12, height = 8, dpi = 300)

