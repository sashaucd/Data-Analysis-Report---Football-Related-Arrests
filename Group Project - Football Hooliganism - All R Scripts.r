#  Merging Arrests Data with Economic Indicators: 

# load the dataset
library(readxl)
fb_arrests <- read_xlsx("football-related-arrest-statistics-england-and-wales-detailed-datasets.xlsx", sheet = "Arrests")
fb_arrests
View(fb_arrests)

# Rearrange the arrests data set in chronological order
fb_arrests_arranged <- fb_arrests %>%
  arrange(date)
View(fb_arrests_arranged)

# Monthly gdp uk
monthly_gdp <- read_xlsx("Copy of mgdp (monthly gdp uk).xlsx")
View(monthly_gdp)
monthly_gdp_ammended <- monthly_gdp %>%
  slice(7:n())
View(monthly_gdp_ammended)

# convert the time variables
library(dplyr)
library(lubridate)

# Convert time variables to a common format (in this case, using lubridate's 'ym' function)
fb_arrests_arranged$time_variable <- ym(fb_arrests_arranged$date)
monthly_gdp_ammended$time_variable <- dmy(monthly_gdp_ammended$Title)

# Now you can join the datasets on the common time variable
joined_df <- left_join(df1, df2, by = "time_common")

# Merging Rankings data with the above dataset to obtain the final dataset

library(dplyr)
library(tidyr)
library("stringr")

# Data Soruces: 
# Crimes around football matches - https://www.gov.uk/government/statistics/football-related-arrests-and-banning-orders-2023-to-2024-season
# Rankings - https://github.com/jfjelstul/englishfootball/tree/master


# Read the datasets
rankings <- read.csv("rankings_filtered.csv")
matches <- read.csv("fb_arrests_monthly_gdp.csv")

View(rankings)
View(matches)


# Function to standardize team names
standardize_team_name <- function(name) {
  name %>%
    # Replace underscores with spaces
    str_replace_all("_", " ") %>%
    # Capitalize first letter of each word
    str_to_title() %>%
    # Trim any extra spaces
    str_trim()
}
# Clean and prepare the rankings data
rankings_clean <- rankings %>%
  select(season_id, team_name, position) %>%
  rename(ranking = position) %>%
  # Standardize team names in rankings
  mutate(team_name_std = standardize_team_name(team_name))
View(rankings_clean)


# Clean and prepare the matches data
matches_clean <- matches %>%
  mutate(
    season_numeric = (as.numeric(sub(".*/", "", season)) - 1),
    season_id = paste0("S-20", season_numeric, "-", 
                      case_when(
                        competition == "premier_league" ~ "1",
                        competition == "championship" ~ "2",
                        competition == "league_one" ~ "3",
                        competition == "league_two" ~ "4",
                        TRUE ~ NA_character_
                      )),
    # Standardize team names in matches
    home_team_std = standardize_team_name(home_team),
    away_team_std = standardize_team_name(away_team)
  )
  View(matches_clean)



# Join home team rankings
matches_with_rankings <- matches_clean %>%
  left_join(
    rankings_clean,
    by = c("season_id" = "season_id", "home_team_std" = "team_name_std")
  ) %>%
  rename(home_team_ranking = ranking)

  View(matches_with_rankings)


# Join away team rankings
final_dataset <- matches_with_rankings %>%
  left_join(
    rankings_clean,
    by = c("season_id" = "season_id", "away_team_std" = "team_name_std")
  ) %>%
  rename(away_team_ranking = ranking)

View(final_dataset)


# Select relevant columns and clean up
final_dataset <- final_dataset %>%
  select(
    season, competition, 
    home_team, away_team,
    home_team_ranking, away_team_ranking,
    everything(),
    -season_numeric,
    -home_team_std,
    -away_team_std,
    -contains("team_name")  # Remove the original team name columns from rankings
  )

  View(final_dataset)


# Check for unmatched teams
unmatched_teams <- final_dataset %>%
  filter(is.na(home_team_ranking) | is.na(away_team_ranking)) %>%
  select(season_id, home_team, away_team, home_team_ranking, away_team_ranking) %>%
  distinct()
View(unmatched_teams)
# Print summary statistics
summary_stats <- final_dataset %>%
  summarise(
    total_rows = n(),
    rows_with_home_ranking = sum(!is.na(home_team_ranking)),
    rows_with_away_ranking = sum(!is.na(away_team_ranking)),
    rows_with_both_rankings = sum(!is.na(home_team_ranking) & !is.na(away_team_ranking))
  )

print("Summary Statistics:")
print(summary_stats)

print("\nUnmatched Teams:")
print(unmatched_teams)

# Save the final dataset
write.csv(final_dataset, "final_dataset_with_rankings.csv", row.names = FALSE)

# Generating Plots - Football Related Arrests by season - DataVizProject-Plot3.png and Football Related Arrests - Home vs Away Supporters - DataVizProject-Plot4.png

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


# Generating Summary Statistics and GVA Regression

---
title: "Final Data Project"
output: html_document
date: "2024-11-29"
---


setwd('/Users/jackoconnell/Desktop/ADWV')
library(ggplot2)
library(car)
library(ggfortify)
library(kdensity)
library(tseries)
library(lmtest)
library(broom)
library(tidyverse)
library(marginaleffects)
library(readxl)
library(ggfortify)
library(modelsummary)
library(lmtest)
library (stargazer)
library(ggfortify)
library(lmtest)
library(fixest)
library(ivreg)
library(gmm)
library(ggbeeswarm)
library(ggrepel)


library(readxl)
fb_arrests_monthly_gdp <- read_excel("fb_arrests_monthly_gdp.xlsx")
View(fb_arrests_monthly_gdp)





# Create the home variable
home=fb_arrests_monthly_gdp$home <- ifelse(fb_arrests_monthly_gdp$home_team == fb_arrests_monthly_gdp$club_supported, 1, 0)
# Boxplot
ggplot(data=fb_arrests_monthly_gdp)+
  aes(x=factor(home), y = number_of_arrests)+
  geom_point(shape = 16, alpha = 0.5)+#EXPERIMENT WITH THIS
  geom_boxplot(colour = "red", outlier.color = NA) +
  geom_jitter(alpha = 0.2, size = 1) +
  labs(x = "Home", y = "Arrests") +
  labs(x = "Home", y = "Number of Arrests", 
       title = "Boxplot: Arrests and Home/Away Support", 
       subtitle = "Home supporter = 1")



# Number of arrests over time,
library(ggplot2)

ggplot(data = fb_arrests_monthly_gdp) +
  aes(x = date, y = number_of_arrests) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Date", y = "Number of Arrests", 
       title = "Arrests", 
       subtitle = "Number of Arrests vs. Time")




# Summary statistics
datasummary_skim(fb_arrests_monthly_gdp, histogram=T)

library(modelsummary)

number_of_arrests=fb_arrests_monthly_gdp$number_of_arrests
home=fb_arrests_monthly_gdp$home
GVA=fb_arrests_monthly_gdp$GVA=fb_arrests_monthly_gdp$'Gross Value Added - Monthly (Index 1dp) :CVM SA'
home_team=fb_arrests_monthly_gdp$home_team
away_team=fb_arrests_monthly_gdp$away_team
GVA_numeric=fb_arrests_monthly_gdp$GVA_numeric <- as.numeric(gsub("GVA", "", as.character(GVA)))







# create the variables home and away, where home = 1 if offender was supporting a team was at home and zero if the offender supports a clud that was playing away, 
# if home_team = club_supported
home=fb_arrests_monthly_gdp$home <- ifelse(fb_arrests_monthly_gdp$home_team == fb_arrests_monthly_gdp$club_supported, 1, 0)
ggplot(data = fb_arrests_monthly_gdp)+
  aes(x = home, y =  number_of_arrests)+
  stat_summary(
    fun.data = mean_cl_normal,
    linewidth = 0.8, size = 0.5
  ) +
  geom_point()+#TIHS LOOKS WEIRD FOR SOME REASON: shape = fb_arrests_monthly_gdp$competition
  geom_smooth(method = "loess")+
  theme_minimal()+#TRY method = "loess" WHERE RELEVANT
  labs(x = "Home", y = "Arrests") +
  labs(x = "Home", y = "Number of Arrests", 
       title = "Arrests are equal for Home and Away supporters", 
       subtitle = "Arrests vs. Home/Away")

              



### Regression equations for part 4, ###
# x = home, y =  number_of_arrests
regression1_1=lm(number_of_arrests~home, data = fb_arrests_monthly_gdp)
# x=GVA, y=number_of_arrests
GVA=fb_arrests_monthly_gdp$'Gross Value Added - Monthly (Index 1dp) :CVM SA'
GVA_numeric=fb_arrests_monthly_gdp$GVA_numeric <- as.numeric(gsub("GVA", "", as.character(GVA)))
Arrests=fb_arrests_monthly_gdp$number_of_arrests
Season_2021=fb_arrests_monthly_gdp$season2020/21
Season_2022=fb_arrests_monthly_gdp$season2020/21
Season_2023=fb_arrests_monthly_gdp$season2020/21

# Regress arrests on the home variable
regression0_1=lm(Arrests~home, data = fb_arrests_monthly_gdp)
regression0_2=lm(Arrests~home+GVA_numeric+season, data = fb_arrests_monthly_gdp)
modelsummary(list(regression0_1, regression0_2))

# Regress arrests on the GVA variable
regression1_4=lm(Arrests~GVA_numeric, data = fb_arrests_monthly_gdp)
regression2_4=lm(Arrests~GVA_numeric+home+season, data = fb_arrests_monthly_gdp)
modelsummary(list(regression1_4, regression2_4))





unique__offences <- unique(fb_arrests_monthly_gdp$offence_type)
print(unique__offences)

# create a dummy variable for each type of offence to be regressed, 
violent_disorder <- ifelse(fb_arrests_monthly_gdp$offence_type == "violent_disorder", 1, 0)
public_disorder  <- ifelse(fb_arrests_monthly_gdp$offence_type == "public_disorder", 1, 0)
throwing_missiles  <- ifelse(fb_arrests_monthly_gdp$offence_type == "throwing_missiles", 1, 0)
entering_the_pitch  <- ifelse(fb_arrests_monthly_gdp$offence_type == "entering_the_pitch", 1, 0)
possession_pyrotechnics  <- ifelse(fb_arrests_monthly_gdp$offence_type == "possession_pyrotechnics", 1, 0)
alcohol_offences  <- ifelse(fb_arrests_monthly_gdp$offence_type == "alcohol_offences", 1, 0)
racist_indecent_chanting  <- ifelse(fb_arrests_monthly_gdp$offence_type == "racist_indecent_chanting", 1, 0)
ticket_touting  <- ifelse(fb_arrests_monthly_gdp$offence_type == "ticket_touting", 1, 0)
breach_of_banning_order  <- ifelse(fb_arrests_monthly_gdp$offence_type == "breach_of_banning_order", 1, 0)
criminal_damage  <- ifelse(fb_arrests_monthly_gdp$offence_type == "criminal_damage", 1, 0)
entering_inside_stadiums_in_possession_of_class_a_drugs  <- ifelse(fb_arrests_monthly_gdp$offence_type == "entering_inside_stadiums_in_possession_of_class_a_drugs", 1, 0)

violent_disorder_regression=lm(violent_disorder~home+GVA_numeric+season, data = fb_arrests_monthly_gdp)
public_disorder_regression=lm(public_disorder~home+GVA_numeric+season, data = fb_arrests_monthly_gdp) 
throwing_missiles_regression=lm(throwing_missiles~home+GVA_numeric+season, data = fb_arrests_monthly_gdp) 
entering_the_pitch_regression=lm(entering_the_pitch~home+GVA_numeric+season, data = fb_arrests_monthly_gdp) 
possession_pyrotechnics_regression=lm(possession_pyrotechnics~home+GVA_numeric+season, data = fb_arrests_monthly_gdp)  
alcohol_offences_regression=lm(alcohol_offences~home+GVA_numeric+season, data = fb_arrests_monthly_gdp)
racist_indecent_chanting_regression=lm(racist_indecent_chanting~home+GVA_numeric+season, data = fb_arrests_monthly_gdp) 
ticket_touting_regression=lm(ticket_touting~home+GVA_numeric+season, data = fb_arrests_monthly_gdp) 
breach_of_banning_order_regression=lm(breach_of_banning_order~home+GVA_numeric+season, data = fb_arrests_monthly_gdp) 
criminal_damage_regression=lm(criminal_damage~home+GVA_numeric+season, data = fb_arrests_monthly_gdp) 
entering_inside_stadiums_in_possession_of_class_a_drugs_regression=lm(entering_inside_stadiums_in_possession_of_class_a_drugs~home+GVA_numeric+season, data = fb_arrests_monthly_gdp)  

modelsummary(list(violent_disorder_regression, public_disorder_regression, throwing_missiles_regression, entering_the_pitch_regression, possession_pyrotechnics_regression, alcohol_offences_regression, racist_indecent_chanting_regression, ticket_touting_regression, breach_of_banning_order_regression, criminal_damage_regression, entering_inside_stadiums_in_possession_of_class_a_drugs_regression))






unique__comp <- unique(fb_arrests_monthly_gdp$competition)
print(unique__comp)



# Plot GVA over time
GVA=fb_arrests_monthly_gdp$'Gross Value Added - Monthly (Index 1dp) :CVM SA'
offence_type=fb_arrests_monthly_gdp$offence_type
  
ggplot(data = fb_arrests_monthly_gdp)+
  aes(x=GVA, y=number_of_arrests, colour=offence_type)+
  geom_point() +
  geom_smooth()+
  geom_label_repel(data = filter(fb_arrests_monthly_gdp, number_of_arrests > 2),
  aes(label = number_of_arrests), alpha = 0.8, size = 2
  )+
  theme_minimal()+ theme(legend.position = "bottom")+ 
  labs(x = "GVA", y = "Arrests") 








### Outliers: checking for them, running regressions w/o them ###
autoplot(regressions_4) +
  theme_bw(base_size=14)

autoplot(regression1_4) +
  theme_bw(base_size=14)

autoplot(regression2_4) +
  theme_bw(base_size=14)

influenceIndexPlot(regressions_4, vars = "hat",
                   id=F, main = "Leverage (hat values)")
influenceIndexPlot(regressions_4, vars = "Studentized",
                   id=F, main = "Studentized Residuals")
influenceIndexPlot(regressions_4, vars = "Cook",
                   id=F, main = "Cook's distance")

# regression w/o outliers
regressions_4_wo <- lm(number_of_arrests~GVA_numeric+home+season+competition+date+offence_type+club_supported, data = fb_arrests_monthly_gdp %>%  slice(-54)) 
modelplot(list(regressions_4, regressions_4_wo),
          coef_omit = 'Interc')+
  geom_vline(xintercept = 0,size=0.2,colour="red")

regression2_4_wo <- lm(number_of_arrests~GVA_numeric+home+season+competition+date+offence_type, data = fb_arrests_monthly_gdp %>%  slice(-54)) 
modelplot(list(regression2_4, regression2_4_wo),
          coef_omit = 'Interc')+
  geom_vline(xintercept = 0,size=0.2,colour="red")

# normality of residuals
fb_arrests_monthly_gdp$ehat <- regression2_4$residuals
# density
kde = kdensity(fb_arrests_monthly_gdp$ehat, start = "normal", kernel = "gaussian")  #initial assumption on distribution is normal, use gaussian kernel for smoothing
plot(kde, main = "Kernel Density Estimate")     # plot the kernel density, main is the title
lines(kde, plot_start = TRUE, col = "red")     # add also the initial assumption on the distribution i.e. normal with mean and sd as in the data
ggsave(filename = "boxplot.png")


#  Plotting Teams with 3+ arrests: 
library("tidyverse")
library("dplyr")
install.packages("viridis")
install.packages("ggrepel")
library("ggrepel")
library(viridis)  # For colorblind-friendly colors

# Title -  Teams with over 3 arrests a season by arrest tyle (2019 - 2024)

setwd("~/Downloads/")

football <- read.csv("final_dataset_with_rankings.csv")
# football <- read_csv("C:/Users/dshee/Downloads/final_dataset_with_rankings (1).csv")

football <- football %>%
  mutate(offence_type = case_when(
    offence_type == "alcohol_offences" ~ "Alcohol Offences",  # Rename "Old Offence 1" to "New Offence 1"
    offence_type == "breach_of_banning_order" ~ "Breach of Banning Order",  # Rename "Old Offence 2" to "New Offence 2"
    offence_type == "criminal_damage" ~ "Criminal Damage",  # Rename "Old Offence 3" to "New Offence 3"
    offence_type == "entering_inside_stadiums_in_possession_of_class_a_drugs" ~ "Stadium entry with Class A Drugs",
    offence_type == "entering_the_pitch" ~ "Entering the Pitch",
    offence_type == "possession_pyrotechnics" ~ "Possession of Pyrotechnics",
    offence_type == "public_disorder" ~ "Public Disorder",
    offence_type == "racist_indecent_chanting" ~ "Racist Indecent Chanting",
    offence_type == "throwing_missiles" ~ "Throwing Missiles",
    offence_type == "ticket_touting" ~ "Ticket Touting",
    offence_type == "violent_disorder" ~ "Violent Disorder",
    TRUE ~ offence_type  # Keep all other offences unchanged
  ))


football <- football %>%
  mutate(club_supported = case_when(
    club_supported == "aston_villa" ~ "Aston Villa",
    club_supported == "swindon_town" ~ "Swindon Town",
    club_supported == "bolton_wanderers" ~ "Bolton Wanderers",
    club_supported == "arsenal" ~ "Arsenal",
    TRUE ~ club_supported  # Keep other values unchanged
  ))

ggplot(football, aes(x = season, y = number_of_arrests)) +
  geom_point(aes(colour= offence_type)) +
  geom_label_repel(
    data = filter(football, number_of_arrests > 3),
    aes(label = club_supported)
  ) +
  labs( 
    x = "Season",  
    y = "Number of arrests",
    title= "Teams with over three arrests a season, 2019-2024"
) +
  theme_bw() +
  theme(legend.title = element_blank(), 
           legend.position = "bottom",
        legend.text= element_text(size=8)
        )




#  claude ggplot

ggplot(football, aes(x = season, y = number_of_arrests)) +
  geom_point(aes(color = offence_type), size = 3, alpha = 0.7) +
  geom_label_repel(
    data = subset(football, number_of_arrests > 3),
    aes(label = club_supported),
    max.overlaps = 20,
    box.padding = 0.9,
    segment = TRUE,
    force = 2
) +
  scale_color_viridis_d(name = "Type of Offence") +
  labs(
    x = "Football Season",
    y = "Number of Arrests",
    title = "Teams with over 3 arrests a season by arrest type (2019 - 2024)",
    
    caption = "Data source: Football arrests dataset"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.box.spacing = unit(0.5, "cm")
  )

  ggsave("final_teams_with_3_or_more_arrests.png", width = 12, height = 8, dpi = 300)

#  Scatter Plot of League Position vs Arrests and  look at correlation and regression model around arrests and team rankings
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
