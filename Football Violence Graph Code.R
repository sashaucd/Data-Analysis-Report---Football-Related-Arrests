library("tidyverse")
library("dplyr")
library("ggrepel")

football <- read_csv("C:/Users/dshee/Downloads/final_dataset_with_rankings (1).csv")

ggplot(football, aes(x = season, y = number_of_arrests)) +
  geom_point(aes(colour= offence_type)) +
  geom_label_repel(
    data = filter(football, number_of_arrests > 3),
    aes(label = club_supported)
  ) +
  labs( 
    x = "Season",  
    y = "Number of arrests",
    title= "Teams with over arrests a Season, 2019-2024"
) +
  theme_bw() +
  theme(legend.title = element_blank(), 
           legend.position = "bottom",
        legend.text= element_text(size=8)
        )


football <- football %>%
  mutate(club_supported = case_when(
    club_supported == "aston_villa" ~ "Aston Villa",
    club_supported == "swindon_town" ~ "Swindon Town",
    club_supported == "bolton_wanderers" ~ "Bolton Wanderers",
    club_supported == "arsenal" ~ "Arsenal",
    TRUE ~ club_supported  # Keep other values unchanged
  ))

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

