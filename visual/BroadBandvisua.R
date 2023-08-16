# Load required libraries
library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)

# Set working directory
setwd("D:/Diy_DataAssign/")

# Read data
Towns <- read_csv("Clean/Cleaned_Town_population.csv") %>% 
  select(shortPostcode, Town, District, County)
BroadbandCleaned <- read_csv("Clean/Cleaned_Broadband_Speed.csv", show_col_types = FALSE)
broadband <- Towns %>% 
  left_join(BroadbandCleaned, by = "shortPostcode")

# Set a more colorful palette
my_colors <- c("Average" = "orange", "Minimum" = "pink")

# Oxfordshire Broadband speed visualization
oxford_plot <- ggplot(broadband, aes(y = Town)) +
  labs(x = "Speeds (Mbits/s)", y = "Town", title = "Oxfordshire Broadband Speeds") +
  geom_bar(data = filter(broadband, County == "OXFORDSHIRE"),
           aes(x = Avgdownload, fill = "Average"), stat = "Identity") +
  geom_bar(data = filter(broadband, County == "OXFORDSHIRE"),
           aes(x = Mindownload, fill = "Minimum"), stat = "Identity") +
  guides(fill = guide_legend("Download Speeds")) +
  scale_fill_manual(values = my_colors) +  # Apply the custom color palette
  theme_minimal() +  # Apply a clean and minimal theme
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 8))

# Display Oxfordshire plot
print(oxford_plot)

# Yorkshire Broadband speed visualization
yorkshire_plot <- ggplot(broadband, aes(y = Town)) +
  labs(x = "Speeds (Mbits/s)", y = "Town", title = "Yorkshire Broadband Speeds") +
  geom_bar(data = filter(broadband, County %in% c("YORKSHIRE", "WEST YORKSHIRE", "SOUTH YORKSHIRE", "NORTH YORKSHIRE")),
           aes(x = Avgdownload, fill = "Average"), stat = "Identity") +
  geom_bar(data = filter(broadband, County %in% c("YORKSHIRE", "WEST YORKSHIRE", "SOUTH YORKSHIRE", "NORTH YORKSHIRE")),
           aes(x = Mindownload, fill = "Minimum"), stat = "Identity") +
  guides(fill = guide_legend("Download Speeds")) +
  scale_fill_manual(values = my_colors) +  # Apply the custom color palette
  theme_minimal() +  # Apply a clean and minimal theme
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 8))

# Display Yorkshire plot
print(yorkshire_plot)


# Average download speed plot
district_colors <- rainbow(length(unique(broadband$District)))

avg_speed_plot <- broadband %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Avgdownload, fill = District)) +
  scale_x_discrete(limits = rev(unique(broadband$District))) +  # Reverse the order for horizontal display
  scale_y_continuous(breaks = seq(0, 1000, 50)) +
  geom_boxplot() +
  labs(title = "Average download speed (Mbit/s) by district",
       x = "District", y = "Average Download Speed (Mbit/s)") +
  scale_fill_manual(values = district_colors) +  # Apply the colorful district colors
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(hjust = 0)) +
  coord_flip()  # Flip the plot to horizontal orientation

# Display average speed plot
print(avg_speed_plot)
       
