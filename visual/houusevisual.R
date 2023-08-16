# Install and load required packages
install.packages("fmsb")
install.packages("ggrepel")

library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)
library(data.table)

# Set working directory
setwd("D:/Diy_DataAssign/")

# Define currency format
euro <- dollar_format(prefix = "\u20ac", big.mark = ",")
# Import the cleaned house prices CSV
Cleaned_HP <- fread("Clean/Clean_Housepricing2019-2022.csv")

# House Price average of each town from 2019 to 2022 (Box Plot)
boxplot_plot <- ggplot(data = Cleaned_HP,
                       aes(x = DISTRICT, y = AVGPRICE, fill = DISTRICT)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1000000)) +
  labs(title = "House Pricing in Different Districts: 2019-2022 (Box Plot)") +
  theme_minimal()


# Average House Price of each town from 2019 to 2022 (Bar Chart)
barplot_plot <- ggplot(data = Cleaned_HP,
                       aes(x = DISTRICT, y = AVGPRICE, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1000000)) +
  labs(title = "Average House Prices by District: 2019-2022 (Bar Chart)") +
  theme_minimal()

# Line Graph of average house price of the 2 counties from 2019 to 2022 (Line Graph)
lineplot_plot <- ggplot(Cleaned_HP %>% 
                          group_by(DISTRICT, YEAR) %>% 
                          summarize(AVGPRICE = mean(AVGPRICE, na.rm = TRUE)),
                        aes(x = YEAR, y = AVGPRICE, color = DISTRICT, group = DISTRICT)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = c(2019, 2020, 2021, 2022)) +
  labs(title = "Average House Prices by Districts: 2019-2022 (Line Graph)",
       x = "Year", y = "Average Price") +
  scale_color_discrete(name = "DISTRICT") +
  theme_minimal()

# Displaying the plots
print(boxplot_plot)
print(barplot_plot)
print(lineplot_plot)
