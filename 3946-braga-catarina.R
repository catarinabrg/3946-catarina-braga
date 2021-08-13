######################################################################################################################################################################
######                                         CODE DEVELOPED FOR ANALYSIS OF CORRELATION BETWEEN GREENHOUSE EMISSIONS                                          ######
######                                          AND POPULATION/TOTAL LAND AREA                                                                                  ######
######                                          AUTHOR: CATARINA BRAGA                                                                                          ######
######################################################################################################################################################################

# 0. Calling libraries, packeges and formats

library(dplyr)
library(ggplot2)
options(scipen=999)
library(ggrepel)
install.packages("ggthemes")
library(ggthemes)

output_folder <- "D:/CATARINA BRAGA/PROFISSIONAL/Applications/2021/ECB/ghxpop.csv"

# 1. Start analysis of Greenhouse emissions x Population

ghxpop <- read.csv("D:/CATARINA BRAGA/PROFISSIONAL/Applications/2021/ECB/ghxpop.csv")

ghxpop_v2 <- ghxpop %>%
  mutate(Population = Population/1000000)

p1 <- ggplot(ghxpop_v2, aes(x=Population, y=Greenhouse.gas.emissions, label=country)) + 
  geom_point(color = "dark blue") +
  theme_light ()+
  theme(text=element_text(family = "Arial"),
        plot.title = element_text(colour = "dark blue"),
        plot.subtitle = element_text(colour = "dark blue"),
        plot.caption = element_text(colour = "dark blue", hjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.title.position = "plot",
        plot.caption.position = "plot")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  geom_text_repel() + 
  labs(title = "Chart 1", subtitle = "Total Greenhouse gas emissions (excluding LULUCF and memory items, including international aviation) 
  vs. Population for EU27 countries in 2019",
       y="Greenhouse gas emissions \n(Thousand tonnes)",
       x = "Population \n(Millions of Inhabitants)",
       caption = "Source: Eurostat
Note: Total greenhouse gases include Carbon dioxide (CO2), Methane (CH4), Methane in CO2 equivalent, Nitrous oxide (N2O), 
Nitrous oxide (NO2) in CO2 equivalent, Hydrofluorocarbones (HFC) in CO2 equivalent, Perfluorocarbones (PFC) in CO2 equivalent, 
Hydrofluorocarbones (HFC) and Perfluorocarbones (PFC) - not specified mix in CO2 equivalent, 
Sulphur hexafluoride (SF6) in CO2 equivalent, Nitrogen trifluoride in CO2 equivalent.")
p1

ggsave(filename = "ghxpop.png", width = 22, height = 10, units = "cm", dpi = 350)

# 2. Start analysis of Greenhouse emissions x Total land area

ghxarea <- read.csv("D:/CATARINA BRAGA/PROFISSIONAL/Applications/2021/ECB/ghxarea.csv")
p2 <- ggplot(ghxarea, aes(x=Land.area, y=Greenhouse.gas.emissions, label=country)) + 
  geom_point(color = "dark blue") +
  theme_light ()+
  theme(text=element_text(family = "Arial"),
        plot.title = element_text(colour = "dark blue"),
        plot.subtitle = element_text(colour = "dark blue"),
        plot.caption = element_text(colour = "dark blue", hjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.title.position = "plot",
        plot.caption.position = "plot")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  geom_text_repel() + 
  labs(title = "Chart 2", subtitle = "Total Greenhouse gas emissions (excluding LULUCF and memory items, including international aviation) 
vs. Size of country for EU27 countries in 2019",
       y="Greenhouse gas emissions \n(Thousand tonnes)",
       x = "Total Land Area \n(Thousands of Squared Meters)",
       caption = "Source: Eurostat
Note: Total greenhouse gases include Carbon dioxide (CO2), Methane (CH4), Methane in CO2 equivalent, Nitrous oxide (N2O), 
Nitrous oxide (NO2) in CO2 equivalent, Hydrofluorocarbones (HFC) in CO2 equivalent, Perfluorocarbones (PFC) in CO2 equivalent, 
Hydrofluorocarbones (HFC) and Perfluorocarbones (PFC) - not specified mix in CO2 equivalent, 
Sulphur hexafluoride (SF6) in CO2 equivalent, Nitrogen trifluoride in CO2 equivalent.")
p2

ggsave(filename = "ghxarea.png", width = 22, height = 10, units = "cm", dpi = 350)
