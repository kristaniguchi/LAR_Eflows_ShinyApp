library(readr)
library(tidyverse)

# DEBUG ------------------------------------------------------



dat <- read.csv("raw/FlowRanges_Species_RecUses_Allnodes_03142021.csv",
                encoding = "UTF-8",stringsAsFactors = FALSE) %>% 
  mutate(Species = factor(Species),
         Species_Label = gsub(" ", "\n",Species_Label),
         Species_Label = factor(Species_Label,levels = c("Willow\nGrowth", "Willow\nAdult", 
                                                         "Typha\nGrowth","Typha\nAdult", 
                                                         "Cladophora\nAdult", "Current\nFlow", 
                                                         "SAS\nGrowth","SAS\nAdult","Steelhead\nMigration\n(Prolonged)",
                                                         "Steelhead\nMigration\n(Burst)","Steelhead\nMigration\n(Smolts)",
                                                         "Rec.\nUse\nKayak","Rec.\nUse\nFishing")),
         Lower_Limit = as.double(Lower_Limit),
         Upper_Limit = as.double(Upper_Limit)) %>% 
  rename(Seasonal_Component= Seasonal.Component)
         
         
seasons = dat %>% select(Seasonal_Component) %>% unlist() %>% unique()
names(seasons)=NULL

lookup <- tibble("Species"= levels(dat$Species_Label) %>% as.character(),
                 "Colors" = c("#fc8d59", "#d73027", "#91bfdb","#4575b4", 
                              "#fee090", "white", "black","yellow",
                              "purple","#067BC2","#84BCDA","#B97375","#F1E4E8"))

df2 <- dat %>% filter(Reach=="LAR 5 - Glendale Narrows",
                      Designation=="Existing",
                      Probability_Threshold=="Medium"|is.na(Probability_Threshold)) %>%
  mutate(Range = paste0(Lower_Limit,"-",Upper_Limit))


ggplot(df2,aes(x=Species_Label, ymin = Lower_Limit,
               lower = Lower_Limit,middle=(Lower_Limit+Upper_Limit)/2,
               upper = Upper_Limit, ymax = Upper_Limit,
               fill= Species_Label)) +
  geom_boxplot(stat = "identity",fatten=NULL) +
  facet_grid(Node~ Seasonal_Component, scales="free") +
  theme(strip.text = element_text(face="bold", size=12),
        strip.background = element_rect(fill="white", colour="black",size=1)) +
  scale_fill_manual(name = "Species - Lifestage", 
                    labels = lookup$Species, values=lookup$Colors) + 
  theme(legend.position="bottom",
        panel.grid.minor.y = element_blank()) +
  labs(title="Flow Ranges",x ="", y = "Flow (cfs)") 

