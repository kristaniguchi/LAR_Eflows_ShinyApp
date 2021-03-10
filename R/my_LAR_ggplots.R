library(readr)
library(tidyverse)


# using .Rdata file -------------------------------------------------------


load("data/Synthesis_FlowRecs_Boxplots_GLEN.Rdata.Rdata")


df <- data %>% 
  mutate(Species = factor(Species),
         Species_Label = gsub(" ", "\n",Species_Label),
         Species_Label = factor(Species_Label,levels = c("Willow\nGrowth", "Willow\nAdult", 
                                                         "Typha\nGrowth","Typha\nAdult", 
                                                         "Cladaphora\nAdult", "Current\nFlow", 
                                                         "Recreational\nUse"))
         )
 


# set colors

lookup <- tibble("Species"= levels(df$Species) %>% as.character(),
                 "Colors" = c("#fc8d59", "#d73027", "#91bfdb", 
                              "#4575b4", "#fee090", "white", "black"))


ggplot(df, aes(x=Species_Label, ymin = Lower_Limit, 
               lower = Lower_Limit, middle = NA, 
               upper = Upper_Limit, ymax = Upper_Limit, 
               fill= Species_Label)) +
  geom_boxplot(stat = "identity") +  
  facet_wrap(~Seasonal.Component, scales="free") +
  theme(strip.text = element_text(face="bold", size=12),
        strip.background = element_rect(fill="white", colour="black",size=1)) +
  scale_fill_manual(name = "Species - Lifestage", 
                    labels = lookup$Species, values=lookup$Colors) + 
  theme(legend.position="bottom") +
  labs(title="Flow Ranges",x ="", y = "Flow (cfs)", subtitle = "GLEN Example") +
  scale_y_log10()




# using csv provided ------------------------------------------------------



MM_March02_Typha_Steelhead <- read.csv("raw/MM_March02_Typha_Steelhead_Cladophora_SAS_Willow_02_05_2021_updated_KI_MM.csv",
                                       encoding = "UTF-8",stringsAsFactors = FALSE) %>% 
  mutate(Species = factor(Species),
         Species_Label = gsub(" ", "\n",Species_Label),
         Species_Label = factor(Species_Label,levels = c("Willow\nGrowth", "Willow\nAdult", 
                                                         "Typha\nGrowth","Typha\nAdult", 
                                                         "Cladophora\nAdult", "Current\nFlow", 
                                                         "SAS\nGrowth","SAS\nAdult","Steelhead\nMigration\n(Prolonged)")),
         Lower_Limit = as.double(Lower_Limit),
         Upper_Limit = as.double(Upper_Limit)) %>% 
  rename(Seasonal_Component= Seasonal.Component)
         
         
         
lookup <- tibble("Species"= levels(MM_March02_Typha_Steelhead$Species_Label) %>% as.character(),
                 "Colors" = c("#fc8d59", "#d73027", "#91bfdb", 
                              "#4575b4", "#fee090", "white", "black","yellow","purple"))         
         
df2 <- MM_March02_Typha_Steelhead %>% filter(Node=="LA20_2",
                                      Designation=="Existing",
                                      Probability_Threshold=="Medium") %>%
  mutate(Range = paste0(Lower_Limit,"-",Upper_Limit))


ggplot(df2,aes(x=Species_Label, ymin = Lower_Limit,
           lower = Lower_Limit,middle=(Lower_Limit+Upper_Limit)/2,
           upper = Upper_Limit, ymax = Upper_Limit,
           fill= Species_Label)) +
  geom_boxplot(stat = "identity",fatten=NULL) +
  facet_wrap(~ Seasonal_Component, scales="free") +
  theme(strip.text = element_text(face="bold", size=12),
        strip.background = element_rect(fill="white", colour="black",size=1)) +
  scale_fill_manual(name = "Species - Lifestage", 
                    labels = lookup$Species, values=lookup$Colors) + 
  theme(legend.position="bottom") +
  labs(title="Flow Ranges",x ="", y = "Flow (cfs)", subtitle = paste0(unique(df2$Node))) 

