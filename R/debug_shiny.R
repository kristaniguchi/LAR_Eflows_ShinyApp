library(readr)
library(tidyverse)
library(ggthemes)

# DEBUG ------------------------------------------------------


dat <- read.csv("raw/FlowRanges_Species_RecUses_Allnodes_BU_03172021.csv",
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



BUNames <- dat$BU_names  %>% strsplit(",") %>% 
  unlist() %>% unique() %>% str_trim()


df2 <- dat %>% filter(Node=="GLEN",
               Designation_BU == "Designated"| is.na(Designation_BU),
               Designation == "Existing"| is.na(Designation),
               BU_names == BUNames[2]|is.na(BU_names),
               metric %in% c("DS_Mag_50","Wet_BFL_Mag_10",
                             "Peak_2 as lower, 
                             Peak_10 as upper")|is.na(metric),
               Probability_Threshold=="Medium"|is.na(Probability_Threshold)) 


df3 <- dat %>% filter(Node=="GLEN",
                      Designation=="Existing"|is.na(Designation),
                      Probability_Threshold=="Medium"|is.na(Probability_Threshold),
                      metric %in% c("DS_Mag_50","Wet_BFL_Mag_10","Peak_2 as lower, Peak_10 as upper")|is.na(metric)) %>%
  mutate(Range = paste0(Lower_Limit,"-",Upper_Limit))


ggplot(df2,aes(x=Species_Label, ymin = Lower_Limit,
               lower = Lower_Limit,middle=(Lower_Limit+Upper_Limit)/2,
               upper = Upper_Limit, ymax = Upper_Limit,
               fill= Species_Label)) +
  geom_boxplot(stat = "identity",fatten=NULL) +
  facet_grid(~ Seasonal_Component, scales="free") +
  theme(strip.text = element_text(face="bold", size=12),
        strip.background = element_rect(fill="white", colour="black",size=1)) +
  scale_fill_colorblind()+
  theme(legend.position="bottom",
        panel.grid.minor.y = element_blank()) +
  labs(title="Flow Ranges",x ="", y = "Flow (cfs)")+
  scale_y_log10()

