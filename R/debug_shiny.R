library(readr)
library(tidyverse)
library(ggthemes)

# DEBUG ------------------------------------------------------


dat <- read.csv("raw/FlowRanges_Species_RecUses_Allnodes_BU_03172021.csv",
                encoding = "UTF-8",stringsAsFactors = FALSE) %>% 
  mutate(Species = factor(Species),
         Species_Label = gsub(" ", "\n",Species_Label),
         Species_Label = factor(Species_Label,
                                levels = c("Willow\nGrowth", "Willow\nAdult",
                                           "Typha\nGrowth","Typha\nAdult",
                                           "Cladophora\nAdult", "Current\nFlow",
                                           "SAS\nGrowth","SAS\nAdult",
                                           "Steelhead\nMigration\n(Prolonged)",
                                           "Steelhead\nMigration\n(Burst)",
                                           "Steelhead\nMigration\n(Smolts)",
                                           "Rec.\nUse\nKayak",
                                           "Rec.\nUse\nFishing")),
         Lower_Limit = as.double(Lower_Limit),
         Upper_Limit = as.double(Upper_Limit),
         Designation_Category = fct_recode(Designation_Category,"Existing"="E",
                                           "Potential"="P",
                                           "Intermittent"="I"),
         Species_Synthesis = fct_recode(Species,"Current Flow"="Current Flow",
                                        "Willow" = "Willow - Growth",
                                        "Willow" = "Willow - Adult",
                                        "Typha" = "Typha - Growth",
                                        "Typha" = "Typha - Adult",
                                        "Cladophora" = "Cladophora - Adult",
                                        "Rec. Use - Kayak" = "Rec. Use - Kayak",
                                        "Rec. Use - Fishing" = "Rec. Use - Fishing")) %>%
  rename(Seasonal_Component= Seasonal.Component)
         
         
seasons = dat %>% select(Seasonal_Component) %>% unlist() %>% unique()
names(seasons)=NULL



BU_Names <- dat$BU_names  %>% strsplit(",") %>% 
  unlist() %>% unique() %>% str_trim()

dat$BU_names[grep("WILD",dat$BU_names)]



df2 <- dat %>% filter(Node=="GLEN",
               Designation_BU == "Designated"| is.na(Designation_BU),
               Designation_Category == "Existing"| is.na(Designation_Category),
               BU_names %in% dat$BU_names[grep("WILD",dat$BU_names)]|is.na(BU_names),
               metric %in% c("DS_Mag_50","Wet_BFL_Mag_10",
                             "Peak_2 as lower, Peak_10 as upper")|is.na(metric),
               Probability_Threshold=="Medium"|is.na(Probability_Threshold)) 


df3 <- dat %>% filter(Node=="GLEN",
                      Designation_BU %in% "Designated"| is.na(Designation_BU),
                      Designation_Category %in% "Existing"| is.na(Designation_Category),
                      BU_names %in% dat$BU_names,
                      metric %in% c("DS_Mag_50","Wet_BFL_Mag_10",
                                    "Peak_2 as lower, Peak_10 as upper")|is.na(metric),
                      Probability_Threshold %in% "Medium"|is.na(Probability_Threshold)) 


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









# Synthesis Rule Set ------------------------------------------------------


# multiple species by season

synthesis <- df2  %>%
  group_by(Seasonal_Component,Species) %>%
  select(Species,Lower_Limit,Upper_Limit)  %>%
  summarise(a=min(Lower_Limit),b=min(Upper_Limit)) %>% 
  group_split()

synthesis_a = lapply(synthesis, function(x) x %>% select(a))
synthesis_b = lapply(synthesis, function(x) x %>% select(b))


synthesis_a_range = lapply(synthesis_a, range)
synthesis_b_range = lapply(synthesis_b, range)


seq_funcion <- function(x){
  c(range(x)[1]:range(x)[2])
}

synthesis_a_seq <- lapply(synthesis_a,seq_funcion)
synthesis_b_seq <- lapply(synthesis_b,seq_funcion)

synthesis_multiple_species = list()
for(i in 1:length(synthesis_a_seq)){
  synthesis_multiple_species[[i]] = Reduce(intersect,
                                           list(synthesis_a_seq[[i]],
                                                synthesis_b_seq[[i]]))

}










