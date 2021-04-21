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

synthesis <- df2  %>% filter(!Species %in% c("Rec. Use - Kayak",
                                             "Rec. Use - Fishing",
                                             "Current Flow")) %>%
  group_by(Seasonal_Component,Species) %>%
  select(Species,Lower_Limit,Upper_Limit)  %>%
  summarise(ranges = list(seq(from = Lower_Limit,to = Upper_Limit))) %>% 
  group_split()




synthesis_current_flows <- df2 %>% filter(Species=="Current Flow") %>% 
  group_by(Seasonal_Component) %>% 
  select(Species,Lower_Limit,Upper_Limit) %>% 
  summarise(ranges = list(seq(from = Lower_Limit,to = Upper_Limit))) %>% 
  pull(ranges)

names(synthesis_current_flows) = seasons





select_species <- df2 %>% filter(!Species %in% c("Rec. Use - Kayak",
                                                 "Rec. Use - Fishing",
                                                 "Current Flow")) %>% 
  select(species) %>% unique




# Glen-summer-synthesis ---------------------------------------------------

willow_growth = c(23:595)
willow_adult = c(23:40590)

typha_adult = c(77:568)
typha_growth = c(23:166)

cladophora_adult = c(538:4759)

Reduce(intersect, 
       list(willow_growth,willow_adult,
            typha_adult,typha_growth)) %>% 
  range()




# functions needed for synthesis ------------------------------------------



# check if is integer(0) 
is_integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}



ranges_print <- function(x)
{
  paste(x[1],"-",x[2])
}




# Find ranges of intersections ---------------------------------------------


seasonal_ranges = list()
seasonal_species = list()

for(i in 1:length(synthesis)){
  seasonal_ranges[[i]] = synthesis %>%
    pluck(i) %>%
    group_by(Species) %>%
    pluck("ranges")
}


for(i in 1:length(synthesis)){
  seasonal_species[[i]] = synthesis %>%
    pluck(i,"Species")
}

names(seasonal_ranges) = seasons
names(seasonal_species) = seasons



# ranges for specific season

summer_ranges <- seasonal_ranges[["Summer Baseflow"]]
names(summer_ranges) = seasonal_species %>% pluck("Summer Baseflow")

winter_ranges <- seasonal_ranges[["Winter Baseflow"]]
names(winter_ranges) = seasonal_species %>% pluck("Winter Baseflow")

peak_ranges <- seasonal_ranges[["Winter Peak Flows"]]
names(peak_ranges) = seasonal_species %>% pluck("Winter Peak Flows")


# intersection of "Willow" species for the summer only
Reduce(intersect,
       c(summer_ranges[grep("Willow",names(summer_ranges))],
            summer_ranges[grep("Typha",names(summer_ranges))] )) %>% 
  range() %>% ranges_print()






Reduce(intersect,
       summer_ranges[grep(str_c(c("Willow","Typha"),collapse = "|"),names(summer_ranges))]) %>% 
  range() %>% ranges_print()





tibble("Season" = "Summer Baseflow",
       "Species" = "Willow",
       "current-flow ranges" = ranges_print(round(
         synthesis_current_flows[["Summer Baseflow"]])),
       "Magnitude" = Reduce(intersect,
                            summer_ranges[grep("Willow",
                                               names(summer_ranges))]) %>%
         range() %>% ranges_print())



# intersection of "typha" species for the summer only


Reduce(intersect,
       summer_ranges[grep("Typha",synthesis_table() %>% 
                            pull(Species) %>% unique()   )]) %>% 
  range()




tibble("Season" = "Summer Baseflow",
       "Species" = "Typha",
       "current-flow ranges" = ranges_print(round(synthesis_current_flows$`Summer Baseflow`)),
       "Magnitude" = Reduce(intersect,
                            summer_ranges[grep("Typha",names(summer_ranges))]) %>%
         range() %>% ranges_print())





# Multiple Species Sythesis Option (SUMMER) -----------------------------------------------


summer_ranges <- seasonal_ranges[["Summer Baseflow"]]
names(summer_ranges) = seasonal_species %>% pluck("Summer Baseflow")


species_list = list()
for(i in 1:length(names(summer_ranges))){
  
  species_list[[i]] = suppressWarnings(
    as_tibble(names(summer_ranges) %>% combn(i) %>% t()) %>%
      rowwise() %>%
      mutate(Ranges = Reduce(intersect,
                        summer_ranges[grep(str_c(c_across(),collapse = "|"),
                                           names(summer_ranges))]) %>% 
             range() %>% ranges_print()) %>% 
      mutate(Range_Diff = Reduce(intersect,
                       summer_ranges[grep(str_c(c_across(),collapse = "|"),
                                          names(summer_ranges))])
             %>% range() %>% diff()) %>%
      filter(Range_Diff != "-Inf") %>%
      ungroup() %>%
      top_n(Range_Diff,n=1))
}

summer_multiple_synthesis <- discard(species_list, function(x) nrow(x) == 0) %>%
  last()


summer_multiple_synthesis %>% select(-c("Ranges","Range_Diff")) %>% 
  unlist(use.names = FALSE)






