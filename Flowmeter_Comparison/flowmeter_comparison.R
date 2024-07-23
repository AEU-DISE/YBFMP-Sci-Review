# YBFMP Scientific Review
# Standardizing Sampling, drift net flow meter comparison to old method (use of E&L flowmeter values)
# switch to independent flow meters: 4/26/2021. compare similar times of year and conditions
  # innundation vs non-innundation... water year types 
rm(list = ls())
# setwd("C:/Users/mminer/OneDrive - California Department of Water Resources/R_Data_Analysis/Scientific Review")
lapply(c("tidyverse","viridis","janitor","rstatix","ggpubr",
         "lubridate","data.table","readxl","dplyr", "ggh4x",
         "scales"),require, character.only=TRUE)

# Load ####
# QC'd flow meter data from edi and Lisa

edi <- read_csv("Flowmeter_Comparison/Drift_data_20210420.csv") %>% clean_names() %>% 
  mutate(event_id = paste0(station, " ", datetime)) %>% 
  select(-lab_comments)

int <- read_csv("Flowmeter_Comparison/Drift_data_20240503.csv") %>% clean_names() %>% #drift qc'd data from Lisa 
  filter(datetime > '2019-12-27 09:40:00 UTC') #overlapping dates with edi data 


drift <- bind_rows(edi, int)
# correct missing data 
drift <- drift %>% fill(inundation,.direction = "down") #filling in missing inundation data 


# flow meter values before sepearate flow meters and after... 
drift$meter_status <- ifelse(drift$datetime < '2021-04-26', "shared meter", "separate meter")

# subset data ####
reg_in <- drift[drift$flow_meter_speed == "Regular" & drift$inundation == "TRUE", ]
low_in <- drift[drift$flow_meter_speed == "Low" & drift$inundation == "TRUE", ]

reg <- drift[drift$flow_meter_speed == "Regular" & drift$inundation == "FALSE", ]
low <- drift[drift$flow_meter_speed == "Low" & drift$inundation == "FALSE", ]

# summary stats ####
statsum <- drift %>% group_by(flow_meter_speed, station, inundation, meter_status) %>% get_summary_stats(flowdiff_adj, type = "mean_sd")


# plotting ####
# labels 
in_labels <- c("Inundated", "Not Inundated")
names(in_labels) <- c(TRUE, FALSE)

rotor_labels <- c("Low Flow Rotor", "High Flow Rotor")
names(rotor_labels) <- c("Low", "Regular")

drift$station <- factor(drift$station, levels = c("STTD", "SHR"))
drift$meter_status <- factor(drift$meter_status, levels = c("shared meter", "separate meter"))

(comp <- ggplot(drift, aes(x = meter_status, y=flowdiff_adj, fill = station))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar")+
  labs(y="adj. flow meter difference", x="", fill = "")+
  scale_fill_manual(values = c("darkorange", "skyblue"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme_classic(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.position = "bottom")+
  facet_grid(inundation~flow_meter_speed, scales = "free_y", labeller = labeller(inundation = in_labels, flow_meter_speed = rotor_labels))
)
ggsave(comp, filename = "flowmeter_comparison_inundation+meter_speed.png", height = 5, width = 7, units = "in",dpi = 600)

# Assumptions ####
# Check variance bewteen groups:
res <- var.test(flowdiff_adj ~ meter_status, data = drift)
res # small p-value, there is a significant diff in variances 

# Welch's T-Test: ####
# all data
same_meter <- drift[drift$meter_status == "shared meter", ]$flowdiff_adj
diff_meter <- drift[drift$meter_status == "separate meter", ]$flowdiff_adj

t <- t.test(same_meter, diff_meter, 
            alternative = "two.sided", var.equal = FALSE)
t #significantly different means 

#try with just low rotors and not inundated 
res_low <- var.test(flowdiff_adj ~ meter_status, data = low)
res_low

same_meter_low <- low[low$meter_status == "shared meter", ]$flowdiff_adj
diff_meter_low <- low[low$meter_status == "separate meter", ]$flowdiff_adj

t_low <- t.test(same_meter_low, diff_meter_low, alternative = "two.sided", var.equal = FALSE)
t_low #significantly different means
# when using flow meters of the same kind, the drift net flow meter has higher revolutions 



##### TAKE AWAYS####
# average values for flow meters are significantly different when using 'shared meter' or 'separate meters', this is true under different flow conditions. 
# there appears to be greater concurrency between sites when using independent flow meters == variance between stations appears more consistent. Prior to using separate flow meters, there was much greater variation at STTD in meter values than at SHR. 