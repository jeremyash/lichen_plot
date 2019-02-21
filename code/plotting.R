## SPATIAL
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(maptools)

## DATA MANAGEMENT
library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(zoo)

## PLOTTING
library(scales)
library(units)
library(viridis)
library(extrafont)
library(gtable)
library(grid)
library(cowplot)
#----------------------------------------------------------------------------

#############################################################################
## functions
#############################################################################


#############################################################################
## load data
#############################################################################

# 
# 1. She has it parsed out by Park/Forest Boundary (green), Ecoregion 4s that overlap the Park/Forest (orange), and Ecoregion 3s that overlap the forest (purple) (although all in the same sheet)
# 2. Within "All ratings" it combines S responses (tolerant/intermediate/sensitive) and N responses (eutroph/mesotroph/oligotroph), but doesn't differentiate which response is which. It also gives an East and West response for each species, without determining where the unit actually is.
# 3. I broke these out into two Tabs "Unit Level N" and "Unit Level S"
# 4. The CL responses are:
# Max= 0% change in detectability
# 80+ = 20% Loss in detectability
# 50+= 50% loss in detectability
# 10+ = 90% loss in detectability



lichen_n <- read_excel("raw_data/Case Study lichens sensitivity_02082019.xlsx",
                         sheet = "Unit Level N")

lichen_s <- read_excel("raw_data/Case Study lichens sensitivity_02082019.xlsx",
                       sheet = "Unit Level S")

lichen <- bind_rows(lichen_n, lichen_s) %>% 
  rename(resp_max = Max, resp_80 = "80+", resp_50 = "50+", resp_10 = "10+", sens_gr = Sensitivity, func_gr = "Fxl Gp1") %>% 
  mutate(plot_max = resp_max, 
         plot_80 = resp_80 - resp_max, 
         plot_50 = resp_50 - resp_80, 
         plot_10 = resp_10 - resp_50) %>% 
  gather(response_class, response_val, c(plot_max:plot_10)) %>% 
  mutate(response_class = factor(response_class, levels = rev(c("plot_max", "plot_80", "plot_50", "plot_10")))) %>%
  mutate(func_gr = factor(func_gr, levels = c("Cya", "For", "Mtx"), labels = c("Cyano Lichen", "Forage Lichen", "Matrix Lichen"))) %>% 
  rename(N_S = "N/S") %>% 
  rename(sci_name = SciName19chklst)




n_dep_range <- read_excel("raw_data/All Case Study Deposition Range.xlsx") %>% 
  add_row(AGENCY = "USFS",
          UNIT_NAME = "Bridger-Teton National Forest",
          CMAQ_MIN = 2.34089517593,
          CMAQ_MAX = 4.88712501526,
          CMAQ_MEAN = NA,
          TDEP_MIN =  2.69973111153,
          TDEP_MAX = 7.90507221222,
          TDEP_MEAN = NA
          )

#############################################################################
## plotting by trophic/sensitivity group or functional group
#############################################################################

# response colors
resp_cols <- c(viridis(3), "gray65")



##-------------
## Nitrogen
##-------------

n_plot <- function(UNIT, GROUP) {
  
  # subset to N data
  dat_n <- lichen %>%
    filter(CaseUnitNFPW == UNIT) %>%
    filter(N_S == "N") %>%
    mutate(sens_gr = factor(str_to_title(sens_gr),  levels = c("Oligotroph", "Mesotroph", "Eutroph")))

  # plotting order by minimum Max CL
  max_order <- dat_n %>%
    filter(response_class == "plot_max") %>%
    arrange(response_val) %>%
    pull(sci_name)
  dat_n$sci_name <- factor(dat_n$sci_name, levels = rev(max_order))

  # get N deposition limits
  n_lims <- n_dep_range %>%
    filter(UNIT_NAME == UNIT)


  # plot -- N deposition
  ggplot(aes(x = sci_name, y = response_val, fill = response_class), data = dat_n) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_fill_manual(values = resp_cols,
                      name = "Decrease in Detection Rate",
                      breaks = c("plot_max", "plot_80", "plot_50", "plot_10"),
                      labels = c("No Change",
                                 "Low Risk",
                                 "Moderate Risk",
                                 "High Risk")) +
    coord_flip() +
    facet_wrap(reformulate(GROUP), ncol = 2, scales = "free")  +
    labs(x = NULL,
         y = expression(paste("N deposition (kg N ", ha^-1, yr^-1, ")", sep = " ")),
         title = paste(UNIT, " - N Response", sep = "")) +
    geom_hline(aes(yintercept = CMAQ_MIN, linetype = "N Deposition limits"), show.legend = TRUE, data = n_lims) +
    geom_hline(yintercept = n_lims$CMAQ_MAX, linetype = "dashed") +
    scale_linetype_manual(name = NULL, labels = "N Deposition Limits", values = "dashed") +
    theme_minimal() +
    theme(legend.position = c(0.75, 0.25),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),

          #@axes
          axis.line.x = element_line(color = "black", size = 0.7),
          axis.ticks.x = element_line(color = "black", size = 0.35),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 7),

          # title
          plot.title = element_text(size = 15, face = "bold"),

          # facets
          strip.text = element_text(size = 13)
    ) +
    guides(fill = guide_legend(order = 1, override.aes = list(linetype = 0)),
           linetype = guide_legend(order = 0, override.aes = list(linetype = 2))) +
    scale_y_continuous(limits = c(0,17.5),
                       breaks = seq(0,15,5),
                       minor_breaks = seq(0,17,1))


  ggsave(paste("figures/", str_replace_all(UNIT, " ", "_"), "_N_", paste(GROUP), ".pdf", sep = ""),
         width = 8,
         height = 10,
         units = "in")
}


n_plot("Superior National Forest", 'func_gr')
n_plot("Superior National Forest", 'sens_gr')

n_plot("Bridger-Teton National Forest", 'func_gr')
n_plot("Bridger-Teton National Forest", 'sens_gr')


##-------------
## Sulfur
##-------------

s_plot <- function(UNIT, GROUP) {
  # subset to S data
  dat_s <- lichen %>% 
    filter(CaseUnitNFPW == UNIT) %>% 
    filter(N_S == "S") %>% 
    mutate(sens_gr = factor(str_to_title(sens_gr), levels = c("Sensitive", "Intermediate", "Tolerant")))
  
  # plotting order by minimum Max CL
  max_order <- dat_s %>% 
    filter(response_class == "plot_max") %>% 
    arrange(response_val) %>% 
    pull(sci_name)
  dat_s$sci_name <- factor(dat_s$sci_name, levels = rev(max_order))
  
  # # get S deposition limits
  # s_lims <- s_dep_range %>% 
  #   filter(UNIT_NAME == UNIT) 
  
  
  # plot -- S deposition
  ggplot(aes(x = sci_name, y = response_val, fill = response_class), data = dat_s) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_fill_manual(values = resp_cols,
                      name = "Decrease in Detection Rate",
                      breaks = c("plot_max", "plot_80", "plot_50", "plot_10"),
                      labels = c("No Change",
                                 "Low Risk",
                                 "Moderate Risk",
                                 "High Risk")) +
    coord_flip() +
    facet_wrap(reformulate(GROUP), ncol = 2, scales = "free")  +
    labs(x = NULL,
         y = expression(paste("S deposition (kg S ", ha^-1, yr^-1, ")", sep = " ")),
         title = paste(UNIT, " - S Response", sep = "")) +
    # geom_hline(aes(yintercept = TDEP_MIN, linetype = "N Deposition limits"), show.legend = TRUE, data = n_lims) +
    # geom_hline(yintercept = n_lims$TDEP_MAX, linetype = "dashed") +
    # scale_linetype_manual(name = NULL, labels = "N Deposition Limits", values = "dashed") +
    theme_minimal() +
    theme(legend.position = c(0.75, 0.25),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          
          #@axes
          axis.line.x = element_line(color = "black", size = 0.7),
          axis.ticks.x = element_line(color = "black", size = 0.35),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 7),
          
          # title
          plot.title = element_text(size = 15, face = "bold"),
          
          # facets
          strip.text = element_text(size = 13)
    ) +
    guides(fill = guide_legend(order = 1, override.aes = list(linetype = 0)),
           linetype = guide_legend(order = 0, override.aes = list(linetype = 2))) +
    scale_y_continuous(limits = c(0,33), 
                       breaks = seq(0,30,5),
                       minor_breaks = seq(0,30,1)) 
  
  
  ggsave(paste("figures/", str_replace_all(UNIT, " ", "_"), "_S_", paste(GROUP), ".pdf", sep = ""),
         width = 8,
         height = 10,
         units = "in")
}


s_plot("Superior National Forest", 'func_gr')
s_plot("Superior National Forest", 'sens_gr')

s_plot("Bridger-Teton National Forest", 'func_gr')
s_plot("Bridger-Teton National Forest", 'sens_gr')

#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

#############################################################################
## testing
#############################################################################



# nitrogen
sup_dat_n <- lichen %>% 
  filter(CaseUnitNFPW == "Superior National Forest") %>% 
  filter(N_S == "N") %>% 
  mutate(Sensitivity = factor(str_to_title(Sensitivity),  levels = c("Oligotroph", "Mesotroph", "Eutroph")))

# plotting order by minimum Max CL
max_order <- sup_dat_n %>% 
  filter(response_class == "plot_max") %>% 
  arrange(response_val) %>% 
  pull(sci_name)

sup_dat_n$sci_name <- factor(sup_dat_n$sci_name, levels = rev(max_order))




# plot -- N deposition
ggplot(aes(x = sci_name, y = response_val, fill = response_class), data = sup_dat_n) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = resp_cols, 
                    name = "Decrease in Detection Rate",
                    breaks = c("plot_max", "plot_80", "plot_50", "plot_10"),
                    labels = c("No Change",
                               "0-20%",
                               "20-50%",
                               "50-90%")
  ) +
  coord_flip() +
  facet_wrap(~Sensitivity, ncol = 2, scales = "free")  +
  labs(x = NULL,
       y = expression(paste("N deposition (kg N ", ha^-1, yr^-1, ")", sep = " ")),
       title = "Superior National Forest - N Response") +
  geom_hline(aes(yintercept = 3.827787, linetype = "N Deposition limits"), show.legend = TRUE) +
  geom_hline(yintercept = 7.095393, linetype = "dashed") +
  scale_linetype_manual(name = NULL, labels = "N Deposition Limits", values = "dashed") +
  theme_minimal() +
  theme(legend.position = c(0.75, 0.25),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        
        #@axes
        axis.line.x = element_line(color = "black", size = 0.7),
        axis.ticks.x = element_line(color = "black", size = 0.35),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 7),
        
        # title
        plot.title = element_text(size = 15, face = "bold"),
        
        # facets
        strip.text = element_text(size = 13)
  ) +
  guides(fill = guide_legend(order = 1, override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 0, override.aes = list(linetype = 2))) +
  scale_y_continuous(limits = c(0,17.5), 
                     breaks = seq(0,15,5),
                     minor_breaks = seq(0,17,1)) 


ggsave("figures/superior_ndep.pdf",
       width = 8,
       height = 10,
       units = "in")





# sulfur
sup_dat_s <- lichen %>% 
  filter(CaseUnitNFPW == "Superior National Forest") %>% 
  filter(N_S == "S") %>% 
  mutate(Sensitivity = factor(str_to_title(Sensitivity),  levels = c("Sensitive", "Intermediate", "Tolerant")))

# plotting order by minimum Max CL
max_order <- sup_dat_s %>% 
  filter(response_class == "plot_max") %>% 
  arrange(response_val) %>% 
  pull(sci_name)

sup_dat_s$sci_name <- factor(sup_dat_s$sci_name, levels = rev(max_order))




# plot -- S deposition
ggplot(aes(x = sci_name, y = response_val, fill = response_class), data = sup_dat_s) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = resp_cols, 
                    name = "Decrease in Detection Rate",
                    breaks = c("plot_max", "plot_80", "plot_50", "plot_10"),
                    labels = c("No Change",
                               "0-20%",
                               "20-50%",
                               "50-90%")
  ) +
  coord_flip() +
  facet_wrap(~Sensitivity, ncol = 2, scales = "free")  +
  labs(x = NULL,
       y = expression(paste("S deposition (kg S ", ha^-1, yr^-1, ")", sep = " ")),
       title = "Superior National Forest - S Response") +
  # geom_hline(aes(yintercept = 3.827787, linetype = "S Deposition limits"), show.legend = TRUE) +
  # geom_hline(yintercept = 7.095393, linetype = "dashed") +
  # scale_linetype_manual(name = NULL, labels = "S Deposition Limits", values = "dashed") +
  theme_minimal() +
  theme(legend.position = c(0.75, 0.25),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        
        #@axes
        axis.line.x = element_line(color = "black", size = 0.7),
        axis.ticks.x = element_line(color = "black", size = 0.35),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 7),
        
        # title
        plot.title = element_text(size = 15, face = "bold"),
        
        # facets
        strip.text = element_text(size = 13)
  ) +
  guides(fill = guide_legend(order = 1, override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 0, override.aes = list(linetype = 2))) +
  scale_y_continuous(limits = c(0,33), 
                     breaks = seq(0,30,5),
                     minor_breaks = seq(0,30,1)) 


ggsave("figures/superior_sdep.pdf",
       width = 8,
       height = 10,
       units = "in")









