library(flexdashboard)
library(tidyverse)
library(sf)
library(terra)
library(stringr)
library(knitr)
library(RColorBrewer)
library(mapview)
library(raster)
library(viridis)
library(ggpubr)
library(ggstance)
library(cowplot)

if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

librarian::shelf("raster", "sf", "tidyverse", "terra", "here", "tictoc", "foreach", "doParallel", "foreign", "dplyr","tigris",
                 "stargazer", "caret", "tidycensus", "ggpubr", "tidyterra", "gridExtra", "rasterVis", "RColorBrewer", "grid",
                 "lme4", "lmerTest", "ggstance", "cowplot", "mapview","viridis","patchwork")

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

aoi <- read_sf("../data/processed_data/masks/aoi_state.shp")
load("../energySiting_analysis/derived/total.RData")

########Variables############
path <- "../data"

display <- function(file, legend=TRUE) {
  path_to_file <- file.path(path, file)
  x <- if(grepl(pattern = ".tif", x = file)) {rast(path_to_file)} else {vect(path_to_file)}
  us <- terra::project(vect(aoi), x)
  terra::plot(x, axes = FALSE, alpha = 0, legend = legend)
  terra::plot(us, axes=FALSE, alpha = 0.7, border = "grey", add = TRUE)
  terra::plot(x, axes=FALSE, add = TRUE, legend = legend)
}


display_mapview_quality <- function(file, layer_name = "value", legend = TRUE) {
  path_to_file <- file.path(path, file)
  x <- raster::raster(path_to_file)
  mapview::mapviewOptions(raster.size=Inf)
  mapview::mapView(x, trim = FALSE, layer.name = layer_name, alpha.regions = 0.7, na.color = "transparent", legend = legend, maxpixels = 2150000)
}


cov.names <- c("Transmission dist",
               "Land acquisition",
               "Road dist", 
               "Substation dist",
               "Slope",
               "Population density", 
               "RPS",
               
               "Unemployment",
               "Minority", 
               "Poverty", 
               
               "Forest", 
               "Grassland", 
               "Shrubland", 
               "Riparian", 
               "Vegetated", 
               "Agriculture", 
               "Developed", 
               "Ohter lands",
               
               "Northeast",
               "Midwest",
               "West",
               "South",
               "Texas",
               "Mtwest")

sw.names <- c("Solar environmental score",
               "Solar CF",
               "Solar lag", 
               "Wind environmental score",
               "Wind CF",
               "Wind lag", 
               
               "Future transmission dist",
               "Future substation dist")

s_reg1 <- rst_s %>% 
  mutate(Group = factor(Group),
         color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.22)) 


s_reg2 <- rst_s %>% 
  filter(Group == 5) %>% 
  mutate(color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var,pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = c(0.7, 0.3),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09)) 


w_reg1 <- rst_w %>% 
  mutate(Group = factor(Group),
         color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.22)) 

w_reg2 <- rst_w %>% 
  filter(Group == 5) %>% 
  mutate(color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = c(0.2, 0.7),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


importance_s <- imp %>% 
  filter(tech == "Solar") %>% 
  ggplot() +
  geom_col(aes(x = reorder(rowname, desc(Overall)), y = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  labs(x = "", y = "Importance (%)", fill = "", title = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "right",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 


importance_w <- imp %>% 
  filter(tech == "Wind") %>% 
  ggplot() +
  geom_col(aes(x = reorder(rowname, desc(Overall)), y = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  labs(x = "", y = "Importance (%)", fill = "", title = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "right",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11,angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.02)) 


s_f4 <- s_d %>% 
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  ggplot(aes(x = R_effect, y = region, xmin=R_effect-SE, xmax=R_effect+SE)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(position = position_dodge2v(height = 0.4), color = "gray10", fatten = 2, size = 0.7) +
  
  facet_wrap(~variable, scales = "free_x") +
  theme_bw(base_size = 8) +
  
  labs(x = "Odds ratio (log scale)", y ="", color = "",
       title = "") +
  
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=8),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=8),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=8),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


w_f4 <-  w_d %>% 
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  ggplot(aes(x = R_effect, y = region, xmin=R_effect-SE, xmax=R_effect+SE)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(position = position_dodge2v(height = 0.4), color = "gray10", fatten = 2, size = 0.7) +
  
  facet_wrap(~variable, scales = "free_x") +
  theme_bw(base_size = 8) +
  
  labs(x = "Odds ratio (log scale)", y ="", color = "",
       title = "") +
  
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=8),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=8),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=8),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


maps <- map(.x = c("Population density", "Transmission dist", "Environmental score"),
            .f = function(x) rgn %>% 
              left_join(s_d, by = "region") %>%
              filter(variable == x) %>%
              
              ggplot() +
              geom_sf(fill = "white", color = "gray0") + # US border
              geom_sf(aes(fill = R_effect), size = 0.3) +
              
              theme_minimal(base_size = 10) +
              scale_fill_gradient2(low = if (x %in% c("Transmission dist", "Environmental score")) "darkorange3" else "royalblue3", 
                                   mid = "white", 
                                   high = if (x %in% c("Transmission dist", "Environmental score")) "royalblue3" else "darkorange3", 
                                   midpoint = 0) +
              
              labs(title = x, fill = "") +
              coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
                       ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
              theme(legend.position = "right",
                    legend.key.size = unit(0.5, "lines"),
                    plot.title = element_text(size = 10, hjust = 0.5))
)

s_f41 <- plot_grid(plotlist = maps, labels = "", label_size = 14, label_fontface = "plain", nrow = 1, hjust = -0.2)


maps <- map(.x = c("Capacity factor", "Spatial lag", "Transmission dist"),
            .f = function(x) rgn %>% 
              left_join(w_d, by = "region") %>%
              filter(variable == x) %>%
              
              ggplot() +
              geom_sf(fill = "white", color = "gray0") + # US border
              geom_sf(aes(fill = R_effect), size = 0.3) +
              
              theme_minimal(base_size = 10) +
              scale_fill_gradient2(low = if (x %in% c("Transmission dist", "Environmental score")) "darkorange3" else "royalblue3", 
                                   mid = "white", 
                                   high = if (x %in% c("Transmission dist", "Environmental score")) "royalblue3" else "darkorange3", 
                                   midpoint = 0) +
              
              labs(title = x, fill = "") +
              coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
                       ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
              theme(legend.position = "right",
                    legend.key.size = unit(0.5, "lines"),
                    plot.title = element_text(size = 10, hjust = 0.5))
)

w_f41 <- plot_grid(plotlist = maps, labels = "", label_size = 14, label_fontface = "plain", nrow = 1, hjust = -0.2)





res <- rast("../energySiting_analysis/derived/results_masked.tif")
names(res) <- c("GLM Solar","Lasso Solar","RF Solar","XGBoost Solar",
                "GLM Wind","Lasso Wind","RF Wind","XGBoost Wind")

f4a <- ggplot() +
  
  # This draws the shape of the US in gray before the raster is added
  geom_sf(data = rgn, fill = "gray80", color = "white", linewidth = 0.2) +
  
  # geom_spatraster will draw the colors on top of the gray land
  geom_spatraster(data = res[[c(1:4)]]) +
  
  geom_sf(data = rgn, fill = NA, color = "white", linewidth = 0.2) +
  
  scale_fill_viridis_c(
    option = "viridis", 
    na.value = "transparent", # CRITICAL: Makes NA areas show the gray map underneath
    name = "Probability   ",
    labels = scales::label_number()
  ) +
  facet_wrap(~lyr, ncol = 2) +
  theme_void() + # Removes the grid and axis labels for a cleaner look
  theme(
    strip.text = element_text(face = "bold", size = 8, margin = margin(b = 10)),
    legend.position = "bottom",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 7),
    plot.margin = margin(10, 10, 10, 10)
  )


f4b <- ggplot() +
  
  # This draws the shape of the US in gray before the raster is added
  geom_sf(data = rgn, fill = "gray80", color = "white", linewidth = 0.2) +
  
  # geom_spatraster will draw the colors on top of the gray land
  geom_spatraster(data = res[[c(5:8)]]) +
  
  geom_sf(data = rgn, fill = NA, color = "white", linewidth = 0.2) +
  
  scale_fill_viridis_c(
    option = "viridis", 
    na.value = "transparent", # CRITICAL: Makes NA areas show the gray map underneath
    name = "Probability   ",
    labels = scales::label_number()
  ) +
  facet_wrap(~lyr, ncol = 2) +
  theme_void() + # Removes the grid and axis labels for a cleaner look
  theme(
    strip.text = element_text(face = "bold", size = 8, margin = margin(b = 10)),
    legend.position = "bottom",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 7),
    plot.margin = margin(10, 10, 10, 10)
  )




rr <- rast("../energySiting_analysis/derived/dac_masked.tif")

names(rr) <- c("Solar_NonDAC", "Solar_DAC", "Wind_NonDAC", "Wind_DAC")

f5a <- ggplot() +
  
  # This draws the shape of the US in gray before the raster is added
  geom_sf(data = rgn, fill = "gray80", color = "white", linewidth = 0.2) +
  
  # geom_spatraster will draw the colors on top of the gray land
  geom_spatraster(data = rr) +
  
  geom_sf(data = rgn, fill = NA, color = "white", linewidth = 0.2) +
  
  scale_fill_viridis_c(
    option = "viridis", 
    na.value = "transparent", # CRITICAL: Makes NA areas show the gray map underneath
    name = "Probability   ",
    labels = scales::label_number()
  ) +
  facet_wrap(~lyr, ncol = 2) +
  theme_void() + # Removes the grid and axis labels for a cleaner look
  theme(
    strip.text = element_text(face = "bold", size = 8, margin = margin(b = 10)),
    legend.position = "bottom",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 7),
    plot.margin = margin(10, 10, 10, 10)
  )



f5b <- ttest_results %>%
  mutate(
    diff = mean_nonDAC - mean_DAC,
    pooled_sd = sqrt((sd_DAC^2 + sd_nonDAC^2) / 2),
    cohen_d = diff / pooled_sd,
    sig = p_value < 0.05
  ) %>% 
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  ggplot(aes(x = model, y = cohen_d)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # points: filled if significant, hollow if not
  geom_point(
    aes(fill = sig),
    shape = 21,
    size = 2,
    stroke = 1
  ) +
  
  facet_grid(tech ~ region, switch = "y") +
  
  scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "black")) +
  
  labs(
    title = "Effect Size (Cohen’s d) for Non‑DAC vs DAC",
    subtitle = "Filled points indicate significant differences (p < 0.05) of t-test",
    x = "Region",
    y = "Effect Size (Cohen’s d)",
    color = "Model",
    fill = "Significant"
  ) +
  
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        strip.text.y.left = element_text(angle = 0, size = 12, face = "bold"),
        strip.placement = "outside",
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1,
                                   color = "black",family="Franklin Gothic Book",size=11),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=14))
