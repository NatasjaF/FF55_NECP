library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(colorspace)
library(gridExtra)
library(grid)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

##########============ FIGURE 2A ========================================================================##########

# -------------------------------
# 1) Read data 
# -------------------------------
df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 1) %>%
  select(Sector, Scenario, `2015`, `2030`, `2050`)

# -------------------------------
# 2) 2015 baseline (Historical)
# -------------------------------
base_2015 <- df %>%
  filter(Scenario == "Historical") %>%
  select(Sector, base2015 = `2015`)

# -------------------------------
# 3) Long format: differences vs 2015
#    Diff = Emissions - 2015 (negative = reduction)
# -------------------------------
df_diff_long <- df %>%
  filter(Scenario != "Historical") %>%
  pivot_longer(cols = c(`2030`, `2050`),
               names_to = "Year", values_to = "Emissions") %>%
  mutate(Year = as.integer(Year)) %>%
  left_join(base_2015, by = "Sector") %>%
  mutate(
    Diff = Emissions - base2015,        # negative = reduction
    X    = paste0(Scenario, "\n", Year),
    Sector = factor(Sector, levels = c(
      "Electricity","Cement","Buildings",
      "Transportation","Industry",
      "Intl Bunkers","Fugitive Emissions"
    ))
  )

# -------------------------------
# 4) X-axis order with gaps (reordered)
# -------------------------------
x_levels_diff <- c(
  "gap1a",
  paste0(c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY"), "\n2030"),
  "gap2a",
  paste0(c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY"), "\n2050")
)

df_diff_long$X <- factor(df_diff_long$X, levels = x_levels_diff)

# -------------------------------
# 5) Colours 
# -------------------------------
sector_cols <- c(
  "Electricity"        = "#3E79D3",
  "Cement"             = "#E34A45",
  "Buildings"          = "#FFD92E",
  "Transportation"     = "#68F394",
  "Industry"           = "#AF6AB1",
  "Intl Bunkers"       = "#FFB743",
  "Fugitive Emissions" = "#AFAFAF"
)

# -------------------------------
# 6) X labels: scenario names, blank for gaps
# -------------------------------
nice_names <- c(
  "FF55_POLICY"    = "FF55\nPOLICY",
  "FF55_COSTOPT"   = "FF55\nCOST-OPT",
  "NECP_POLICY"    = "NECP\nPOLICY",
  "NECP_COSTOPT"   = "NECP\nCOST-OPT"
)

x_lab_fun <- function(x) {
  base <- sub("\\n.*$", "", x)     
  ifelse(
    x == "2015", "Historical",
    ifelse(
      grepl("^gap", x), "",
      nice_names[base]              
    )
  )
}


# -------------------------------
# 7) Y-scale setup
# -------------------------------

totals_by_X <- df_diff_long %>%
  group_by(X) %>%
  summarise(total_height = sum(-Diff, na.rm = TRUE), .groups = "drop")

ymax_plot <- ceiling(max(totals_by_X$total_height, na.rm = TRUE) / 100) * 100

# -------------------------------
# 8) Plot
# -------------------------------
p2a_diff <- ggplot(df_diff_long, aes(x = X, y = -Diff, fill = Sector)) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_x_discrete(
    drop = FALSE,
    labels = x_lab_fun,
    expand = expansion(add = c(-0.4, 0.2))   
  ) +
  scale_y_continuous(
    breaks = seq(0, ymax_plot, by = 300),     
    labels = function(v) -v,                  
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = sector_cols, drop = FALSE) +
  labs(
    title    = expression(bold("Change in fossil CO"[2] * " emissions by sector")),
    subtitle = "EU-27, difference vs 2015",
    x        = NULL,
    y        = expression(Delta~"MtCO"[2]),
    fill     = "Sector"
  ) +
  geom_hline(yintercept = 0, colour = "grey0", linewidth = 0.5) +   
  geom_vline(xintercept = 1.4, colour = "grey0", linewidth = 0.5) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.line          = element_blank(),
    axis.ticks.y       = element_line(colour = "grey0"),
    axis.ticks.x       = element_blank(),
    axis.text.x        = element_text(size = 14, vjust = 1, hjust = 0.5),
    axis.text.y        = element_text(size = 14),
    legend.position    = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    plot.title         = element_text(face = "bold", size = 24),
    plot.subtitle      = element_text(face = "plain", size = 24)
  ) +
  coord_cartesian(ylim = c(-0.15 * ymax_plot, ymax_plot), clip = "off") 
  

# -------------------------------
# 9) Year-group labels 
# -------------------------------
lvl_diff   <- levels(df_diff_long$X)
pos_2030s  <- match(paste0(c("FF55_POLICY","FF55_COSTOPT","NECP_POLICY","NECP_COSTOPT"), "\n2030"), lvl_diff)
pos_2050s  <- match(paste0(c("FF55_POLICY","FF55_COSTOPT","NECP_POLICY","NECP_COSTOPT"), "\n2050"), lvl_diff)


y_lab <- -0.05 * ymax_plot  

p2a_diff <- p2a_diff +
  annotate("text", x = mean(pos_2030s), y = y_lab, label = "2030",
           vjust = 1, size = 6, colour = "gray30", fontface = "bold") +
  annotate("text", x = mean(pos_2050s), y = y_lab, label = "2050",
           vjust = 1, size = 6, colour = "gray30", fontface = "bold") +
  theme(
    axis.text.x = element_text(
      vjust = 1.2,    
      hjust = 0.5,
      margin = margin(t = -3)  
    )
  )


print(p2a_diff)


##########============FIGURE 2B========================================================================##########


df <- read_excel("..../GCAM_Europe_results.xlsx", sheet = 2) %>%
  select(Category, Country, FF55_POLICY, FF55_COSTOPT, NECP_POLICY, NECP_COSTOPT)


# --- Long format ---
df_long <- df %>%
  pivot_longer(cols = c(FF55_POLICY, FF55_COSTOPT, NECP_POLICY, NECP_COSTOPT),
               names_to = "Scenario", values_to = "Reduction") %>%
  filter(Reduction < 0) %>%
  mutate(
    Scenario = factor(Scenario,
                      levels = c("FF55_COSTOPT",
                                 "NECP_COSTOPT",
                                 "FF55_POLICY",
                                 "NECP_POLICY"))
  )

country_order <- df %>%
  distinct(Category, Country) %>%
  arrange(factor(Category, levels = c("Northwestern Europe","Eastern Europe","Southern Europe"))) %>%
  pull(Country)
df_long$Country <- factor(df_long$Country, levels = country_order)

# --- Palettes ---
northwest_cols <- sequential_hcl(10, "TealGrn")
eastern_cols   <- sequential_hcl(11, "Magenta")
southern_cols  <- sequential_hcl(6, "OrYel")  

country_cols <- c(
  setNames(northwest_cols, country_order[1:10]),
  setNames(eastern_cols,   country_order[11:21]),
  setNames(southern_cols,  country_order[22:27])
)


# --- Main plot ---
p <- ggplot(df_long, aes(x = Scenario, y = Reduction, fill = Country)) +
  geom_bar(stat = "identity", width = 0.4) +
  scale_x_discrete(labels = x_lab_fun) +       # <--- add this line
  scale_fill_manual(values = country_cols, drop = FALSE,
                    guide = guide_legend(ncol = 1)) +   # force 1 column legend
  scale_y_reverse(
    breaks = seq(0, -1400, -200),   # ticks every 200 down to -1400
    limits = c(0, -1400)            # axis range
  ) +
  labs(
    title = expression(bold("Contributions to fossil CO"[2]*" decrease")),
    subtitle = "towards 2030",
    x = NULL,
    y = expression("Mt fossil CO"[2]* " in 2030 vs 2015")
  ) +
  theme_minimal(base_size = 16) +
  geom_hline(yintercept = 0, colour = "grey0", linewidth = 0.5) + 
  geom_vline(xintercept = 0.405, colour = "grey0", linewidth = 0.5) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.ticks.y       = element_line(colour = "grey0", linewidth = 0.5),
    axis.text.x = element_text(size = 12, vjust = 1, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "plain", size = 14)
  )

p


# --- Extract legend ---
leg <- cowplot::get_legend(p)

# --- Overlay blocks for regions ---
overlay_blocks <- grobTree(
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27)/2, "npc"), width = unit(0.02, "npc"), height = unit(9/27, "npc"),
           gp = gpar(fill = "#0BB5AD", col = NA)),
  textGrob("Northwestern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27)/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27 + 12/27/2), "npc"), width = unit(0.02, "npc"), height = unit(12/27, "npc"),
           gp = gpar(fill = "#D174A6", col = NA)),
  textGrob("Eastern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27 + 12/27/2), "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(6/27/2, "npc"), width = unit(0.02, "npc"), height = unit(6/27, "npc"),
           gp = gpar(fill = "#F39B4C", col = NA)),
  textGrob("Southern Europe", x = unit(0.06, "npc"),
           y = unit(6/27/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1))
)

# --- Combine plot + legend with blocks ---
p2b <- cowplot::plot_grid(
  p + theme(legend.position = "none"),
  grobTree(leg, overlay_blocks),
  rel_widths = c(3, 1)
)

print(p2b)



##########============COMBINE 2A & 2B ==================================================================#########

# Combine side by side with white background
figure2B <- plot_grid(
  p2a_diff + theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12, vjust = 1),
    axis.title   = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "plain"),
    plot.margin = margin(t = 15, r = 20, b = 20, l = 10)
  ),
  p2b + theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, size = 12),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face ="plain"),
    plot.margin = margin(t = 5, r = 0, b = 20, l = 20)
  ),
  labels = c("A", "B"),
  label_size = 20,
  ncol = 2,
  rel_widths = c(0.9, 1.3)   
) + theme(plot.background = element_rect(fill = "white", color = NA))


figure2B 

#--Save high-res 
ggsave("Emissions_AB_2.png", figure2B, width = 16, height = 8, dpi = 600, bg = "white")
ggsave("Emissions_AB_2.pdf", figure2B, width = 16, height = 8, bg = "white")


##########============FIGURE 3A========================================================================##########

# -------------------------------
# 1) Read data 
# -------------------------------
df_fe <- read_excel("..../GCAM_Europe_results.xlsx", sheet = 3) %>%
  select(Fuel, Scenario, `2015`, `2030`, `2050`)

# -------------------------------
# 2) 2015 baseline (Historical)
# -------------------------------
base_2015_fe <- df_fe %>%
  filter(Scenario == "Historical") %>%
  select(Fuel, base2015 = `2015`)

# -------------------------------
# 3) Long format: differences vs 2015
#    Diff = Value - 2015  
# -------------------------------
df_fe_diff <- df_fe %>%
  filter(Scenario != "Historical") %>%      
  pivot_longer(cols = c(`2030`, `2050`),
               names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year)) %>%
  left_join(base_2015_fe, by = "Fuel") %>%
  mutate(
    Diff = Value - base2015,
    X    = paste0(Scenario, "\n", Year),
    Fuel = factor(Fuel, levels = c(
      "Biomass","Coal","Gas","Liquids","Electricity","Hydrogen"
    ))
  )

# -------------------------------
# 4) X-axis order with gaps (reordered)
# -------------------------------
x_levels_fe  <- c(
  "gap1a",
  paste0(c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY"), "\n2030"),
  "gap2a",
  paste0(c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY"), "\n2050")
)

df_fe_diff$X <- factor(df_fe_diff$X, levels = x_levels_fe)


# -------------------------------
# 5) Colours for fuels 
# -------------------------------
fuel_cols <- c(
  "Biomass"     = "#1b9e77",
  "Coal"        =  "black",
  "Gas"         = "#7570b3",
  "Liquids"     = "#e7298a",
  "Electricity" = "#FF764B",
  "Hydrogen"    = "#84DEF7"
)

# -------------------------------
# 6) X labels
# -------------------------------


nice_names <- c(
  "FF55_POLICY"    = "FF55\nPOLICY",
  "FF55_COSTOPT"   = "FF55\nCOST-OPT",
  "NECP_POLICY"    = "NECP\nPOLICY",
  "NECP_COSTOPT"   = "NECP\nCOST-OPT"
)

x_lab_fun_fe <- function(x) {
  base <- sub("\\n.*$", "", x)      
  ifelse(
    x == "2015", "Historical",
    ifelse(
      grepl("^gap", x), "",
      nice_names[base]            
    )
  )
}

# -------------------------------
# 7) Y-scale based on STACKED totals
#    
# -------------------------------
stack_extents <- df_fe_diff %>%
  group_by(X) %>%
  summarise(
    pos_sum = sum(pmax(Diff, 0), na.rm = TRUE),  
    neg_sum = sum(pmin(Diff, 0), na.rm = TRUE),  
    .groups = "drop"
  )

ymax_stack <- max(stack_extents$pos_sum, na.rm = TRUE)
ymin_stack <- min(stack_extents$neg_sum, na.rm = TRUE)
y_span     <- ymax_stack - ymin_stack


y_lab_fe <- ymin_stack - 0.08 * y_span

# -------------------------------
# 8) Main plot: stacked differences vs 2015
# -------------------------------

p3a_diff <- ggplot(df_fe_diff, aes(x = X, y = Diff, fill = Fuel)) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_x_discrete(
    drop = FALSE,
    labels = x_lab_fun_fe,
    expand = expansion(add = c(-0.3, 0.2))
  ) +
  scale_y_continuous(
    breaks = seq(-400, 200, 100),  
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  labs(
    title    = "Change in final energy use by fuel",
    subtitle = "EU-27, difference vs 2015",
    x        = NULL,
    y        = expression(Delta~"Mtoe"),
    fill     = "Fuel"
  ) +
  geom_hline(yintercept = 0, colour = "grey0", linewidth = 0.5) + 
  geom_vline(xintercept = 1.3, colour = "grey0", linewidth = 0.5) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.line          = element_blank(),
    axis.ticks.y       = element_line(colour = "grey0"),
    axis.ticks.x       = element_blank(),
    axis.text.x        = element_text(size = 14, vjust = 1, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.position = "bottom",
    legend.title    = element_text(size = 14, face = "bold"),
    plot.title      = element_text(face = "bold", size = 24),
    plot.subtitle   = element_text(face = "plain", size = 24)
  ) +
  coord_cartesian(ylim = c(y_lab_fe, ymax_stack), clip = "off")


# -------------------------------
# 9) Year-group labels 
# -------------------------------

lvl_fe   <- levels(df_fe_diff$X)
pos_2030 <- match(paste0(c("FF55_POLICY","FF55_COSTOPT","NECP_POLICY","NECP_COSTOPT"), "\n2030"), lvl_fe)
pos_2050 <- match(paste0(c("FF55_POLICY","FF55_COSTOPT","NECP_POLICY","NECP_COSTOPT"), "\n2050"), lvl_fe)

p3a_diff <- p3a_diff +
  annotate("text", x = mean(pos_2030), y = y_lab_fe, label = "2030",
           vjust = 0.5, size = 6, colour = "gray30", fontface = "bold") +
  annotate("text", x = mean(pos_2050), y = y_lab_fe, label = "2050",
           vjust = 0.5, size = 6, colour = "gray30", fontface = "bold")

print(p3a_diff)


##########===============FIGURE 3B======================================================================##########

 df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 4) %>%
   select(Category, Country, FF55_POLICY, FF55_COSTOPT, NECP_POLICY, NECP_COSTOPT)


df_long <- df %>%
  pivot_longer(
    cols = c(FF55_POLICY, FF55_COSTOPT, NECP_POLICY, NECP_COSTOPT),
    names_to = "Scenario", values_to = "Reduction"
  ) %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY")
    )
  )


# --- Preserve block order: NW -> E -> S ---
df_long$Category <- factor(df_long$Category,
                           levels = c("Northwestern Europe","Eastern Europe","Southern Europe"))


# --- Palettes ---
northwest_cols <- sequential_hcl(10, "TealGrn")
eastern_cols   <- sequential_hcl(11, "Magenta")
southern_cols  <- sequential_hcl(6, "OrYel")

country_cols <- c(
  setNames(northwest_cols, unique(df_long$Country[df_long$Category=="Northwestern Europe"])),
  setNames(eastern_cols,   unique(df_long$Country[df_long$Category=="Eastern Europe"])),
  setNames(southern_cols,  unique(df_long$Country[df_long$Category=="Southern Europe"]))
)

# --- Fix global order of countries (light -> dark) ---
df_long$Country <- factor(df_long$Country, levels = names(country_cols))


nice_names <- c(
  "FF55_POLICY"    = "FF55\nPOLICY",
  "FF55_COSTOPT"  = "FF55\nCOST-OPT",
  "NECP_POLICY"    = "NECP\nPOLICY",
  "NECP_COSTOPT"  = "NECP\nCOST-OPT"
)

x_lab_fun_fe <- function(x) {
  # use pretty names where available, otherwise fall back to original
  out <- nice_names[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

# --- Plot ---
p3 <- ggplot(df_long, aes(x = Scenario, y = Reduction, fill = Country)) +
  geom_bar(stat = "identity", width = 0.4, position = position_stack()) +
  scale_x_discrete(labels = x_lab_fun_fe) +    
  scale_fill_manual(values = country_cols, drop = FALSE,
                    guide = guide_legend(ncol = 1)) +
  scale_y_reverse(
    limits = c(0, -140),              # reversed scale
    breaks = seq(0, -140, by = -20),  # tick marks every 20
    labels = function(x) sprintf("%d", x)
  ) +
  labs(
    title = "Contributions to energy efficiency",
    subtitle = "towards 2030",
    x = NULL,
    y = "Mtoe in 2030 vs 2015"
  ) +
  theme_minimal(base_size = 16) +
  geom_hline(yintercept = 0, colour = "grey0", linewidth = 0.5) + 
  geom_vline(xintercept = 0.405, colour = "grey0", linewidth = 0.5) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.ticks.y       = element_line(colour = "grey0", linewidth = 0.5),
    axis.text.x = element_text(vjust = 1, hjust = 0.5),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "plain", size = 14)
  )

print(p3)

# --- Extract legend ---
leg <- cowplot::get_legend(p3)

# --- Overlay blocks for regions ---
overlay_blocks <- grobTree(
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27)/2, "npc"), width = unit(0.02, "npc"), height = unit(9/27, "npc"),
           gp = gpar(fill = "#0BB5AD", col = NA)),
  textGrob("Northwestern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27)/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27 + 12/27/2), "npc"), width = unit(0.02, "npc"), height = unit(12/27, "npc"),
           gp = gpar(fill = "#D174A6", col = NA)),
  textGrob("Eastern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27 + 12/27/2), "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(6/27/2, "npc"), width = unit(0.02, "npc"), height = unit(6/27, "npc"),
           gp = gpar(fill = "#F39B4C", col = NA)),
  textGrob("Southern Europe", x = unit(0.06, "npc"),
           y = unit(6/27/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1))
)

# --- Combine plot and legend ---
p3b <- cowplot::plot_grid(
  p3 + theme(legend.position = "none"),
  grobTree(leg, overlay_blocks),
  rel_widths = c(3, 1)
)

print(p3b)


##########============================COMBINE 3A + 3B==================================##########

figure3B <- plot_grid(
  p3a_diff + theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12, vjust = 0.7),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold",
                                margin = margin(b = 4)),
    plot.subtitle = element_text(size = 14, face = "plain",
                                 margin = margin(b = 25)),
    plot.margin = margin(t = 15, r = 20, b = 20, l = 10)
  ),
  p3b + theme(
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold",
                                margin = margin(b = 4)),
    plot.subtitle = element_text(size = 14, face ="plain",
                                 margin = margin(b = 40)),  
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 5, r = 0, b = 20, l = 20)
  ),
  labels = c("A", "B"),
  label_size = 20,
  ncol = 2,
  rel_widths = c(0.9, 1.3)
) + theme(plot.background = element_rect(fill = "white", color = NA))


print(figure3B)


# Save high-res with white background
ggsave("Efficiency_AB_2.png", figure3B, width = 16, height = 8, dpi = 600, bg = "white")
ggsave("Efficiency_AB_2.pdf", figure3B, width = 16, height = 8, bg = "white")


##########=========================== FIGURE 4A ==================================##########

# -------------------------------
# 1) Read data 
# -------------------------------
df_fe <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 5) %>%
  select(Fuel, Scenario, `2015`, `2030`, `2050`)


# -------------------------------
# 2) Long format
# -------------------------------
df_long <- df_res %>%
  pivot_longer(
    c(`2015`, `2030`, `2050`),
    names_to  = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(
    !(Scenario == "Historical" & Year %in% c(2030, 2050)),
    !(Scenario != "Historical" & Year == 2015)
  ) %>%
  mutate(Scenario = ifelse(Year == 2015, "Historical", Scenario))

# -------------------------------
# 3) Split marker vs fuels
# -------------------------------
df_marker <- df_long %>%
  filter(
    Fuel == "% renewables in FE",
    Year %in% c(2030, 2050)           # keep ONLY 2030 & 2050
  ) %>%
  mutate(
    Percent = Value * 100,
    X       = paste0(Scenario, "\n", Year)
  )

df_stack_raw <- df_long %>%
  filter(Fuel != "% renewables in FE")

# -------------------------------
# 4) 2015 baseline for fuels (for Δ vs 2015)
# -------------------------------
base_2015_ren <- df_stack_raw %>%
  filter(Scenario == "Historical", Year == 2015) %>%
  select(Fuel, base2015 = Value)

# -------------------------------
# 5) Differences vs 2015 for 2030 & 2050 only
# -------------------------------
df_stack_diff <- df_stack_raw %>%
  filter(Scenario != "Historical", Year %in% c(2030, 2050)) %>%
  left_join(base_2015_ren, by = "Fuel") %>%
  mutate(
    Diff = Value - base2015,
    X    = paste0(Scenario, "\n", Year)
  )

# -------------------------------
# 6) X levels (NO 2015)
# -------------------------------
x_levels <- c(
  paste0(c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY"), "\n2030"),
  "gap2a",
  paste0(c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY"), "\n2050")
)

df_stack_diff$X <- factor(df_stack_diff$X, levels = x_levels)
df_marker$X     <- factor(df_marker$X,     levels = x_levels)

# -------------------------------
# 7) X labels (scenario names only)
# -------------------------------
nice_names <- c(
  "FF55_POLICY"   = "FF55\nPOLICY",
  "FF55_COSTOPT"  = "FF55\nCOST-OPT",
  "NECP_POLICY"   = "NECP\nPOLICY",
  "NECP_COSTOPT"  = "NECP\nCOST-OPT"
)

x_lab_fun <- function(x) {
  base <- sub("\\n.*$", "", x)   # strip "\n2030"/"\n2050"
  ifelse(
    grepl("^gap", x), "",
    nice_names[base]
  )
}

# -------------------------------
# 8) Colours for fuels 
# -------------------------------
fuel_cols <- c(
  "Solid biomass"         = "#3B7D23",
  "Biofuels"              = "#4EA72E",
  "Biogas"                = "#8ED973",
  "Renewable electricity" = "#F0E338",
  "Green hydrogen"        = "#3E7AD3",
  "Ambient heat"          = "#A6CAEC",
  "Solar thermal"         = "#FF0000"
)

# -------------------------------
# 9) Y range for ΔMtoe (stacked)
# -------------------------------
stack_extents <- df_stack_diff %>%
  group_by(X) %>%
  summarise(
    pos_sum = sum(pmax(Diff, 0), na.rm = TRUE),
    neg_sum = sum(pmin(Diff, 0), na.rm = TRUE),
    .groups = "drop"
  )

ymax_stack <- max(stack_extents$pos_sum, na.rm = TRUE)
ymin_stack <- min(stack_extents$neg_sum, na.rm = TRUE)
y_span     <- ymax_stack - ymin_stack

# choose a nice upper bound for ΔMtoe scale
y_left_max <- ceiling(ymax_stack / 50) * 50   # round up to nearest 50
y_left_min <- floor(ymin_stack  / 50) * 50   # round down to nearest 50

# we’ll place year labels a bit below 0
y_lab <- y_left_min - 0.08 * (y_left_max - y_left_min)

# -------------------------------
# 10) Main plot: Δ vs 2015 (left axis) + % marker (right axis)
# -------------------------------

p4a_diff <- ggplot(df_stack_diff, aes(x = X, y = Diff, fill = Fuel)) +
  geom_bar(stat = "identity", width = 0.75) +
  geom_point(
    data = df_marker,
    aes(
      x = X,
      y = Percent * y_left_max / 100,
      shape = "% RES share"      # label used in the legend
    ),
    size = 3,
    colour = "black",
    inherit.aes = FALSE
  ) +
  scale_x_discrete(
    drop   = FALSE,
    labels = x_lab_fun
  ) +
  scale_y_continuous(
    name   = expression(Delta~"Mtoe"),
    breaks = seq(y_left_min, y_left_max, by = 50),
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~ . * 100 / y_left_max,
      name   = "% renewables in FE",
      breaks = seq(0, 100, 10)
    )
  ) +
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  labs(
    title    = "Change in renewable energy use by fuel",
    subtitle = "EU-27, difference vs 2015",
    x        = NULL,
    fill     = "Fuel"
  ) +
  scale_shape_manual(
    name   = "",
    values = c("% RES share" = 17)
  ) +
  guides(
    fill  = guide_legend(order = 1, nrow = 4),
    shape = guide_legend(order = 2)
  ) +
  geom_hline(yintercept = 0, colour = "grey0", linewidth = 0.5) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.line.y.left   = element_line(colour = "grey0", linewidth = 0.5),
    axis.line.y.right  = element_line(colour = "grey0", linewidth = 0.5),
    axis.line.x        = element_blank(),
    axis.ticks.y       = element_line(colour = "grey0"),
    axis.ticks.x       = element_blank(),
    axis.text.x = element_text(
      size       = 16,
      vjust      = -0.6,
      hjust      = 0.5,
      lineheight = 0.75,
      margin     = margin(t = 12)
    ),
    axis.text.y   = element_text(size = 16),
    legend.text   = element_text(size = 12),
    legend.position = "bottom",
    legend.title  = element_text(size = 14, face = "bold"),
    plot.margin   = margin(10, 20, 50, 20),
    plot.title    = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(face = "plain", size = 24)
  )

# -------------------------------
# 11) Year-group labels ("2030", "2050") below axis
# -------------------------------
lvl      <- levels(df_stack_diff$X)
pos_2030 <- match(paste0(c("FF55_POLICY","FF55_COSTOPT","NECP_POLICY","NECP_COSTOPT"), "\n2030"), lvl)
pos_2050 <- match(paste0(c("FF55_POLICY","FF55_COSTOPT","NECP_POLICY","NECP_COSTOPT"), "\n2050"), lvl)

p4a_diff <- p4a_diff +
  coord_cartesian(ylim = c(y_lab, y_left_max), clip = "off") +
  annotate(
    "text", x = mean(pos_2030), y = y_lab, label = "2030",
    vjust = 0.5, size = 6, colour = "gray30", fontface = "bold"
  ) +
  annotate(
    "text", x = mean(pos_2050), y = y_lab, label = "2050",
    vjust = 0.5, size = 6, colour = "gray30", fontface = "bold"
  )

print(p4a_diff)

#==============FIGURE 4B===============================================================

df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 6) %>%
  select(Category, Country, FF55_POLICY, FF55_COSTOPT, NECP_POLICY, NECP_COSTOPT)


# --- Long format ---
df_long <- df %>%
  pivot_longer(cols = c(FF55_POLICY, FF55_COSTOPT, NECP_POLICY, NECP_COSTOPT),
               names_to = "Scenario", values_to = "Contribution")

country_order <- df %>%
  distinct(Category, Country) %>%
  arrange(factor(Category,
                 levels = c("Northwestern Europe","Eastern Europe","Southern Europe"))) %>%
  pull(Country)
df_long$Country <- factor(df_long$Country, levels = country_order)

df_long$Scenario <- factor(df_long$Scenario,
                           levels = c("FF55_COSTOPT", "NECP_COSTOPT", "FF55_POLICY", "NECP_POLICY"))

# --- Palettes ---
northwest_cols <- sequential_hcl(10, "TealGrn")
eastern_cols   <- sequential_hcl(11, "Magenta")
southern_cols  <- sequential_hcl(6, "OrYel")  

country_cols <- c(
  setNames(northwest_cols, country_order[1:10]),
  setNames(eastern_cols,   country_order[11:21]),
  setNames(southern_cols,  country_order[22:27])
)

nice_names <- c(
  "FF55_POLICY"    = "FF55\nPOLICY",
  "FF55_COSTOPT"  = "FF55\nCOST-OPT",
  "NECP_POLICY"    = "NECP\nPOLICY",
  "NECP_COSTOPT"  = "NECP\nCOST-OPT"
)

x_lab_fun <- function(x) {
  out <- nice_names[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

# --- Main plot ---
p <- ggplot(df_long, aes(x = Scenario, y = Contribution, fill = Country)) +
  geom_bar(stat = "identity", width = 0.4) +
  scale_x_discrete(labels = x_lab_fun) +  
  scale_fill_manual(values = country_cols, drop = FALSE,
                    guide = guide_legend(ncol = 1)) +
  scale_y_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 20)
  ) +
  labs(
    title = "Contributions to renewable energy",
    subtitle = "towards 2030",
    x = NULL,
    y = "Mtoe in 2030 vs 2015"
  ) +
  theme_minimal(base_size = 16) +
  geom_hline(yintercept = 0, colour = "grey0", linewidth = 0.5) + 
  geom_vline(xintercept = 0.405, colour = "grey0", linewidth = 0.5) +
  theme_minimal(base_size = 16) +
  geom_hline(yintercept = 0, colour = "grey0", linewidth = 0.5) + 
  geom_vline(xintercept = 0.405, colour = "grey0", linewidth = 0.5) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.ticks.y       = element_line(colour = "grey0", linewidth = 0.5),
    axis.text.x = element_text(size = 12, vjust = 1, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "plain", size = 14)
  )


print(p)


# --- Extract legend ---
leg <- cowplot::get_legend(p)

# --- Overlay blocks for regions ---
overlay_blocks <- grobTree(
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27)/2, "npc"), width = unit(0.02, "npc"), height = unit(9/27, "npc"),
           gp = gpar(fill = "#0BB5AD", col = NA)),
  textGrob("Northwestern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27)/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27 + 12/27/2), "npc"), width = unit(0.02, "npc"), height = unit(12/27, "npc"),
           gp = gpar(fill = "#D174A6", col = NA)),
  textGrob("Eastern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27 + 12/27/2), "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(6/27/2, "npc"), width = unit(0.02, "npc"), height = unit(6/27, "npc"),
           gp = gpar(fill = "#F39B4C", col = NA)),
  textGrob("Southern Europe", x = unit(0.06, "npc"),
           y = unit(6/27/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 1))
)

# --- Combine plot and legend ---
p4b <- cowplot::plot_grid(
  p + theme(legend.position = "none"),
  grobTree(leg, overlay_blocks),
  rel_widths = c(3, 1)
)

print(p4b)


##########============================COMBINE 4A + 4B==================================##########

# Combine side by side with white background
figure4 <- plot_grid(
  p4a_diff + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, vjust = 0.8),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "plain"),
    plot.margin = margin(t = 15, r = 20, b = 20, l = 10)
    
  ),
  p4b + theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, size = 12),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face ="plain"),
    plot.margin = margin(t = 5, r = 0, b = 20, l = 20)
  ),
  labels = c("A", "B"),
  label_size = 20,
  ncol = 2,
  rel_widths = c(0.95, 1.25)
) + theme(plot.background = element_rect(fill = "white", color = NA))

# Show
print(figure4)

# Save high-res
ggsave("Renewables_AB.png", figure4, width = 16, height = 8, dpi = 600, bg = "white")
ggsave("Renewables_AB.pdf", figure4, width = 16, height = 8, bg = "white")


##########============================SUPPLEMENTARY FIGURE S1 ==================================##########

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Fix country naming mismatches for GCAM mapping
world$name_long[world$name_long == "Russian Federation"] <- "Russia"
world$name_long[world$name_long == "Czechia"] <- "Czech Republic"
world$name_long[world$name_long == "Republic of the Congo"] <- "Republic of Congo"
world$name_long[world$name_long == "Côte d'Ivoire"] <- "Ivory Coast"


# Define GCAM region-country mapping
gcam_mapping <- list(
  Africa_Eastern = c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Madagascar", "Mauritius", "Reunion", "Rwanda", "Sudan", "South Sudan", "Somalia", "Uganda"),
  Africa_Northern = c("Algeria", "Egypt", "Western Sahara", "Libya", "Morocco", "Tunisia"),
  Africa_Southern = c("Angola", "Botswana", "Lesotho", "Mozambique", "Malawi", "Namibia", "Eswatini", "Tanzania", "Zambia", "Zimbabwe"),
  Africa_Western = c("Benin", "Burkina Faso", "Central African Republic", "Ivory Coast", "Cameroon", "Democratic Republic of the Congo", "Republic of Congo",
                     "Cape Verde", "Gabon", "Ghana", "Guinea", "Gambia", "Guinea-Bissau", "Equatorial Guinea", "Liberia", "Mali", "Mauritania", "Niger",
                     "Nigeria", "Senegal", "Sierra Leone", "Sao Tome and Principe", "Chad", "Togo"),
  Argentina = "Argentina",
  Australia_NZ = c("Australia", "New Zealand"),
  Brazil = "Brazil",
  Canada = "Canada",
  Central_America_and_Caribbean = c("Aruba", "Anguilla", "Netherlands Antilles", "Antigua and Barbuda", "Bahamas", "Belize", "Bermuda", "Barbados",
                                    "Costa Rica", "Cuba", "Cayman Islands", "Dominica", "Dominican Republic", "Guadeloupe", "Grenada", "Guatemala",
                                    "Honduras", "Haiti", "Jamaica", "Saint Kitts and Nevis", "Saint Lucia", "Montserrat", "Martinique", "Nicaragua",
                                    "Panama", "El Salvador", "Trinidad and Tobago", "Saint Vincent and the Grenadines"),
  Central_Asia = c("Armenia", "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Mongolia", "Tajikistan", "Turkmenistan", "Uzbekistan"),
  China = "China",
  Colombia = "Colombia",
  India = "India",
  Indonesia = "Indonesia",
  Japan = "Japan",
  Mexico = "Mexico",
  Middle_East = c("United Arab Emirates", "Bahrain", "Iran", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Oman", "Palestine", "Qatar",
                  "Saudi Arabia", "Syria", "Yemen"),
  Pakistan = "Pakistan",
  Russia = "Russia",
  South_Africa = "South Africa",
  South_America_Northern = c("French Guiana", "Guyana", "Suriname", "Venezuela"),
  South_America_Southern = c("Bolivia", "Chile", "Ecuador", "Peru", "Paraguay", "Uruguay"),
  South_Asia = c("Afghanistan", "Bangladesh", "Bhutan", "Sri Lanka", "Maldives", "Nepal"),
  Southeast_Asia = c("American Samoa", "Brunei", "Cocos Islands", "Cook Islands", "Christmas Island", "Fiji", "Micronesia", "Guam", "Cambodia", "Kiribati",
                     "Laos", "Marshall Islands", "Myanmar", "Northern Mariana Islands", "Malaysia", "Mayotte", "New Caledonia", "Norfolk Island", "Niue",
                     "Nauru", "Philippines", "Palau", "Papua New Guinea", "North Korea", "French Polynesia", "Singapore", "Solomon Islands", "Seychelles",
                     "Thailand", "Tokelau", "Timor-Leste", "Tonga", "Tuvalu", "Vietnam", "Vanuatu", "Samoa"),
  South_Korea = "South Korea",
  Taiwan = "Taiwan",
  USA = "United States",
  Europe = c("Bulgaria", "Cyprus", "Czech Republic", "Estonia", "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", "Slovakia", "Slovenia",
             "Andorra", "Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "Greenland", "Ireland", "Italy", "Luxembourg",
             "Monaco", "Netherlands", "Portugal", "Sweden", "Spain", "United Kingdom", "Belarus", "Moldova", "Ukraine", "Iceland", "Norway",
             "Switzerland", "Albania", "Bosnia and Herzegovina", "Croatia", "North Macedonia", "Montenegro", "Serbia", "Turkey")
)

# Assign region and Europe highlight
country_region_df <- do.call(rbind, lapply(names(gcam_mapping), function(region) {
  data.frame(name_long = gcam_mapping[[region]], GCAM_Region = region, stringsAsFactors = FALSE)
}))

# Merge region info with spatial data
world_mapped <- world %>%
  left_join(country_region_df, by = c("name_long")) %>%
  mutate(Europe_Highlight = ifelse(GCAM_Region == "Europe", name_long, NA))

# Get all non-Europe regions
non_europe_regions <- unique(world_mapped$GCAM_Region[!is.na(world_mapped$GCAM_Region) & world_mapped$GCAM_Region != "Europe"])

# Generate a palette with enough colors
non_europe_colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(non_europe_regions))

# Assign colors to each region
region_color_map <- setNames(non_europe_colors, non_europe_regions)

# Europe individual colors
europe_countries <- sort(unique(na.omit(world_mapped$Europe_Highlight)))
europe_colors <- setNames(colorRampPalette(brewer.pal(9, "OrRd"))(length(europe_countries)), europe_countries)

# Final color mapping
world_mapped$fill_color <- ifelse(!is.na(world_mapped$Europe_Highlight),
                                  europe_colors[world_mapped$Europe_Highlight],
                                  region_color_map[world_mapped$GCAM_Region])


# Plot the map (no legend, large size)
ggplot(world_mapped) +
  geom_sf(aes(fill = fill_color), color = "gray30", size = 0.1) +
  scale_fill_identity() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA)
  )


-------------------------------------
  
  library(ggpattern)  # <- for hatching

# Load map

world <- ne_countries(scale = "medium", returnclass = "sf")
world$name_long[world$name_long == "Czechia"] <- "Czech Republic"
world$name_long[world$name_long == "North Macedonia"] <- "Macedonia"

# Define grid regions
region_map <- list(
  "British Isles" = c("United Kingdom", "Ireland"),
  "Iberian Peninsula" = c("Spain", "Portugal"),
  "Appenine Peninsula" = c("Italy"),
  "Central Western Europe" = c("France", "Belgium", "Netherlands", "Germany", "Switzerland", "Austria", "Luxembourg"),
  "Northern Europe" = c("Norway", "Sweden", "Finland", "Denmark", "Estonia", "Latvia", "Lithuania"),
  "Central Eastern Europe" = c("Poland", "Slovakia", "Czech Republic", "Hungary", "Slovenia"),
  "South Eastern Europe" = c("Croatia", "Bosnia and Herzegovina", "Serbia", "Montenegro", "Kosovo", "Albania", "North Macedonia", "Bulgaria", "Romania", "Greece"),
  "Ukraine-Moldova" = c("Ukraine", "Moldova"),
  "National grid" = c("Turkey", "Russia", "Belarus")
)

# Build region mapping df
region_df <- do.call(rbind, lapply(names(region_map), function(region) {
  data.frame(name_long = region_map[[region]], region = region, stringsAsFactors = FALSE)
}))

# Join to spatial data
europe_map <- world %>%
  left_join(region_df, by = "name_long") %>%
  filter(!is.na(region))


# Define colors for regions (excluding national grid)
region_names <- c(
  "British Isles",
  "Iberian Peninsula",
  "Appenine Peninsula",
  "Central Western Europe",
  "Northern Europe",
  "Central Eastern Europe",
  "South Eastern Europe",
  "Ukraine-Moldova"
)

# Get colors from RColorBrewer's "Accent" palette
region_colors <- setNames(
  RColorBrewer::brewer.pal(length(region_names), "Accent"),
  region_names
)

# Add fill and pattern columns
europe_map <- europe_map %>%
  mutate(
    fill_color = region_colors[region],
    fill_color = ifelse(region == "National grid", "white", fill_color),
    pattern = ifelse(region == "National grid", "stripe", "none")
  )

# Final map with ggpattern
ggplot(europe_map) +
  geom_sf_pattern(
    aes(fill = region, pattern = pattern),
    color = "black", size = 0.05,
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.01
  ) +
  scale_fill_manual(
    name = "Grid Region",
    values = c(region_colors, "National grid" = "white"),
    breaks = c(names(region_colors), "National grid"),
    labels = c(names(region_colors), "National grid")
  ) +
  scale_pattern_manual(
    values = c("none" = "none", "stripe" = "stripe")
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        pattern = c(rep("none", length(region_colors)), "stripe"),
        fill = c(unname(region_colors), "white"),
        pattern_fill = c(rep(NA, length(region_colors)), "black"),
        pattern_density = 0.1,
        pattern_spacing = 0.01,
        pattern_angle = 45
      )
    ),
    pattern = "none"
  ) +
  coord_sf(
    xlim = c(-12, 45),
    ylim = c(34, 72),
    expand = FALSE,
    crs = st_crs(world)
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) 


ggsave("GCAM_Europe_regions.png", figure4, width = 7, height = 6, dpi = 600, bg = "white")
ggsave("GCAM_Europe_regions.pdf", figure4, width = 7, height = 6, bg = "white")


##########============================SUPPLEMENTARY FIGURE S2 ==================================##########

df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 7)

eu_long <- df %>%
  pivot_longer(
    cols = `2005`:`2050`,
    names_to  = "Year",
    values_to = "Emissions"
  ) %>%
  mutate(
    Year     = as.integer(Year),
    Scenario = as.character(Scenario),
    Country  = as.character(Country)
  )

# Countries to show
sel_countries <- c(
  "France", "Germany", "Greece", "Italy",
  "Lithuania", "Netherlands", "Poland",
  "Spain", "Sweden"
)

eu_long <- eu_long %>%
  filter(Country %in% sel_countries)

# ------------------------------------------------------------------
# 1) Define scenario levels (including Historical)
# ------------------------------------------------------------------
scen_levels <- c(
  "Historical",
  "FF55_COST_OPT",
  "NECP_COST_OPT",
  "FF55_POLICY",
  "NECP_POLICY"
)

hist_line <- eu_long %>%
  filter(Scenario == "FF55_POLICY", Year <= 2015) %>%
  mutate(
    Scenario = factor("Historical", levels = scen_levels)
  )

# Scenario trajectories from 2015 onward
traj_data <- eu_long %>%
  filter(Scenario %in% scen_levels[-1],
         Year >= 2015, Year <= 2030) %>%
  mutate(
    Scenario = factor(Scenario, levels = scen_levels)
  )

# NECP 2030 targets
necp_targets <- eu_long %>%
  filter(Scenario == "NECP_target", Year == 2030)

# ------------------------------------------------------------------
# 2) Colours
# ------------------------------------------------------------------
scenario_cols <- c(
  "Historical"     = "black",
  "FF55_COST_OPT"  = "#2F79B5",
  "NECP_COST_OPT"  = "#9448A0",
  "FF55_POLICY"    = "#9BC8E0",
  "NECP_POLICY"    = "#FF7B00"
)

necp_target_col <- "#4EBF7B"

# ------------------------------------------------------------------
# 3) Plot
# ------------------------------------------------------------------
FigS2 <- ggplot() +
  # Historical line
  geom_line(
    data = hist_line,
    aes(x = Year, y = Emissions,
        colour = Scenario, group = Country),
    linewidth = 0.9
  ) +
  geom_line(
    data = traj_data,
    aes(x = Year, y = Emissions,
        colour = Scenario,
        group = interaction(Country, Scenario)),
    linewidth = 0.8
  ) +
  geom_hline(
    data = necp_targets,
    aes(yintercept = Emissions),
    colour   = necp_target_col,
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  scale_colour_manual(
    values = scenario_cols,
    breaks = scen_levels,
    name   = NULL
  ) +
  facet_wrap(~ Country, ncol = 3, scales = "free_y") +
  labs(
    x = NULL,
    y = expression("MtCO"[2]),
    title = expression(bold("National CO"[2] * " emissions"))
  ) +
  theme_classic() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text       = element_text(size = 12),
    plot.title       = element_text(size = 16),
    legend.position  = "bottom",
    legend.text      = element_text(size = 11),
    axis.text        = element_text(size = 11),
    axis.title.y     = element_text(size = 12)
  )

print(FigS2)

ggsave("National_emissions.png", FigS2, width = 10, height = 6, dpi = 600, bg = "white")
ggsave("National_emissions.pdf", FigS2, width = 10, height = 6, bg = "white")
