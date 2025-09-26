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

#Code to reproduce the figures in the journal article
#Comparing cost-optimal to policy-driven scenarios for a decarbonised European energy system


##########============================FIGURE 1 ==================================##########

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

world$name_long[world$name_long == "Russian Federation"] <- "Russia"

# Define GCAM region-country mapping
gcam_mapping <- list(
  Africa_Eastern = c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Madagascar", "Mauritius", "Reunion", "Rwanda", "Sudan", "Somalia", "Uganda"),
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
R <- ggplot(world_mapped) +
  geom_sf(aes(fill = fill_color), color = "gray30", size = 0.1) +
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA)
  )

R

# --- Save
ggsave("GCAM_regions_map.png", R, width = 14, height = 8, dpi = 600)
ggsave("GCAM_regions_map.png.pdf", R, width = 14, height = 8)


##########============================FIGURE 3A==================================##########

# 1) Read sheet 1
df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 1) %>%
  select(Sector, Scenario, `2015`, `2030`, `2050`)

# 2) Long format 
df_long <- df %>%
  pivot_longer(c(`2015`, `2030`, `2050`), names_to = "Year", values_to = "Emissions") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!(Scenario == "Historical" & Year %in% c(2030, 2050)),
         !(Scenario != "Historical" & Year == 2015)) %>%
  mutate(
    Scenario = ifelse(Year == 2015, "Historical", Scenario),
    X = ifelse(Year == 2015, "2015", paste0(Scenario, "\n", Year)),
    Sector = factor(Sector, levels = c("Electricity","Cement","Buildings",
                                       "Transportation","Industry",
                                       "Intl Bunkers","Fugitive Emissions"))
  )

# 3) X-axis order with empty gap placeholders
x_levels <- c(
  "2015", "gap1a",
  paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2030"),
  "gap2a",
  paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2050")
)
df_long$X <- factor(df_long$X, levels = x_levels)

# 4) Manual colours
sector_cols <- c(
  "Electricity"        = "#3E79D3",
  "Cement"             = "#E34A45",
  "Buildings"          = "#FFD92E",
  "Transportation"     = "#68F394",
  "Industry"           = "#AF6AB1",
  "Intl Bunkers"       = "#FFB743",
  "Fugitive Emissions" = "#AFAFAF"
)


# --- Custom X labels: Historical & scenario names -----------------
x_lab_fun <- function(x) {
  ifelse(
    x == "2015", "Historical",
    ifelse(grepl("^gap", x), "", sub("\\n.*$", "", x))  # strip "\n2030"/"\n2050"
  )
}

# --- Base plot -----------------------------------------------------
p3a <- ggplot(df_long, aes(x = X, y = Emissions, fill = Sector)) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_x_discrete(drop = FALSE, labels = x_lab_fun) +
  scale_y_continuous(
    breaks = seq(0, 4000, 500),
    expand = expansion(mult = c(0.12, 0.05))  # extra space below axis
  ) +
  scale_fill_manual(values = sector_cols, drop = FALSE) +
  labs(
    title = expression(bold("Fossil CO"[2] * " emissions by sector")),
    subtitle = "EU-27",
    x = NULL,
    y = expression("MtCO"[2]),
    fill = "Sector"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x   = element_text(vjust = 1, hjust = 0.5),
    legend.position = "bottom",
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "plain", size = 13)
  )

p3a 

# --- Positions of year groups on X axis ----------------------------
lvl <- levels(df_long$X)
pos_2015  <- match("2015", lvl)
pos_2030s <- match(paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2030"), lvl)
pos_2050s <- match(paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2050"), lvl)

# --- Year labels below axis ----------------------------------------
ymax <- max(df_long$Emissions, na.rm = TRUE)
y_lab <- -0.20 * ymax   # place below axis, tweak multiplier if needed

p3a <- p3a +
  annotate("text", x = pos_2015,        y = y_lab, label = "2015",
           vjust = 1.5, size = 4, colour = "gray30") +
  annotate("text", x = mean(pos_2030s), y = y_lab, label = "2030",
           vjust = 1.5, size = 4, colour = "gray30") +
  annotate("text", x = mean(pos_2050s), y = y_lab, label = "2050",
           vjust = 1.5, size = 4, colour = "gray30")

print(p3a)


##########============================FIGURE 3B==================================##########

df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 2) %>%
  select(Category, Country, FF55_POLICY, FF55_FREE, NECP_POLICY, NECP_FREE)

# --- Long format ---
df_long <- df %>%
  pivot_longer(cols = c(FF55_POLICY, FF55_FREE, NECP_POLICY, NECP_FREE),
               names_to = "Scenario", values_to = "Reduction")

df_long <- df_long %>% filter(Reduction < 0)


country_order <- df %>%
  distinct(Category, Country) %>%
  arrange(factor(Category, levels = c("Northwestern Europe","Eastern Europe","Southern Europe"))) %>%
  pull(Country)
df_long$Country <- factor(df_long$Country, levels = country_order)

# --- Palettes ---
northwest_cols <- sequential_hcl(9, "TealGrn")
eastern_cols   <- sequential_hcl(12, "Magenta")
southern_cols  <- sequential_hcl(6, "OrYel")   # orange-yellow

country_cols <- c(
  setNames(northwest_cols, country_order[1:9]),
  setNames(eastern_cols,   country_order[10:21]),
  setNames(southern_cols,  country_order[22:27])
)


# --- Main plot ---
p <- ggplot(df_long, aes(x = Scenario, y = Reduction, fill = Country)) +
  geom_bar(stat = "identity", width = 0.4) +
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
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 1, hjust = 0.5),
    legend.position = "right",
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
           gp = gpar(fontface = "bold", cex = 0.8)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27 + 12/27/2), "npc"), width = unit(0.02, "npc"), height = unit(12/27, "npc"),
           gp = gpar(fill = "#D174A6", col = NA)),
  textGrob("Eastern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27 + 12/27/2), "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 0.8)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(6/27/2, "npc"), width = unit(0.02, "npc"), height = unit(6/27, "npc"),
           gp = gpar(fill = "#F39B4C", col = NA)),
  textGrob("Southern Europe", x = unit(0.06, "npc"),
           y = unit(6/27/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 0.8))
)

# --- Combine plot + legend with blocks ---
p3b <- cowplot::plot_grid(
  p + theme(legend.position = "none"),
  grobTree(leg, overlay_blocks),
  rel_widths = c(3, 1)
)

print(p3b)

##########============================COMBINE 3A + 3B==================================##########

# Combine side by side

figure3 <- plot_grid(
  p3a + theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "plain")   # <-- subtitle size
  ),
  p3b + theme(
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face ="plain"),  # <-- subtitle size
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  ),
  labels = c("A", "B"),
  label_size = 20,
  ncol = 2,
  rel_widths = c(1, 1.1)
)

print(figure3)

#--Save high-res

ggsave("Emissions_AB.png", figure3, width = 16, height = 8, dpi = 600)
ggsave("Emissions_AB.pdf", figure3, width = 16, height = 8)


##########============================FIGURE 4A==================================##########

# 1) Read sheet 3
df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 3) %>%
  select(Fuel, Scenario, `2015`, `2030`, `2050`)

# 2) Long format
df_long <- df %>%
  pivot_longer(c(`2015`, `2030`, `2050`), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!(Scenario == "Historical" & Year %in% c(2030, 2050)),
         !(Scenario != "Historical" & Year == 2015)) %>%
  mutate(Scenario = ifelse(Year == 2015, "Historical", Scenario))

# 3) Assign positions
df_long <- df_long %>%
  mutate(xpos = case_when(
    Year == 2015 ~ 1,
    Year == 2030 & Scenario == "FF55_POLICY" ~ 2,
    Year == 2030 & Scenario == "FF55_FREE"   ~ 3,
    Year == 2030 & Scenario == "NECP_POLICY" ~ 4,
    Year == 2030 & Scenario == "NECP_FREE"   ~ 5,
    Year == 2050 & Scenario == "FF55_POLICY" ~ 6,
    Year == 2050 & Scenario == "FF55_FREE"   ~ 7,
    Year == 2050 & Scenario == "NECP_POLICY" ~ 8,
    Year == 2050 & Scenario == "NECP_FREE"   ~ 9
  ))

# 4) Colours for fuels
fuel_cols <- c(
  "Biomass"     = "#1b9e77",
  "Coal"        = "#747474",
  "Gas"         = "#7570b3",
  "Liquids"     = "#e7298a",
  "Electricity" = "#FF764B",
  "Hydrogen"    = "#84DEF7"
)


# -- Add X (factor with gap placeholders) -------------------------
df_long <- df_long %>%
  mutate(
    # unique x keys; we'll show pretty labels via scale_x_discrete()
    X = ifelse(Year == 2015, "2015", paste0(Scenario, "\n", Year))
  )

x_levels <- c(
  "2015", "gap1a",
  paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2030"),
  "gap2a",
  paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2050")
)
df_long$X <- factor(df_long$X, levels = x_levels)

# -- Helper: label only 'Historical' and scenario names (hide gaps) ------------
x_lab_fun <- function(x) {
  ifelse(
    x == "2015", "Historical",
    ifelse(grepl("^gap", x), "", sub("\\n.*$", "", x))  # strip "\n2030"/"\n2050"
  )
}

# -- Build plot ----------------------------------------------------
p <- ggplot(df_long, aes(x = X, y = Value, fill = Fuel)) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_x_discrete(drop = FALSE, labels = x_lab_fun) +
  scale_y_continuous(breaks = seq(0, 4000, 200)) +   # <-- every 200 now
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  labs(
    title  = "Final energy use by fuel",
    subtitle = "EU-27",
    x = NULL,
    y = "Mtoe",
    fill = "Fuel"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 1, hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(10, 20, 35, 20),
    plot.title = element_text(face = "bold", size = 14) 
  )

# -- Add group year labels centered under bars ---------------------
lvl <- levels(df_long$X)
pos_2015  <- match("2015", lvl)
pos_2030s <- match(paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2030"), lvl)
pos_2050s <- match(paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2050"), lvl)

ymax <- max(df_long$Value, na.rm = TRUE)
y_lab <- -0.06 * ymax  # put labels just below axis; adjust if needed

p4a <- p +
  coord_cartesian(ylim = c(y_lab, NA), clip = "off") +
  annotate("text", x = pos_2015,           y = y_lab, label = "2015", vjust = 1, size = 5) +
  annotate("text", x = mean(pos_2030s),    y = y_lab, label = "2030", vjust = 1, size = 5) +
  annotate("text", x = mean(pos_2050s),    y = y_lab, label = "2050", vjust = 1, size = 5)


print(p4a)


##########============================FIGURE 4B==================================##########

# --- Read sheet 4 (Energy Efficiency contributions) ---
df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 4) %>%
  select(Category, Country, FF55_POLICY, FF55_FREE, NECP_POLICY, NECP_FREE)

# --- Long format ---
df_long <- df %>%
  pivot_longer(cols = c(FF55_POLICY, FF55_FREE, NECP_POLICY, NECP_FREE),
               names_to = "Scenario", values_to = "Reduction")

# --- Preserve block order: NW -> E -> S ---
df_long$Category <- factor(df_long$Category,
                           levels = c("Northwestern Europe","Eastern Europe","Southern Europe"))

# --- Palettes ---
northwest_cols <- sequential_hcl(9, "TealGrn")
eastern_cols   <- sequential_hcl(12, "Magenta")
southern_cols  <- sequential_hcl(6, "OrYel")

country_cols <- c(
  setNames(northwest_cols, unique(df_long$Country[df_long$Category=="Northwestern Europe"])),
  setNames(eastern_cols,   unique(df_long$Country[df_long$Category=="Eastern Europe"])),
  setNames(southern_cols,  unique(df_long$Country[df_long$Category=="Southern Europe"]))
)

# --- Fix global order of countries (light -> dark) ---
df_long$Country <- factor(df_long$Country, levels = names(country_cols))

# --- Plot ---
p4 <- ggplot(df_long, aes(x = Scenario, y = Reduction, fill = Country)) +
  geom_bar(stat = "identity", width = 0.4, position = position_stack()) +
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
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 1, hjust = 0.5),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p4)

# --- Extract legend ---
leg <- cowplot::get_legend(p4)

# --- Overlay blocks for regions ---
overlay_blocks <- grobTree(
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27)/2, "npc"), width = unit(0.02, "npc"), height = unit(9/27, "npc"),
           gp = gpar(fill = "#0BB5AD", col = NA)),
  textGrob("Northwestern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27)/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 0.8)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27 + 12/27/2), "npc"), width = unit(0.02, "npc"), height = unit(12/27, "npc"),
           gp = gpar(fill = "#D174A6", col = NA)),
  textGrob("Eastern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27 + 12/27/2), "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 0.8)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(6/27/2, "npc"), width = unit(0.02, "npc"), height = unit(6/27, "npc"),
           gp = gpar(fill = "#F39B4C", col = NA)),
  textGrob("Southern Europe", x = unit(0.06, "npc"),
           y = unit(6/27/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 0.8))
)

# --- Combine plot and legend ---
p4b <- cowplot::plot_grid(
  p4 + theme(legend.position = "none"),
  grobTree(leg, overlay_blocks),
  rel_widths = c(3, 1)
)

print(p4b)


##########============================COMBINE 4A + 4B==================================##########

# Combine side by side

figure4 <- plot_grid(
  p4a + theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "plain")   # <-- subtitle size
  ),
  p4b + theme(
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 14),
    plot.title   = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face ="plain"),  # <-- subtitle size
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  ),
  labels = c("A", "B"),
  label_size = 20,
  ncol = 2,
  rel_widths = c(1, 1.1)
)

# Show
print(figure4)


ggsave("Efficiency_AB.png", figure4, width = 16, height = 8, dpi = 600)
ggsave("Efficiency_AB.pdf", figure4, width = 16, height = 8)


#========FIGURE 5A==========================================================================================


# 1) Read sheet 5
df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 5) %>%
  select(Fuel, Scenario, `2015`, `2030`, `2050`)

# 2) Long format
df_long <- df %>%
  pivot_longer(c(`2015`, `2030`, `2050`), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!(Scenario == "Historical" & Year %in% c(2030, 2050)),
         !(Scenario != "Historical" & Year == 2015)) %>%
  mutate(Scenario = ifelse(Year == 2015, "Historical", Scenario))

# Separate % renewables marker
df_marker <- df_long %>% filter(Fuel == "% renewables in FE") %>%
  mutate(Percent = Value * 100)  # convert to %
df_stack  <- df_long %>% filter(Fuel != "% renewables in FE")

# 3) X positions with gaps
df_stack <- df_stack %>%
  mutate(X = ifelse(Year == 2015, "2015", paste0(Scenario, "\n", Year)))
x_levels <- c(
  "2015", "gap1a",
  paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2030"),
  "gap2a",
  paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2050")
)
df_stack$X  <- factor(df_stack$X, levels = x_levels)
df_marker$X <- factor(ifelse(df_marker$Year == 2015, "2015", paste0(df_marker$Scenario, "\n", df_marker$Year)),
                      levels = x_levels)

# 4) Custom palette
fuel_cols <- c(
  "Solid biomass"        = "#3B7D23",
  "Biofuels"             = "#4EA72E",
  "Biogas"               = "#8ED973",
  "Renewable electricity"= "#F0E338",
  "Ambient heat"         = "#A6CAEC",
  "Solar thermal"        = "#FF0000"
)

# 5) Label function
x_lab_fun <- function(x) {
  ifelse(x == "2015", "Historical",
         ifelse(grepl("^gap", x), "", sub("\\n.*$", "", x)))
}

# 6) Find max for left axis scaling
y_left_max <- 503.0284562   # or max(df_stack$Value, na.rm = TRUE)

# 7) Plot with proper secondary axis scaling
p5a <- ggplot(df_stack, aes(x = X, y = Value, fill = Fuel)) +
  geom_bar(stat = "identity", width = 0.75) +
  # Triangles scaled to left axis
  geom_point(data = df_marker,
             aes(x = X, y = Percent * y_left_max / 100),
             shape = 17, size = 3, color = "black", inherit.aes = FALSE) +
  scale_x_discrete(drop = FALSE, labels = x_lab_fun) +
  scale_y_continuous(
    name = "Mtoe",
    breaks = seq(0, 600, 50),
    limits = c(0, y_left_max),
    sec.axis = sec_axis(~ . * 100 / y_left_max,
                        name = "% renewables in FE",
                        breaks = seq(0, 100, 10))
  ) +
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  labs(
    title = "Renewable energy use by fuel",
    subtitle = "EU-27",
    x = NULL,
    fill = "Fuel"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 1, hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(10, 20, 35, 20),
    plot.title = element_text(face = "bold", size = 14)  # <-- bold + size 14
  )

p5a

## 1) Where the groups sit on the X axis
lvl <- levels(df_stack$X)
pos_2015  <- match("2015", lvl)
pos_2030s <- match(paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2030"), lvl)
pos_2050s <- match(paste0(c("FF55_POLICY","FF55_FREE","NECP_POLICY","NECP_FREE"), "\n2050"), lvl)

## 2) Fixed offset for the labels
y_lab <- -30   # adjust down if still too close

## 3) Plot
p5a <- ggplot(df_stack, aes(x = X, y = Value, fill = Fuel)) +
  geom_bar(stat = "identity", width = 0.75) +
  geom_point(data = df_marker,
             aes(x = X, y = Percent * y_left_max / 100),
             shape = 17, size = 3, color = "black", inherit.aes = FALSE) +
  scale_x_discrete(drop = FALSE, labels = x_lab_fun) +
  scale_y_continuous(
    name   = "Mtoe",
    breaks = seq(0, 600, 50),
    sec.axis = sec_axis(~ . * 100 / y_left_max,
                        name = "% renewables in FE",
                        breaks = seq(0, 100, 10)),
    expand = c(0, 0)   # no default expansion
  ) +
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  labs(
    title = "Renewable energy use by fuel",
    subtitle = "EU-27",
    x = NULL,
    fill = "Fuel"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x   = element_text(vjust = 1, hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(10, 20, 50, 20),
    plot.title    = element_text(face = "bold", size = 14)
  ) +
  # Allow extra space below zero for labels, without cutting bars
  coord_cartesian(ylim = c(-50, y_left_max), clip = "off") +
  annotate("text", x = pos_2015,        y = y_lab, label = "2015",
           vjust = 1, size = 4, colour = "gray30", fontface = "plain") +
  annotate("text", x = mean(pos_2030s), y = y_lab, label = "2030",
           vjust = 1, size = 4, colour = "gray30", fontface = "plain") +
  annotate("text", x = mean(pos_2050s), y = y_lab, label = "2050",
           vjust = 1, size = 4, colour = "gray30", fontface = "plain")

print(p5a)


#==============FIGURE 5B===============================================================

# --- Read sheet 6 (Renewables contributions towards 2030) ---
df <- read_excel(".../GCAM_Europe_results.xlsx", sheet = 6) %>%
  select(Category, Country, FF55_POLICY, FF55_FREE, NECP_POLICY, NECP_FREE)

# --- Long format ---
df_long <- df %>%
  pivot_longer(cols = c(FF55_POLICY, FF55_FREE, NECP_POLICY, NECP_FREE),
               names_to = "Scenario", values_to = "Contribution")

# --- Preserve block order: NW -> E -> S ---
country_order <- df %>%
  distinct(Category, Country) %>%
  arrange(factor(Category,
                 levels = c("Northwestern Europe","Eastern Europe","Southern Europe"))) %>%
  pull(Country)
df_long$Country <- factor(df_long$Country, levels = country_order)

# --- Palettes ---
northwest_cols <- sequential_hcl(9, "TealGrn")
eastern_cols   <- sequential_hcl(12, "Magenta")
southern_cols  <- sequential_hcl(6, "OrYel")

country_cols <- c(
  setNames(northwest_cols, country_order[1:9]),
  setNames(eastern_cols,   country_order[10:21]),
  setNames(southern_cols,  country_order[22:27])
)

# --- Main plot ---
p <- ggplot(df_long, aes(x = Scenario, y = Contribution, fill = Country)) +
  geom_bar(stat = "identity", width = 0.4) +
  scale_fill_manual(values = country_cols, drop = FALSE,
                    guide = guide_legend(ncol = 1)) +
  scale_y_continuous(
    limits = c(0, 200),
    breaks = seq(0, 200, by = 20)
  ) +
  labs(
    title = "Contributions to renewable energy",
    subtitle = "towards 2030",
    x = NULL,
    y = "Mtoe in 2030 vs 2015"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background  = element_rect(fill = "white", colour = NA),  
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 1, hjust = 0.5),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)  # <-- bold + size 14
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
           gp = gpar(fontface = "bold", cex = 0.8)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(1 - (9/27 + 12/27/2), "npc"), width = unit(0.02, "npc"), height = unit(12/27, "npc"),
           gp = gpar(fill = "#D174A6", col = NA)),
  textGrob("Eastern Europe", x = unit(0.06, "npc"),
           y = unit(1 - (9/27 + 12/27/2), "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 0.8)),
  
  rectGrob(x = unit(0, "npc"), just = "left",
           y = unit(6/27/2, "npc"), width = unit(0.02, "npc"), height = unit(6/27, "npc"),
           gp = gpar(fill = "#F39B4C", col = NA)),
  textGrob("Southern Europe", x = unit(0.06, "npc"),
           y = unit(6/27/2, "npc"), rot = 90, just = "centre",
           gp = gpar(fontface = "bold", cex = 0.8))
)

# --- Combine plot and legend ---
p5b <- cowplot::plot_grid(
  p + theme(legend.position = "none"),
  grobTree(leg, overlay_blocks),
  rel_widths = c(3, 1)
)

print(p5b)


##########============================COMBINE 5A + 5B==================================##########

library(cowplot)

# Combine side by side
figure5 <- plot_grid(
  p5a + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold")
  ),
  p5b + theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),   # legend text size
    legend.title = element_text(size = 12, face = "bold")  # (optional) legend title
  ),
  labels = c("A", "B"),
  label_size = 20,
  ncol = 2,
  rel_widths = c(1, 1.1)   # adjust width ratio if left panel needs more space
)

# Show
print(figure5)

ggsave("Renewables_AB.png", figure5, width = 16, height = 8, dpi = 600)
ggsave("Renewables_AB.pdf", figure5, width = 16, height = 8)

#._
