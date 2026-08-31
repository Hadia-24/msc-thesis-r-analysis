# MSc Thesis Analysis

# 1. Packages
library(ggplot2)
library(readr)
library(dplyr)
library(readxl)
library(patchwork)
library(grid)
library(tidyr)
library(GGally)     
library(corrplot)   
library(zoo)        

# 2. Data import and preparation

# Charcoal (CHAR)
load("kisi_char.RData") 
char_df <- chardat

# Climate
climate <- read_delim(
  "climate_data.csv",
  delim = ";"
) %>% mutate(age_median_bp = age_median_bp * 1000)

# Vegetation
veg <- read_csv("pollen_data.csv") 

# Clean vegetation data
veg_clean_fixed <- veg %>%
  mutate(
    AP = ifelse(AP == 0, NA, AP),
    NAP = ifelse(NAP == 0, NA, NAP),
    AP_NAP_ratio = AP / NAP
  ) %>%
  filter(!is.na(AP_NAP_ratio), !is.na(Ages))

# Charcoal counts for concentration + morphotypes
charcoal_conc <- read_excel("charcoal_counts.xlsx") %>%
  mutate(
    mid_age = (age_top + age_bot) / 2,
    charcoal_concentration = charcoal_sum / volume
  )

# Vegetation
veg <- read_csv(
  "pollen_data.csv"
) %>% mutate(AP_NAP_ratio = AP / NAP) %>% filter(!is.na(Ages))

# 3. Summary statistics

# CHAR
mean_CHAR       <- mean(char_df$CHAR, na.rm = TRUE)
sd_CHAR         <- sd(char_df$CHAR, na.rm = TRUE)
mean_background <- mean(char_df$back, na.rm = TRUE)
sd_background   <- sd(char_df$back, na.rm = TRUE)

# Climate
mean_temp <- mean(climate$temp_july, na.rm = TRUE)
sd_temp   <- sd(climate$temp_july, na.rm = TRUE)
mean_temp_ann <- mean(climate$temp_ann, na.rm = TRUE)
sd_temp_ann   <- sd(climate$temp_ann, na.rm = TRUE)
mean_prec <- mean(climate$prec_ann, na.rm = TRUE)
sd_prec   <- sd(climate$prec_ann, na.rm = TRUE)

# Vegetation
mean_ratio <- mean(veg$AP_NAP_ratio, na.rm = TRUE)
sd_ratio   <- sd(veg$AP_NAP_ratio, na.rm = TRUE)
mean_AP  <- mean(veg$AP, na.rm = TRUE)
sd_AP    <- sd(veg$AP, na.rm = TRUE)
mean_NAP <- mean(veg$NAP, na.rm = TRUE)
sd_NAP   <- sd(veg$NAP, na.rm = TRUE)

# Print summary
cat("CHAR: ", sprintf("%.1f ± %.1f", mean_CHAR, sd_CHAR), "\n")
cat("Background: ", sprintf("%.1f ± %.1f", mean_background, sd_background), "\n")
cat("July Temp: ", sprintf("%.2f ± %.2f °C", mean_temp, sd_temp), "\n")
cat("Annual Temp: ", sprintf("%.2f ± %.2f °C", mean_temp_ann, sd_temp_ann), "\n")
cat("Precipitation: ", sprintf("%.1f ± %.1f mm", mean_prec, sd_prec), "\n")
cat("AP/NAP Ratio: ", sprintf("%.2f ± %.2f", mean_ratio, sd_ratio), "\n")
cat("AP %: ", sprintf("%.1f ± %.1f", mean_AP, sd_AP), "\n")
cat("NAP %: ", sprintf("%.1f ± %.1f", mean_NAP, sd_NAP), "\n")

# 4. Multi-proxy record

# Common age scale
all_min <- min(char_df$age, climate$age_median_bp, veg$Ages, na.rm = TRUE)
all_max <- max(char_df$age, climate$age_median_bp, veg$Ages, na.rm = TRUE)
gap <- 200
x_hi <- all_max + gap
x_lo <- all_min - gap

major_breaks <- seq(0, 12000, by = 2000)
minor_breaks <- seq(0, 12000, by = 1000)

common_scale_blank <- scale_x_reverse(
  limits = c(x_hi, x_lo),
  breaks = major_breaks,
  minor_breaks = minor_breaks,
  labels = NULL,
  expand = c(0, 0)
)

common_scale_label <- scale_x_reverse(
  limits = c(x_hi, x_lo),
  breaks = major_breaks,
  minor_breaks = minor_breaks,
  labels = major_breaks,
  expand = c(0, 0)
)

# Panel theme
panel_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.border = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90"),
    axis.line = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )

# Panel 1: CHAR + Background
p_char <- ggplot(char_df, aes(age)) +
  geom_line(aes(y = CHAR, color = "CHAR"), size = 0.8) +
  geom_line(aes(y = back, color = "Background"), size = 0.8) +
  scale_color_manual(NULL, values = c("CHAR" = "firebrick", "Background" = "steelblue")) +
  common_scale_blank +
  labs(y = "CHAR", x = NULL) +
  panel_theme +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(0.85, 0.80),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.8, "lines")
  )

# Panel 2: July Temp
p_temp <- ggplot(climate, aes(age_median_bp, temp_july)) +
  geom_line(color = "royalblue", size = 0.8) +
  common_scale_blank +
  labs(y = "July Temp (\u00b0C)", x = NULL) +
  panel_theme +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

# Panel 3: Annual Temp
p_temp_ann <- ggplot(climate, aes(age_median_bp, temp_ann)) +
  geom_line(color = "darkorange", size = 0.8) +
  common_scale_blank +
  labs(y = "Annual Temp (\u00b0C)", x = NULL) +
  panel_theme +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

# Panel 4: Precipitation
p_prec <- ggplot(climate, aes(age_median_bp, prec_ann)) +
  geom_line(color = "steelblue", size = 0.8) +
  common_scale_blank +
  labs(y = "Precip. (mm)", x = NULL) +
  panel_theme +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

# Panel 5: AP/NAP Ratio (with NA correction)
p_ratio <- ggplot(veg_clean_fixed, aes(Ages, AP_NAP_ratio)) +
  geom_line(color = "forestgreen", size = 0.8) +
  common_scale_label +
  labs(y = "AP/NAP Ratio", x = "Age (years BP)") +
  panel_theme

# Combine all panels into Figure 2
png("Figure2_main.png", width = 20, height = 25, units = "cm", res = 300)
final_plot <- (p_char / p_temp / p_temp_ann / p_prec / p_ratio) +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1, 1)) +
  plot_annotation(title = "Holocene Multi-Proxy Record: Fire, Climate, Precipitation, and Vegetation", tag_levels = 'A') &
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
print(final_plot)
dev.off()

# Save multi-proxy figure
final_plot_labeled <- final_plot +
  plot_annotation(
    title = "Holocene Multi-proxy Record from Lake Kisi",
    tag_levels = 'A'
  ) &
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

ggsave(
  filename = "Figure2_Multiproxy.png",
  plot = final_plot_labeled,
  width = 20, height = 30, units = "cm", dpi = 300
)

# 5. Charcoal concentration

p_char_conc <- ggplot(charcoal_conc, aes(x = mid_age, y = charcoal_concentration)) +
  geom_line(color = "#b71c1c", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "#4682b4") +
  scale_x_reverse() +
  labs(
    title = "Charcoal Concentration Over Time",
    x = "Age (years BP)",
    y = "Charcoal Concentration (particles/cm³)"
  ) +
  theme_minimal()

print(p_char_conc)

ggsave(
  filename = "Figure3_Charcoal_Concentration.png",
  plot = p_char_conc,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# 6. AP and NAP percentages

veg_clean_fixed <- veg %>%
  mutate(
    AP = ifelse(AP == 0, NA, AP),
    NAP = ifelse(NAP == 0, NA, NAP)
  ) %>%
  filter(!is.na(AP), !is.na(NAP), !is.na(Ages))

p_ap_nap <- ggplot(veg_clean_fixed, aes(x = Ages)) +
  geom_line(aes(y = AP, color = "AP"), linewidth = 1.2) +
  geom_line(aes(y = NAP, color = "NAP"), linewidth = 1.2) +
  geom_smooth(aes(y = AP, color = "AP"), method = "loess", span = 0.5, se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = NAP, color = "NAP"), method = "loess", span = 0.5, se = FALSE, linetype = "dashed") +
  scale_x_reverse(breaks = seq(0, 12000, 2000)) +
  scale_color_manual(values = c("AP" = "forestgreen", "NAP" = "skyblue")) +
  labs(
    title = "AP and NAP Percentages Over Time",
    x = "Age (years BP)",
    y = "Pollen Percentage"
  ) +
  theme_minimal()

print(p_ap_nap)

ggsave(
  "Figure7_AP_NAP_Percentages.png",
  plot = p_ap_nap,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# 7. Charcoal morphotypes

# Continuous morphotype plot
charcoal_conc <- charcoal_conc %>%
  mutate(
    conc_ANG = sum_ANG / volume,
    conc_ELO = sum_ELO / volume,
    conc_IRR = sum_IRR / volume
  )

morpho_plot <- ggplot(charcoal_conc, aes(x = mid_age)) +
  geom_line(aes(y = conc_ANG, color = "ANG")) +
  geom_line(aes(y = conc_ELO, color = "ELO")) +
  geom_line(aes(y = conc_IRR, color = "IRR")) +
  scale_color_manual(values = c("ANG" = "tomato", "ELO" = "orange", "IRR" = "purple")) +
  scale_x_reverse(breaks = seq(0, 12000, 2000)) +
  labs(
    title = "Charcoal Morphotypes (Concentration)",
    x = "Age (years BP)",
    y = "Concentration (particles/cm³)"
  ) +
  theme_minimal()

print(morpho_plot)

ggsave(
  "Figure5_Morphotypes.png",
  plot = morpho_plot,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# 500-year binned morphotype plot
morpho_binned <- charcoal_conc %>%
  select(mid_age, conc_ANG, conc_ELO, conc_IRR) %>%
  pivot_longer(cols = starts_with("conc_"), names_to = "Type", values_to = "Conc") %>%
  mutate(
    Type = recode(Type, conc_ANG = "ANG", conc_ELO = "ELO", conc_IRR = "IRR"),
    mid_age_bin = cut(
      mid_age,
      breaks = seq(0, 12000, by = 500),
      labels = seq(250, 11750, by = 500)
    )
  ) %>%
  group_by(mid_age_bin, Type) %>%
  summarise(mean_conc = mean(Conc, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(mid_age_bin)) %>%
  mutate(mid_age_bin = as.numeric(as.character(mid_age_bin)))

morpho_binned_plot <- ggplot(morpho_binned, aes(x = mid_age_bin, y = mean_conc, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_x_reverse(breaks = seq(0, 12000, 1000)) +
  scale_fill_manual(values = c("ANG" = "tomato", "ELO" = "orange", "IRR" = "purple")) +
  labs(
    title = "Charcoal Morphotypes (500 yr bins)",
    x = "Age bin (years BP)",
    y = "Mean Concentration (particles/cm³)"
  ) +
  theme_minimal()

print(morpho_binned_plot)

ggsave(
  "Figure6_Morphotypes-binned.png",
  plot = morpho_binned_plot,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# 8. Charcoal size classes

size_data <- charcoal_conc %>%
  select(mid_age, sum_S_rel, sum_M_rel, sum_L_rel) %>%
  pivot_longer(cols = starts_with("sum_"), names_to = "Size", values_to = "Percent") %>%
  mutate(
    Size = recode(Size, sum_S_rel = "Small", sum_M_rel = "Medium", sum_L_rel = "Large"),
    mid_age_bin = cut(
      mid_age,
      breaks = seq(0, 12000, by = 500),
      labels = seq(250, 11750, by = 500)
    )
  ) %>%
  group_by(mid_age_bin, Size) %>%
  summarise(mean_percent = mean(Percent, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(mid_age_bin)) %>%
  mutate(mid_age_bin_num = as.numeric(as.character(mid_age_bin)))

size_plot <- ggplot(size_data, aes(x = mid_age_bin_num, y = mean_percent, fill = Size)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("Small" = "#66c2a5", "Medium" = "#fc8d62", "Large" = "#8da0cb")) +
  scale_x_reverse(breaks = seq(0, 12000, 1000)) +
  labs(
    title = "Charcoal Size Classes (Relative %)",
    x = "Age bin (years BP)",
    y = "%"
  ) +
  theme_minimal()

print(size_plot)

ggsave(
  "Figure6_SizeClasses.png",
  plot = size_plot,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# 9. CHAR–vegetation correlations

# Merge CHAR with vegetation data
merged <- full_join(
  char_df %>% select(age, CHAR) %>% rename(Ages = age),
  veg %>% select(Ages, AP, NAP, AP_NAP_ratio),
  by = "Ages"
) %>% arrange(Ages)

# Interpolate vegetation variables to CHAR ages
merged_interp <- merged %>%
  mutate(
    AP           = na.approx(AP,           x = Ages, na.rm = FALSE),
    NAP          = na.approx(NAP,          x = Ages, na.rm = FALSE),
    AP_NAP_ratio = na.approx(AP_NAP_ratio, x = Ages, na.rm = FALSE)
  ) %>%
  filter(!is.na(CHAR), !is.na(AP_NAP_ratio))

# Correlations using interpolated data
cor_ap_char    <- cor(merged_interp$AP, merged_interp$CHAR, method = "pearson")
cor_nap_char   <- cor(merged_interp$NAP, merged_interp$CHAR, method = "pearson")
cor_ratio_char <- cor(merged_interp$AP_NAP_ratio, merged_interp$CHAR, method = "pearson")

cat("Correlation (AP vs CHAR, interpolated): ", cor_ap_char, "\n")
cat("Correlation (NAP vs CHAR, interpolated): ", cor_nap_char, "\n")
cat("Correlation (AP/NAP vs CHAR, interpolated): ", cor_ratio_char, "\n")

# Scatterplots using interpolated data
p_ap_char <- ggplot(merged_interp, aes(x = AP, y = CHAR)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "AP vs CHAR (Interpolated)", x = "AP (%)", y = "CHAR") +
  theme_minimal()

ggsave(
  "Figure4a_AP_vs_CHAR_Interpolated.png",
  plot = p_ap_char,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

p_nap_char <- ggplot(merged_interp, aes(x = NAP, y = CHAR)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "NAP vs CHAR (Interpolated)", x = "NAP (%)", y = "CHAR") +
  theme_minimal()

ggsave(
  "Figure4b_NAP_vs_CHAR_Interpolated.png",
  plot = p_nap_char,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

p_ratio_char <- ggplot(merged_interp, aes(x = AP_NAP_ratio, y = CHAR)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "AP/NAP Ratio vs CHAR (Interpolated)", x = "AP/NAP Ratio", y = "CHAR") +
  theme_minimal()

ggsave(
  "Figure4c_Ratio_vs_CHAR_Interpolated.png",
  plot = p_ratio_char,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# Aggregate to 100-year intervals
merged_binned <- merged_interp %>%
  mutate(age_bin = round(Ages, -2)) %>%
  group_by(age_bin) %>%
  summarise(
    AP = mean(AP, na.rm = TRUE),
    NAP = mean(NAP, na.rm = TRUE),
    AP_NAP_ratio = mean(AP_NAP_ratio, na.rm = TRUE),
    CHAR = mean(CHAR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(AP), !is.na(NAP), !is.na(CHAR))

# Correlations using 100-year data
cor_ap_char_bin    <- cor(merged_binned$AP, merged_binned$CHAR, method = "pearson")
cor_nap_char_bin   <- cor(merged_binned$NAP, merged_binned$CHAR, method = "pearson")
cor_ratio_char_bin <- cor(merged_binned$AP_NAP_ratio, merged_binned$CHAR, method = "pearson")

cat("Correlation (AP vs CHAR, 100-yr bins): ", cor_ap_char_bin, "\n")
cat("Correlation (NAP vs CHAR, 100-yr bins): ", cor_nap_char_bin, "\n")
cat("Correlation (AP/NAP vs CHAR, 100-yr bins): ", cor_ratio_char_bin, "\n")

# Scatterplots using 100-year data
p_ap_char_bin <- ggplot(merged_binned, aes(x = AP, y = CHAR)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "AP vs CHAR (100-yr bins)", x = "AP (%)", y = "CHAR") +
  theme_minimal()

ggsave(
  "Figure7a_AP_vs_CHAR_100.png",
  plot = p_ap_char_bin,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

p_nap_char_bin <- ggplot(merged_binned, aes(x = NAP, y = CHAR)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "NAP vs CHAR (100-yr bins)", x = "NAP (%)", y = "CHAR") +
  theme_minimal()

ggsave(
  "Figure7b_NAP_vs_CHAR_100.png",
  plot = p_nap_char_bin,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

p_ratio_char_bin <- ggplot(merged_binned, aes(x = AP_NAP_ratio, y = CHAR)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "AP/NAP Ratio vs CHAR (100-yr bins)", x = "AP/NAP Ratio", y = "CHAR") +
  theme_minimal()

ggsave(
  "Figure7c_Ratio_vs_CHAR_100.png",
  plot = p_ratio_char_bin,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# Correlation matrix and heatmap
cor_matrix <- cor(
  merged_binned %>% select(AP, NAP, AP_NAP_ratio, CHAR),
  method = "pearson"
)
print(cor_matrix)

png("Figure9_Heatmap.png", width = 18, height = 12, units = "cm", res = 300)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  diag = FALSE,
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Heatmap (100-yr bins)",
  mar = c(0, 0, 1, 0)
)

dev.off()
