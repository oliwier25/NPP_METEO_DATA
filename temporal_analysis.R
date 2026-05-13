library(tidyverse)
library(ggplot2)
library(lubridate)

#Figure 4
#Set your own path

path <- "/Users/oliwier/Downloads/npp_monthly_pine_mean.csv"
df <- read.csv(path, header = FALSE)

colnames(df)[1] <- "NPP"

df$date <- seq(as.Date("2002-01-01"), by = "month", length.out = nrow(df))

df$NPP <- df$NPP

event_dates <- as.Date(c("2006-07-01", "2009-04-01", "2015-08-01", "2019-06-01"))
event_labels <- c("July 2006", "April 2009", "August 2015", "June 2019")

event_points <- df[df$date %in% event_dates, ]

ggplot(df, aes(x = date, y = NPP)) +
  geom_line(color = "#228B22", linewidth = 0.7) +
  geom_point(color = "#228B22", shape = 21, fill = "#90EE90", size = 1.3, alpha = 0.8) +
  geom_vline(xintercept = event_dates, color = "red", linetype = "dashed", linewidth = 0.6) +
  geom_point(data = event_points, aes(x = date, y = NPP), color = "black", size = 2) +
  annotate("text", x = event_dates, y = 155, label = event_labels, 
           color = "red", vjust = 0, size = 3.5, fontface = "plain") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(-5, 165), breaks = seq(0, 150, 50)) +
  labs(y = expression(NPP ~ "[" * gC ~ m^{-2} ~ month^{-1} * "]"), x = NULL) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0"),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(30, 10, 10, 10) # margines górny na napisy
  )





#Figure 5
#Set your own path

df$Year <- year(df$date)
df$Month <- as.character(month(df$date, label = TRUE, abbr = FALSE)) 

target_months <- c("April", "June", "July", "August")

df_filtered <- df %>% 
  filter(Month %in% target_months) %>%
  mutate(Month = factor(Month, levels = target_months)) 

highlight_points <- data.frame(
  Year = c(2006, 2009, 2015, 2019),
  Month = factor(c("July", "April", "August", "June"), levels = target_months),
  Label = c("July 2006", "April 2009", "August 2015", "June 2019")
) %>%
  left_join(df_filtered, by = c("Year", "Month"))

ggplot(df_filtered, aes(x = Year, y = NPP, color = Month, group = Month)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2, alpha = 0.5) +
  scale_color_manual(values = c(
    "April" = "#40E0D0",  
    "June" = "#006400",   
    "July" = "#FFFF00",   
    "August" = "#FFA500"  
  )) +
  
  geom_point(data = highlight_points, aes(x = Year, y = NPP, fill = Month), 
             color = "black", shape = 21, size = 3.5, stroke = 1, show.legend = FALSE) +
  
  scale_fill_manual(values = c(
    "April" = "#40E0D0", "June" = "#006400", "July" = "#FFFF00", "August" = "#FFA500"
  )) +
  
  geom_text(data = highlight_points, aes(label = Label), 
            color = "black", vjust = c(1.8, -1.5, 1.8, 1.8), size = 3.5) +
  scale_x_continuous(breaks = 2002:2023, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(25, 155), breaks = seq(25, 150, 25)) +
  labs(y = expression(NPP ~ "[" * gC ~ m^{-2} ~ month^{-1} * "]"), x = NULL, color = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0"),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )









# Figure 6

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(gridExtra)

# Set your own path
base_path <- "/Users/oliwier/Desktop/NPP_analizy_6_05/"

npp_raw <- read.csv(paste0(base_path, "psn_monthly_pine.csv"), header = FALSE)
t_raw   <- read.csv(paste0(base_path, "T_monthly.csv"), header = FALSE)
p_raw   <- read.csv(paste0(base_path, "P_monthly.csv"), header = FALSE)
eto_raw <- read.csv(paste0(base_path, "ETo_monthly.csv"), header = FALSE)

df <- data.frame(
  NPP = npp_raw[,1] * 1000,
  T   = t_raw[,1],
  P   = p_raw[,1],
  ETo = eto_raw[,1]
)

df$Date  <- seq(as.Date("2002-01-01"), by = "month", length.out = nrow(df))
df$Month <- month(df$Date)
df$CWB   <- df$P - df$ETo

df_std <- df %>%
  group_by(Month) %>%
  mutate(
    ZNPP = as.vector(scale(NPP)),
    ZT   = as.vector(scale(T)),
    ZP   = as.vector(scale(P)),
    ZETo = as.vector(scale(ETo)),
    ZCWB = as.vector(scale(CWB))
  ) %>%
  ungroup() %>%
  arrange(Date)

df_long <- df_std %>%
  dplyr::select(Date, ZNPP, ZT, ZP, ZETo, ZCWB) %>%
  pivot_longer(cols = -Date, names_to = "Variable", values_to = "Z_value")


df_long$Variable <- factor(df_long$Variable, levels = c("ZNPP", "ZT", "ZP", "ZETo", "ZCWB"))

get_stats_label <- function(data, var_name) {
  sub_data <- data[data$Variable == var_name, ]
  model <- lm(Z_value ~ Date, data = sub_data)
  s <- summary(model)
  
  slope <- model$coefficients[2]
  r2 <- s$r.squared
  p_val <- s$coefficients[2, 4]
  
  label <- paste0(
    "y=", sprintf("%.5f", slope), "x\n",
    "R²=", sprintf("%.3f", r2), "\n",
    "p=", if(p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
  )
  return(label)
}

event_dates <- as.Date(c("2006-07-01", "2009-04-01", "2015-08-01", "2019-06-01"))
df_events <- df_long %>% filter(Date %in% event_dates)

vars <- levels(df_long$Variable)
colors <- c("#228B22", "#D32F2F", "#1976D2", "#455A64", "#FBC02D")

plots <- list()

for (i in 1:length(vars)) {
  v_name <- vars[i]
  v_col  <- colors[i]
  v_txt  <- get_stats_label(df_long, v_name)
  
  p <- ggplot(df_long[df_long$Variable == v_name, ], aes(x = Date, y = Z_value)) +
    theme_bw() +
    geom_line(color = v_col, linewidth = 0.5) +
    geom_vline(xintercept = event_dates, linetype = "dashed", color = "black", alpha = 0.3) +
    geom_point(data = df_events[df_events$Variable == v_name, ], color = "black", size = 1.8) +
    annotate("text", x = Inf, y = Inf, label = v_txt, vjust = 1.1, hjust = 1.05, size = 2.8) +
    scale_y_continuous(limits = c(-4, 4), breaks = seq(-2, 2, 2)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(y = v_name, x = NULL) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(face = "bold", size = 8),
      axis.text.x = if(i < 5) element_blank() else element_text(angle = 45, hjust = 1),
      axis.ticks.x = if(i < 5) element_blank() else element_line()
    )
  
  plots[[i]] <- p
}

final_plot <- grid.arrange(grobs = plots, ncol = 1, heights = c(1, 1, 1, 1, 1.3))






# Figure 8
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(car)

# Set your own path
base_path <- "/Users/oliwier/Desktop/NPP_analizy_7_05/"

npp_raw <- read.csv(paste0(base_path, "npp_monthly_pine_mean.csv"), header = FALSE)
t_raw   <- read.csv(paste0(base_path, "T_monthly_pine_mean.csv"), header = FALSE)
p_raw   <- read.csv(paste0(base_path, "P_monthly_pine_mean.csv"), header = FALSE)
eto_raw <- read.csv(paste0(base_path, "ETo_monthly_pine_mean.csv"), header = FALSE)

df <- data.frame(
  NPP = npp_raw[,1],
  T   = t_raw[,1],
  P   = p_raw[,1],
  ETo = eto_raw[,1]
)

df$Date  <- seq(as.Date("2002-01-01"), by = "month", length.out = nrow(df))
df$Month <- month(df$Date)
df$CWB   <- df$P - df$ETo

df_std <- df %>%
  group_by(Month) %>%
  mutate(
    ZNPP = as.vector(scale(NPP)),
    ZT   = as.vector(scale(T)),
    ZP   = as.vector(scale(P)),
    ZETo = as.vector(scale(ETo)),
    ZCWB = as.vector(scale(CWB))
  ) %>%
  ungroup()

target_months <- c(4, 6, 7, 8)
month_names <- c("April", "June", "July", "August")

df_plot <- df_std %>%
  filter(Month %in% target_months) %>%
  mutate(MonthLabel = factor(Month, levels = target_months, labels = month_names)) %>%
  select(MonthLabel, Date, ZNPP, ZT, ZP, ZETo, ZCWB) %>%
  pivot_longer(cols = c(ZT, ZP, ZETo, ZCWB), names_to = "Variable", values_to = "Z_Meteo") %>%
  mutate(Variable = factor(Variable, levels = c("ZT", "ZP", "ZETo", "ZCWB")))

highlight_dates <- as.Date(c("2009-04-01", "2019-06-01", "2006-07-01", "2015-08-01"))
df_highlights <- df_plot %>% 
  filter(Date %in% highlight_dates) %>%
  mutate(Year = year(Date))

get_stats <- df_plot %>%
  group_by(MonthLabel, Variable) %>%
  do(model = lm(Z_Meteo ~ ZNPP, data = .)) %>%
  mutate(
    r2 = sprintf("R^2 == %.3f", summary(model)$r.squared),
    p_val = {
      p <- summary(model)$coefficients[2,4]
      if(p < 0.001) "p < 0.001" else sprintf("p == %.3f", p)
    }
  )

p_final <- ggplot(df_plot, aes(x = ZNPP, y = Z_Meteo)) +
  facet_grid(Variable ~ MonthLabel, scales = "free_y") +
  geom_point(aes(color = Variable), size = 1.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "grey40", se = FALSE, linewidth = 0.5) +
  geom_point(data = df_highlights, aes(fill = Variable), 
             shape = 21, size = 3, color = "black", stroke = 1) +
  geom_text(data = df_highlights, aes(label = Year), 
            hjust = 1.2, vjust = 1.2, size = 3, family = "Arial", color = "black") +
  

  geom_text(data = get_stats, aes(x = Inf, y = Inf, label = r2), 
            hjust = 1.1, vjust = 1.5, parse = TRUE, size = 3.2, family = "Arial", inherit.aes = FALSE) +
  geom_text(data = get_stats, aes(x = Inf, y = Inf, label = p_val), 
            hjust = 1.1, vjust = 3.2, parse = TRUE, size = 3.2, family = "Arial", inherit.aes = FALSE) +
  

  scale_color_manual(values = c("ZT"="#D32F2F", "ZP"="#1976D2", "ZETo"="#2E7D32", "ZCWB"="#FBC02D")) +
  scale_fill_manual(values = c("ZT"="#D32F2F", "ZP"="#1976D2", "ZETo"="#2E7D32", "ZCWB"="#FBC02D")) +

  labs(x = "ZNPP", y = NULL) +
  theme_bw() +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0")
  )

print(p_final)









# Table 1

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(car)

# Set your own path
base_path <- "/Users/oliwier/Desktop/NPP_analizy_6_05/"

npp_raw <- read.csv(paste0(base_path, "psn_monthly_pine.csv"), header = FALSE)
t_raw   <- read.csv(paste0(base_path, "T_monthly.csv"), header = FALSE)
p_raw   <- read.csv(paste0(base_path, "P_monthly.csv"), header = FALSE)
eto_raw <- read.csv(paste0(base_path, "ETo_monthly.csv"), header = FALSE)


df <- data.frame(
  NPP = npp_raw[,1], 
  T   = t_raw[,1],
  P   = p_raw[,1],
  ETo = eto_raw[,1]
)


df$Date  <- seq(as.Date("2002-01-01"), by = "month", length.out = nrow(df))
df$Month <- month(df$Date)
df$CWB   <- df$P - df$ETo

target_months <- c(4, 6, 7, 8)
month_names <- c("April", "June", "July", "August")
reg_results <- data.frame()

df_std <- df %>%
  group_by(Month) %>%
  mutate(
    ZNPP = as.vector(scale(NPP)),
    ZT   = as.vector(scale(T)),
    ZP   = as.vector(scale(P)),
    ZETo = as.vector(scale(ETo))
  ) %>%
  ungroup()

results_detailed <- data.frame()

for (i in 1:length(target_months)) {
  m_num <- target_months[i]
  m_name <- month_names[i]
  
  m_data <- df_std %>% filter(Month == m_num) %>% drop_na()

  model_det <- lm(ZNPP ~ ZT + ZP + ZETo, data = m_data)
  s_det <- summary(model_det)
  vif_det <- car::vif(model_det)
  
  results_detailed <- rbind(results_detailed, data.frame(
    Month = m_name,
    R2_adj = round(s_det$adj.r.squared, 3),
    Beta_T = round(coef(model_det)["ZT"], 3),
    Beta_P = round(coef(model_det)["ZP"], 3),
    Beta_ETo = round(coef(model_det)["ZETo"], 3),
    Max_VIF = round(max(vif_det), 2)
  ))
}

print("--- WYNIKI REGRESJI (T + P + ETo) ---")
print(results_detailed)


