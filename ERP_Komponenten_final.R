# Hier erfolgt die Analyse der ERP Komponenten im Einzlenen

library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)

n100 <- read.csv('/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/ERPs/N100_clean.csv', sep = ";", dec = ",")
p200 <- read.csv('/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/ERPs/P200_clean.csv' , sep = ";", dec =",")
p50  <- read.csv('/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/ERPs/P50_clean.csv', sep = ";", dec = ",")
act_all_data <- read_excel('/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/lab_grand_summary.xlsx')

EG <- c("vp02ha", "vp02hl", "vp03hl", "vp04ha", "vp04hl", "vp05ha", "vp06ha", "vp07ha", "vp08ha", "vp09ha", "vp09hl",
        "vp10ha", "vp10hl", "vp11ha", "vp11hl", "vp13ha", "vp14hl", "vp15hl","vp16hl", "vp17hl", "vp18ha", "vp19ha", 
        "vp20hl", "vp21ha", "vp21hl", "vp22ha", "vp22hl", "vp23ha", "vp24ha", "vp32hl", "vp35hl", "vp39hl", "vp41hl", 
        "vp42hl")
CG <- c("vp15ha", "vp16ha", "vp25hl", "vp26hl", "vp27hl", "vp30hl", "vp31hl", "vp37hl", "vp38ha", "vp40hl", "vp42ha", 
        "vp43hl", "vp44hl", "vp45hl", "vp46hl", "vp47hl", "vp48hl", "vp49hl", "vp50ha", "vp53hl")

# Code von anderen Skript kopiert, da gut für Korrelationen da SIN score etc. schon gut sortiert drin ist.
info_tabelle_extended <- act_all_data %>%
  select(id, pta_4000, age, wm_lab_pre,wm_lab_post, wm_lab_delta,
         fa_lab_pre, fa_lab_post, fa_lab_delta, hearing_aid, olsa_lab_growth,
         sin_lab_nonoise_pre, sin_lab_lownoise_pre, sin_lab_highnoise_pre,
         sin_lab_nonoise_post, sin_lab_lownoise_post, sin_lab_highnoise_post, sin_lab_delta,
         sin_lab_growth) %>% 
  distinct(id, .keep_all = TRUE ) %>%
  mutate(id = tolower(id)) %>%
  rename(
    pta = pta_4000,
    wm_pre = wm_lab_pre, 
    wm_post = wm_lab_post,
    wm_delta = wm_lab_delta,
    sa_pre = fa_lab_pre,
    sa_post = fa_lab_post,
    sa_delta = fa_lab_delta,
    olsa_learner = olsa_lab_growth,
    sin_learner = sin_lab_growth
  )

# Baseline group comparison
baseline_check_df <- act_all_data %>%
  mutate(id_lower = tolower(id)) %>%
  mutate(
    group = case_when(
      id_lower %in% EG ~ "EG",
      id_lower %in% CG ~ "CG",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group)) %>%
  distinct(id, group, .keep_all = TRUE)

# Age
shapiro.test(baseline_check_df$age[baseline_check_df$group == "EG"]) # p-value = 0.1367
shapiro.test(baseline_check_df$age[baseline_check_df$group == "CG"]) # p-value = 0.4754
# PTA
shapiro.test(baseline_check_df$pta_4000[baseline_check_df$group == "EG"]) # p-value = 0.4904
shapiro.test(baseline_check_df$pta_4000[baseline_check_df$group == "CG"]) # p-value = 0.08583
# WM
shapiro.test(baseline_check_df$wm_lab_pre[baseline_check_df$group == "EG"]) # p-value = 2.247e-05
shapiro.test(baseline_check_df$wm_lab_pre[baseline_check_df$group == "CG"]) # p-value = 0.1347
# SA
shapiro.test(baseline_check_df$fa_lab_pre[baseline_check_df$group == "EG"]) # p-value = 0.05739
shapiro.test(baseline_check_df$fa_lab_pre[baseline_check_df$group == "CG"]) # p-value = 0.6918

# Age t-test
t.test(age ~ group, data = baseline_check_df) # p-value = 0.08853
# PTA t-Test
t.test(pta_4000 ~ group, data = baseline_check_df) # p-value = 0.3877
# SA t-Test
t.test(fa_lab_pre ~ group, data = baseline_check_df) # p-value = 0.85
# WM Wilcoxon-Test, da EG nicht normalverteilt
wilcox.test(wm_lab_pre ~ group, data = baseline_check_df) # p-value = 0.07651

# ---> Keine Baseline Unterschiede zwischen den Gruppen 

n100 <- n100 %>%
  mutate(
    group = case_when(
      substr(subject, 1, 6) %in% EG ~ "EG",
      substr(subject, 1, 6) %in% CG ~ "CG",
      TRUE ~ NA_character_ # Falls keine Übereinstimmung gefunden wird
    ),
    session = case_when(
      grepl("_t0", subject) ~ "T0",
      grepl("_t1", subject) ~ "T1",
      TRUE ~ NA_character_
    ))

p200 <- p200 %>%
  mutate(
    group = case_when(
      substr(subject, 1, 6) %in% EG ~ "EG",
      substr(subject, 1, 6) %in% CG ~ "CG",
      TRUE ~ NA_character_
    ),
    session = case_when(
      grepl("_t0", subject) ~ "T0",
      grepl("_t1", subject) ~ "T1",
      TRUE ~ NA_character_
    )
  )

p50 <- p50 %>%
  mutate(
    group = case_when(
      substr(subject, 1, 6) %in% EG ~ "EG",
      substr(subject, 1, 6) %in% CG ~ "CG",
      TRUE ~ NA_character_ # Falls keine Übereinstimmung gefunden wird
    ),
    session = case_when(
      grepl("_t0", subject) ~ "T0",
      grepl("_t1", subject) ~ "T1",
      TRUE ~ NA_character_
    ))

# Alle Zeilen mit NA bei Gruppe werden gelöscht, da ein Proband feherhaft ist oder so. Zumindest gehört der keiner Gruppe an, deswegen hier nochmal die Kontrolle 
n100 <- subset(n100, !is.na(group))
p200 <- subset(p200, !is.na(group))
p50 <- subset(p50, !is.na(group))

n100$subject <- substr(n100$subject, 1, 6)
p200$subject <- substr(p200$subject, 1, 6)
p50$subject <- substr(p50$subject, 1, 6)

# Outlier exclusion within group & session für Latenz und Amplituden
n100 <- n100 %>%
  group_by(group, session) %>%
  mutate(
    mean_lat = mean(latency_ms, na.rm = TRUE),
    sd_lat = sd(latency_ms, na.rm = TRUE)
  ) %>%
  filter(latency_ms >= (mean_lat - 2.5 * sd_lat) &
           latency_ms <= (mean_lat + 2.5 * sd_lat)) %>%
  ungroup()

n100 <- n100 %>%
  group_by(group, session) %>%
  mutate(
    mean_lat = mean(amp, na.rm = TRUE),
    sd_lat = sd(amp, na.rm = TRUE)
  ) %>%
  filter(amp >= (mean_lat - 2.5 * sd_lat) &
           amp <= (mean_lat + 2.5 * sd_lat)) %>%
  ungroup()

p200 <- p200 %>%
  group_by(group, session) %>%
  mutate(
    mean_lat = mean(latency_ms, na.rm = TRUE),
    sd_lat = sd(latency_ms, na.rm = TRUE)
  ) %>%
  filter(latency_ms >= (mean_lat - 2.5 * sd_lat) &
           latency_ms <= (mean_lat + 2.5 * sd_lat)) %>%
  ungroup()

p200 <- p200 %>%
  group_by(group, session) %>%
  mutate(
    mean_lat = mean(amp, na.rm = TRUE),
    sd_lat = sd(amp, na.rm = TRUE)
  ) %>%
  filter(amp >= (mean_lat - 2.5 * sd_lat) &
           amp <= (mean_lat + 2.5 * sd_lat)) %>%
  ungroup()

p50 <- p50 %>%
  group_by(group, session) %>%
  mutate(
    mean_lat = mean(latency_ms, na.rm = TRUE),
    sd_lat = sd(latency_ms, na.rm = TRUE)
  ) %>%
  filter(latency_ms >= (mean_lat - 2.5 * sd_lat) &
           latency_ms <= (mean_lat + 2.5 * sd_lat)) %>%
  ungroup()

p50 <- p50 %>%
  group_by(group, session) %>%
  mutate(
    mean_lat = mean(amp, na.rm = TRUE),
    sd_lat = sd(amp, na.rm = TRUE)
  ) %>%
  filter(amp >= (mean_lat - 2.5 * sd_lat) &
           amp <= (mean_lat + 2.5 * sd_lat)) %>%
  ungroup()

# ----Outlier auf Basis der Differenz zwischen T0 und T1 ermitteln ----
#
n100_latency <- n100 %>%
  group_by(subject) %>%
  mutate(delta_internal = {
    t0 <- latency_ms[session == "T0"]
    t1 <- latency_ms[session == "T1"]
    if(length(t0) == 1 && length(t1) == 1) { t1 - t0 } else { as.numeric(NA) }
  }) %>%
  group_by(group) %>%
  mutate(m_delta = mean(delta_internal, na.rm = TRUE), sd_delta = sd(delta_internal, na.rm = TRUE),
         is_outlier = ifelse(is.na(delta_internal), FALSE,
                             delta_internal < (m_delta - 2.5 * sd_delta) |
                               delta_internal > (m_delta + 2.5 * sd_delta))) %>%
  filter(is_outlier == FALSE) %>%
  select(-delta_internal, -m_delta, -sd_delta, -is_outlier) %>%
  ungroup()

n100_amp <- n100 %>%
  group_by(subject) %>%
  mutate(delta_internal = {
    t0 <- amp[session == "T0"]
    t1 <- amp[session == "T1"]
    if(length(t0) == 1 && length(t1) == 1) { t1 - t0 } else { as.numeric(NA) }
  }) %>%
  group_by(group) %>%
  mutate(m_delta = mean(delta_internal, na.rm = TRUE), sd_delta = sd(delta_internal, na.rm = TRUE),
         is_outlier = ifelse(is.na(delta_internal), FALSE,
                             delta_internal < (m_delta - 2.5 * sd_delta) |
                               delta_internal > (m_delta + 2.5 * sd_delta))) %>%
  filter(is_outlier == FALSE) %>%
  select(-delta_internal, -m_delta, -sd_delta, -is_outlier) %>%
  ungroup()

p200_latency <- p200 %>%
  group_by(subject) %>%
  mutate(delta_internal = {
    t0 <- latency_ms[session == "T0"]
    t1 <- latency_ms[session == "T1"]
    if(length(t0) == 1 && length(t1) == 1) { t1 - t0 } else { as.numeric(NA) }
  }) %>%
  group_by(group) %>%
  mutate(m_delta = mean(delta_internal, na.rm = TRUE), sd_delta = sd(delta_internal, na.rm = TRUE),
         is_outlier = ifelse(is.na(delta_internal), FALSE,
                             delta_internal < (m_delta - 2.5 * sd_delta) |
                               delta_internal > (m_delta + 2.5 * sd_delta))) %>%
  filter(is_outlier == FALSE) %>%
  select(-delta_internal, -m_delta, -sd_delta, -is_outlier) %>%
  ungroup()

p200_amp <- p200 %>%
  group_by(subject) %>%
  mutate(delta_internal = {
    t0 <- amp[session == "T0"]
    t1 <- amp[session == "T1"]
    if(length(t0) == 1 && length(t1) == 1) { t1 - t0 } else { as.numeric(NA) }
  }) %>%
  group_by(group) %>%
  mutate(m_delta = mean(delta_internal, na.rm = TRUE), sd_delta = sd(delta_internal, na.rm = TRUE),
         is_outlier = ifelse(is.na(delta_internal), FALSE,
                             delta_internal < (m_delta - 2.5 * sd_delta) |
                               delta_internal > (m_delta + 2.5 * sd_delta))) %>%
  filter(is_outlier == FALSE) %>%
  select(-delta_internal, -m_delta, -sd_delta, -is_outlier) %>%
  ungroup()

p50_latency <- p50 %>%
  group_by(subject) %>%
  mutate(delta_internal = {
    t0 <- latency_ms[session == "T0"]
    t1 <- latency_ms[session == "T1"]
    if(length(t0) == 1 && length(t1) == 1) { t1 - t0 } else { as.numeric(NA) }
  }) %>%
  group_by(group) %>%
  mutate(m_delta = mean(delta_internal, na.rm = TRUE), sd_delta = sd(delta_internal, na.rm = TRUE),
         is_outlier = ifelse(is.na(delta_internal), FALSE,
                             delta_internal < (m_delta - 2.5 * sd_delta) |
                               delta_internal > (m_delta + 2.5 * sd_delta))) %>%
  filter(is_outlier == FALSE) %>%
  select(-delta_internal, -m_delta, -sd_delta, -is_outlier) %>%
  ungroup()

p50_amp <- p50 %>%
  group_by(subject) %>%
  mutate(delta_internal = {
    t0 <- amp[session == "T0"]
    t1 <- amp[session == "T1"]
    if(length(t0) == 1 && length(t1) == 1) { t1 - t0 } else { as.numeric(NA) }
  }) %>%
  group_by(group) %>%
  mutate(m_delta = mean(delta_internal, na.rm = TRUE), sd_delta = sd(delta_internal, na.rm = TRUE),
         is_outlier = ifelse(is.na(delta_internal), FALSE,
                             delta_internal < (m_delta - 2.5 * sd_delta) |
                               delta_internal > (m_delta + 2.5 * sd_delta))) %>%
  filter(is_outlier == FALSE) %>%
  select(-delta_internal, -m_delta, -sd_delta, -is_outlier) %>%
  ungroup()

mean_ci95 <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  se <- sd(x) / sqrt(n)
  ci <- se * qt(0.975, df = n - 1)
  data.frame(
    y = mean(x),
    ymin = mean(x) - ci,
    ymax = mean(x) + ci
  )
}

# ---- Plots Latenzen & Amplituden
n100_latency %>%
  mutate(group = factor(group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = session, y = latency_ms, group = subject)) +
  geom_line(aes(color = group), alpha = 0.2, linewidth = 0.4) + 
  geom_point(aes(color = group), alpha = 0.2) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "line", linewidth = 1) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "point", size = 2.5) +
  stat_summary(aes(group = group, color = group), fun.data = mean_ci95, geom = "errorbar", width = 0.1, linewidth = 0.8) +
  facet_wrap(~ group) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
  geom_text(
    data = data.frame(
      group = factor(c("EG", "CG"), levels = c("EG", "CG")),
      label = c("b", ""), 
      session = 1,
      latency_ms = max(n100_latency$latency_ms, na.rm = TRUE)
    ),
    aes(x = session, y = latency_ms, label = label),
    fontface = "bold", size = 5, color = "black", inherit.aes = FALSE,
    hjust = 1.5, vjust = 1
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = NULL,
    x = "Session",
    y = "Latency (ms)"
  )

p200_latency %>%
  mutate(group = factor(group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = session, y = latency_ms, group = subject)) +
  geom_line(aes(color = group), alpha = 0.2, linewidth = 0.4) + 
  geom_point(aes(color = group), alpha = 0.2) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "line", linewidth = 1) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "point", size = 2.5) +
  stat_summary(aes(group = group, color = group), fun.data = mean_ci95, geom = "errorbar", width = 0.1, linewidth = 0.8) +
  facet_wrap(~ group) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
  geom_text(
    data = data.frame(
      group = factor("EG", levels = c("EG", "CG")),
      label = "b",
      session = 1,
      latency_ms = max(p200_latency$latency_ms, na.rm = TRUE)
    ),
    aes(x = session, y = latency_ms, label = label),
    fontface = "bold", size = 5, color = "black", inherit.aes = FALSE,
    hjust = 1.5, vjust = 1
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = NULL,
    x = "Session",
    y = "Latency (ms)"
  )

p50_latency %>%
  mutate(group = factor(group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = session, y = latency_ms, group = subject)) +
  geom_line(aes(color = group), alpha = 0.2, linewidth = 0.4) + 
  geom_point(aes(color = group), alpha = 0.2) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "line", linewidth = 1) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "point", size = 2.5) +
  stat_summary(aes(group = group, color = group), fun.data = mean_ci95, geom = "errorbar", width = 0.1, linewidth = 0.8) +
  facet_wrap(~ group) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
  geom_text(
    data = data.frame(
      group = factor("EG", levels = c("EG", "CG")),
      label = "b",
      session = 1,
      latency_ms = max(p50_latency$latency_ms, na.rm = TRUE)
    ),
    aes(x = session, y = latency_ms, label = label),
    fontface = "bold", size = 5, color = "black", inherit.aes = FALSE,
    hjust = 1.5, vjust = 1
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = NULL,
    x = "Session",
    y = "Latency (ms)"
  )

n100_amp %>%
  mutate(group = factor(group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = session, y = amp, group = subject)) +
  geom_line(aes(color = group), alpha = 0.2, linewidth = 0.4) + 
  geom_point(aes(color = group), alpha = 0.2) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "line", linewidth = 1) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "point", size = 2.5) +
  stat_summary(aes(group = group, color = group), fun.data = mean_ci95, geom = "errorbar", width = 0.1, linewidth = 0.8) +
  facet_wrap(~ group) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
  geom_text(
    data = data.frame(
      group = factor("EG", levels = c("EG", "CG")),
      label = "a",
      session = 1,
      amp = max(n100_amp$amp, na.rm = TRUE)
    ),
    aes(x = session, y = amp, label = label),
    fontface = "bold", size = 5, color = "black", inherit.aes = FALSE,
    hjust = 1.5, vjust = 1
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = NULL,
    x = "Session",
    y = expression(bold("Amplitude (" * mu * "V)"))
  )


p200_amp %>%
  mutate(group = factor(group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = session, y = amp, group = subject)) +
  geom_line(aes(color = group), alpha = 0.2, linewidth = 0.4) + 
  geom_point(aes(color = group), alpha = 0.2) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "line", linewidth = 1) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "point", size = 2.5) +
  stat_summary(aes(group = group, color = group), fun.data = mean_ci95, geom = "errorbar", width = 0.1, linewidth = 0.8) +
  facet_wrap(~ group) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
  geom_text(
    data = data.frame(
      group = factor("EG", levels = c("EG", "CG")),
      label = "a",
      session = 1,
      amp = max(p200_amp$amp, na.rm = TRUE)
    ),
    aes(x = session, y = amp, label = label),
    fontface = "bold", size = 5, color = "black", inherit.aes = FALSE,
    hjust = 1.5, vjust = 1
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = NULL,
    x = "Session",
    y = expression(bold("Amplitude (" * mu * "V)"))
  )

p50_amp %>%
  mutate(group = factor(group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = session, y = amp, group = subject)) +
  geom_line(aes(color = group), alpha = 0.2, linewidth = 0.4) + 
  geom_point(aes(color = group), alpha = 0.2) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "line", linewidth = 1) +
  stat_summary(aes(group = group, color = group), fun = mean, geom = "point", size = 2.5) +
  stat_summary(aes(group = group, color = group), fun.data = mean_ci95, geom = "errorbar", width = 0.1, linewidth = 0.8) +
  facet_wrap(~ group) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
  geom_text(
    data = data.frame(
      group = factor("EG", levels = c("EG", "CG")),
      label = "a",
      session = 1,
      amp = max(p50_amp$amp, na.rm = TRUE)
    ),
    aes(x = session, y = amp, label = label),
    fontface = "bold", size = 5, color = "black", inherit.aes = FALSE,
    hjust = 1.5, vjust = 1
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = NULL,
    x = "Session",
    y = expression(bold("Amplitude (" * mu * "V)"))
  )


# ---- Baseline Unterschiede EG und CG ausschließen ----
# Daten für T0 vorbereiten
n100_baseline <- subset(n100_amp, session == "T0")
p200_baseline <- subset(p200_amp, session == "T0")
p50_baseline <- subset(p50_amp, session == "T0")

aggregate(amp ~ group, data = n100_baseline,
          FUN = function(x) c(Mean = mean(x), SD = sd(x)))
aggregate(amp ~ group, data = p200_baseline,
          FUN = function(x) c(Mean = mean(x), SD = sd(x)))
aggregate(amp ~ group, data = p50_baseline,
          FUN = function(x) c(Mean = mean(x), SD = sd(x)))

n100_baseline %>% split(.$group) %>% map(~shapiro.test(.x$amp))
p200_baseline %>% split(.$group) %>% map(~shapiro.test(.x$amp))
p50_baseline %>% split(.$group) %>% map(~shapiro.test(.x$amp))

# t-Test für unabhängige Stichproben, da Normalverteilt
t_test_results_n1 <- t.test(amp ~ group, data = n100_baseline)
t_test_results_p2 <- t.test(amp ~ group, data = p200_baseline)
t_test_results_p5 <- t.test(amp ~ group, data = p50_baseline)

print(t_test_results_n1) # p-value = 0.1327
print(t_test_results_p2) # p-value = 0.2409
print(t_test_results_p5) # p-value = 0.9131
#Keine Baselineunterschiede zwischen den Gruppen

n100_post <- subset(n100_amp, session == "T0")
p200_post <- subset(p200_amp, session == "T0")
p50_post <- subset(p50_amp, session == "T0")

n100_post %>% split(.$group) %>% map(~shapiro.test(.x$amp))
p200_post %>% split(.$group) %>% map(~shapiro.test(.x$amp))
p50_post  %>% split(.$group) %>% map(~shapiro.test(.x$amp))

# Mittelwerte und SD für T1 berechnen
aggregate(amp ~ group, data = n100_post, FUN = function(x) c(Mean = mean(x), SD = sd(x)))
aggregate(amp ~ group, data = p200_post, FUN = function(x) c(Mean = mean(x), SD = sd(x)))
aggregate(amp ~ group, data = p50_post,  FUN = function(x) c(Mean = mean(x), SD = sd(x)))

n100_delta_df <- n100_amp %>% select(subject, group, session, amp, latency_ms) %>%
  pivot_wider(names_from = session, values_from = c(amp, latency_ms)) %>%
  mutate(amp_delta = amp_T1 - amp_T0, lat_delta = latency_ms_T1 - latency_ms_T0) %>%
  filter(!is.na(amp_delta) & !is.na(lat_delta))

p200_delta_df <- p200_amp %>% select(subject, group, session, amp, latency_ms) %>%
  pivot_wider(names_from = session, values_from = c(amp, latency_ms)) %>%
  mutate(amp_delta = amp_T1 - amp_T0, lat_delta = latency_ms_T1 - latency_ms_T0) %>%
  filter(!is.na(amp_delta) & !is.na(lat_delta))

p50_delta_df <- p50_amp %>% select(subject, group, session, amp, latency_ms) %>%
  pivot_wider(names_from = session, values_from = c(amp, latency_ms)) %>%
  mutate(amp_delta = amp_T1 - amp_T0, lat_delta = latency_ms_T1 - latency_ms_T0) %>%
  filter(!is.na(amp_delta) & !is.na(lat_delta))

message("--- Shapiro-Wilk für N100 Delta ---")
n100_delta_df %>% split(.$group) %>% map(~shapiro.test(.x$amp_delta)) #normal

message("--- Shapiro-Wilk für P200 Delta ---")
p200_delta_df %>% split(.$group) %>% map(~shapiro.test(.x$amp_delta)) # normal

message("--- Shapiro-Wilk für P50 Delta ---")
p50_delta_df %>% split(.$group) %>% map(~shapiro.test(.x$amp_delta)) # normal

t_test_delta_n1 <- t.test(amp_delta ~ group, data = n100_delta_df)
t_test_delta_p2 <- t.test(amp_delta ~ group, data = p200_delta_df)
t_test_delta_p5 <- t.test(amp_delta ~ group, data = p50_delta_df)

# Ergebnisse anzeigen
print(t_test_delta_n1)
print(t_test_delta_p2)
print(t_test_delta_p5)

#--> Da zu T1 keine signifikanten Unterschiede LMMs und den möglichen Einfluss von Covariablen zu checken 

info_tabelle <- act_all_data %>%
  select(id, pta_4000, age, hearing_aid) %>%
  distinct()
info_tabelle$id <- tolower(info_tabelle$id) 

# ERP Tabellen um age und PTA erweitern für LMM
n100_latency <- n100_latency %>%
  left_join(info_tabelle, by = c("subject" = "id")) %>%
  rename(pta = pta_4000)

n100_amp <- n100_amp %>%
  left_join(info_tabelle, by = c("subject" = "id")) %>%
  rename(pta = pta_4000)

p200_latency <- p200_latency %>%
  left_join(info_tabelle, by = c("subject" = "id")) %>%
  rename(pta = pta_4000)

p200_amp <- p200_amp %>%
  left_join(info_tabelle, by = c("subject" = "id")) %>%
  rename(pta = pta_4000)

p50_latency <- p50_latency %>%
  left_join(info_tabelle, by = c("subject" = "id")) %>%
  rename(pta = pta_4000)

p50_amp <- p50_amp %>%
  left_join(info_tabelle, by = c("subject" = "id")) %>%
  rename(pta = pta_4000)

# z-Transformation für AGE und PTA
z_transform <- function(df) {
  df %>% mutate(across(c(age, pta), ~ as.numeric(scale(.x))))
}

n100_latency <- z_transform(n100_latency)
n100_amp     <- z_transform(n100_amp)
p200_latency <- z_transform(p200_latency)
p200_amp     <- z_transform(p200_amp)
p50_latency <- z_transform(p50_latency)
p50_amp     <- z_transform(p50_amp)

# LMMs
lmm_n100_latency <- lmer(latency_ms ~ group * session *hearing_aid + age + pta + (1|subject),
                         data = n100_latency)
summary(lmm_n100_latency)

lmm_p200_latency <- lmer(latency_ms ~ group * session * hearing_aid + age + pta + (1|subject),
                         data = p200_latency)
summary(lmm_p200_latency)

lmm_p50_latency <- lmer(latency_ms ~ group * session * hearing_aid + age + pta + (1|subject),
                        data = p50_latency)
summary(lmm_p50_latency)

lmm_n100_amplitude <- lmer(amp ~ group * session * hearing_aid + age + pta +(1|subject),
                           data = n100_amp)
summary(lmm_n100_amplitude)

lmm_p200_amplitude <- lmer(amp ~ group * session * hearing_aid + age + pta +(1|subject),
                           data = p200_amp)
summary(lmm_p200_amplitude)

lmm_p50_amplitude <- lmer(amp ~ group * session * hearing_aid + age + pta +(1|subject),
                          data = p50_amp)
summary(lmm_p50_amplitude)

# Korr. SIN Score und Latenzveränderung

info_with_specific_deltas <- info_tabelle_extended %>%
  mutate(
    delta_nonoise  = sin_lab_nonoise_post - sin_lab_nonoise_pre,
    delta_lownoise = sin_lab_lownoise_post - sin_lab_lownoise_pre,
    delta_highnoise = sin_lab_highnoise_post - sin_lab_highnoise_pre
  )

n100_delta_df <- n100_amp %>%
  select(subject, group, session, amp, latency_ms) %>%
  pivot_wider(
    names_from = session, 
    values_from = c(amp, latency_ms),
    values_fn = mean
  ) %>%
  mutate(
    amp_delta = amp_T1 - amp_T0,
    lat_delta = latency_ms_T1 - latency_ms_T0
  ) %>%
  filter(!is.na(amp_delta) & !is.na(lat_delta))

n100_matched_specific <- n100_delta_df %>%
  rename(id = subject) %>%
  inner_join(info_with_specific_deltas, by = "id")

p200_matched_specific <- p200_delta_df %>%
  rename(id = subject) %>%
  inner_join(info_with_specific_deltas, by = "id")

p50_matched_specific <- p50_delta_df %>%
  rename(id = subject) %>%
  inner_join(info_with_specific_deltas, by = "id")

n100_matched_specific %>% split(.$group) %>% map(~shapiro.test(.x$lat_delta))

message("--- Korrelationen für die EXPERIMENTALGRUPPE (EG) ---")
eg_data <- filter(n100_matched_specific, group == "EG")
cor.test(eg_data$lat_delta, eg_data$delta_nonoise, method = "spearman", exact = FALSE)
cor.test(eg_data$lat_delta, eg_data$delta_lownoise, method = "spearman", exact = FALSE)
cor.test(eg_data$lat_delta, eg_data$delta_highnoise, method = "spearman", exact = FALSE)
sum(!is.na(eg_data$lat_delta & eg_data$delta_nonoise))

message("--- Korrelationen für die KONTROLLGRUPPE (CG) ---")
cg_data <- filter(n100_matched_specific, group == "CG")
cor.test(cg_data$lat_delta, cg_data$delta_nonoise, method = "spearman", exact = FALSE)
cor.test(cg_data$lat_delta, cg_data$delta_lownoise, method = "spearman", exact = FALSE)
cor.test(cg_data$lat_delta, cg_data$delta_highnoise, method = "spearman", exact = FALSE)


# Daten fürs Plotten ins Langformat bringen + Reihenfolge der Faktoren erzwingen
plot_data_n100 <- n100_matched_specific %>%
  select(id, group, lat_delta, delta_nonoise, delta_lownoise, delta_highnoise) %>%
  pivot_longer(
    cols = c(delta_nonoise, delta_lownoise, delta_highnoise),
    names_to = "noise_condition",
    values_to = "sin_delta"
  ) %>%
  mutate(
    noise_condition = case_when(
      noise_condition == "delta_nonoise" ~ "No Noise",
      noise_condition == "delta_lownoise" ~ "Low Noise",
      noise_condition == "delta_highnoise" ~ "High Noise"
    ),
    # Hier legst du fest: No Noise links, High Noise rechts
    noise_condition = factor(noise_condition, levels = c("No Noise", "Low Noise", "High Noise")),
    # Hier legst du fest: EG oben, CG unten (EG an erster Stelle = oben)
    group = factor(group, levels = c("EG", "CG"))
  )

# Panel-Buchstaben
panel_labels <- data.frame(
  group = factor(rep(c("EG", "CG"), each = 3), levels = c("EG", "CG")),
  noise_condition = factor(rep(c("No Noise", "Low Noise", "High Noise"), times = 2), levels = c("No Noise", "Low Noise", "High Noise")),
  label = c("a", "b", "c", "d", "e", "f"),
  lat_delta = -8,  
  sin_delta = 0.5    
)

ggplot(plot_data_n100, aes(x = lat_delta, y = sin_delta)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4, alpha = 0.6) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4, alpha = 0.6) + 
  geom_point(aes(color = group), size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray", alpha = 0.2) +
  facet_grid(group ~ noise_condition, scales = "fixed", switch = "y") + 
  scale_y_continuous(
    limits = c(-0.6, 0.6), 
    breaks = seq(-0.6, 0.6, by = 0.2),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
  geom_text(
    data = panel_labels, 
    aes(x = lat_delta, y = sin_delta, label = label), 
    fontface = "bold", size = 5, color = "black", inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    strip.text = element_text(face = "bold", size = 12),
    strip.text.y.left = element_text(angle = 0), 
    strip.placement = "outside",               
    strip.background = element_blank()         
  ) +
  labs(
    title = NULL,
    x = expression(bold(Delta ~ "N100 Latency (ms)")),
    y = expression(bold(Delta ~ "SIN Score"))
  )

# Einlesen der ganzen ERP Verläufe um einen Wellenförmigen GA für die MA zu plotten
ERP_pfad <- '/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/ERPs/erp verlaufe'

file_list <- list.files(path = ERP_pfad, pattern = "\\.dat$", full.names = TRUE)

read_erp_files <- function(filename) {
  amplitudes <- read.table(filename, header = FALSE)$V1
  time_axis <- seq(-100, 400, length.out = 256)
  baseline_mean <- mean(amplitudes[time_axis < 0])
  amplitudes_corrected <- amplitudes - baseline_mean
  base_name <- basename(filename)
  clean_name <- gsub("\\.dat$", "", base_name)
  parts <- strsplit(clean_name, "_")[[1]]
  subject_id <- tolower(parts[1]) 
  timepoint_id <- toupper(parts[3]) 
  
  group_label <- case_when(
    subject_id %in% EG ~ "EG",
    subject_id %in% CG ~ "CG",
    TRUE ~ "Aussortiert"
  )
  
  data.frame(Time = time_axis, Amplitude = amplitudes_corrected, 
             Subject = subject_id, Timepoint = timepoint_id, Group = group_label)
}

all_erp_data <- map_df(file_list, read_erp_files) %>% filter(Group != "Aussortiert")

valid_subjects <- n100_latency$subject %>% 
  intersect(n100_amp$subject) %>% 
  intersect(p200_latency$subject) %>% 
  intersect(p200_amp$subject) %>% 
  intersect(p50_amp$subject) %>% 
  intersect(p50_latency$subject)


all_erp_data <- map_df(file_list, read_erp_files) %>%
  filter(Group != "Aussortiert") %>%            
  filter(Subject %in% valid_subjects)          

# Check
print("Gefundene Kombinationen:")
print(table(all_erp_data$Group, all_erp_data$Timepoint))


all_erp_data %>% 
  filter(Timepoint == "T0") %>% 
  mutate(Group = factor(Group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = Time, y = Amplitude, color = Group, fill = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
  scale_y_continuous(breaks = seq(-4, 4, 1)) + 
  coord_cartesian(ylim = c(-4, 4)) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281"), breaks = c("EG", "CG"), name = "Group") +
  scale_fill_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281"), breaks = c("EG", "CG"), name = "Group") +
  annotate("text", x = min(all_erp_data$Time, na.rm = TRUE), y = 3.6, label = "a", fontface = "bold", size = 5, hjust = -0.2, vjust = 0) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = NULL,
    x = "Time (ms)",
    y = expression(bold("Amplitude (" * mu * "V)"))
  )

all_erp_data %>% 
  filter(Timepoint == "T1") %>% 
  mutate(Group = factor(Group, levels = c("EG", "CG"))) %>%
  ggplot(aes(x = Time, y = Amplitude, color = Group, fill = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
  scale_y_continuous(breaks = seq(-4, 4, 1)) + 
  coord_cartesian(ylim = c(-4, 4)) +
  scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281"), breaks = c("EG", "CG"), name = "Group") +
  scale_fill_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281"), breaks = c("EG", "CG"), name = "Group") +
  annotate("text", x = min(all_erp_data$Time, na.rm = TRUE), y = 3.6, label = "b", fontface = "bold", size = 5, hjust = -0.2, vjust = 0) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = NULL,
    x = "Time (ms)",
    y = expression(bold("Amplitude (" * mu * "V)"))
  )
