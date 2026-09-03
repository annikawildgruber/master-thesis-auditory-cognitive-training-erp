# ERP Analyse für die N1-P2 peak to peak Amplitude

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(factoextra) 
library(MKinfer)
library(ggplot2)
library(ggdist)
library(gghalves)

EG <- c("vp02ha", "vp02hl", "vp03hl", "vp04ha", "vp04hl", "vp05ha", "vp06ha", "vp07ha", "vp08ha", "vp09ha", "vp09hl",
        "vp10ha", "vp10hl", "vp11ha", "vp11hl", "vp13ha", "vp14hl", "vp15hl","vp16hl", "vp17hl", "vp18ha", "vp19ha", 
        "vp20hl", "vp21ha", "vp21hl", "vp22ha", "vp22hl", "vp23ha", "vp24ha", "vp32hl", "vp35hl", "vp39hl", "vp41hl", 
        "vp42hl")
CG <- c("vp15ha", "vp16ha", "vp25hl", "vp26hl", "vp27hl", "vp30hl", "vp31hl", "vp37hl", "vp38ha", "vp40hl", "vp42ha", 
        "vp43hl", "vp44hl", "vp45hl", "vp46hl", "vp47hl", "vp48hl", "vp49hl", "vp50ha", "vp53hl")

path_n1  <- '/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/Peaks_all_N100.txt'
path_p2  <- '/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/Peaks_all_P200.txt'
path_p50 <- '/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/Peaks_all_P50.txt'
act_all_data <- read_excel('/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/lab_grand_summary.xlsx')


info_tabelle_extended <- act_all_data %>%
  select(id, pta_4000, age, wm_lab_pre, wm_lab_post, wm_lab_delta,
         fa_lab_pre, fa_lab_post, fa_lab_delta, hearing_aid, olsa_lab_growth,
         sin_lab_nonoise_pre, sin_lab_lownoise_pre, sin_lab_highnoise_pre,
         sin_lab_nonoise_post, sin_lab_lownoise_post, sin_lab_highnoise_post, sin_lab_delta,
         sin_lab_growth) %>% 
  distinct(id, .keep_all = TRUE) %>%
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

load_erp_raw <- function(path, comp) {
  raw <- read_table(path, skip = 3, col_names = FALSE)
  header <- readLines(path, n = 3)[3] %>% str_trim() %>% 
    str_replace_all("-Peak Detection-L", "_Latency") %>%
    str_replace_all("-Peak Detection-V", "_Amplitude") %>%
    str_split("\\s+") %>% unlist()
  colnames(raw) <- header
  
  raw %>% rename(subject = File) %>%
    pivot_longer(cols = -subject, names_to = c("Electrode", ".value"), names_sep = "_") %>%
    mutate(
      Component = comp, 
      VP_ID = tolower(str_split_i(subject, "_", 1)) %>% str_remove("\\.dat"),
      Session = toupper(str_split_i(subject, "_", 3)) %>% str_remove("\\.dat")
    )
}

all_comps <- bind_rows(load_erp_raw(path_n1, "N100"), 
                       load_erp_raw(path_p2, "P200"),
                       load_erp_raw(path_p50, "P50")) %>%
  mutate(Group = case_when(VP_ID %in% tolower(EG) ~ "EG", VP_ID %in% tolower(CG) ~ "CG", TRUE ~ "Aussortiert")) %>%
  filter(Group != "Aussortiert")

# Elektroden-Grid definieren
cluster_grid <- all_comps %>%
  filter(Electrode %in% c("F3", "Fz", "F4", "C3", "Cz", "C4", "P3", "Pz", "P4")) %>%
         mutate(
           Anteriority = factor(case_when(
             Electrode %in% c("F3", "Fz", "F4") ~ "frontal",
             Electrode %in% c("C3", "Cz", "C4") ~ "central",
             Electrode %in% c("P3", "Pz", "P4") ~ "parietal"
           ), levels = c("frontal", "central", "parietal")),
           Laterality = factor(case_when(
             Electrode %in% c("F3", "C3", "P3") ~ "left",
             Electrode %in% c("Fz", "Cz", "Pz") ~ "midline",
             Electrode %in% c("F4", "C4", "P4") ~ "right"
           ), levels = c("left", "midline", "right"))
         )
         
         cluster_grid <- cluster_grid %>%
           mutate(subject_clean = str_split_i(subject, "_", 1)) %>%
           left_join(info_tabelle_extended, by = c("subject_clean" = "id")) %>% 
           mutate(
             age = as.numeric(scale(age)), 
             pta = as.numeric(scale(pta))
           )
         clean_and_pair_data <- function(df) {
           possible_vars <- c("VP_ID", "Component", "Anteriority", "Laterality")
           group_vars <- intersect(names(df), possible_vars)
           outlier_vars <- intersect(names(df), c("Group", "Session", "Component", "Anteriority", "Laterality"))
           df %>%
             group_by(across(all_of(outlier_vars))) %>%
             mutate(
               Amplitude = ifelse(abs(Amplitude - mean(Amplitude, na.rm=T)) > 2.5 * sd(Amplitude, na.rm=T), NA, Amplitude),
               Latency = ifelse(abs(Latency - mean(Latency, na.rm=T)) > 2.5 * sd(Latency, na.rm=T), NA, Latency)
             ) %>%
             drop_na(Amplitude, Latency) %>% 
             ungroup() %>%
             group_by(across(all_of(group_vars))) %>%
             filter(n() == 2) %>% 
             ungroup()
         }
         
         # Die drei Cluster testen
         data_central_roi <- cluster_grid %>%
           filter(Electrode %in% c("C3", "Cz", "C4")) %>%
           group_by(VP_ID, Session, Group, Component, age, pta, sin_learner) %>%
           summarise(Amplitude = mean(Amplitude), Latency = mean(Latency), .groups = "drop") %>%
           clean_and_pair_data()
         
         cluster_anterior <- cluster_grid %>%
           group_by(VP_ID, Session, Group, Anteriority, age, pta, Component) %>%
           summarise(Amplitude = mean(Amplitude, na.rm = TRUE), Latency = mean(Latency, na.rm = TRUE), .groups = "drop") %>%
           clean_and_pair_data()
         
         cluster_lateral <- cluster_grid %>%
           group_by(VP_ID, Session, Group, Laterality, age, pta, Component) %>%
           summarise(Amplitude = mean(Amplitude, na.rm = TRUE), Latency = mean(Latency, na.rm = TRUE), .groups = "drop") %>%
           clean_and_pair_data()
         
         data_n1p2_evolution <- cluster_grid %>%
           filter(Electrode %in% c("C3", "Cz", "C4")) %>%
           filter(Component %in% c("N100", "P200")) %>%
           group_by(VP_ID, Session, Group, Component, age, pta) %>%
           summarise(Amplitude = mean(Amplitude, na.rm = TRUE), .groups = "drop") %>%
           pivot_wider(names_from = Component, values_from = Amplitude, names_prefix = "Amp_") %>%
           mutate(N1P2_Distanz = Amp_P200 - Amp_N100) %>%
           left_join(info_tabelle_extended %>% select(id, wm_pre, wm_post, wm_delta, sa_pre, sa_post, sa_delta), 
                     by = c("VP_ID" = "id")) %>%
           mutate(
             wm = case_when(Session == "T0" ~ wm_pre, Session == "T1" ~ wm_post),
             sa = case_when(Session == "T0" ~ sa_pre, Session == "T1" ~ sa_post)
           ) 
         
         # N1P2 Differenzen berechnen +Outlier filtern
         n1p2_change <- data_n1p2_evolution %>%
           select(VP_ID, Group, Session, N1P2_Distanz, wm_pre, wm_delta, sa_pre, sa_delta, age, pta) %>%
           pivot_wider(names_from = Session, values_from = N1P2_Distanz) %>%
           mutate(diff_N1P2 = T1 - T0) %>%
           mutate(diff_N1P2 = ifelse(abs(diff_N1P2 - mean(diff_N1P2, na.rm=T)) > 2.5 * sd(diff_N1P2, na.rm=T), NA, diff_N1P2)) %>%
           drop_na(diff_N1P2)
         
         n1p2_deltas <- data_n1p2_evolution %>%
           select(VP_ID, Session, N1P2_Distanz, Group) %>%
           pivot_wider(names_from = Session, values_from = N1P2_Distanz) %>%
           mutate(n1p2_delta = T1 - T0) %>%
           select(VP_ID, n1p2_delta, Group) %>%
           mutate(VP_ID = tolower(VP_ID))
         
         # Ausgabe zum checken
         data_n1p2_summary_clean <- n1p2_change %>%
           group_by(Group) %>%
           summarise(
             Mean_T0 = mean(T0, na.rm = TRUE),
             SD_T0   = sd(T0, na.rm = TRUE),
             Mean_T1 = mean(T1, na.rm = TRUE),
             SD_T1   = sd(T1, na.rm = TRUE),
             Mean_Diff = mean(diff_N1P2, na.rm = TRUE),
             N       = n()
           )
         print(data_n1p2_summary_clean)
         
         # Baseline-Unterschiede (T0) prüfen
         eeg_baseline <- data_n1p2_evolution %>% filter(Session == "T0")
         shapiro.test(eeg_baseline$N1P2_Distanz[eeg_baseline$Group == "CG"])
         shapiro.test(eeg_baseline$N1P2_Distanz[eeg_baseline$Group == "EG"])
         print(wilcox.test(N1P2_Distanz ~ Group, data = eeg_baseline))
         
         data_n1p2_pre <- data_n1p2_evolution %>% filter(Session == "T0")
         tapply(data_n1p2_pre$wm_pre, data_n1p2_pre$Group, shapiro.test)
         wilcox.test(wm_pre ~ Group, data = data_n1p2_pre)
         tapply(data_n1p2_pre$sa_pre, data_n1p2_pre$Group, shapiro.test) 
         t.test(sa_pre ~ Group, data = data_n1p2_pre)
         tapply(data_n1p2_pre$age, data_n1p2_pre$Group, shapiro.test) 
         t.test(age ~ Group, data = data_n1p2_pre)
         tapply(data_n1p2_pre$pta, data_n1p2_pre$Group, shapiro.test) 
         t.test(pta ~ Group, data = data_n1p2_pre)
         
         calc_region_diff <- function(df, region_name) {
           df %>%
             filter(Component %in% c("N100", "P200")) %>%
             group_by(VP_ID, Group, Session, Component) %>%
             summarise(Amp = mean(Amplitude, na.rm = TRUE), .groups = "drop") %>%
             pivot_wider(names_from = Component, values_from = Amp) %>%
             mutate(N1P2 = P200 - N100) %>%
             select(VP_ID, Group, Session, N1P2) %>%
             pivot_wider(names_from = Session, values_from = N1P2) %>%
             mutate(diff = T1 - T0, Region = region_name)
         }
         all_regions_diff <- bind_rows(
           calc_region_diff(cluster_anterior, "Anterior"),
           calc_region_diff(data_central_roi, "Central"),
           calc_region_diff(cluster_lateral, "Lateral")
         )
         all_regions_diff %>%
           group_by(Region, Group) %>%
           summarise(Mean_Change = mean(diff, na.rm = TRUE), .groups = "drop")
         
         # CLUSTER 
         full_data_complete <- n1p2_change %>%
           select(VP_ID, Group) %>% 
           inner_join(info_tabelle_extended, by = c("VP_ID" = "id")) %>%
           select(VP_ID, Group, age, pta, sa_pre, wm_pre, sin_learner, sin_lab_delta,
                  sin_lab_nonoise_pre, sin_lab_nonoise_post, sin_lab_lownoise_pre,
                  sin_lab_lownoise_post, sin_lab_highnoise_pre, sin_lab_highnoise_post) %>%
           drop_na()
         
         full_data_complete <- full_data_complete %>%
           mutate(
             age_z = as.numeric(scale(age)),
             pta_z = as.numeric(scale(pta)),
             sin_learner = ifelse(sin_learner == "Learner", 1, 0)
           )
         
         cluster_features <- full_data_complete %>% select(age_z, pta_z, sa_pre, wm_pre)
         set.seed(123) 
         kmeans_global <- kmeans(cluster_features, centers = 2, nstart = 25)
         full_data_complete$Cluster <- as.factor(kmeans_global$cluster)

         fviz_cluster(kmeans_global, data = cluster_features, geom = c("point", "text"),
                      ellipse.type = "convex", ggtheme = theme_minimal(), main = "Globale Clusterstruktur")
         
         cluster_profile <- full_data_complete %>%
           group_by(Cluster) %>%
           summarise(Anzahl_VPs = n(), Alter_Mittelwert = mean(age), PTA_Mittelwert = mean(pta),
                     Aufmerksamkeit_Mittelwert = mean(sa_pre), Arbeitsgedaechtnis_Mittelwert = mean(wm_pre))
         print(cluster_profile)
         
         tapply(full_data_complete$age, full_data_complete$Cluster, shapiro.test)
         t.test(age ~ Cluster, data = full_data_complete)
         tapply(full_data_complete$pta, full_data_complete$Cluster, shapiro.test)
         t.test(pta ~ Cluster, data = full_data_complete)
         tapply(full_data_complete$sa_pre, full_data_complete$Cluster, shapiro.test)
         wilcox.test(sa_pre ~ Cluster, data = full_data_complete)
         tapply(full_data_complete$wm_pre, full_data_complete$Cluster, shapiro.test)
         wilcox.test(wm_pre ~ Cluster, data = full_data_complete)
         

         # Bootstrapped t-test für Gruppenunterschiede in N1P2 amplitude
         print(boot.t.test(diff_N1P2 ~ Group, data = n1p2_change, R = 999))
         
         # LMMs für cognition
         model_n1p2_wm <- lmer(N1P2_Distanz ~ Group * Session * wm + age + pta + (1 | VP_ID), data = data_n1p2_evolution)
         summary(model_n1p2_wm)
         
         model_n1p2_sa <- lmer(N1P2_Distanz ~ Group * Session * sa + age + pta + (1 | VP_ID), data = data_n1p2_evolution)
         summary(model_n1p2_sa)
         
         # LMMs für SIN Performance (N1P2 Prädiktor für SIN performance?)
         sin_long <- info_tabelle_extended %>%
           rename(VP_ID = id) %>%
           pivot_longer(cols = starts_with("sin_lab_"), names_to = c("noise_level", "session"),
                        names_pattern = "sin_lab_(.*)_(pre|post)", values_to = "sin_score") %>%
           mutate(
             session = case_when(session == "pre" ~ "T0", session == "post" ~ "T1", TRUE ~ session),
             age = as.numeric(scale(age)),
             pta = as.numeric(scale(pta))
           ) %>%
           left_join(data_n1p2_evolution %>% select(VP_ID, Session, N1P2_Distanz, Group) %>% rename(session = Session), 
                     by = c("VP_ID", "session"))
         
         sin_long$noise_level <- relevel(factor(sin_long$noise_level), ref = "nonoise")
         
         model_n1p2_sin <- lmer(sin_score ~ N1P2_Distanz * noise_level + pta + Group + (1 | VP_ID), data = sin_long)
         summary(model_n1p2_sin)
         
         model_t0 <- lmer(sin_score ~ N1P2_Distanz * noise_level + pta + age + (1|VP_ID), data = subset(sin_long, session == "T0"))
         summary(model_t0)
         
         model_t1_all <- lmer(sin_score ~ N1P2_Distanz * noise_level + pta + age + (1 | VP_ID), data = subset(sin_long, session == "T1"))
         summary(model_t1_all)
         
         sin_long_complete <- full_data_complete %>%
           pivot_longer(cols = c(sin_lab_nonoise_pre, sin_lab_nonoise_post, sin_lab_lownoise_pre,
                                 sin_lab_lownoise_post, sin_lab_highnoise_pre, sin_lab_highnoise_post),
                        names_to = "Measurement", values_to = "SIN_Score") %>%
           mutate(
             Condition = case_when(str_detect(Measurement, "nonoise") ~ "nonoise",
                                   str_detect(Measurement, "lownoise") ~ "lownoise",
                                   str_detect(Measurement, "highnoise") ~ "highnoise"),
             Time = case_when(str_detect(Measurement, "pre") ~ "pre", str_detect(Measurement, "post") ~ "post"),
             Condition = factor(Condition, levels = c("nonoise", "lownoise", "highnoise")),
             Time = factor(Time, levels = c("pre", "post"))
           )
         
         sin_long_complete$Condition <- relevel(factor(sin_long_complete$Condition), ref = "lownoise")
         lmm_prediction_model <- lmer(SIN_Score ~ Condition * Cluster * Time + (1 | VP_ID), data = sin_long_complete)
         summary(lmm_prediction_model)
         
         # Correlations
         eg_analysis <- n1p2_change %>% filter(Group == "EG")
         shapiro.test(eg_analysis$diff_N1P2)
         shapiro.test(eg_analysis$wm_delta)
         shapiro.test(eg_analysis$sa_delta)
         
         print(cor.test(eg_analysis$diff_N1P2, eg_analysis$wm_delta, method = "spearman", exact = FALSE))
         print(cor.test(eg_analysis$diff_N1P2, eg_analysis$sa_delta, method = "pearson", exact = FALSE))
         
         cg_analysis <- n1p2_change %>% filter(Group == "CG")
         shapiro.test(cg_analysis$diff_N1P2)
         shapiro.test(cg_analysis$wm_delta)
         shapiro.test(cg_analysis$sa_delta)
         
         print(cor.test(cg_analysis$diff_N1P2, cg_analysis$wm_delta, method = "pearson", exact = FALSE))
         print(cor.test(cg_analysis$diff_N1P2, cg_analysis$sa_delta, method = "pearson", exact = FALSE))
         
         sin_deltas_split <- sin_long %>%
           select(VP_ID, Group, noise_level, session, sin_score) %>%
           pivot_wider(names_from = session, values_from = sin_score, names_prefix = "sin_") %>%
           mutate(sin_delta = sin_T1 - sin_T0)
         
         correlation_split <- left_join(n1p2_deltas, sin_deltas_split, by = c("VP_ID", "Group"))
         
         for(g in c("EG", "CG")) {
           for(nl in c("nonoise", "lownoise", "highnoise")) {
             sub_dat <- correlation_split %>% filter(Group == g & noise_level == nl)
             cat("\n--- Test für Gruppe:", g, "| Noise:", nl, "---\n")
             print(shapiro.test(sub_dat$sin_delta))
             print(cor.test(sub_dat$n1p2_delta, sub_dat$sin_delta, method = "pearson"))
           }
         }
         
         # plots
 
         ggplot(n1p2_change, aes(x = Group, y = diff_N1P2, color = Group, fill = Group)) +
           geom_half_violin(
             data = subset(n1p2_change, Group == "EG"),
             side = "l", position = position_nudge(x = -0.15), 
             alpha = 0.4, trim = TRUE, color = NA
           ) +
           geom_half_violin(
             data = subset(n1p2_change, Group == "CG"),
             side = "r", position = position_nudge(x = 0.15), 
             alpha = 0.4, trim = TRUE, color = NA
           ) +
           geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5, linewidth = 0.7) +
           geom_point(position = position_jitter(width = 0.08, seed = 1), size = 2.2, alpha = 0.6) +
           geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
           scale_x_discrete(limits = c("EG", "CG")) +
           scale_y_continuous(breaks = seq(-7, 7, by = 1)) +
           scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
           scale_fill_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
           theme_minimal() +
           theme(
             legend.position = "none",
             axis.title = element_text(face = "bold", size = 12),
             axis.text = element_text(size = 11)
           ) +
           labs(
             title = NULL,
             x = "Group",
             y = expression(bold(Delta ~ "N1-P2 Amplitude (" * mu * "V)"))
           )
         
         # Cluster Visualisierung 
         plot_cluster_data <- full_data_complete %>%
           left_join(n1p2_change %>% select(VP_ID, diff_N1P2), by = "VP_ID") %>%
           mutate(Cluster_Label = case_when(
             Cluster == "1" ~ "Cluster 1",
             Cluster == "2" ~ "Cluster 2"
           ))
         
         set.seed(42)
        
         ggplot(plot_cluster_data, aes(x = Group, y = diff_N1P2)) +
           # 1. CG-Gruppe: Beide Wolken (Cluster 1 & 2) rechts vom CG-Boxplot
           geom_half_violin(
             data = subset(plot_cluster_data, Group == "CG"),
             aes(fill = Cluster_Label),
             side = "r", 
             position = position_nudge(x = 0.12), 
             alpha = 0.35, 
             trim = TRUE, 
             color = NA
           ) +
           
           geom_half_violin(
             data = subset(plot_cluster_data, Group == "EG"),
             aes(fill = Cluster_Label),
             side = "l", 
             position = position_nudge(x = -0.12), 
             alpha = 0.35, 
             trim = TRUE, 
             color = NA
           ) +
           geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
           geom_boxplot(outlier.shape = NA, fill = "gray94", color = "gray50", width = 0.15, linewidth = 0.7) +
           geom_point(
             aes(color = Cluster_Label, shape = Cluster_Label),
             position = position_jitter(width = 0.18, height = 0, seed = 42),
             size = 3, 
             alpha = 0.85
           ) +
           scale_color_manual(
             name = "Cluster",
             values = c("Cluster 1" = "#1F4E78", "Cluster 2" = "#70AD47")
           ) +
           scale_fill_manual(
             name = "Cluster",
             values = c("Cluster 1" = "#1F4E78", "Cluster 2" = "#70AD47")
           ) +
           scale_shape_manual(
             name = "Cluster",
             values = c("Cluster 1" = 16, "Cluster 2" = 17)
           ) +
           scale_x_discrete(limits = c("EG", "CG")) +
           scale_y_continuous(breaks = seq(-7, 7, by = 1)) +
           theme_minimal() +
           theme(
             axis.title = element_text(face = "bold", size = 12),
             axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
             axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
             axis.text = element_text(size = 11),
             legend.position = "right",
             legend.title = element_text(face = "bold", size = 11),
             legend.text = element_text(size = 10)
           ) +
           labs(
             title = NULL,
             x = "Group",
             y = expression(bold(Delta ~ "N1-P2 Amplitude (" * mu * "V)"))
           )
        
         plot_data_combined <- correlation_split %>%  
           mutate(
             sin_delta_pct = sin_delta * 100,
             Noise_Label = factor(
               case_when(
                 noise_level == "nonoise"   ~ "No Noise",
                 noise_level == "lownoise"  ~ "Low Noise",
                 noise_level == "highnoise" ~ "High Noise"
               ),
               levels = c("No Noise", "Low Noise", "High Noise")
             ),
             Group_Label = factor(Group, levels = c("EG", "CG"))
           )
         
         # Panel-Labels
         panel_labels <- data.frame(
           Group_Label = factor(rep(c("EG", "CG"), each = 3), levels = c("EG", "CG")),
           Noise_Label = factor(rep(c("No Noise", "Low Noise", "High Noise"), times = 2), levels = c("No Noise", "Low Noise", "High Noise")),
           label = c("a", "b", "c", "d", "e", "f"),
           n1p2_delta = min(plot_data_combined$n1p2_delta, na.rm = TRUE), # Passt sich dynamisch dem linken Rand an
           sin_delta = 0.5    
         )
         
         ggplot(plot_data_combined, aes(x = n1p2_delta, y = sin_delta)) +
           geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4, alpha = 0.6) + 
           geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4, alpha = 0.6) + 
           geom_point(aes(color = Group), size = 2.5, alpha = 0.7) +
           geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray", alpha = 0.2, linewidth = 0.8) +
           facet_grid(Group_Label ~ Noise_Label, scales = "free_y", switch = "y") +
           scale_y_continuous(
             limits = c(-0.6, 0.6), 
             breaks = seq(-0.6, 0.6, by = 0.2),
             labels = function(x) sprintf("%.1f", x)
           ) +
           scale_color_manual(values = c("EG" = "#C84B00", "CG" = "#4D4281")) +
           geom_text(
             data = panel_labels, 
             aes(x = n1p2_delta, y = sin_delta, label = label), 
             fontface = "bold", size = 5, color = "black", inherit.aes = FALSE
           ) +
           theme_minimal() +
           theme(
             legend.position = "none",
             plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
             axis.title = element_text(face = "bold", size = 12),
             axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
             axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
             axis.text = element_text(size = 10),
             strip.text = element_text(face = "bold", size = 12),
             strip.text.y.left = element_text(angle = 0),
             strip.placement = "outside",
             strip.background = element_blank()
           ) +
           labs(
             title = NULL,
             x = expression(bold(Delta ~ "N1-P2 Amplitude (" * mu * "V)")),
             y = expression(bold(Delta ~ "SIN Score"))
           )
         
         # Subgroup analyse
         age_cutoff <- n1p2_change %>% filter(Group == "EG") %>% pull(age) %>% quantile(0.25, na.rm = TRUE)
         wm_cutoff  <- n1p2_change %>% filter(Group == "EG") %>% pull(wm_pre) %>% quantile(0.75, na.rm = TRUE)
         sa_cutoff  <- n1p2_change %>% filter(Group == "EG") %>% pull(sa_pre) %>% quantile(0.75, na.rm = TRUE)
         pta_cutoff_best <- n1p2_change %>% filter(Group == "EG") %>% pull(pta) %>% quantile(0.25, na.rm = TRUE)
         
         n1p2_change <- n1p2_change %>%
           mutate(
             highlight_age = case_when(
               is.na(age) ~ NA_character_,
               Group == "EG" & age <= age_cutoff ~ "Youngest Quartile",
               Group == "EG" & age > age_cutoff  ~ "Remaining EG",
               TRUE ~ "Control Group"
             ),
             highlight_wm  = case_when(
               is.na(wm_pre) ~ NA_character_,
               Group == "EG" & wm_pre >= wm_cutoff ~ "Highest WM Quartile",
               Group == "EG" & wm_pre < wm_cutoff  ~ "Remaining EG",
               TRUE ~ "Control Group"
             ),
             highlight_sa  = case_when(
               is.na(sa_pre) ~ NA_character_, # <--- Fängt das NA sauber ab!
               Group == "EG" & sa_pre >= sa_cutoff ~ "Highest SA Quartile",
               Group == "EG" & sa_pre < sa_cutoff  ~ "Remaining EG",
               TRUE ~ "Control Group"
             ),
             highlight_pta = case_when(
               is.na(pta) ~ NA_character_,
               Group == "EG" & pta <= pta_cutoff_best ~ "Best PTA Quartile",
               Group == "EG" & pta > pta_cutoff_best  ~ "Remaining EG",
               TRUE ~ "Control Group"
             )
           )
         
         plot_settings <- list(
           list(col = "highlight_age", file = "highlight_age.png", label = "Youngest Quartile", panel_label = "a"),
           list(col = "highlight_wm",  file = "highlight_wm.png",  label = "Highest WM Quartile", panel_label = "d"),
           list(col = "highlight_sa",  file = "highlight_sa.png",  label = "Highest SA Quartile", panel_label = "c"),
           list(col = "highlight_pta", file = "highlight_pta.png", label = "Best PTA Quartile", panel_label = "b")
         )
         
         
         
         for (setting in plot_settings) {
           
           # Farben: Highlight (Grün), Rest EG (Orange), CG (Lila)
           current_colors <- c("#009E73", "#C84B00", "#4D4281")
           names(current_colors) <- c(setting$label, "Remaining EG", "Control Group")
           
           # Formen: Raute (18) vs. Kreise (16)
           current_shapes <- c(18, 16, 16)
           names(current_shapes) <- c(setting$label, "Remaining EG", "Control Group")
           
           # Größen: Highlight größer dargestellt
           current_sizes <- c(3.8, 2.6, 2.6)
           names(current_sizes) <- c(setting$label, "Remaining EG", "Control Group")
           
           set.seed(42)
           
           p <- ggplot(n1p2_change, aes(x = Group, y = diff_N1P2)) +
             
             geom_half_violin(
               data = filter(n1p2_change, Group == "CG"),
               fill = "#4D4281",
               side = "r", 
               position = position_nudge(x = 0.15), 
               alpha = 0.35, 
               trim = TRUE, 
               color = NA
             ) +
             
             geom_half_violin(
               data = n1p2_change %>% filter(Group == "EG", .data[[setting$col]] == setting$label),
               fill = "#009E73",
               side = "l", 
               position = position_nudge(x = -0.15), 
               alpha = 0.35, 
               trim = TRUE, 
               color = NA
             ) +
             
             geom_half_violin(
               data = n1p2_change %>% filter(Group == "EG", .data[[setting$col]] != setting$label),
               fill = "#C84B00",
               side = "l", 
               position = position_nudge(x = -0.15), 
               alpha = 0.30, 
               trim = TRUE, 
               color = NA
             ) +

             geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +

             geom_boxplot(data = filter(n1p2_change, Group == "EG"),
                          color = "#C84B00", fill = "#C84B00",
                          outlier.shape = NA, alpha = 0.15, width = 0.15, linewidth = 0.7) +
             geom_boxplot(data = filter(n1p2_change, Group == "CG"),
                          color = "#4D4281", fill = "#4D4281",
                          outlier.shape = NA, alpha = 0.15, width = 0.15, linewidth = 0.7) +
             geom_point(
               aes(color = .data[[setting$col]], 
                   shape = .data[[setting$col]], 
                   size  = .data[[setting$col]]),
               position = position_jitter(width = 0.18, height = 0, seed = 42),
               alpha = 0.85
             ) +
             
             coord_cartesian(ylim = c(-7, 5.8), clip = "off") +
             
             annotate(
               "text", 
               x = 0.5,          
               y = 5.6,          
               label = setting$panel_label, 
               fontface = "bold", 
               size = 5, 
               hjust = 0, 
               vjust = 0
             ) +
             
             # Punktfarben & Co.
             scale_color_manual(
               name = "EG Subgroup", 
               values = current_colors,
               breaks = c(setting$label, "Remaining EG")
             ) +
             scale_shape_manual(
               name = "EG Subgroup", 
               values = current_shapes,
               breaks = c(setting$label, "Remaining EG")
             ) +
             scale_size_manual(
               name = "EG Subgroup", 
               values = current_sizes,
               breaks = c(setting$label, "Remaining EG")
             ) +
             scale_x_discrete(limits = c("EG", "CG")) +
             scale_y_continuous(breaks = seq(-7, 7, by = 1)) +
             theme_minimal() +
             theme(
               axis.title = element_text(face = "bold", size = 12),
               axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
               axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
               axis.text = element_text(size = 11),
               legend.position = "right",
               legend.title = element_text(face = "bold", size = 11),
               legend.text = element_text(size = 10)
             ) +
             labs(
               title = NULL,
               x = "Group",
               y = expression(bold(Delta ~ "N1-P2 Amplitude (" * mu * "V)"))
             )
           
           print(p)
         }
         