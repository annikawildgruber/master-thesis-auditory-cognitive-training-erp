# ==============================================================================
# BASELINE COMPARISONS: SEPARATE MAIN (AUDIOGRAM) & SUPPLEMENTARY (GRID) PLOTS
# ==============================================================================
# 1. Subsample: Uses the FULL parent cohort (all EG-HL and CG-HL participants).
# 2. Runs 5 t-tests (Age, MoCA, Training Time, WM, FA). PTA is plotted but NOT tested.
# 3. Corrects p-values for multiple comparisons (FDR) across the 5 tests.
# 4. Safely strips names and pre-builds strings to PREVENT ggplot recycling.
# 5. Generates separate plots: Main Text Audiogram and Supplementary 2x3 Grid.
# ==============================================================================
# Ursprünglicher Code von: Julian Ockelmann
# Angepasst und erweitert von: Annika [Dein Nachname]

options(stringsAsFactors = FALSE, scipen = 999)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, gghalves, ggpubr)

# --- 1. DATA LOADING & FILTERING FULL COHORT ---
cat("\n>>> MODULE 1: Loading Data and Isolating Full Cohort... <<<\n")

base_dir <- getwd()

path_1 <- "C:/Users/JulianOckelmann/Desktop/act_connectivity_data"
path_2 <- "C:/Users/julia/Desktop/act_connectivity_data"
base_dir <- '/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Code/R Studio'

file_beh   <- file.path(base_dir, "lab_grand_summary.xlsx")
file_audio <- file.path(base_dir, "act_audiometry_plot.xlsx")

if(!all(file.exists(c(file_beh, file_audio)))) stop("CRITICAL: Missing files.")

# Load full lab data and isolate all unique participants at baseline
df_beh_full <- read_excel(file_beh) %>% mutate(id_clean = tolower(trimws(id)))

lab_data <- df_beh_full %>%
  mutate(session_clean = if("session" %in% names(.)) session else timepoints_sin) %>%
  filter(group %in% c("EG-HL", "CG-HL"), session_clean == "Pre-Training") %>%
  distinct(id_clean, .keep_all = TRUE)

full_cohort_ids <- lab_data$id_clean

cat(paste("SUCCESS: Filtered baseline behavioral data down to", nrow(lab_data), "participants.\n"))

# Load and Filter Audiogram Data
audiogram_filtered <- read_excel(file_audio) %>%
  mutate(id_clean = tolower(trimws(id))) %>%
  filter(id_clean %in% full_cohort_ids) %>%
  pivot_longer(cols = ends_with("_unaided_t0"), names_to = c("Frequenz", "Ohr"), names_pattern = "(\\d+)([rl])_unaided_t0", values_to = "Hoerschwelle") %>%
  mutate(
    Frequenz_Zahl = parse_number(Frequenz),
    Ohr = factor(Ohr, levels = c("l", "r")), 
    group = ifelse(grepl("ha", id_clean), "EG-HL", "CG-HL") 
  )

# Audiogram Summary Stats for the plot
mittelwerte <- audiogram_filtered %>%
  group_by(Frequenz_Zahl, Ohr, group) %>%
  summarise(Mittelwert = mean(Hoerschwelle, na.rm = TRUE), sd = sd(Hoerschwelle, na.rm = TRUE), .groups = 'drop')


# ==============================================================================
# --- 2. STATISTICAL TESTS & STRING PRE-BUILDING ---
# ==============================================================================
cat("\n>>> MODULE 2: Running Baseline Statistics & FDR Correction... <<<\n")

# Run 5 independent t-tests
t_age   <- t.test(age ~ group, data = lab_data)
t_moca  <- t.test(moca_pre ~ group, data = lab_data)
t_time  <- t.test(training_time ~ group, data = lab_data)
t_wm    <- t.test(wm_lab_pre ~ group, data = lab_data)
t_fa    <- t.test(fa_lab_pre ~ group, data = lab_data)

# Info über hearing aid usage hinzufügen
ha_table <- table(lab_data$group, lab_data$hearing_aid)
chi_ha   <- chisq.test(ha_table)

raw_p_vals <- c(age = t_age$p.value, moca = t_moca$p.value, 
                time = t_time$p.value, wm = t_wm$p.value, fa = t_fa$p.value, ha = chi_ha$p.value)

# Apply False Discovery Rate (FDR) Correction
adj_p_vals <- p.adjust(raw_p_vals, method = "fdr")

cat("\n--- RAW vs CORRECTED P-VALUES ---\n")
print(data.frame(Raw_P = raw_p_vals, FDR_Corrected_P = adj_p_vals))

cat("\n--- Hearing Aid Distribution ---\n")
print(ha_table)
cat(sprintf("\nChi-Square Test: X² = %.2f, df = %d, p = %.3f\n", 
            chi_ha$statistic, chi_ha$parameter, chi_ha$p.value))

# ----------------------------------------------------------------------------
# PRE-BUILD LABELS (Unnaming guarantees ggplot doesn't recycle variables)
# ----------------------------------------------------------------------------
lbl_age  <- sprintf("italic(t)(%.2f) == %.2f * ', ' ~ italic(p) == '%s'", 
                    unname(t_age$parameter), unname(t_age$statistic), sprintf("%.3f", unname(adj_p_vals["age"])))
lbl_wm   <- sprintf("italic(t)(%.2f) == %.2f * ', ' ~ italic(p) == '%s'", 
                   unname(t_wm$parameter), unname(t_wm$statistic), sprintf("%.3f", unname(adj_p_vals["wm"])))
lbl_fa   <- sprintf("italic(t)(%.2f) == %.2f * ', ' ~ italic(p) == '%s'", 
                   unname(t_fa$parameter), unname(t_fa$statistic), sprintf("%.3f", unname(adj_p_vals["fa"])))


# ==============================================================================
# --- 3. PLOTTING ---
# ==============================================================================
cat("\n>>> MODULE 3: Generating Plots... <<<\n")

theme_desc <- theme(legend.position = "top", plot.title = element_text(color = "black", size = 14, face = "bold", hjust = -0.018), panel.background = element_blank(), axis.line = element_line(linewidth = 0.2), axis.ticks = element_line(linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.title.x = element_text(margin = margin(t = 0.5)), text = element_text(size = 12))

# a: Age
fig_age <- ggplot(lab_data, aes(x = group, y = age, fill = group, colour = group)) + geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5, linewidth = 0.7) + scale_fill_manual(values = c("EG-HL" = "#C84B00", "CG-HL" = "#4D4281"), labels = c("EG-HL" = "EG", "CG-HL" = "CG")) + scale_color_manual(values = c("EG-HL" = "#C84B00", "CG-HL" = "#4D4281"), guide = "none") + geom_jitter(width = 0.1, height = 0.15, alpha = 0.5, size = 2, na.rm = TRUE) + scale_x_discrete(labels = c("EG-HL" = "EG", "CG-HL" = "CG")) + scale_y_continuous(breaks = seq(65, 85, by = 5)) + expand_limits(y = c(60, 90)) + labs(x = "Group", y = "Age (years)", title = "a", fill = NULL) + theme_desc + geom_half_violin(side = "r", nudge = 0.15, alpha = 0.4, na.rm = TRUE,color = NA) + coord_flip()

# b: PTA4000 (No stats annotation)
fig_pta <- ggplot(lab_data, aes(x = group, y = pta_4000, fill = group, colour = group)) + geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5, linewidth = 0.7) + scale_fill_manual(values = c("EG-HL" = "#C84B00", "CG-HL" = "#4D4281")) + scale_color_manual(values = c("EG-HL" = "#C84B00", "CG-HL" = "#4D4281"), guide = "none") + geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, size = 2, na.rm = TRUE) + scale_x_discrete(labels = c("EG-HL" = "EG", "CG-HL" = "CG")) + scale_y_continuous(breaks = seq(20, 60, by = 10)) + expand_limits(y = c(15, 65)) + labs(x = "Group", y = "PTA (dB HL)", title = "b", fill = NULL) + theme_desc + geom_half_violin(side = "r", nudge = 0.15, alpha = 0.4,na.rm = TRUE, color = NA) + coord_flip() 

# c: Working Memory
fig_wm <- ggplot(lab_data, aes(x = group, y = wm_lab_pre, fill = group, colour = group)) + geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5, linewidth = 0.7) + scale_fill_manual(values = c("EG-HL" = "#C84B00", "CG-HL" = "#4D4281")) + scale_color_manual(values = c("EG-HL" = "#C84B00", "CG-HL" = "#4D4281"), guide = "none") +geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, size = 2, na.rm = TRUE) + scale_x_discrete(labels = c("EG-HL" = "EG", "CG-HL" = "CG")) + scale_y_continuous(breaks = seq(0, 1, by = 0.25)) + expand_limits(y = c(-0.2, 1.2)) + labs(x = "Group", y = "Working Memory", title = "c", fill = NULL) + theme_desc + geom_half_violin(side = "r", nudge = 0.15, alpha = 0.4,na.rm = TRUE, color = NA) + coord_flip() 

# d: Focused Attention
fig_fa <- ggplot(lab_data, aes(x = group, y = fa_lab_pre, fill = group, colour = group)) + geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5, linewidth = 0.7) + scale_fill_manual(values = c("CG-HL" = "#4D4281", "EG-HL" = "#C84B00")) + scale_color_manual(values = c("EG-HL" = "#C84B00", "CG-HL" = "#4D4281"), guide = "none") +geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, size = 2, na.rm = TRUE) + scale_x_discrete(labels = c("EG-HL" = "EG", "CG-HL" = "CG")) + scale_y_continuous(breaks = seq(-5, 25, by = 5)) + expand_limits(y = c(-5.2, 25.2)) + labs(x = "Group", y = "Selective Attention", title = "d", fill = NULL) + theme_desc + geom_half_violin(side = "r", nudge = 0.15, alpha = 0.4,na.rm = TRUE, color = NA) + coord_flip() 


# --- 3.1. PLOTTING FÜR AWI MA ---

pd <- position_dodge(width = 0.35) 
farbe_map <- c("CG-HL" = "#4D4281", "EG-HL" = "#C84B00")

fig_audiogram <- ggplot() +
  geom_line(data = mittelwerte, aes(x = factor(Frequenz_Zahl), y = Mittelwert, group = group, color = group), linewidth = 1.6, position = pd, na.rm = TRUE) +
  geom_errorbar(data = mittelwerte, aes(x = factor(Frequenz_Zahl), ymin = Mittelwert - sd, ymax = Mittelwert + sd, color = group), width = 0.2, linewidth = 0.9, position = pd, na.rm = TRUE) +
  scale_color_manual(values = farbe_map, labels = c("CG-HL" = "CG", "EG-HL" = "ACT")) +
  labs(title = "g") +
  coord_cartesian(clip = "off") + 
  theme_bw() + 
  theme(
    legend.position = "none", # Legend is suppressed here because it shares the master legend at the top
    strip.background = element_blank(), 
    strip.text = element_text(size = 13, face = "bold"),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(color = "black", size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 20, 10, 20)
  )


# --- 4. EXPORT / RENDER UNIFIED FIGURE ---

cat("\n>>> Combining Plots into Unified Layout <<<\n")

# 2x2 Grid für die 4 Variablen (Age, PTA, Working Memory, Focused Attention)
final_unified_plot <- ggarrange(
  fig_age, fig_pta,  
  fig_wm, fig_fa,  
  nrow = 2, ncol = 2, 
  align = "hv", 
  common.legend = TRUE, 
  legend = "top"
)

print(final_unified_plot)

# ==============================================================================
# --- 5. CONSOLE OUTPUT DESCRIPTIVE STATISTICS ---
# ==============================================================================
cat("\n==========================================================\n")
cat("          DESCRIPTIVE STATISTICS (FULL N=53 COHORT) \n")
cat("==========================================================\n\n")

cat("--- Sample Size ---\n")
print(lab_data %>% group_by(group) %>% summarise(n = n()))

cat("\n--- Demographics & Cognition (Mean (SD)) ---\n")
print(lab_data %>% 
        group_by(group) %>% 
        summarise(
          age = paste0(round(mean(age, na.rm=T), 1), " (", round(sd(age, na.rm=T), 1), ")"),
          moca = paste0(round(mean(moca_pre, na.rm=T), 1), " (", round(sd(moca_pre, na.rm=T), 1), ")"),
          pta_4000 = paste0(round(mean(pta_4000, na.rm=T), 1), " (", round(sd(pta_4000, na.rm=T), 1), ")"),
          training_time = paste0(round(mean(training_time, na.rm=T), 1), " (", round(sd(training_time, na.rm=T), 1), ")"),
          wm = paste0(round(mean(wm_lab_pre, na.rm=T), 1), " (", round(sd(wm_lab_pre, na.rm=T), 1), ")"),
          fa = paste0(round(mean(fa_lab_pre, na.rm=T), 1), " (", round(sd(fa_lab_pre, na.rm=T), 1), ")")
        ))

cat("\n--- Demographics & Cognition (Median (Min, Max)) ---\n")
print(lab_data %>% 
        group_by(group) %>% 
        summarise(
          age = paste0(round(median(age, na.rm=T), 1), " (", round(min(age, na.rm=T), 1), ", ", round(max(age, na.rm=T), 1), ")"),
          moca = paste0(round(median(moca_pre, na.rm=T), 1), " (", round(min(moca_pre, na.rm=T), 1), ", ", round(max(moca_pre, na.rm=T), 1), ")"),
          pta_4000 = paste0(round(median(pta_4000, na.rm=T), 1), " (", round(min(pta_4000, na.rm=T), 1), ", ", round(max(pta_4000, na.rm=T), 1), ")"),
          training_time = paste0(round(median(training_time, na.rm=T), 1), " (", round(min(training_time, na.rm=T), 1), ", ", round(max(training_time, na.rm=T), 1), ")"),
          wm = paste0(round(median(wm_lab_pre, na.rm=T), 1), " (", round(min(wm_lab_pre, na.rm=T), 1), ", ", round(max(wm_lab_pre, na.rm=T), 1), ")"),
          fa = paste0(round(median(fa_lab_pre, na.rm=T), 1), " (", round(min(fa_lab_pre, na.rm=T), 1), ", ", round(max(fa_lab_pre, na.rm=T), 1), ")")
        ))

