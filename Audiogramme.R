# Praktikumsbericht Audiogramme


library(readxl)
library(tidyverse)

audiogramm_daten <- read_excel('/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/UZH/Arches Meeting/act_audiometry_plot.xlsx')

# Muster: Zahl(Frequenz) + r/l(Ohr) + Rest(_unaided_t0)
pattern <- "(\\d+)([rl])_unaided_t0"

audiogramm_long <- audiogramm_daten %>%
  # Alle Spalten umformen, die auf "_unaided_t0" enden
  pivot_longer(
    cols = ends_with("_unaided_t0"),
    names_to = c("Frequenz", "Ohr"),
    names_pattern = pattern, # Teilt den Namen in zwei Gruppen
    values_to = "Hoerschwelle"
  ) %>%
  # Spalten vorbereiten
  mutate(
    # Frequenz_Zahl für die Achsen-Sortierung
    Frequenz_Zahl = parse_number(Frequenz), 
    # Ohr als Faktor für die Facetten-Erstellung (Plots)
    Ohr = factor(Ohr, levels = c("r", "l")),
    # Gruppe als Faktor für die Linientypen
    group = factor(group, levels = c("EG-HL", "CG-HL")) 
    # (Annahme: Ihre Gruppen heißen EG-HL und CG-HL oder nur EG und CG)
  )

# Mittelwerte pro Frequenz, Ohr und Gruppe berechnen
mittelwerte <- audiogramm_long %>%
  group_by(Frequenz_Zahl, Ohr, group) %>%
  summarise(
    Mittelwert = mean(Hoerschwelle, na.rm = TRUE),
    .groups = 'drop'
  )

# Farben und Linientypen definieren (wie zuvor)
linien_typ_map <- c("EG-HL" = "solid", "CG-HL" = "dashed") # EG durchgehend, CG gestrichelt
gruppen_farben <- c("r" = "red", "l" = "blue")

ggplot() +
  
  # 1. Einzelaudiogramme (dünner und heller)
  geom_line(
    data = audiogramm_long,
    aes(x = factor(Frequenz_Zahl), y = Hoerschwelle, 
        group = interaction(id, group), # Gruppiert jede Einzellinie eindeutig
        linetype = group, # Linientyp basierend auf der 'group'
        color = Ohr),
    linewidth = 0.5, # Dünnere Linie
    alpha = 0.25     # Heller/Transparenter
  ) +
  
  # 2. Mittelwerte (Fette Linien)
  geom_line(
    data = mittelwerte,
    aes(x = factor(Frequenz_Zahl), y = Mittelwert, 
        group = group, 
        linetype = group, 
        color = Ohr),
    linewidth = 1.5, # Dicke, prominente Linie
    alpha = 1
  ) +
  
  # 3. Mittelwerte (Fette Punkte)
  geom_point(
    data = mittelwerte,
    aes(x = factor(Frequenz_Zahl), y = Mittelwert, color = Ohr),
    size = 4,
    shape = 19
  ) +
  
  # 4. Facetten (separate Plots für links/rechts)
  facet_wrap(~ Ohr, 
             labeller = as_labeller(c("r" = "Rechtes Ohr", "l" = "Linkes Ohr"))) +
  
  # 5. Farben zuweisen
  scale_color_manual(
    values = gruppen_farben,
    guide = "none" # Farblegende nicht anzeigen
  ) +
  
  # # 6. Linientypen zuweisen
  # scale_linetype_manual(
  #   values = linien_typ_map,
  #   name = "Studygroup"
  # ) +
  
  # 6. Linientypen zuweisen und Legende unterdrücken (NEUE ANPASSUNG)
  scale_linetype_manual(
    values = linien_typ_map,
    guide = "none" # Linientyp-Legende unterdrückt
  ) +
  
  # 7. Achsen und Titel anpassen
  scale_x_discrete(
    name = "Frequency (kHz)",
    position = "top",
    # Hier definieren wir die Labels manuell (von Hz in kHz umgerechnet)
    labels = c("500" = "0.5", "1000" = "1", "2000" = "2", "3000" = "3", "4000" = "4", "6000" = "6", "8000" = "8")
  ) +
  scale_y_reverse(
    name = "Threshold (dB HL)", 
    breaks = seq(0, 100, by = 20)
  ) +
  
  
  # 8. Theme anpassen
  theme_bw() +
  theme(
    # Achsentitel vergrößern (Frequency / Hearing Threshold)
    axis.title = element_text(size = 13, face = "bold"), 
    
    # Achsenzahlen vergrößern (125, 250... / 0, 10, 20...)
    axis.text = element_text(size = 11, color = "black"),
    
    # Falls du die Facetten-Überschriften (Rechtes/Linkes Ohr) doch behalten willst:
    strip.text = element_text(size = 13, face = "bold"),
    
    #plot.margin = margin(5, 5, 5, 5, "pt"),
    
    # Deine restlichen Einstellungen:
    strip.text.x = element_blank(), 
    strip.background = element_blank(),
    legend.position = "none"
  )


