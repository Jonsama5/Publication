# ==================
# Diversity analysis
# ==================

# Alpha diversity

Richness_b <- plot_richness(phy_b, x = "Country", measures = c("Shannon", "Simpson"), title = "B") + 
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  geom_boxplot(alpha = 0.6) +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0, size = 17, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(), 
        axis.title.x.bottom =  element_blank()) 
Richness_v <- plot_richness(phy_v, x = "Country", measures = c("Shannon", "Simpson"), title = "C") + 
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  geom_boxplot(alpha = 0.6) +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0, size = 17, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) 
Richness_g <- plot_richness(phy_g, x = "Country", measures = c("Shannon", "Simpson"), title = "A") + 
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  geom_boxplot(alpha = 0.6) +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0, size = 17, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) 

Richness_A_general <- ggarrange(Richness_g, Richness_b, Richness_v, ncol = 3)

# Alpha diversity by variables

Riqueza_general_sex <- plot_richness(phy_g_filtered, x = "Country", measures = c("Shannon"), title = "Sex") + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Sex, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon")

Riqueza_general_zone <- plot_richness(phy_g_filtered, x = "Country", measures = c("Shannon"), title = "Zone") + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Zone, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon")

Riqueza_general_infected <- plot_richness(phy_g, x = "Country", measures = c("Shannon"), title = "Infection status") + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Infection, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon")

Riqueza_general_variables_SH <- ggarrange(Riqueza_general_sex, Riqueza_general_zone, Riqueza_general_infected, ncol=3)

Riqueza_general_sex <- plot_richness(phy_g_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Sex, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson")

Riqueza_general_zone <- plot_richness(phy_g_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Zone, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson")

Riqueza_general_infected <- plot_richness(phy_g_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Infection, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson")

Riqueza_general_variables_SS <- ggarrange(Riqueza_general_sex, Riqueza_general_zone, Riqueza_general_infected, ncol=3)

phy_b_filtered <- subset_samples(phy_b, Zone != "NA")
phy_b_filtered <- subset_samples(phy_b_filtered, Sex != "Undefined")

Riqueza_bacteriana_sex <- plot_richness(phy_b_filtered, x = "Country", measures = c("Shannon"), title = "Sex") + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Sex, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon (Bacteria)")

Riqueza_bacteriana_zone <- plot_richness(phy_b_filtered, x = "Country", measures = c("Shannon"), title = "Zone") + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Zone, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon (Bacteria)")

Riqueza_bacteriana_infected <- plot_richness(phy_b_filtered, x = "Country", measures = c("Shannon"), title = "Infection status") + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Infection, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon (Bacteria)")

Riqueza_bacteriana_variables_SH <- ggarrange(Riqueza_bacteriana_sex, Riqueza_bacteriana_zone, Riqueza_bacteriana_infected, ncol = 3)

Riqueza_bacteriana_sex <- plot_richness(phy_b_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Sex, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson (Bacteria)")

Riqueza_bacteriana_zone <- plot_richness(phy_b_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Zone, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson (Bacteria)") 

Riqueza_bacteriana_infected <- plot_richness(phy_b, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Infection, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson (Bacteria)")

Riqueza_bacteriana_variables_SS <- ggarrange(Riqueza_bacteriana_sex, Riqueza_bacteriana_zone, Riqueza_bacteriana_infected, ncol=3)

Riqueza_viral_sex <- plot_richness(phy_v_filtered, x = "Country", measures = c("Shannon")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Sex, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon (Virus)")

Riqueza_viral_zone <- plot_richness(phy_v_filtered, x = "Country", measures = c("Shannon")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Zone, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon (Virus)")

Riqueza_viral_infected <- plot_richness(phy_v_filtered, x = "Country", measures = c("Shannon")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Infection, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Shannon (Virus)")

Riqueza_viral_variables_SH <- ggarrange(Riqueza_viral_sex, Riqueza_viral_zone, Riqueza_viral_infected, ncol=3)

Riqueza_viral_sex <- plot_richness(phy_v_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Sex, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson (Virus)")

Riqueza_viral_zone <- plot_richness(phy_v_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5) +
  facet_wrap(~ Zone, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"), 
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson (Virus)")

Riqueza_viral_infected <- plot_richness(phy_v_filtered, x = "Country", measures = c("Simpson")) + 
  geom_boxplot(width = 0.5)+
  facet_wrap(~ Infection, scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey", size = 0.5), 
        plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold"),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x.bottom =  element_blank()) +
  ylab(label = "Simpson (Virus)")


Riqueza_viral_variables_SS <- ggarrange(Riqueza_viral_sex, Riqueza_viral_zone, Riqueza_viral_infected, ncol=3)

Riqueza_general_variable_fg <- ggarrange(Riqueza_general_variables_SH, Riqueza_general_variables_SS, nrow=2, align = "v")

Riqueza_bacteriana_variable <- ggarrange(Riqueza_bacteriana_variables_SH, Riqueza_bacteriana_variables_SS, nrow=2, align = "v")

Riqueza_viral_variable <- ggarrange(Riqueza_viral_variables_SH, Riqueza_viral_variables_SS, nrow=2, align = "v")

Riqueza_bacterianayviral_variable_fg  <- ggarrange(Riqueza_bacteriana_variable, Riqueza_viral_variable, 
                                                   nrow=2, align = "v", widths = )

# Tests
  #Kruskal-Wallis
kruskal_alpha_diversity(phy_b, group_var = "Sex", measures = c("Shannon", "Simpson"))
kruskal_alpha_diversity(phy_b, group_var = "Zone", measures = c("Shannon", "Simpson"))
kruskal_alpha_diversity(phy_b, group_var = "Infection", measures = c("Shannon", "Simpson"))

dunn_alpha_b <- dunn_post_hoc_phy(physeq = phy_b, measures = c("Shannon", "Simpson")) %>%
  filter(P_adjusted < 0.05)

kruskal_alpha_diversity(phy_v, group_var = "Sex", measures = c("Shannon", "Simpson"))
kruskal_alpha_diversity(phy_v, group_var = "Zone", measures = c("Shannon", "Simpson"))
kruskal_alpha_diversity(phy_v, group_var = "Infection", measures = c("Shannon", "Simpson"))

dunn_alpha_v <- dunn_post_hoc_phy(physeq = phy_v, measures = c("Shannon", "Simpson")) %>%
  filter(P_adjusted < 0.05)
