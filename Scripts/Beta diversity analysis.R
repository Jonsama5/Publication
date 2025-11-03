# =======================
# Beta diversity analysis
# =======================

# Distances
dist_bj <- phyloseq::distance(phy_b, method = "jaccard")
dist_vj <- phyloseq::distance(phy_v, method = "jaccard")
ordination_bj <- ordinate(phy_b, method = "PCoA", distance = dist_bj)
ordination_vj <- ordinate(phy_v, method = "PCoA", distance = dist_vj)
dist_bb <- phyloseq::distance(phy_b, method = "bray")
dist_vb <- phyloseq::distance(phy_v, method = "bray")
ordination_bb <- ordinate(phy_b, method = "PCoA", distance = dist_bb)
ordination_vb <- ordinate(phy_v, method = "PCoA", distance = dist_vb)

# Plots
Richness_bj <- plot_ordination(phy_b, ordination_bj) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 16, color = "black", face = "bold"),
        axis.title.y.left = element_text(size = 13, face = "bold"), 
        axis.title.x.bottom = element_text(size = 13, face = "bold" )) +
  ggtitle("B") + 
  labs(x = "Axe 1", y = "Axe 2") 
Richness_bb <- plot_ordination(phy_b, ordination_bb) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 16, color = "black", face = "bold"),
        axis.title.y.left = element_text(size = 13, face = "bold"), 
        axis.title.x.bottom = element_text(size = 13, face = "bold" )) +
  ggtitle("A") + 
  labs(x = "Axe 1", y = "Axe 2")
Richness_vj <- plot_ordination(phy_v, ordination_vj) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 16, color = "black", face = "bold"),
        axis.title.y.left = element_text(size = 13, face = "bold"), 
        axis.title.x.bottom = element_text(size = 13, face = "bold" )) +
  ggtitle("D") + 
  labs(x = "Axe 1", y = "Axe 2") 
Richness_vb <- plot_ordination(phy_v, ordination_vb) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 16, color = "black", face = "bold"),
        axis.title.y.left = element_text(size = 13, face = "bold"), 
        axis.title.x.bottom = element_text(size = 13, face = "bold" )) +
  ggtitle("C") + 
  labs(x = "Axe 1", y = "Axe 2") 
Beta_diversity_fg <- ggarrange(Richness_bb, Richness_bj,Richness_vb, Richness_vj, ncol=2, nrow = 2)

# Tests
  
  # Before applying permanova: test homocedasticity.
  # Prepare the dispersion indexes with betadisper()

dist_homo <- distance(phy_g, method = "bray")
sex <- sample_data(phy_g)$Sex
zone <- sample_data(phy_g)$Zone
infected <- sample_data(phy_g)$Infection
year <- sample_data(phy_g)$Year
latitude <- sample_data(phy_g)$Latitude
longitude <- sample_data(phy_g)$Longitude
disp_sex <- betadisper(dist_homo, sex)
disp_zone <- betadisper(dist_homo, zone)
disp_infected <- betadisper(dist_homo, infected)
disp_year <- betadisper(dist_homo, year)
disp_latitude <- betadisper(dist_homo, latitude)
disp_longitude <- betadisper(dist_homo, longitude)

    # Anova 

anova(disp_sex) # Significant --> Tukey
anova(disp_zone) # Significant --> Tukey
anova(disp_infected) # # Not Significant --> Homocedasticity
anova(disp_year) # Significant --> Tukey
anova(disp_latitude) # Significant --> Tukey
anova(disp_longitude) # Significant --> Tukey

    # Tukey: these are the groups that do not fulfill the homocedasticity concepts,
    #         thus their Permanova results should be considered carefully.

TukeyHSD(disp_sex) # Male and Female are different as expected
TukeyHSD(disp_zone) # North - Southeast
TukeyHSD(disp_infected) # Homocedasticity
TukeyHSD(disp_year) # # 2005 - 09 and 13
TukeyHSD(disp_latitude)$group %>% # Solo ocurre en una categoría --> aceptamos homocedasticidad
  as.data.frame() %>%
  tibble::rownames_to_column("comparison") %>%
  filter(`p adj` < 0.05)
TukeyHSD(disp_longitude)$group %>% #Ocurre poquísimo --> aceptamos homocedasticidad
  as.data.frame() %>%
  tibble::rownames_to_column("comparison") %>%
  filter(`p adj` < 0.05)

  # Permanova (Bray distance)
    # We use filtered to remove troublesome NAs 
permanova_tbl_b <- bind_rows(
  Sex = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Sex"), rownames = "Term"),
  Zone = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Zone"), rownames = "Term"),
  Location = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Location"), rownames = "Term"),
  Infection = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Infection"), rownames = "Term"),
  .id = "Variable") %>%
  mutate(Group = "Bacteria")

permanova_tbl_v <- bind_rows(
  Sex = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Sex"), rownames = "Term"),
  Zone = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Zone"), rownames = "Term"),
  Location = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Location"), rownames = "Term"),
  Infection = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Infection"), rownames = "Term"),
  .id = "Variable") %>%
  mutate(Group = "Virus")

    # Join
permanova_tbl_bray <- bind_rows(permanova_tbl_b, permanova_tbl_v)

  # Permanova (Jaccard distance)
permanova_tbl2_b <- bind_rows(
  Sex = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Sex", method = "jaccard"), rownames = "Term"),
  Zone = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Zone", method = "jaccard"), rownames = "Term"),
  Location = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Location", method = "jaccard"), rownames = "Term"),
  Infection = as_tibble(beta_permanova_by_variables(phy_b_filtered, vars = "Infection", method = "jaccard"), rownames = "Term"),
  .id = "Variable") %>%
  mutate(Group = "Bacteria")

permanova_tbl2_v <- bind_rows(
  Sex = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Sex", method = "jaccard"), rownames = "Term"),
  Zone = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Zone", method = "jaccard"), rownames = "Term"),
  Location = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Location", method = "jaccard"), rownames = "Term"),
  Infection = as_tibble(beta_permanova_by_variables(phy_v_filtered, vars = "Infection", method = "jaccard"), rownames = "Term"),
  .id = "Variable") %>%
  mutate(Group = "Virus")

# Join
permanova_tbl_jaccard <- bind_rows(permanova_tbl2_b, permanova_tbl2_v)