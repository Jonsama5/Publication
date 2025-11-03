# ===================
# Plasmodium analysis
# ===================

# Extract abundances and metadata
otu <- as.data.frame(otu_table(phy_g))
if (taxa_are_rows(phy_g)) otu <- t(otu)  # muestras como filas
meta <- as.data.frame(sample_data(phy_g))

# Extract plasmodium abundance
num_var <- meta[["Plasmodium_abundance"]]

# Normalization
otu_rel <- otu / rowSums(otu)  # normaliza por fila

  # Correlation between taxa and plasmodium abundance
    cor_results <- apply(otu_rel, 2, function(x) {
      suppressWarnings(cor.test(x, num_var, method = "spearman"))
    })
    
    res_df <- data.frame(
      OTU = names(cor_results),
      Spearman_rho = sapply(cor_results, function(x) x$estimate),
      p_value = sapply(cor_results, function(x) x$p.value),
      stringsAsFactors = FALSE
    )
    
    # Apply Bonferroni-Hochberg adjustment
    res_df$padj <- p.adjust(res_df$p_value, method = "BH")
    
    # Add taxonomy to results
    if (!is.null(tax_table(phy_g))) {
      tax <- as.data.frame(tax_table(phy_g))
      res_df <- cbind(res_df, tax[match(res_df$OTU, rownames(tax)), ])
    }
    
     <- subset(res_df, padj < 0.05)

  # "Correlation" between variables and plasmodium abundance
  #   Note: It's not a correlation per se, we are checking wether the plasmodium
  #         abundance variable is significally different between groups of the independent variable.

    # Quantitative variable (actual correlation; 2 numeric variables)

      correlacion_plas_vs_año <- cor.test(metadata$Plasmodium_abundance, metadata$Year, method = "spearman")

    # Categoric variable (K < 2)
        # First we check global differences then pairwise ones
     
      kruskal.test(Plasmodium_abundance ~ Zone, data = metadata)
      correlacion_plas_vs_zona <- pairwise.wilcox.test(metadata$Plasmodium_abundance, metadata$Zone, p.adjust.method = "fdr")
      kruskal.test(Plasmodium_abundance ~ Sex, data = metadata)
      correlacion_plas_vs_sexo <- pairwise.wilcox.test(metadata$Plasmodium_abundance, metadata$Sex, p.adjust.method = "fdr")

# Interacción entre variables
metadata$Infected_bin <- ifelse(metadata$Infection == "Infected", 1, 0)
modelo_interaccion <- glm(Infected_bin ~ Sex, data = metadata, family = binomial)
modelo_interaccion_summary <- summary(modelo_interaccion)
anova(modelo_interaccion, test = "LRT") # Poca devianza, pero significativa

modelo_sin_int <- glm(Infected_bin ~ Sex + Zone, data = metadata, family = binomial)
modelo_con_int <- glm(Infected_bin ~ Sex * Zone, data = metadata, family = binomial)
anova(modelo_sin_int, modelo_con_int, test = "LRT") # No hay interacción entre sexo y zona
