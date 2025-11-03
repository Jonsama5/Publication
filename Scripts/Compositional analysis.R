# ======================
# Compositional analysis
# ======================

# Filter top 50 taxa
phy_b_top <- get_top_taxa(phy_b_a, 50)
phy_v_top <- get_top_taxa(phy_v_a, 50)
phy_g_top <- get_top_taxa(phy_g_a, 50)


# Composition (genus level)
  # Color palette
  color_genus <- c("#000000", "#A52A2A", "#FF69B4", "#6A5ACD", "#1E90FF", "#7B68EE", "#808080", "#FFD700", "#556B2F", 
                    "#00FA9A", "#FF8C00", "#DA70D6", "#F8F8FF", "#000080", "#DC143C", "#9932CC", "#2F4F4F", "#FF1493", 
                    "#B0C4DE", "#FFDAB9", "#008000", "#778795")

bacteria_genera <- plot_bar(phy_b_top, fill = "Genus") +
  labs(title = paste("A")) +
  theme_minimal(base_size = 12) +
  labs(x = "Samples", fill = "Genus", y = "Bacterial relative abundance (%)") +
  theme(
    plot.title = element_text(hjust = 0, size = 17, face = "bold"), 
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y.left = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey", size = 0.5)  # Añade la cuadrícula para el eje Y
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Limita el eje Y hasta 100
  scale_fill_manual(values = color_genus, guide = guide_legend(ncol = 1))


virus_genera <- plot_bar(phy_v_top, fill = "Genus")  +
  labs(title = paste("B")) +
  theme_minimal(base_size = 12) +
  labs(x = "Samples", fill = "Genus", y = "Viral relative abundance (%)") +
  theme(
    plot.title = element_text(hjust = 0, size = 17, face = "bold"), 
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8), 
    axis.title.y.left = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_line(color = "grey", size = 0.5)  # Añade la cuadrícula para el eje Y
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Limita el eje Y hasta 100
  scale_fill_manual(values = color_genus) 

Composition_fg <- ggarrange(bacteria_genera, virus_genera, 
                            ncol = 1, 
                            nrow = 2, 
                            align = "v") 

# Composition by variables

Composition_genera_by_sex <- plot_bar(phy_g_top, fill = "Genus") +
  facet_wrap(~ Sex, scales = "free_x") +
  labs(title = paste("A")) +
  theme_minimal(base_size = 12) +
  labs(x = "Samples", fill = "Genus", y = "Relative abundance (%)") +
  theme(
    plot.title = element_text(hjust = 0, size = 17, face = "bold"), 
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y.left = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey", size = 0.5),# Añade la cuadrícula para el eje Y
    strip.text = element_text(size = 14)
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Limita el eje Y hasta 100
  scale_fill_manual(values = color_genus, guide = guide_legend(ncol = 1))
phy_g_top <- subset_samples(phy_g_top, !is.na(Zone)) # Elimino los NA de zonas

Composition_genera_by_zone <- plot_bar(phy_g_top, fill = "Genus") +
  facet_wrap(~ Zone, scales = "free_x") +
  labs(title = paste("B")) +
  theme_minimal(base_size = 12) +
  labs(x = "Samples", fill = "Genus", y = "Relative abundance (%)") +
  theme(
    plot.title = element_text(hjust = 0, size = 17, face = "bold"), 
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y.left = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey", size = 0.5),# Añade la cuadrícula para el eje Y
    strip.text = element_text(size = 14)
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Limita el eje Y hasta 100
  scale_fill_manual(values = color_genus, guide = guide_legend(ncol = 1))
Composition_genera_by_infection <- plot_bar(phy_g_top, fill = "Genus") +
  facet_wrap(~ Infection, scales = "free_x") + 
  labs(title = paste("C")) +
  theme_minimal(base_size = 12) +
  labs(x = "Samples", fill = "Genus", y = "Relative abundance (%)") +
  theme(
    plot.title = element_text(hjust = 0, size = 17, face = "bold"), 
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y.left = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 9), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey", size = 0.5),# Añade la cuadrícula para el eje Y
    strip.text = element_text(size = 14)
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Limita el eje Y hasta 100
  scale_fill_manual(values = color_genus, guide = guide_legend(ncol = 1))
Composition_vars_fg <- ggarrange(Composition_genera_by_sex, Composition_genera_by_zone, Composition_genera_by_infection, 
                                 nrow = 3, common.legend = TRUE, legend = "right", heights = c(1,1.5,1))

# Tests
  # Differences in composition
otu_table_phy_g_top <- as(otu_table(phy_g_top), "matrix")
otu_table_phy_g_top <- t(otu_table_phy_g_top)  # samples must be rows
meta_phy_g_top <- as(sample_data(phy_g_top), "data.frame")
vars <- list("Sex", "Zone", "Infection")
fit_list <- list()
anova_list <- list()
for (var in vars) {
  formula <- as.formula(paste0("otu_table_phy_g_top ~ meta_phy_g_top[['", var, "']]"))
  fit_list[[var]] <- manyglm(formula, family = "negative.binomial")
  anova_list[[var]] <- anova(fit_list[[var]], test = "LR")
}
print(anova_list) # Sex, Zone and Infection were significant

# Differentially abundant taxa per variable
create_MAplot_from_phy <- function(physeq, condition_var, title = NULL, contrast = NULL) {
  library(DESeq2)
  library(phyloseq)
  library(ggplot2)
  library(ggrepel)
  # Asegurarse que la variable existe en los metadatos
  if (!(condition_var %in% colnames(sample_data(physeq)))) {
    stop(paste("La variable", condition_var, "no está en los metadatos del objeto phyloseq"))
  }
  # Crear título automático si no se proporciona
  if (is.null(title)) {
    title <- condition_var
  }
  # Crear objeto DESeq2
  dds <- phyloseq_to_deseq2(physeq, as.formula(paste("~", condition_var)))
  dds <- DESeq(dds)
  # Obtener resultados (contraste específico si se da)
  if (!is.null(contrast)) {
    # Validación: el argumento debe ser un vector de longitud 3
    if (length(contrast) != 3) {
      stop("El argumento 'contrast' debe tener la forma c('variable', 'grupo_comparado', 'grupo_referencia')")
    }
    res <- results(dds, contrast = contrast)
  } else {
    res <- results(dds)
  }
  # Preparar dataframe para MA plot
  ma_data <- data.frame(
    baseMean = res$baseMean,
    log2FoldChange = res$log2FoldChange,
    padj = res$padj,
    OTU = rownames(res)
  )
  # Extraer taxonomía
  tax <- as.data.frame(tax_table(physeq))
  ma_data$Genus <- tax[match(ma_data$OTU, rownames(tax)), "Genus"]
  # Etiquetas para puntos significativos
  label_data <- subset(ma_data, padj < 0.05 & is.finite(log2FoldChange))
  label_data$Label <- ifelse(is.na(label_data$Genus), label_data$OTU, label_data$Genus)
  # Filtrar datos válidos
  ma_data <- subset(ma_data, is.finite(baseMean) & is.finite(log2FoldChange) & !is.na(padj))
  # Crear MA plot
  plot <- ggplot(ma_data, aes(x = log10(baseMean + 1), y = log2FoldChange)) +
    geom_point(aes(color = padj < 0.05), size = 3, alpha = 0.7) +
    scale_color_manual(values = c("FALSE" = "gray70", "TRUE" = "red")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = c(-1, 1), linetype = "dashed", color = "darkgray") +
    geom_text_repel(data = label_data, aes(label = Label), size = 4, max.overlaps = 20) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 17, face = "bold"), 
      axis.text.x = element_text(angle = 70, vjust = 1, hjust = 0.5, size = 9),
      axis.title.x.bottom = element_text(face = "bold"),
      axis.text.y = element_text(size = 8), 
      axis.title.y.left = element_text(size = 13, face = "bold"),
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 14), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_line(color = "grey", size = 0.5)
    ) +
    labs(
      title = title,
      x = "Log10(Relative abundance + 1)",
      y = paste("Log2 change (by", condition_var, ")"),
      color = "Significance (padj < 0.05)"
    )
  return(list(plot = plot, data = ma_data))
}

MA_sex <- create_MAplot_from_phy(physeq = phy_g_filtered, condition_var = "Sex", title = "A", contrast = c("Sex", "F", "M"))
MA_infection <- create_MAplot_from_phy(physeq = phy_g_filtered, condition_var = "Infection", title = "B", contrast = c("Infection", "Infected", "Not infected"))

MA_fg <- ggarrange(MA_sex$plot, MA_infection$plot ,nrow = 2, common.legend = TRUE, legend = "right", heights = c(1,1.5))

  # For the "Zone" variable instead, I'll just confirm the pairwise comparison
zones <- c("Central", "North", "West", "South", "Southwest", "Southeast")
pairwise <- t(combn(zones, 2))

AD_zones <- list()

for (i in seq_len(nrow(pairwise))) {
  grp1 <- pairwise[i, 1]
  grp2 <- pairwise[i, 2]
  
  # Filter samples from both groups
  phy_pair <- subset_samples(phy_g_filtered, Zone %in% c(grp1, grp2))
  
  # Title
  plot_title <- paste("Comparación:", grp1, "vs", grp2)
  
  # MA plot
  p <- create_MAplot_from_phy(
    physeq = phy_pair, 
    condition_var = "Zone", 
    title = plot_title
  )
  
  # Save plots
  AD_zones[[paste0(grp1, "_vs_", grp2)]] <- p
}

AD_all_zones <- AD_zones
