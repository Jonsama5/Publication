# ==============================
# Phyloseq type object creation
# ==============================

# Tax and Otu tables must contain the same rownames
rownames_b <- sprintf("otu%03d", seq_len(nrow(b_otu)))
rownames(b_otu) <- rownames_b
rownames(b_tax) <- rownames_b

rownames_v <- sprintf("otu%03d", seq_len(nrow(v_otu)))
rownames(v_otu) <- rownames_v
rownames(v_tax) <- rownames_v

rownames_p <- sprintf("otu%03d", seq_len(nrow(p_otu)))
rownames(p_otu) <- rownames_p
rownames(p_tax) <- rownames_p

rownames_g <- sprintf("otu%03d", seq_len(nrow(g_otu)))
rownames(g_otu) <- rownames_g
rownames(g_tax) <- rownames_g

# Samples with metadata, sample name as rownames

samples <- metadata
samples <- samples %>%
  column_to_rownames("Sample.ID")

# Phyloseq

samples <- sample_data(samples)

  # Bacteria
b_OTU <- otu_table(b_otu, taxa_are_rows = TRUE)
b_tax <- as.matrix(b_tax)
b_TAX <- tax_table(b_tax)

phy_b <- phyloseq(b_OTU, b_TAX, samples)

  # Virus
v_OTU <- otu_table(v_otu, taxa_are_rows = TRUE)
v_tax <- as.matrix(v_tax)
v_TAX <- tax_table(v_tax)

phy_v <- phyloseq(v_OTU, v_TAX, samples)

  # Plasmodium
p_OTU <- otu_table(p_otu, taxa_are_rows = TRUE)
p_tax <- as.matrix(p_tax)
p_TAX <- tax_table(p_tax)

phy_p <- phyloseq(p_OTU, p_TAX, samples)

  # Global
g_OTU <- otu_table(g_otu, taxa_are_rows = TRUE)
g_tax <- as.matrix(g_tax)
g_TAX <- tax_table(g_tax)

phy_g <- phyloseq(g_OTU, g_TAX, samples)
