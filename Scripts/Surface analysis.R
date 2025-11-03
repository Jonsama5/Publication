# ================
# Surface analysis
# ================

# Transform read counts to relative ones (we will not be using phy_p)
phy_b_a = transform_sample_counts(phy_b,function(x) 100* x/sum(x))
phy_v_a = transform_sample_counts(phy_v,function(x) 100* x/sum(x))
phy_g_a = transform_sample_counts(phy_g,function(x) 100* x/sum(x))

# Remove NAs and Undefined
phy_g_filtered <- subset_samples(phy_g, Zone != "NA")
phy_g_filtered <- subset_samples(phy_g_filtered, Sex != "Undefined")

phy_b_filtered <- subset_samples(phy_b, Zone != "NA")
phy_b_filtered <- subset_samples(phy_b_filtered, Sex != "Undefined")

phy_v_filtered <- subset_samples(phy_v, Zone != "NA")
phy_v_filtered <- subset_samples(phy_v_filtered, Sex != "Undefined")

# Number of samples per zone
samples_zone <- ggplot(samples, aes(x = Zone)) +
  geom_bar() +
  labs(title = "Sample count per Zone",
       x = "Zone",
       y = "Sample count") +
  theme_minimal() 

# Summary statistics

phy_b_statistics <- microsummary(phy_b)
phy_v_statistics <- microsummary(phy_v)
phy_g_statistics <- microsummary(phy_g)
