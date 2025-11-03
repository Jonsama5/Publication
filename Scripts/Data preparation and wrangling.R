# ==============================
# Data preparation and wrangling
# ==============================

setwd("C:/Users/jonco/Desktop/Bioinformática/UNIR - 2º Cuatrimestre/TFM/Publicacion/")

# Library calls

library(ggplot2)
library(readxl)
library(dplyr)
library(tibble)
library(microbiome) 
library(phyloseq)
library(vegan)
library(FSA) 
library(gt) 
library(tidyr)
library(ggpubr) 
library(mvabund) 
library(microvars)

# Upload data

bacteria_csv <- read.csv("BACTERIA_SPP_TABLE.csv")
virus_csv <- read.csv("VIRUSES_SPP_TABLE.csv")
plas_csv <- read.csv("plasmodio_SPP_table.csv")
metadata <- read.csv("Muestras mosquito Cameroon - R metadata.csv")
metadata[,1] <- sub("-.*", "", metadata[,1])

# Tidying up name, eliminate sample duplicates and separate
# taxonomy and abundance table

  # Bacteria
    
    colnames(bacteria_csv) <- sub("\\..*", "", colnames(bacteria_csv))
    bacteria_csv <- subset(bacteria_csv, select = -AN0007)
    bacteria_csv <- subset(bacteria_csv, select = -AN0425.1)
    
    b_otu <- bacteria_csv[,1:221]
    b_tax <- bacteria_csv[,222:length(bacteria_csv)]
    
    for (col in colnames(b_tax)) {
      if (is.character(b_tax[[col]])) {
        b_tax[[col]][is.na(b_tax[[col]]) | b_tax[[col]] == ""] <- "Other"
        
        b_tax[[col]][grepl("^unk_", b_tax[[col]])] <- "Other"
      }
    }
    
  # Virus
    
    colnames(virus_csv) <- sub("\\..*", "", colnames(virus_csv))
    virus_csv <- subset(virus_csv, select = -AN0007)
    virus_csv <- subset(virus_csv, select = -AN0425.1)
    
    v_otu <- virus_csv[,1:221]
    v_tax <- virus_csv[,222:length(virus_csv)]
    
    for (col in colnames(v_tax)) {
      if (is.character(v_tax[[col]])) {
        v_tax[[col]][is.na(v_tax[[col]]) | v_tax[[col]] == ""] <- "Other"
        
        v_tax[[col]][grepl("^unk_", v_tax[[col]])] <- "Other"
      }
    }
    
  # Plasmodium
    
    colnames(plas_csv) <- sub("\\..*", "", colnames(plas_csv))
    colnames(plas_csv)[colnames(plas_csv) == "AN007"] <- "AN0070"
    plas_csv <- subset(plas_csv, select = -AN0070)
    plas_csv <- subset(plas_csv, select = -AN0455)

    p_otu <- plas_csv[,1:221]
    p_tax <- plas_csv[,222:length(plas_csv)]
    
  # Data with all the species
    
    global_csv <- rbind(bacteria_csv, virus_csv, plas_csv)
    g_otu <- global_csv[,1:221]    
    g_tax <- global_csv[,222:length(global_csv)]    
    for (col in colnames(g_tax)) {
      if (is.character(g_tax[[col]])) {
        g_tax[[col]][is.na(g_tax[[col]]) | g_tax[[col]] == ""] <- "Other"
        
        g_tax[[col]][grepl("^unk_", g_tax[[col]])] <- "Other"
      }
    }

# Tidying up metadata
  
    # New variable: zone assigned to sample
    
    metadata <- metadata %>%
      mutate(Zone = case_when(
        Location %in% c("Doulougou", "Massila", "Daiguene", "Moussourtouk", "Makabay (Djarengo)", 
                        "Moulva", "Laf", "Badjawa", "Lougol", "Mayo Lebride", "Lamoudan", "Lainde Mbana", "Mayo Boki") ~ "North",
        Location %in% c("Tibati", "Palama", "Carrefour Poli", "Mgbandji", "Nkolondom", "Djaba", 
                        "Balda Bouri", "Banda", "Teckel", "Mabarangal'L", "Beka Goto", "Tekel") ~ "Central",
        Location %in% c("Bamendi", "Manda", "Mfelap", "Manchoutvi") ~ "West",
        Location %in% c("Oitibili", "Ahala", "Obala", "Essos") ~ "Southwest",
        Location %in% c("Gado Badzere", "Zembe Borongo", "Mayos", "Mbalmayo", "Lougol", "Mayo Dafan", 
                        "Avebe", "Nlozok", "DombÃ©") ~ "Southeast",
        Location %in% c("Afan-Essokye", "Foulassi I") ~ "South",
        TRUE ~ NA_character_  
      ))
    
    # New variable: parasite abundance
    metadata$Plasmodium_abundance <- colSums(p_otu)
    
    # New variable: parasite presence in sample
    metadata <- metadata %>%
      mutate(Infection = if_else(Plasmodium_abundance > 0, "Infected", "Not infected"))
    
    # Reformat variables
    metadata <- metadata %>%
      mutate(across(c(Country, Location, Sex, Zone, Infection), as.factor))


