########## ------------------------------------------------------------- ##########
########## ------------------------------------------------------------- ##########
########## ----------------STRATEGIES OF CLIFF PLANTS------------------- ##########
########## ------------------------------------------------------------- ##########
########## ------------------------------------------------------------- ##########
  
  # This is the script for the study: "Specialist plants on vertical calcareous rock
  # formations in the Pyrenees share common traits and strategies." All the script
  # is detailed and explained, and should work as far as you have downloaded the folder
  # from github "https://github.com/albertocirera/Traits-and-strategies-of-specialist-cliff-plants-in-the-Pyrenees" and you have access to internet. In fact, it
  # should work just with the raw data csv, but you need some other files for the map
  # part. Any doubt dont hesitate to ask to cireralberto@gmail.com

  # All the code can run in less than 5 minutes (and that is in my computer, which
  # is not potent at all and its quite slow)

  
  # Mejorar alguna cosa de la figura de los lmm. El cliff_specialist, ver si quiero dejar esos nombres
  # de variables o poner otros...

  # En la figura del PCA igual puedo hacer algo mas pequeño los símbolos de type. Y se que es dificil pero si se pudiesen juntar mas...

  # TODAS LAS FIGURAS 16CM DE ANCHURA (Menos la de lmm que igual tengo que tener mas cuidado con la altura. reducir un poco altura)  

  
  

  




  ######### ---------------------------- #########
  ######### ---------------------------- #########
  #########         INTRODUCCION         ######### 
  ######### ---------------------------- #########
  ######### ---------------------------- #########
  
    # In this script we analyse some functional traits of some cliff plants and 
    # some generalist plants. For more information read manuscript.

    # The structure in this scripts goes as it follows:

      # 1. INTRODUCTION
      # 2. DATA INTRODUCTION
        # Libraries
        # Procedure
          # Elevation
          # Completing dataframe
      # 3. EXPLORATORY ANALYSIS
        # Libraries
        # Procedure
          # Univariate analysis
          # Bivariate analysis
      # 4. MODELS
        # Linear mixed models
          # Libraries
          # Procedure
          # Figure 2: Linear mixed models
        # Multivariate analysis
          # Ordenation plot
            # PCA
          # Numerical analysis
            # RDA
            # PERMANOVA
            # BETADISPER
          # Figure 3: PCA
      # 5. COMPLEMENTS
        # Maps
          # Libraries
          # Procedure
          # Figure 1: Sample locations
        # Phylogenomic tree
          # Libraries
          # Procedure
          # Supplementary figure 1: Phylogenomic tree
      # 6. FINAL CONSIDERATIONS
  
  
  
  
  
  
 
  
  
  ######### ---------------------------- #########
  ######### ---------------------------- #########
  #########       DATA INTRODUCTION      ######### 
  ######### ---------------------------- #########
  ######### ---------------------------- #########
  
    # Here we open the csv where we have the data, and we switch the variables to 
    # factor so R reads them correctly. 
    # Then elevation is extracted from the position of the data, and the data frame
    # is completed and reordered. 

    # First of all really important to set working directory to the folder downloaded
    # In my case...
    setwd("C:/Users/cirer/Desktop/Trabajo/IPE/TFM/Traits-and-strategies-of-specialist-cliff-plants-in-the-Pyrenees")

    # Install packages. Some manual instalations are needed for the script to work 
    # properly, other than the normal CRAN installations.
    
      # # webshot
      # install.packages("webshot")
      # webshot::install_phantomjs()
      # 
      # # ggtree
      # if (!require("BiocManager", quietly = TRUE))
      #   install.packages("BiocManager")
      # BiocManager::install("ggtree")
      # 
      # # V.PhyloMaker2
      # if (!require("devtools", quietly = TRUE))
      #   install.packages("devtools")
      # library("devtools")
      # devtools::install_github("jinyizju/V.PhyloMaker2")

    

   


 
    
    ######## --------------------- ########
    ######## ----- Libraries ----- ########
    ######## --------------------- ########
    
      # First you may need to install packages if needed

      library (lubridate) # To read dates correctly with R
      library (elevatr) # To extract the elevation from the data
      library (sf) # To map the data
      library (readxl)
      library (stringr)
      library (dplyr)
      library (tidyverse)
    





    
    
    ######## --------------------- ########
    ######## ----- Procedure ----- ########
    ######## --------------------- ########
    
      # Introduce the data from csv
      Datos <- read.csv("Raw_data/Rawdata_Cirera_TFM.csv", sep=";")
      
      # summary to see the data
      summary (Datos)
      
      # switch to factor
      Datos$Cod_sample <- factor(Datos$Cod_sample)
      Datos$Cod_ind <- factor(Datos$Cod_ind)
      Datos$Species <- factor(Datos$Species)
      Datos$Family <- factor(Datos$Family)
      Datos$Type <- factor(Datos$Type)
      Datos$Pairs <- factor(Datos$Pairs)
      Datos$Locality <- factor(Datos$Locality)
      Datos$Organ <- factor(Datos$Organ)

      # switch date variable so R can read it correctly
      Datos$Sample_date <- dmy(Datos$Sample_date)
      
      # another summary to check if the changes worked correctly
      summary (Datos)
      
      
      
      
      
      
      
      ####### ------------- #######
      #######   Elevation   ####### 
      ####### ------------- #######
         
        # For this internet access is needed.
        # This is done in order to get the elevation of our data
        
        # Remove NA from coords
        Datos_sf <- Datos[!is.na(Datos$Coord_X), ]
        
        # Duplicate Coordinates
        Datos_sf$Cor_X <- Datos_sf$Coord_X
        Datos_sf$Cor_Y <- Datos_sf$Coord_Y
        
        # Convert to sf object
        Datos_sf <- st_as_sf(Datos_sf, coords = c("Coord_X", "Coord_Y"), crs = 4326)
        
        # Fetch elevation data
        elev_data <- get_elev_point(Datos_sf, prj = 4326, src = "aws", z = 10)
        
        # Combine elevation with original data
        Datos_sf$Elevation <- NA
        Datos_sf$Elevation <- elev_data$elevation
        
        # Add elevation to original data frame
        Datos <- merge(Datos, Datos_sf[, c("Cod_sample", "Elevation")], by = "Cod_sample", all.x = TRUE)
        
        # Delete geometry
        Datos$geometry <- NULL
        
        # Reorder Datos
        Datos <- Datos[, c(colnames(Datos)[1:7], "Elevation", colnames(Datos)[8:35] # Remaining columns
        )]
        
      
        
        
        
        
        
      ####### ----------------------- #######
      #######  Completing data frame  #######
      ####### ----------------------- #######
      
        # First, as we want complete data to do our comparisons, we need to clean
        # the dataframe, delete species that we didnt end up sampling, or calculate
        # some secondary traits as flower area or relative flower area.
      
        # Filter the columns that are not completly NA (in case there are)
        Datos_m <- Datos %>%
        select_if(~ !all(is.na(.)))
      
        # Calculate the mean of leaf area per species
        medias <- Datos_m %>%
          group_by(Species) %>%
          summarize(media_leaf_area = mean(Leaf_area, na.rm = TRUE))
      
        # Identify species where leaf area equals to zero 
        Speciess_a_eliminar <- medias %>%
          filter(is.na(media_leaf_area)) %>%
          pull(Species)
      
        # Identify pairs of that species
        generos_a_eliminar <- Datos_m %>%
          filter(Species %in% Speciess_a_eliminar) %>%
          pull(Pairs) %>%
          unique()
        
        # Identify all the species from those pairs
        Speciess_a_eliminar_todas <- Datos_m %>%
          filter(Pairs %in% generos_a_eliminar) %>%
          pull(Species) %>%
          unique()
        
        # Convert it to character
        Speciess_a_eliminar_todas <- as.character(Speciess_a_eliminar_todas)
        
        # Filtrate original dataframe to eliminate all the species from that pairs
        Datos_m <- Datos_m %>%
          filter(!Pairs %in% generos_a_eliminar)
        
        # Calculate flower area
        Datos_m$Flower_area <- NA
        for (i in 1:nrow(Datos_m)) {
          if (Datos_m$Organ[i] == "Flower") {
            if (!is.na(Datos_m$Flower_diameter[i])) {
              Datos_m$Flower_area[i] <- pi * (Datos_m$Flower_diameter[i]/2)^2
            } else if (!is.na(Datos_m$Flower_major_axis[i])) {
              Datos_m$Flower_area[i] <- Datos_m$Flower_major_axis[i] * Datos_m$Flower_minor_axis[i]
            }
          }
        }
        
        # Order the species for pairs and type for future plots
        Datos_m <- Datos_m %>%
          arrange(Pairs, Type) %>%
          mutate(Species = factor(Species, levels = unique(Species)))
        
        # Summarize per individual
        Datos_m_individ <- Datos_m %>%
          group_by(Family, Type, Pairs, Locality, Species, Cod_ind) %>%
          summarise(across(c(2, 9:31), mean, na.rm = TRUE))
        
        # Calculate relative flower area per individual
        Datos_m_individ$Relative_flower_area <- NA
        for (i in 1:nrow(Datos_m_individ)) {
          Datos_m_individ$Relative_flower_area[i] <- Datos_m_individ$Flower_area[i] / Datos_m_individ$Plant_max_veg_height[i]
        }     
          
        
          
        
        
  
        
        
        
  # ######### ------------------------------- #########
  # ######### ------------------------------- #########
  # #########       EXPLORATORY ANALYSIS      #########
  # ######### ------------------------------- #########
  # ######### ------------------------------- #########
  # 
  #   # The idea is to make some graphs and tables to see what could be happening
  #   # with our data. This is a really important step, but it is really long and
  #   # it is not strictly needed for the analysis, so it is not "active".
  #   # If you want R to read it you have to select all the rows of Exploratory Analysis
  #   # and press "ctr + alt + C" to delete all "#".
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  #     ######## --------------------- ########
  #     ######## ----- Libraries ----- ########
  #     ######## --------------------- ########
  # 
  #       library(ggpubr)
  #       library(ggplot2)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  #     ######## --------------------- ########
  #     ######## ----- Procedure ----- ########
  #     ######## --------------------- ########
  # 
  #       # First we are going to do some univariates, and then bivariates and more
  # 
  #       # Colours for the graphs
  #       colores_grafico <- c("Generalist" = "#009E73", "Cliff_specialist" = "#E69F00")
  #       coloType <- c("Ramonda" = "#9467bd",
  #                     "Antirrhinum" = "#d62728",
  #                     "Asperula" = "green",
  #                     "Campanula" = "purple",
  #                     "Hieracium" = "black",
  #                     "Lonicera" = "#2ca02c",
  #                     "Androsace" = "#1f77b4",
  #                     "Petrocoptis" = "#A65628",
  #                     "Saxifraga" = "#ff7f0e",
  #                     "Hypericum" = "yellow",
  #                     "Sarcocapnos" = "#2F4F4F",
  #                     "Dioscorea" = "#F781BF")
  # 
  #       # shape
  #       formas <- c("Generalist" = 2, "Cliff_specialist" = 19)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  #       ####### ------------------------ #######
  #       #######    Univariate analysis   #######
  #       ####### ------------------------ #######
  # 
  #         # This is to see the behaviour and how are distributed our different variables
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ----------- ######
  #         ######   Species   ######
  #         ###### ----------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Species)
  # 
  #           # It is a categorical variable, so we can do a bar graph
  #           ggplot(Datos_m, aes(x = Species, fill = Type)) +
  #             geom_bar() +
  #             theme_minimal() +
  #             labs(
  #               title = "Frecuencia de Speciess",
  #               x = "Species",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas para mejor legibilidad
  # 
  #           # As for some plants there are not 4 flowers per individual,
  #           # the graph is not homogeneous
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------- ######
  #         ######   Locality   ######
  #         ###### ------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Locality)
  # 
  #           # It is a categorical variable, so we can do a bar graph
  #           ggplot(Datos_m, aes(x = Locality)) +
  #             geom_bar() +
  #             theme_minimal() +
  #             labs(
  #               title = "Localityes de recoleccion",
  #               x = "Species",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas para mejor legibilidad
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------- ######
  #         ######   Elevation   ######
  #         ###### ------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Elevation)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Elevation)) +
  #               geom_histogram(binwidth = 300, fill = "blue", color = "black", alpha = 0.7) +
  #               theme_minimal() +
  #               labs(
  #                 title = "Distribución de Elevación",
  #                 x = "Elevación",
  #                 y = "Frecuencia"
  #               ) +
  #               theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # looks like a normal distribution
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------- ######
  #         ######   Leaf_area   ######
  #         ###### ------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Leaf_area)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Leaf_area)) +
  #             geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Leaf_area",
  #               x = "Leaf_area",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ---------------- ######
  #         ######   Petiole_area   ######
  #         ###### ---------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Petiole_area)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Petiole_area)) +
  #             geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Petiole_area",
  #               x = "Petiole_area",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### -------------------------- ######
  #         ######   Leaf_area_with_petiole   ######
  #         ###### -------------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Leaf_area_with_petiole)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Leaf_area_with_petiole)) +
  #             geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Leaf_area_with_petiole",
  #               x = "Leaf_area_with_petiole",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------------ ######
  #         ######   Leaf_thickness   ######
  #         ###### ------------------ ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Leaf_thickness)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Leaf_thickness)) +
  #             geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Leaf_thickness",
  #               x = "Leaf_thickness",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------------------ ######
  #         ######   Petiole_fresh_weight   ######
  #         ###### ------------------------ ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Petiole_fresh_weight)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Petiole_fresh_weight)) +
  #             geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Petiole_fresh_weight",
  #               x = "Petiole_fresh_weight",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ---------------------------------- ######
  #         ######   Leaf_fresh_weight_with_petiole   ######
  #         ###### ---------------------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Leaf_fresh_weight_with_petiole)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Leaf_fresh_weight_with_petiole)) +
  #             geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Leaf_fresh_weight_with_petiole",
  #               x = "Leaf_fresh_weight_with_petiole",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------------- ######
  #         ######   Leaf_dry_weight   ######
  #         ###### ------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Leaf_dry_weight)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Leaf_dry_weight)) +
  #             geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Leaf_dry_weight",
  #               x = "Leaf_dry_weight",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ---------------------- ######
  #         ######   Petiole_dry_weight   ######
  #         ###### ---------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Petiole_dry_weight)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Petiole_dry_weight)) +
  #             geom_histogram(binwidth = 0.005, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Petiole_dry_weight",
  #               x = "Petiole_dry_weight",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### -------------------------------- ######
  #         ######   Leaf_dry_weight_with_petiole   ######
  #         ###### -------------------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Leaf_dry_weight_with_petiole)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Leaf_dry_weight_with_petiole)) +
  #             geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Leaf_dry_weight_with_petiole",
  #               x = "Leaf_dry_weight_with_petiole",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ----------------------- ######
  #         ######   SLA_without_petiole   ######
  #         ###### ----------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$SLA_without_petiole)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = SLA_without_petiole)) +
  #             geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de SLA_without_petiole",
  #               x = "SLA_without_petiole",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### -------------------- ######
  #         ######   SLA_with_petiole   ######
  #         ###### -------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$SLA_with_petiole)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = SLA_with_petiole)) +
  #             geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de SLA_with_petiole",
  #               x = "SLA_with_petiole",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------------------ ######
  #         ######   LDMC_without_petiole   ######
  #         ###### ------------------------ ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$LDMC_without_petiole)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = LDMC_without_petiole)) +
  #             geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de LDMC_without_petiole",
  #               x = "LDMC_without_petiole",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### --------------------- ######
  #         ######   LDMC_with_petiole   ######
  #         ###### --------------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$LDMC_with_petiole)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = LDMC_with_petiole)) +
  #             geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de LDMC_with_petiole",
  #               x = "LDMC_with_petiole",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### --------------- ######
  #         ######   Flower_area   ######
  #         ###### --------------- ######
  # 
  #           # First we do the summary
  #           summary (Datos_m$Flower_area)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m, aes(x = Flower_area)) +
  #             geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Flower_area",
  #               x = "Flower_area",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------------------ ######
  #         ######   Relative_flower_area   ######
  #         ###### ------------------------ ######
  # 
  #           # First we do the summary
  #           summary (Datos_m_individ$Relative_flower_area)
  # 
  #           # It is a numerical variable, so we can do a histogram
  #           ggplot(Datos_m_individ, aes(x = Relative_flower_area)) +
  #             geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  #             theme_minimal() +
  #             labs(
  #               title = "Distribución de Relative_flower_area",
  #               x = "Relative_flower_area",
  #               y = "Frecuencia"
  #             ) +
  #             theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #           # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  #         ###### ------------------------ ######
  #         ######   Plant_max_veg_height   ######
  #         ###### ------------------------ ######
  # 
  #         # First we do the summary
  #         summary (Datos_m$Plant_max_veg_height)
  # 
  #         # It is a numerical variable, so we can do a histogram
  #         ggplot(Datos_m, aes(x = Plant_max_veg_height)) +
  #           geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  #           theme_minimal() +
  #           labs(
  #             title = "Distribución de Plant_max_veg_height",
  #             x = "Plant_max_veg_height",
  #             y = "Frecuencia"
  #           ) +
  #           theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  # 
  #         # Lots of small values, and less of bigger
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  #       ####### -------------------------------- #######
  #       #######    Bivariate analysis and more   #######
  #       ####### -------------------------------- #######
  # 
  #         # Now the idea is to see how the different variables change in function
  #         # of the cliff specialist and generalist species. To see that in an exploratory
  #         # way, we can do a graph with all the species filling with the type
  #         # variable, and do a "global" graph comparing generalist with cliff species.
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------- ######
  #           ######   Elevation   ######
  #           ###### ------------- ######
  # 
  #           # Graph with all the species
  #           ggboxplot(Datos_m, x = "Species", y = "Elevation", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #             theme_bw() +
  #             theme(
  #               panel.grid.major = element_blank(),
  #               panel.grid.minor = element_blank(),
  #               legend.position = "top",
  #               axis.line = element_line(color = "black"),
  #               axis.text.x = element_text (angle = 90),
  #               legend.key = element_rect(fill = "white", color = "black"))
  # 
  #           # Total
  #           ggboxplot(Datos_m, x = "Type", y = "Elevation", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #             theme_bw() +
  #             theme(
  #               panel.grid.major = element_blank(),
  #               panel.grid.minor = element_blank(),
  #               legend.position = "top",
  #               axis.line = element_line(color = "black"),
  #               axis.text.x = element_text (angle = 90),
  #               legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------- ######
  #           ######   Leaf_area   ######
  #           ###### ------------- ######
  # 
  #             # Graph with all the species
  #             ggplot(Datos_m, aes(x = Species, y = Leaf_area, fill = Type)) +
  #               geom_boxplot(color = "black", alpha = 0.7, width = 0.5) +
  #               scale_fill_manual(values = colores_grafico, labels = c("Generalist" = "Generalist species", "Cliff_specialist" = "Cliff species")) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 legend.title = element_blank(),  # Quitar el título de la leyenda
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),  # Etiquetas en cursiva y ligeramente inclinadas
  #                 legend.key = element_rect(fill = "white", color = "black"),
  #                 plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Título principal
  #                 axis.title.y = element_text(size = 14, face = "bold"),  # Título del eje y
  #                 axis.title.x = element_text(size = 14, face = "bold")  # Título del eje x
  #               ) +
  #               labs(
  #                 x = "Species",  # Título del eje x
  #                 y = expression(Leaf~area~(cm^2)),  # Título del eje y con cm^2
  #                 title = "Leaf Area by Species")  # Añadir título principal
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Leaf_area", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  #             ##### -------------------------------------------------- #####
  #             ##### Graphs with just some species for the presentation #####
  #             ##### -------------------------------------------------- #####
  # 
  #               # for the presentation I want...
  # 
  #               # Sarcocapnos
  #               ggplot(Datos_m[Datos_m$Pairs == "Sarcocapnos", ], aes(x = Species, y = Leaf_area, fill = Type)) +
  #                 geom_boxplot(color = "black", alpha = 0.7, width = 0.5) +
  #                 scale_fill_manual(values = colores_grafico, labels = c("Generalist" = "Generalist species", "Cliff_specialist" = "Cliff species")) +
  #                 theme_bw() +
  #                 theme(
  #                   panel.grid.major = element_blank(),
  #                   panel.grid.minor = element_blank(),
  #                   legend.position = "top",
  #                   legend.title = element_blank(),  # Quitar el título de la leyenda
  #                   axis.line = element_line(color = "black"),
  #                   axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, face = "italic"),  # Etiquetas en cursiva y ligeramente inclinadas
  #                   legend.key = element_rect(fill = "white", color = "black"),
  #                   plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Título principal
  #                   axis.title.y = element_text(size = 14, face = "bold"),  # Título del eje y
  #                   axis.title.x = element_text(size = 14, face = "bold")  # Título del eje x
  #                 ) +
  #                 labs(
  #                   x = "Species",  # Título del eje x
  #                   y = expression(Leaf~area~(cm^2)),  # Título del eje y con cm^2
  #                   title = "Leaf Area by Species")  # Añadir título principal
  # 
  #               # Lonicera
  #               ggplot(Datos_m[Datos_m$Pairs == "Lonicera", ], aes(x = Species, y = Leaf_area, fill = Type)) +
  #                 geom_boxplot(color = "black", alpha = 0.7, width = 0.5) +
  #                 scale_fill_manual(values = colores_grafico, labels = c("Generalist" = "Generalist species", "Cliff_specialist" = "Cliff species")) +
  #                 theme_bw() +
  #                 theme(
  #                   panel.grid.major = element_blank(),
  #                   panel.grid.minor = element_blank(),
  #                   legend.position = "top",
  #                   legend.title = element_blank(),  # Quitar el título de la leyenda
  #                   axis.line = element_line(color = "black"),
  #                   axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, face = "italic"),  # Etiquetas en cursiva y ligeramente inclinadas
  #                   legend.key = element_rect(fill = "white", color = "black"),
  #                   plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Título principal
  #                   axis.title.y = element_text(size = 14, face = "bold"),  # Título del eje y
  #                   axis.title.x = element_text(size = 14, face = "bold")  # Título del eje x
  #                 ) +
  #                 labs(
  #                   x = "Species",  # Título del eje x
  #                   y = expression(Leaf~area~(cm^2)),  # Título del eje y con cm^2
  #                   title = "Leaf Area by Species")  # Añadir título principal
  # 
  #               # Androsace
  #               ggplot(Datos_m[Datos_m$Pairs == "Androsace", ], aes(x = Species, y = Leaf_area, fill = Type)) +
  #                 geom_boxplot(color = "black", alpha = 0.7, width = 0.5) +
  #                 scale_fill_manual(values = colores_grafico, labels = c("Generalist" = "Generalist species", "Cliff_specialist" = "Cliff species")) +
  #                 theme_bw() +
  #                 theme(
  #                   panel.grid.major = element_blank(),
  #                   panel.grid.minor = element_blank(),
  #                   legend.position = "top",
  #                   legend.title = element_blank(),  # Quitar el título de la leyenda
  #                   axis.line = element_line(color = "black"),
  #                   axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, face = "italic"),  # Etiquetas en cursiva y ligeramente inclinadas
  #                   legend.key = element_rect(fill = "white", color = "black"),
  #                   plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Título principal
  #                   axis.title.y = element_text(size = 14, face = "bold"),  # Título del eje y
  #                   axis.title.x = element_text(size = 14, face = "bold")  # Título del eje x
  #                 ) +
  #                 labs(
  #                   x = "Species",  # Título del eje x
  #                   y = expression(Leaf~area~(cm^2)),  # Título del eje y con cm^2
  #                   title = "Leaf Area by Species")  # Añadir título principal
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ---------------- ######
  #           ######   Petiole_area   ######
  #           ###### ---------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Petiole_area", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Petiole_area", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### -------------------------- ######
  #           ######   Leaf_area_with_petiole   ######
  #           ###### -------------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Leaf_area_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Leaf_area_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------------ ######
  #           ######   Leaf_thickness   ######
  #           ###### ------------------ ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Leaf_thickness", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Leaf_thickness", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------------------ ######
  #           ######   Petiole_fresh_weight   ######
  #           ###### ------------------------ ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Petiole_fresh_weight", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Petiole_fresh_weight", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ---------------------------------- ######
  #           ######   Leaf_fresh_weight_with_petiole   ######
  #           ###### ---------------------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Leaf_fresh_weight_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Leaf_fresh_weight_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------------- ######
  #           ######   Leaf_dry_weight   ######
  #           ###### ------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Leaf_dry_weight", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Leaf_dry_weight", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ---------------------- ######
  #           ######   Petiole_dry_weight   ######
  #           ###### ---------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Petiole_dry_weight", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Petiole_dry_weight", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### -------------------------------- ######
  #           ######   Leaf_dry_weight_with_petiole   ######
  #           ###### -------------------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Leaf_dry_weight_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Leaf_dry_weight_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ----------------------- ######
  #           ######   SLA_without_petiole   ######
  #           ###### ----------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "SLA_without_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "SLA_without_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### -------------------- ######
  #           ######   SLA_with_petiole   ######
  #           ###### -------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "SLA_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "SLA_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------------------ ######
  #           ######   LDMC_without_petiole   ######
  #           ###### ------------------------ ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "LDMC_without_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "LDMC_without_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### --------------------- ######
  #           ######   LDMC_with_petiole   ######
  #           ###### --------------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "LDMC_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "LDMC_with_petiole", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  #             
  #           ###### --------------- ######
  #           ######   Flower_area   ######
  #           ###### --------------- ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Flower_area", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Flower_area", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------------------ ######
  #           ######   Relative_flower_area   ######
  #           ###### ------------------------ ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m_individ, x = "Species", y = "Relative_flower_area", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m_individ, x = "Type", y = "Relative_flower_area", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  # 
  # 
  # 
  # 
  # 
  #           ###### ------------------------ ######
  #           ######   Plant_max_veg_height   ######
  #           ###### ------------------------ ######
  # 
  #             # Graph with all the species
  #             ggboxplot(Datos_m, x = "Species", y = "Plant_max_veg_height", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))
  # 
  #             # Total
  #             ggboxplot(Datos_m, x = "Type", y = "Plant_max_veg_height", fill = "Type", color = "black", alpha = 0.7, width = 0.5) +
  #               theme_bw() +
  #               theme(
  #                 panel.grid.major = element_blank(),
  #                 panel.grid.minor = element_blank(),
  #                 legend.position = "top",
  #                 axis.line = element_line(color = "black"),
  #                 axis.text.x = element_text (angle = 90),
  #                 legend.key = element_rect(fill = "white", color = "black"))








              
  ######### ----------------- #########
  ######### ----------------- #########
  #########       MODELS      ######### 
  ######### ----------------- #########
  ######### ----------------- #########           
  
    # Now we are going to make some different models to try to get the most out of
    # the data, and answer all of our questions. First we are going to do a correlation
    # matrix to see that many of our variables are almost the same (e.g. leaf area
    # without petiole and leaf area with petiole). Then we will do linear mixed models
    # for all individual traits, including "Pairs" and "Species" as random effects
    # and then we will do some multivariate analysis.
        
        
        
        
        
        
        
      ####### ------------------------ #######
      ####### -- Correlation matrix -- #######
      ####### ------------------------ #######   
     
        # The idea here is to do a correlation matrix to have numerical comprobation 
        # of the high similarity between some of the traits (e.g. leaf area
        # without petiole and leaf area with petiole)
        
        
        
        
        
        
        ###### --------------- ######
        ###### -- Libraries -- ######
        ###### --------------- ######
        
          library(corrplot) 
          library(ggcorrplot)
        
        
        
        
        
        
        ###### --------------- ######
        ###### -- Procedure -- ######
        ###### --------------- ######
        
          # I have to make a matrix with all the variables of which I want to do
          # the correlation matrix
          Datos_m_Ord <- Datos_m_individ[!is.na(Datos_m_individ$Flower_area),]
          matrizcor <- Datos_m_Ord[ ,c(8:21, 26:31)]
          rownames(matrizcor) <- Datos_m_Ord$Cod_ind
          
          # Now the correlation matrix
          mydata.cor = cor(matrizcor, use = "complete.obs")
          
          # I could copy and paste it in an excel for better visualization
          
          # One way of visualizing it
          corrplot(mydata.cor, tl.cex = 0.3)
          
          # Another way of visualizing it
          correlation <- ggcorrplot(
            mydata.cor, 
            method = "circle", 
            ggtheme = theme_classic(),
            lab = TRUE,
            lab_col = "black",
            lab_size = 1.5,
            title = "Correlation plot",
            tl.cex = 7,
            tl.col = "black",
            tl.srt = 45)
          
          # Visualize it
          correlation
          
          # Save it as an image
          ggsave(filename = "Supplementary_Figure2_Correlation.png", 
                 plot = correlation,
                 path = "Figures",
                 width = 16, 
                 height = 14, 
                 units = "cm", 
                 dpi = 300)
          
          
          
          
        
      
              
                        
    ######## ------------------------- ########
    ######## -- Linear mixed models -- ########
    ######## ------------------------- ########

      # Now we are going to do linear mixed models (lmm) with all the variables
      # of interest to see if they are affected by if a plant is cliff specialist
      # or generalist

              
              
              
              
              
              
      ####### --------------- #######
      ####### -- Libraries -- #######
      ####### --------------- #######
      
        # We get the libraries that we need for the analysis
            
        library(lme4)
        library(lmerTest) # Libreries for p-value
        library(pbkrtest) # Libreries for p-value
        library(car)
        library(emmeans)
        library(MuMIn) # for R2
      
            
            
            
            
            
      
      ####### --------------- #######
      ####### -- Procedure -- #######
      ####### --------------- #######
      
        # Here we are doing a lmm for each different trait. To do so properly, 
        # first we do all the process without any transformation, then we check the
        # residuals, and if needed (in all the cases is needed) we transform the data 
        # with a log. To get different ways to see the significance of the data, we do 
        # a base model, with all the random effects ("Pairs" and "Speciess" nested inside)
        # but with no explanatory variable, and then the same model with the explanatory
        # variable "Type". We can then compare the models, as another way of checking if
        # adding "Type" to the model adds information or not. Of course, we also check the
        # summary and the anova of the complete model, but like this, we have 2 ways of 
        # looking at the model and not only 1 p-value. We always check the emmeans graphs
        # and the exploratory graphs to interpretate the models. The models of the log
        # data are also checked to see if assumptions are met (they are in all cases). 
        # With the library "MuMIn" we can get the R square values of the model, and also 
        # one considering only the explanatory variable. 
          
        # At first, elevation was added to the models, but as it was no significant in 
        # almost all cases, it was deleted to simplify the models. It is only still there
        # in the case of "Plant_max_veg_height" (Plant size), where it was significant and it
        # changed the significance of the trait. As it is discussed in the manuscript,
        # it looks safe to believe that the being a cliff specialist or not actually
        # has an effect on plant size, even though elevation might have an effect also. 
            
            
            
            
            
            
        ###### --------------- ######
        ###### -- Leaf_area -- ###### 
        ###### --------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
            
            
            
            
            
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(Leaf_area ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(Leaf_area ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
          
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
            
          
          
          
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(Leaf_area) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(Leaf_area) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)
          
          
          
          
        
  
        ###### -------------------- ######
        ###### -- Leaf_thickness -- ###### 
        ###### -------------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
          
          
          
          
          
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
          # First with the raw data
          
          # Models
          Modelo_0 <- lmer(Leaf_thickness ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
          Modelo_1 <- lmer(Leaf_thickness ~ Type + (1|Pairs/Species), data=Datos_m_individ)
          
          # Summary
          summary(Modelo_0)
          summary(Modelo_1)
          
          # Model comparison
          anova(Modelo_0, Modelo_1)

          # Anova of the best model
          anova(Modelo_1)
          
          # Residuals to see how good is the model
          qqPlot((resid(Modelo_1))) # Normality
          plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
          hist((resid(Modelo_1))) # Normality with histogram (worse)
          
          # Calculate R2
          # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
          # estimated by fixed and random effects, the R2 of the model. 
          r.squaredGLMM(Modelo_1)
          
          # Emmeans to compare
          emmeans(Modelo_1, ~Type, adjust="fdr")
          plot(emmeans(Modelo_1, ~Type), horizontal=F)
          
          
          
          
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(Leaf_thickness) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(Leaf_thickness) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)  
          
            
            
          
  
            
        ###### ----------------------- ######
        ###### -- Leaf_fresh_weight -- ###### 
        ###### ----------------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
          
          
          
          
          
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(Leaf_fresh_weight ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(Leaf_fresh_weight ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
            
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
          
          
          
          
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(Leaf_fresh_weight) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(Leaf_fresh_weight) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)  
         
            
            
            
            
         
        ###### --------------------- ######
        ###### -- Leaf_dry_weight -- ###### 
        ###### --------------------- ######
            
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
            
            
            
            
            
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
            
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(Leaf_dry_weight ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(Leaf_dry_weight ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
            
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
            
            
            
            
            
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
            
            # If a transformation is needed
            
            # Models
            Modelo_log_0 <- lmer(log(Leaf_dry_weight) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(Leaf_dry_weight) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F) 
         
         
            
            
            
            
        ###### ------------------------- ######
        ###### -- SLA_without_petiole -- ###### 
        ###### ------------------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
          
          
          
          
          
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(SLA_without_petiole ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(SLA_without_petiole ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
            
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
          
          
          
          
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(SLA_without_petiole) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(SLA_without_petiole) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)
           
            
            
            
            
  
        ###### -------------------------- ######
        ###### -- LDMC_without_petiole -- ###### 
        ###### -------------------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
          
          
          
          
          
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(LDMC_without_petiole ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(LDMC_without_petiole ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
            
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
            
            
          
          
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(LDMC_without_petiole) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(LDMC_without_petiole) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)
            
  
            
            
            
            
        ###### ----------------- ######
        ###### -- Flower_area -- ###### 
        ###### ----------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
          
          
          
          
          
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(Flower_area ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(Flower_area ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
            
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
            
            
          
            
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(Flower_area) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(Flower_area) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)  
           
            
            
     
            
            
        ###### -------------------------- ######
        ###### -- Plant_max_veg_height -- ###### 
        ###### -------------------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
            
          # Here, as discussed in the manuscript and in the introduction part of
          # procedure of lmm, we leave the code for adding elevation as a covariable
          # in the log-tranformed models. 
          
          
          
          
          
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(Plant_max_veg_height ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(Plant_max_veg_height ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
            
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
            
            
            
            
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(Plant_max_veg_height) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(Plant_max_veg_height) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_2 <- lmer(log(Plant_max_veg_height) ~ Type + Elevation + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            summary(Modelo_log_2)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1, Modelo_log_2)

            # Anova of the best model
            anova(Modelo_log_1)
            anova(Modelo_log_2)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_2))) # Normality
            plot(resid(Modelo_log_2)~fitted(Modelo_log_2)) # Variance homogeneity
            hist((resid(Modelo_log_2))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            r.squaredGLMM(Modelo_log_2)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)
           
            
          
    
            
            
        ###### -------------------------- ######
        ###### -- Relative_flower_area -- ###### 
        ###### -------------------------- ######
          
          # First we do models with the data but if it is needed we transform it
          # so the assumptions of the lmm are met
          
          
          
          
          
          ##### ---------------------------- #####
          ##### -- Without transformation -- ##### 
          ##### ---------------------------- #####
          
            # First with the raw data
            
            # Models
            Modelo_0 <- lmer(Relative_flower_area ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_1 <- lmer(Relative_flower_area ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summary
            summary(Modelo_0)
            summary(Modelo_1)
            
            # Model comparison
            anova(Modelo_0, Modelo_1)

            # Anova of the best model
            anova(Modelo_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_1))) # Normality
            plot(resid(Modelo_1)~fitted(Modelo_1)) # Variance homogeneity
            hist((resid(Modelo_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_1)
            
            # Emmeans to compare
            emmeans(Modelo_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_1, ~Type), horizontal=F)
            
            
          
          
          
          ##### -------------------------- #####
          ##### -- With transformations -- ##### 
          ##### -------------------------- #####
          
            # If a transformaion is needed
            
            # Models
            Modelo_log_0 <- lmer(log(Relative_flower_area) ~ 1 + (1|Pairs/Species), data=Datos_m_individ)
            Modelo_log_1 <- lmer(log(Relative_flower_area) ~ Type + (1|Pairs/Species), data=Datos_m_individ)
            
            # Summaries
            summary(Modelo_log_0)
            summary(Modelo_log_1)
            
            # Model comparison
            anova(Modelo_log_0, Modelo_log_1)

            # Anova of the best model
            anova(Modelo_log_1)
            
            # Residuals to see how good is the model
            qqPlot((resid(Modelo_log_1))) # Normality
            plot(resid(Modelo_log_1)~fitted(Modelo_log_1)) # Variance homogeneity
            hist((resid(Modelo_log_1))) # Normality with histogram (worse)
            
            # Calculate R2
            # R2m is the marginal R2, by fixed effects, and R2c is the conditional,
            # estimated by fixed and random effects, the R2 of the model. 
            r.squaredGLMM(Modelo_log_1)
            
            # Emmeans to compare
            emmeans(Modelo_log_1, ~Type, adjust="fdr")
            plot(emmeans(Modelo_log_1, ~Type), horizontal=F)  
         
            
        
        
         
            
               
      ####### --------------------------------------- #######
      ####### -- Figure 2: Graphic with the models -- #######
      ####### --------------------------------------- #######
      
        # The idea is to do a graph were I can summarize all the information of the
        # models. To do so, I need to do the graphs for the different traits one by one
        # and then ensamble them. I want to see both boxplots and also lines for the
        # different species, for all the different traits. 
            
        # At the end I will and R2 squared as text and *** depending on significance
              
              
        
              
        
                    
        ###### --------------- ######
        ###### -- Libraries -- ######
        ###### --------------- ######      
                    
          # General plot libraries and library to ensamble
          
          library(ggplot2)
          library(tidyverse)
          library(patchwork)
          library(ggnewscale)
              
              
              
              
              
              
        ###### --------------- ######
        ###### -- Procedure -- ######
        ###### --------------- ######      
       
          # The idea is to do all the different graphs first. For each different
          # graph, the idea is to do boxplots, dots for the mean of each different
          # species, and lines that connect the dots so it is more visual.
              
          # summarize per species
          Datos_m_Species <- Datos_m_individ %>%
            group_by(Family, Type, Pairs, Locality, Species) %>%
            summarise(across(c(2:26), mean, na.rm = TRUE))
            
          # Colours for the graphs
          colores_grafico <- c("Generalist" = "#009E73", "Cliff_specialist" = "#E69F00")
          coloType <- c("Ramonda" = "#F781BF",
                        "Antirrhinum" = "#d62728",
                        "Asperula" = "green",
                        "Campanula" = "purple",
                        "Hieracium" = "yellow",
                        "Lonicera" = "#2ca02c",
                        "Androsace" = "#0000CD",
                        "Petrocoptis" = "#A65628",
                        "Saxifraga" = "#ff7f0e",
                        "Hypericum" = "black",
                        "Sarcocapnos" = "#2F4F4F",
                        "Dioscorea" = "#00CED1",
                        "0" = "grey")
          
          # shape 
          formas <- c("Generalist" = 24, "Cliff_specialist" = 21)
          
          # Create graphs list
          graphs <- list()
              
              
              
              
              
          ##### --------------- #####
          ##### -- Leaf_area -- #####
          ##### --------------- ##### 
              
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "Leaf_area")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = Leaf_area)
            
            # Create the graph and save it
            graphs[[1]] <- ggplot(Datos_m_Species, aes(x = Type, y = Leaf_area)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist")) +
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_fill_manual(values = coloType) +
              scale_shape_manual(values = formas) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              labs(title = NULL, x = NULL, y = "Leaf area (cm2)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = "*", x = 2.1, y = 17, size=4) +
              annotate("text", label = "R2m = 0.04", x = 0.9, y = 32, size=2) +
              annotate("text", label = "R2c = 0.97", x = 0.9, y = 29, size=2) 

            
              
         
        
          ##### -------------------- #####
          ##### -- Leaf_thickness -- #####
          ##### -------------------- #####
              
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "Leaf_thickness")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = Leaf_thickness)
              
            # Create the graph and save it
            graphs[[2]] <- ggplot(Datos_m_Species, aes(x = Type, y = Leaf_thickness)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist")) +
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_fill_manual(values = coloType) +
              scale_shape_manual(values = formas) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              labs(title = NULL, x = NULL, y = "Leaf thickness (mm)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = "", x = 2.1, y = 1.1, size=4) +
              annotate("text", label = "R2m = 0.02", x = 2.1, y = 1.45, size=2) +
              annotate("text", label = "R2c = 0.92", x = 2.1, y = 1.34, size=2) 
            
            
            
            
            
          ##### ----------------------- #####
          ##### -- Leaf_fresh_weight -- #####
          ##### ----------------------- #####
             
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "Leaf_fresh_weight")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = Leaf_fresh_weight)
              
            # Create the graph and save it
            graphs[[3]] <- ggplot(Datos_m_Species, aes(x = Type, y = Leaf_fresh_weight)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist")) +
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_fill_manual(values = coloType) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_shape_manual(values = formas) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              labs(title = NULL, x = NULL, y = "Leaf fresh weight (mg)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = "", x = 2.1, y = 2, size=4) +
              annotate("text", label = "R2m = 0.02", x = 0.9, y = 2.8, size=2) +
              annotate("text", label = "R2c = 0.97", x = 0.9, y = 2.6, size=2) 
          
            
            
            
              
          ##### ------------------------- #####
          ##### -- SLA_without_petiole -- #####
          ##### ------------------------- #####
            
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "SLA_without_petiole")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = SLA_without_petiole)
            
            # Create the graph and save it
            graphs[[4]] <- ggplot(Datos_m_Species, aes(x = Type, y = SLA_without_petiole)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist")) +
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_fill_manual(values = coloType) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_shape_manual(values = formas) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              labs(title = NULL, x = NULL, y = "SLA (cm2/mg)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = ".", x = 2.15, y = 345, size=5) +
              annotate("text", label = "R2m = 0.07", x = 0.9, y = 430, size=2) +
              annotate("text", label = "R2c = 0.86", x = 0.9, y = 400, size=2) 
            
            
            
            
            
          ##### -------------------------- #####
          ##### -- LDMC_without_petiole -- #####
          ##### -------------------------- #####
              
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "LDMC_without_petiole")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = LDMC_without_petiole)
            
            # Create the graph and save it
            graphs[[5]] <- ggplot(Datos_m_Species, aes(x = Type, y = LDMC_without_petiole)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist")) +
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_fill_manual(values = coloType) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_shape_manual(values = formas) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              labs(title = NULL, x = NULL, y = "LDMC (mg/mg)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = "", x = 2.1, y = 0.5, size=4) +
              annotate("text", label = "R2m = 0.02", x = 0.9, y = 0.62, size=2) +
              annotate("text", label = "R2c = 0.94", x = 0.9, y = 0.58, size=2) 
            
            
            
            
            
          ##### -------------------------- #####
          ##### -- Plant_max_veg_height -- #####
          ##### -------------------------- #####
             
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "Plant_max_veg_height")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = Plant_max_veg_height)
            
            # Create the graph and save it
            graphs[[6]] <- ggplot(Datos_m_Species, aes(x = Type, y = Plant_max_veg_height)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist")) +
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_fill_manual(values = coloType) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              scale_shape_manual(values = formas) +
              labs(title = NULL,
                   x = NULL,
                   y = "Plant size (cm)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = "**", x = 2.2, y = 70, size=4) +
              annotate("text", label = "R2m = 0.21", x = 0.9, y = 230, size=2) +
              annotate("text", label = "R2c = 0.93", x = 0.9, y = 210, size=2) 

            
            
            
            
          ##### ----------------- #####
          ##### -- Flower_area -- #####
          ##### ----------------- #####
              
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "Flower_area")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = Flower_area)
            
            # Create the graph and save it
            graphs[[7]] <- ggplot(Datos_m_Species, aes(x = Type, y = Flower_area)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist")) +
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_fill_manual(values = coloType) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_shape_manual(values = formas) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              labs(title = NULL,
                   x = NULL,
                   y = "Flower area (mm2)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = "", x = 2.1, y = 1000, size=4) +
              annotate("text", label = "R2m = 0.00", x = 0.9, y = 900, size=2) +
              annotate("text", label = "R2c = 0.98", x = 0.9, y = 780, size=2) 
            
            
            
            
            
          ##### -------------------------- #####
          ##### -- Relative_flower_area -- #####
          ##### -------------------------- #####
              
            # Transform data for geom_segment
            datos_segmentos <- Datos_m_Species[ , c("Type", "Pairs", "Relative_flower_area")]
            datos_segmentos <- datos_segmentos %>%
              pivot_wider(names_from = Type, values_from = Relative_flower_area)
            
            # Create the graph and save it
            graphs[[8]] <- ggplot(Datos_m_Species, aes(x = Type, y = Relative_flower_area)) +
              geom_boxplot(aes(fill = Type), color = "black", alpha = 0.5, size = 0.5) +
              scale_fill_manual(
                values = colores_grafico,
                labels = c("Cliff specialist", "Generalist"))+
              new_scale_fill() +
              geom_segment(data = datos_segmentos,
                           aes(x = 'Generalist', xend = 'Cliff_specialist',
                               y = Generalist, yend = Cliff_specialist, color = Pairs),
                           size = 0.5) +
              geom_point(color= "black", aes(fill = Pairs, shape = Type), size = 1.5) +
              scale_fill_manual(values = coloType) +
              scale_color_manual(
                values = coloType,
                guide  = guide_legend(
                  theme = theme(
                    legend.text = element_text(face = "italic")))) +
              scale_shape_manual(values = formas) +
              scale_x_discrete(labels = c("Cliff specialist", "Generalist")) +
              labs(title = NULL,
                   x = NULL,
                   y = "Relative flower area (mm2/cm)") +
              theme_classic(base_size = 15) +
              theme(legend.position = "none",
                    legend.key = element_rect(fill = NA, color = NA),
                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                    axis.text = element_text(size = 6.5),
                    axis.title = element_text(size = 9, face = "bold"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
              geom_text(label = "**", x = 1.2, y = 80, size=4) +
              annotate("text", label = "R2m = 0.09", x = 2.1, y = 286, size=2) +
              annotate("text", label = "R2c = 0.97", x = 2.1, y = 266, size=2) 
            
            
            
            
              
          ##### ------------------ #####
          ##### -- All together -- #####
          ##### ------------------ #####
              
            # Combine graphs in two columns
            combined_plot <- wrap_plots(graphs, ncol = 2) + plot_layout(guides = "collect")
            
            # Add general legend
            final_plot <- combined_plot +
              plot_annotation(
                title = "Trait comparisson between cliff specialists and generalist species",
                theme = theme(plot.title = element_text(size = 10.5, face = "bold", hjust = 0.5))
              ) +
              theme(
                legend.position = "right",
                legend.title = element_text(size = 9, face = "bold"),
                legend.text = element_text(size = 8)
              ) +
              guides(
                fill = "none",
                shape = "none",
                color = guide_legend(
                  title = "Pairs",
                  theme = theme(
                    legend.text = element_text(face = "italic"))))

            # Show final graph
            print(final_plot)
            
            # Save it as an image
            ggsave(filename = "Figure2_lmm.png", 
                   plot = final_plot,
                   path = "Figures",
                   width = 14, 
                   height = 20, 
                   units = "cm", 
                   dpi = 300)
              
                   
            
            
            
            
            
            
    ######## --------------------------- ########
    ######## -- Multivariate analysis -- ########
    ######## --------------------------- ########      
    
      # What I want here is to see how "Type" and "Pairs" affect all the traits together
      # and its coordination. Well, actually I only want to see the effect of "Type", but
      # as the effect of "Pairs" looks really strong, I need to take it into account 
      # as it is producing "noise". 
      
      # For that, I can do ordenation plots (PCA, MDS, RDA) and visualize the data
      # (there are some differences between them, mainly between RDA (supervised) and
      # the others (unsupervised)). I should choose the visualization plot were the
      # differences that I am looking for are more visible, which in my case is PCA.
            
      # Other than that, I should try to analyse numerically if the differences that
      # I am seeing in the plot are significant or not, for which I am using RDA and
      # PERMANOVA (I could just use one but it is always more robust to use more than one).
      # I end with a BETADISPER analysis, that tells me if the differences are due 
      # to dispersion or just the values of the traits. 
            
      # In my case, I see significant differences in both "Pairs" (of course) and "Type",
      # and interpreting the plot and BETADISPER I conclude that the differences are due
      # to both values and dispersion (see the manuscript for more)
            
          
          
            
            
            
          
      ####### ---------------------- #######
      ####### -- Ordenation plots -- #######
      ####### ---------------------- #######
       
        # I am doing a PCA. I also did a MDS but it was less clear and the script is
        # already too long, so I deleted it. The aim here is to plot the traits of 
        # the different individuals in a reduced dimensionality space, and try to see
        # differences between cliff specialist and generalist species.
          
            
            
            
          
              
        ###### --------- ######
        ###### -- PCA -- ######
        ###### --------- ######
            
          # The PCA (Principal Component Analysis crates new PC and orders them 
          # by their eigenvalues or variance explained. I chose the 2 Principal
          # Components that explained more and plot the different individuals sampled
          # there. Then, adding colors, shapes, lines and polygons clear differences
          # between cliff specialists and generalists species can be seen.
            
            
            
            
            
          ##### --------------- #####
          ##### -- Libraries -- #####
          ##### --------------- ##### 
            
            library(ggplot2)
            library(tidyr)
            library(dplyr)
            library(ggalt)
            library(wesanderson)
            library(RColorBrewer)
            library(grid)
            
            
            
            
            
          ##### --------------- #####
          ##### -- Procedure -- #####
          ##### --------------- #####
            
            # For the procedure fist I do the matrix with the variables to introduce
            # in the PCA and then I do the PCA. PCA uses euclidean distances, which
            # is not bad for my data (not lots of 0s for example).
            
            
            
            
            #### -------------------------- ####
            #### -- Matrix ready and PCA -- ####
            #### -------------------------- ####
            
              # I have to make a matrix with my data so... 
              Datos_m_Ord <- Datos_m_individ[!is.na(Datos_m_individ$Flower_area),]
              matrizmorf <- Datos_m_Ord[ ,c(8, 11, 12, 18, 20, 27, 30, 31)]
              rownames(matrizmorf) <- Datos_m_Ord$Cod_ind
            
              # Do the PCA  
              pca <- prcomp(matrizmorf, scale=TRUE) 
              
              ## plot pc1 and pc2
              plot(pca$x[,1], pca$x[,2])
              
              
              
              
            #### ------------------------------------ ####
            #### -- Contribution of traits and PCs -- ####
            #### ------------------------------------ ####
            
              # get the name of the top 10 traits that contribute most to pc1.
              loading_scores1 <- pca$rotation[,1]
              gene_scores1 <- abs(loading_scores1) # get the magnitudes
              gene_score_ranked1 <- sort(gene_scores1, decreasing=TRUE)
              top_genes1 <- names(gene_score_ranked1)
              
              # show the scores (and +/- sign)
              pca$rotation[top_genes1,1]
              
              # get the name of the top 10 traits that contribute most to pc2.
              loading_scores2 <- pca$rotation[,2]
              gene_scores2 <- abs(loading_scores2) # get the magnitudes
              gene_score_ranked2 <- sort(gene_scores2, decreasing=TRUE)
              top_genes2 <- names(gene_score_ranked2)
              
              # show the scores (and +/- sign)
              pca$rotation[top_genes2,2]
            
              ## make a scree plot to see the variation explained by the PCs
              pca.var <- pca$sdev^2
              pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
              barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
              
              
              
              
            #### ----------------------------- ####
            #### -- Plot. Start of figure 3 -- ####
            #### ----------------------------- ####  
            
              # Colour for the type of species
              colores_grafico <- c("Generalist" = "#009E73", "Cliff_specialist" = "#E69F00")
              
              # colour for the pairs
              coloType <- c("Ramonda" = "#F781BF",
                            "Antirrhinum" = "#d62728",
                            "Asperula" = "green",
                            "Campanula" = "purple",
                            "Hieracium" = "yellow",
                            "Lonicera" = "#2ca02c",
                            "Androsace" = "#0000CD",
                            "Petrocoptis" = "#A65628",
                            "Saxifraga" = "#ff7f0e",
                            "Hypericum" = "black",
                            "Sarcocapnos" = "#2F4F4F",
                            "Dioscorea" = "#00CED1")
              
              # colour for the pairs
              coloType1 <- c("Ram" = "#F781BF",
                            "Ant" = "#d62728",
                            "Asp" = "green",
                            "Cam" = "purple",
                            "Hie" = "yellow",
                            "Lon" = "#2ca02c",
                            "And" = "#0000CD",
                            "Pet" = "#A65628",
                            "Sax" = "#ff7f0e",
                            "Hyp" = "black",
                            "Sar" = "#2F4F4F",
                            "Dio" = "#00CED1")
              
              # shape 
              formas <- c("Generalist" = 24, "Cliff_specialist" = 21)
              
              
              
              ### ---------------------- ###
              ### -- Dataframe of PCA -- ###
              ### ---------------------- ###
              
                # Create the data frame for ggplot   
                pca.data <- data.frame(X=pca$x[,1], Y=pca$x[,2])
                pca.data <- cbind(pca.data, Type = Datos_m_Ord$Type) #add grouping variable
                pca.data <- cbind(pca.data, Pairs = Datos_m_Ord$Pairs) #add grouping variable
                head(pca.data)
                
                
                
              ### ------------------------- ###
              ### -- Loadings for arrows -- ###
              ### ------------------------- ###
            
                # Extract the loaings from PCA
                loadings <- pca$rotation
                
                # Rescale the loadings to adjust to graph
                loadings <- as.data.frame(loadings)
                loadings$PC1 <- loadings$PC1 * max(abs(pca$x[,1]))
                loadings$PC2 <- loadings$PC2 * max(abs(pca$x[,2]))
                
                # Add trait names
                loadings$variable <- rownames(loadings)
                
                # Add custom labeller
                custom_label <- c(
                  "Plant_max_veg_height" = "Plant size",
                  "Flower_area" = "Flower area",
                  "LDMC_without_petiole" = "LDMC",
                  "Leaf_fresh_weight" = "Leaf weight",
                  "Leaf_area" = "Leaf area",
                  "Leaf_thickness" = "Leaf thickness",
                  "SLA_without_petiole" = "SLA",
                  "Relative_flower_area" = "Relative flower area")
            
                # Add a new column to loadings with custom labels
                loadings$custom_labels <- custom_label[loadings$variable]
                
                
                
              ### ----------- ###
              ### -- Hulls -- ###
              ### ----------- ###
            
                # To do the hulls
                grp.a <- pca.data[pca.data$Type == "Generalist", ][chull(pca.data[pca.data$Type == "Generalist", c("X", "Y")]), ]  # hull values for grp A
                grp.b <- pca.data[pca.data$Type == "Cliff_specialist", ][chull(pca.data[pca.data$Type == "Cliff_specialist", c("X", "Y")]), ]  # hull values for grp A
                hull.data <- rbind(grp.a, grp.b)  # combine grp.a and grp.b
                hull.data
            
                
                
              ### ------------ ###
              ### -- Spider -- ###
              ### ------------ ###
           
                # For the spider
                cent1 <- aggregate(cbind(X, Y) ~ Pairs, data = pca.data, FUN = mean)
                Abv <- c("And", "Ant", "Asp", "Cam", "Dio", "Hie", "Hyp", "Lon", "Pet", "Ram", "Sar", "Sax")
                cent <- cbind(cent1, Abv)

                segs <- merge(pca.data, setNames(cent, c('Pairs','oX','oY')),
                              by = 'Pairs', sort = FALSE)
                
                
                
              ### ---------------- ###
              ### -- Final plot -- ###
              ### ---------------- ### 
          
                # FINAL PLOT
                pca.plot.morf <- ggplot(pca.data, aes(x = X, y = Y)) +
                  geom_segment(data = segs, aes(xend = oX, yend = oY), linewidth = 0.1) +
                  geom_point(color = "black", aes(fill= Pairs, shape = factor(Type)), size = 2) +
                  scale_fill_manual(
                    values = coloType,
                    guide  = guide_legend(
                      override.aes = list(shape = 21, size = 1.5),
                      theme = theme(
                        legend.text = element_text(face = "italic")))) +
                  scale_shape_manual(values = formas, guide  = "none") +
                  new_scale_fill() + 
                  geom_point(data = cent, size = 6, shape = 15, colour = "white") +
                  geom_point(data = cent, size = 6, shape = 22) +
                  geom_text(data = cent, size = 2, aes(label = Abv, colour = Abv)) +
                  scale_colour_manual(values = coloType1, guide  = "none") +
                  geom_polygon(data = hull.data, aes(fill = Type), alpha = 0.1) +
                  scale_fill_manual(values = colores_grafico, labels = c("Generalist plants", "Cliff plants")) +
                  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
                  geom_text(data = loadings, aes(x = PC1, y = PC2, label = custom_labels), size = 3, color = "blue", vjust = -1) +
                  annotate("point", x = -7.48, y = 3.7, shape = 24, size = 1.5, color = "black", fill = "grey", stroke = 0.5) + 
                  annotate("point", x = -7.48, y = 3.45, shape = 21, size = 1.5, color = "black", fill = "grey", stroke = 0.5) + 
                  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) + 
                  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
                  labs(colour = "Species pairs", fill = "Type of species") +  
                  coord_fixed() +
                  theme_classic() + 
                  theme(
                    panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"),
                    legend.position = c(0.122, 0.69),
                    legend.background = element_rect(fill = NA, colour = "black"), 
                    legend.text = element_text(size = 7),
                    legend.title = element_text(size = 9, face ="bold"),
                    legend.key.size = unit(0.3, "cm"),     # Tamaño de la clave de la leyenda
                    legend.spacing = unit(0.07, "cm"),      # Espaciado entre las claves de la leyenda
                    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
                    axis.title.y = element_text(size = 10),  
                    axis.title.x = element_text(size = 10),
                    axis.text = element_text(size = 8)) +
                  ggtitle("PCA of multiple physiological traits") 
                
                # plot
                print(pca.plot.morf)
                
                # Save it as an image
                ggsave(filename = "Figure3_PCA.png", 
                       plot = pca.plot.morf,
                       path = "Figures",
                       width = 16, 
                       height = 12, 
                       units = "cm", 
                       dpi = 300)
            
            
                
            
            
            
            
      ####### ------------------------ #######
      ####### -- Numerical analysis -- #######
      ####### ------------------------ #######
      
        # In the PCA it look like there were differences for generalist and cliff
        # species in general and inside pairs of species. It looked like they were 
        # apart from each other, but also that cliff species were more disperse. 
        # Now the idea is to test this differences in numbers. 
      
        # For that we are doing an RDA, where we can see if there is significant
        # difference between cliff species and generalist accounting for pairs,
        # then we are doing a PERMANOVA analysis to check again for the differences,
        # and we end up with PERMDISP to see the differences in dispersion between
        # groups.
                
        # We conclude that there is differences in both position and dispersion
        # for cliff specialist species and generalist species. 
   
          
              
              
              
                  
        ###### --------- ######
        ###### -- RDA -- ######
        ###### --------- ######
      
          # Redundancy Analysis. It is a supervised method where the distance gets
          # maximized by the explanatory matrix. This means that is not represent
          # the maximum variance, but the best way to explain based on one or more variables
          # One of the good thing about RDA is that you can add "Condition" in a model
          # to add Random factors, or the factors that make noise and you are not interested
          # in. You can also check if the model is significant or not. Here more info;
          # https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/rda-and-dbrda/
         
             
              
              
               
          ##### --------------- #####
          ##### -- Libraries -- #####
          ##### --------------- ##### 
          
            library(ggplot2)
            library(vegan)
                
          
          
          
          
          ##### --------------- #####
          ##### -- Procedure -- #####
          ##### --------------- #####
            
            # First we do the data frame with the traits 
            Datos_m_Ord <- Datos_m_individ[!is.na(Datos_m_individ$Flower_area),]
            matrizmorf <- Datos_m_Ord[ ,c(8, 11, 12, 18, 20, 27, 30, 31)]
            rownames(matrizmorf) <- Datos_m_Ord$Cod_ind
            
            # Data frame of explanatory variables
            explanatory <- Datos_m_Ord[, c("Type", "Pairs")]
            
            # Convert to factor
            explanatory$Type <- as.factor(explanatory$Type)
            explanatory$Pairs <- as.factor(explanatory$Pairs)
            
            # Analysis RDA, using Pairs as condition and as explanatory variable
            rda_model <- rda(matrizmorf ~ explanatory$Type + Condition (explanatory$Pairs), data = explanatory)
            rda_model1 <- rda(matrizmorf ~ explanatory$Type * explanatory$Pairs, data = explanatory)
            
            # Print models
            print(rda_model)
            print(rda_model1)
            
            # R square for both
            RsquareAdj(rda_model)
            RsquareAdj(rda_model1)
            
            # Summary of RDA model
            sum_rda_model <- summary(rda_model)
            sum_rda_model
            sum_rda_model$concont
            sum_rda_model$cont
            
            # Summary of RDA model1
            sum_rda_model1 <- summary(rda_model1)
            sum_rda_model1
            sum_rda_model1$concont
            sum_rda_model1$cont
            
            # Anova of RDA model
            anova(rda_model)
            anova(rda_model, by="axis", perm.max=500) # test axes for significance
            anova(rda_model, by="terms", permu=200) # test for sign. environ. variables
            
            # Anova of RDA model1
            anova(rda_model1)
            anova(rda_model1, by="axis", perm.max=500) # test axes for significance
            anova(rda_model1, by="terms", permu=200) # test for sign. environ. variables
            
       
          
            
            
            
        ###### --------------- ######
        ###### -- PERMANOVA -- ######
        ###### --------------- ######
   
          # PERMANOVA (and adonis2) is another method to check for significant 
          # differences for a group of traits based on one or more explanatory
          # variables. 
          
          # Currently, adonis2() does not permit the specification of random effects.
          # An alternative way to account for random effects is to use sequential
          # sums of squares and fit the term that would otherwise be designated 
          # as a random effect as the very first term in the model.  Doing so results
          # in a model in which as much variation as possible is explained with that
          # term before the other factor(s) are evaluated.
            
          # That is why I am doing my models with "Pairs" first and then "Type".
          
          # More usefull information here:
          # https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permanova/
        
            
            
            
            
          ##### --------------- #####
          ##### -- Libraries -- #####
          ##### --------------- ##### 
          
            library(ggplot2)
            library(ggpubr)
            library(vegan)
            
          
          
          
          
          ##### --------------- #####
          ##### -- Procedure -- #####
          ##### --------------- #####
          
            # First the dataframe
            Datos_m_Ord <- Datos_m_individ[!is.na(Datos_m_individ$Flower_area),]
            matrizmorf <- Datos_m_Ord[ ,c(8, 11, 12, 18, 20, 27, 30, 31)]
            rownames(matrizmorf) <- Datos_m_Ord$Cod_ind
            
            # And adonis2 for the model
            perm_model <- adonis2(Datos_m_Ord[ ,c(8, 11, 12, 18, 20, 27, 30, 31)] ~ Pairs * Type,
              data = Datos_m_Ord,
              method = "euclidean")
            
            # Check the model
            perm_model
          
      
          
      
            
                
        ###### -------------- ######
        ###### -- PERMDISP -- ######
        ###### -------------- ######

          # Having done first RDA and PERMANOVA, I know that there are differences
          # for generalist species and cliff specialist species for the set of traits
          # analysed. What I dont know is if those differences are in dispersion, 
          # location, or both. To answer that I do a PERMDISP. If it is not significant
          # all the differences are due to location. If it is significiant (as it is),
          # it is not possible on the basis of these two tests to determine conclusively
          # whether the groups differ only in dispersion or both dispersion and location.
          # We’ve visually explored our data and decided that it is due to both.
            
          # More information here:
          # https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permdisp/
      
            
            
      
                  
          ##### --------------- #####
          ##### -- Libraries -- #####
          ##### --------------- ##### 
          
            library(ggplot2)
            library(vegan)
            
          
          
          
          
          ##### --------------- #####
          ##### -- Procedure -- #####
          ##### --------------- #####
            
            # First the matrix
            Datos_m_Ord <- Datos_m_individ[!is.na(Datos_m_individ$Flower_area),]
            matrizmorf <- Datos_m_Ord[ ,c(8, 11, 12, 18, 20, 27, 30, 31)]
            rownames(matrizmorf) <- Datos_m_Ord$Cod_ind
            
            # The the explanatory matrix
            explanatory <- Datos_m_Ord[, c("Type", "Pairs")]
            
            # Calculate the distances with euclidean distance
            dist_matrix <- vegdist(matrizmorf, method="euclidean")
            
            # Betadisper model (with Type)
            betadisp_model <- betadisper(d = dist_matrix,
                                         group = explanatory$Type,
                                         type = "centroid")
            
            # Betadisper model1 (with Pairs)
            betadisp_model1 <- betadisper(d = dist_matrix,
                                         group = explanatory$Pairs,
                                         type = "centroid")
            
            # Check the models
            betadisp_model
            betadisp_model1
            
            # See the distances
            betadisp_model$distances
            betadisp_model$distances1
            
            # Anova of the models
            anova(betadisp_model)
            anova(betadisp_model1)
            
            # Tukey of the models
            TukeyHSD(betadisp_model)
            TukeyHSD(betadisp_model1)
            
            # Boxplot to see dispersion of the models
            boxplot(betadisp_model)
            boxplot(betadisp_model1)
            
            # Plot of the models
            plot(betadisp_model)
            plot(betadisp_model1)
            
          
  
                
            
            
        ###### ----------------- ######
        ###### -- Final table -- ######
        ###### ----------------- ######
            
          # The idea is to do a Table with the information of the numerical models
          # to introduce it after as part of Figure 3.
          
            
            
          
              
          ##### --------------- #####
          ##### -- Libraries -- #####
          ##### --------------- ##### 
            
            library(gridExtra)
            library(gt)
            library(tidyverse)
            library(ggplot2)
            library(cowplot)
            library(magick)
            library(grid)
            library(ggimage)
            
            
            
            
            
          ##### --------------- #####
          ##### -- Procedure -- #####
          ##### --------------- #####
            
            # Create the table with gt package!
            
            
            
            
            #### ------------------ ####
            #### -- Create table -- ####
            #### ------------------ ####
            
              # First we create the dataframe with the information from the analysis
              Analisis <- c("RDA", "RDA", "RDA", "RDA", "PERMANOVA", "PERMANOVA", "PERMANOVA", "PERMANOVA", "BETADISPER", "BETADISPER", "BETADISPER", "BETADISPER")
              Trait <- c("Pairs", "Pairs", "Type", "Type", "Pairs", "Pairs", "Type", "Type", "Pairs", "Pairs", "Type", "Type")
              Statistic <- c("R2", "Pr(>F)", "R2", "Pr(>F)", "R2", "Pr(>F)", "R2", "Pr(>F)", "R2", "Pr(>F)", "R2", "Pr(>F)")
              Value <- c("0.8994", ">0.001***", "0.0095", ">0.001***", "0.8207", ">0.001***", "0.0098", ">0.001***", NA, ">0.001***", NA, "0.003**")
              table <- data.frame(Analisis = Analisis, Trait = Trait, Statistic = Statistic, Value = Value)
              
              # Pivot the table to see it with gt
              table_wide <- table %>%
                pivot_wider(names_from = c(Analisis, Trait), values_from = Value)
              
              # Create gt table
              gt_table <- gt(data = table_wide) %>%
                cols_label(
                  Statistic = ""
                ) %>%
                tab_spanner(
                  label = "RDA",
                  columns = c(RDA_Pairs, RDA_Type)
                ) %>%
                tab_spanner(
                  label = "PERMANOVA",
                  columns = c(PERMANOVA_Pairs, PERMANOVA_Type)
                ) %>%
                tab_spanner(
                  label = "BETADISPER",
                  columns = c(BETADISPER_Pairs, BETADISPER_Type)
                ) %>%
                cols_label(
                  RDA_Pairs = "Pairs",
                  RDA_Type = "Type",
                  PERMANOVA_Pairs = "Pairs",
                  PERMANOVA_Type = "Type",
                  BETADISPER_Pairs = "Pairs",
                  BETADISPER_Type = "Type"
                ) %>%
                fmt_missing(
                  columns = everything(),
                  missing_text = "NA"
                ) %>%
                cols_align(
                  align = "center",
                  columns = everything()
                ) %>%
                tab_style(
                  style = cell_text(weight = "bold"),
                  locations = cells_column_labels(everything())
                ) %>%
                tab_options(
                  table.font.names = "Arial",
                  table.font.size = 12,
                  table.border.top.color = "black",
                  table.border.bottom.color = "black",
                  heading.title.font.size = 16,
                  heading.subtitle.font.size = 14,
                  column_labels.border.bottom.width = px(3),
                  column_labels.border.bottom.color = "black",
                  table.border.bottom.width = px(3))
              
              # Show table
              gt_table
              
              # Save table
              gtsave(gt_table, filename = "Multivariate_table.png")
            
              
            
            
              
              
                  
      ####### ------------------- #######
      ####### -- Figure 3: PCA -- #######
      ####### ------------------- #######
            
        # Insert image into plot
        p_with_table <- pca.plot.morf +
          geom_image(
            data = tibble(X = -5.3, Y = -3.2),
            aes(image = "Multivariate_table.png"),
            size = 0.6
            )
              
        # Show plot
        print(p_with_table)
      
        # Save it as an image
        ggsave(filename = "Figure3_PCA.png",
               path = "Figures",
               plot = p_with_table, 
               width = 16, 
               height = 12, 
               units = "cm", 
               dpi = 300)
        
        # Remove table
        file.remove("Multivariate_table.png")
    
  



 
    
 
              
  ######### ----------------------------------- #########
  ######### ----------------------------------- #########
  #########             COMPLEMENTS             ######### 
  ######### ----------------------------------- #########
  ######### ----------------------------------- #########
        
    # I name this section as complements because it is true that this are big parts
    # of the script that are not mandatory for the models and the data analysis. 
    # Anyways, both the phylogenomic tree and the map are important parts of the
    # manuscript.
        
    # What I am doing here then is a map to show where I sampled the studied species
    # and some context of the area, and then a phylogenomic tree to check the relatedness
    # of my species. They are actually Figures 1 and Supplementary figure 1 in the manuscript.
        
     
        
        
      
        
        
          
    ######## ---------------------------- ########
    ######## ----------- Maps ----------- ######## 
    ######## ---------------------------- ########

      # We want to do some maps with the location of the samples in our data.
      # It is actually just one map with the location of the samples and the context.
      
      # For this part we need some downloaded layers that are in the github folder,
      # more concrete in the "Maps" folder,
    
    
    
        
    
        
            
      ####### --------------------- #######
      ####### ----- Libraries ----- #######
      ####### --------------------- #######
      
        library(sf)
        library(ggplot2)
        library(rnaturalearth)
        library(rnaturalearthdata)
        library(dplyr)
        library(ggspatial)  
        library(leaflet)
        library(leaflet.extras)
        library(mapSpain)
        library(tidyterra)
        library(raster)
        library(terra)
        library(ggnewscale) 
        library(cowplot)
            
      
        
        
        
        
        
      ####### --------------------- #######
      ####### ----- Procedure ----- #######
      ####### --------------------- #######
            
        # What we are doing is to get the dataframe with the samples ready, 
        # to load some layers and tranform them (so all have the same crs) to 
        # do the base map, and to graph the map. For the map we are doing a first
        # map, then a context map, and then the final map (Figure 1) where we add
        # them both together.
            
            
            
        
        
        
        ###### ------------------------------- ######
        ###### ----- Get dataframe ready ----- ######
        ###### ------------------------------- ######
        
          # I want to do a simple Datos_sf_filtrados data frame to map it
          Datos_sf_filtrados <- Datos[!is.na(Datos$Coord_X), ]
          
          # summarie by species
          Datos_sf_filtrados <- Datos_sf_filtrados %>%
            group_by(Species, Locality) %>%
            slice(1)
          
          # Duplicate Coordinates
          Datos_sf_filtrados$Cor_X <- Datos_sf_filtrados$Coord_X
          Datos_sf_filtrados$Cor_Y <- Datos_sf_filtrados$Coord_Y
          
          # Convert to sf object
          Datos_sf_filtrados <- st_as_sf(Datos_sf_filtrados, coords = c("Coord_X", "Coord_Y"), crs = 4326)
          
          # Remove streptocarpus from map, as it is recolected from pots and not from nature
          Datos_sf_filtrados <- Datos_sf_filtrados[Datos_sf_filtrados$Species!="Streptocarpus ionanthus", ]
          
          
          
          
          
          
        ###### ---------------------------------- ######
        ###### ----- Data load and download ----- ######
        ###### ---------------------------------- ######
      
          # Get layer for Huesca (province) extension
          Huesca <- esp_get_munic_siane(region = "Huesca") %>%
            mutate(Provincia = esp_dict_translate(ine.prov.name, "es"))
        
          # Get digital elevations model for the base of the map
            # For this, two dem have been downloaded from 
            # https://centrodedescargas.cnig.es/CentroDescargas/ to cover all the study
            # area, from "Modelo Digital del Terreno de 1ª cobertura (2009-2015) con
            # paso de malla de 200 metros (MDT200) de España"
            dem31 <- rast("Maps/PNOA_MDT200_ETRS89_HU31_Huesca.tif")
            dem30 <- rast("Maps/PNOA_MDT200_ETRS89_HU30_Huesca.tif")
          
          # Get other layers to complete base map from 
            # https://centrodedescargas.cnig.es/CentroDescargas/ for study area
            # data from Base Cartográfica Nacional 1:200.000 
            LIM_ADM_sf <- read_sf("Maps/BCN200_0101S_LIM_ADM.shp") # limits
            L_RIO_sf <- read_sf("Maps/BCN200_0301L_RIO.shp") # rivers
            NUC_POB_sf <- read_sf("Maps/BCN200_0501S_NUC_POB.shp") # populations
            AUTOVIA_sf <- read_sf("Maps/BCN200_0601L_AUTOVIA.shp") # roads
            CARR_NAC_sf <- read_sf("Maps/BCN200_0603L_CARR_NAC.shp") # roads
            CARR_AUTON_sf <- read_sf("Maps/BCN200_0604L_CARR_AUTON.shp") # roads
            
          
          
          
          
          
        ###### ----------------------------------------------- ######
        ###### ----- Data transformation and adjustments ----- ######
        ###### ----------------------------------------------- ######
          
          # Reproyect dem31 to be on the same crs of dem30
          dem31_reprojected <- project(dem31, crs(dem30))
          
          # Resample dem31
          dem31_resampled <- resample(dem31_reprojected, dem30)
          
          # Unification of dems
          dem_combined <- merge(dem30, dem31_resampled)
          
          # Transforme diferent shapefiles layers to dem crs
          Huesca <- st_transform(Huesca, crs(dem_combined))
          Samples <- st_transform(Datos_sf_filtrados, crs(dem_combined))
          LIM_ADM_sf <- st_transform(LIM_ADM_sf, crs(dem_combined))
          L_RIO_sf <- st_transform(L_RIO_sf, crs(dem_combined))
          NUC_POB_sf <- st_transform(NUC_POB_sf, crs(dem_combined))
          CARR_NAC_sf <- st_transform(CARR_NAC_sf, crs(dem_combined))
          AUTOVIA_sf <- st_transform(AUTOVIA_sf, crs(dem_combined))
          CARR_AUTON_sf <- st_transform(CARR_AUTON_sf, crs(dem_combined))
          
          # Cut dem to match study area "Huesca" extension
          dem_huesca <- mask(dem_combined, vect(Huesca))
          
          # Transform into data frame for visualization
          dem_huesca_df <- as.data.frame(dem_huesca, xy = TRUE)
          colnames(dem_huesca_df) <- c("x", "y", "elevation")
          
          
          
          
          
          
        ###### ---------------------- ######
        ###### ----- Graph maps ----- ######
        ###### ---------------------- ###### 
          
          # Now we graph the maps. First we define shape and colors (same as in
          # all the script) and then we plot the maps.
          
          # color for elevation palette
          elevation_colors <- c("darkgreen", "yellow", "saddlebrown", "white")
          elevation_colors <- c("grey100", "grey80", "grey60", "grey40", "grey20", "grey1")
          

          # Colour for the type of species
          colores_grafico <- c("Generalist" = "#009E73", "Cliff_specialist" = "#E69F00")
          
          # colour for the pairs
          coloType <- c("Ramonda" = "#F781BF",
                        "Antirrhinum" = "#d62728",
                        "Asperula" = "green",
                        "Campanula" = "purple",
                        "Hieracium" = "yellow",
                        "Lonicera" = "#2ca02c",
                        "Androsace" = "#0000CD",
                        "Petrocoptis" = "#A65628",
                        "Saxifraga" = "#ff7f0e",
                        "Hypericum" = "black",
                        "Sarcocapnos" = "#2F4F4F",
                        "Dioscorea" = "#00CED1")
          
          # shape 
          formas <- c("Generalist" = 24, "Cliff_specialist" = 21) 
          
        
          
          
          
          ##### -------------------- #####
          ##### ----- Main map ----- #####
          ##### -------------------- #####
          
            # First the main map, then the context map and then we ensamble them
        
          
          
        
            #### ------------------------------------ ####
            #### ----- Main map without legends ----- ####
            #### ------------------------------------ ####
          
              # Create the map
              map_plot <- ggplot() +
                geom_sf(
                  data = LIM_ADM_sf,
                  color = "black",
                  size = 1,
                  show.legend = FALSE) +
                geom_sf(
                  data = L_RIO_sf,
                  color = "#00BFFF", 
                  aes(alpha = 0.6),
                  size = .1,
                  show.legend = FALSE) +
                geom_sf(
                  data = AUTOVIA_sf,
                  color = "grey50",
                  aes(alpha = 0.6),
                  size = 0.15,
                  show.legend = FALSE) +
                geom_sf(
                  data = CARR_NAC_sf,
                  color = "grey50", 
                  aes(alpha = 0.6),
                  size = 0.15,
                  show.legend = FALSE) +
                geom_sf(
                  data = CARR_AUTON_sf,
                  color = "grey50",
                  aes(alpha = 0.6),
                  size = 0.15,
                  show.legend = FALSE) +
                geom_sf(
                  data = NUC_POB_sf,
                  fill = "red",
                  color = "red4", 
                  aes(alpha = 0.6),
                  size = 0.15,
                  show.legend = FALSE) +
                geom_raster(
                  data = dem_huesca_df,
                  aes(x = x, y = y, fill = elevation, alpha = 0.6),
                  show.legend = TRUE) +
                scale_fill_gradientn(colors = elevation_colors, na.value = NA, name = "Elevation (m)") +
                new_scale_fill() +
                geom_sf(
                  data = Samples,
                  color = "black",
                  aes(fill = Pairs, shape = Type),
                  size = 3) +
                scale_fill_manual(name = "Pairs", values = coloType) +
                scale_shape_manual(name = "Type", values = formas) +
                annotation_north_arrow(
                  location = "tr", which_north = "true",
                  style = north_arrow_orienteering,
                  height = unit(0.7, "cm"),
                  width = unit(0.7, "cm")) +
                annotation_scale(location = "br", width_hint = 0.2, height = unit(0.2, "cm")) +
                labs(
                  title = "Location of the samples",
                  x = "Longitude",
                  y = "Latitude") +
                theme_minimal() +
                theme(legend.position = "right",
                      panel.background = element_rect(
                        fill = "white", colour = "black",
                        size = 1, linetype = "solid"),
                      plot.background = element_rect(
                        fill = "white", colour = NA,),
                      plot.title = element_text(hjust = 0.5, face = "bold")) +
                guides(alpha = "none")   

          
          
          
            #### -------------------------- ####
            #### ----- Manual legends ----- ####
            #### -------------------------- ####
          
              # Manual legend for "Type"
              type_legend1 <- ggplot(data.frame(Type = c("Generalist", "Cliff specialist"),
                                               Shape = c(24, 21)), aes(x = 1, y = Type)) +
                geom_point(aes(shape = Type), size = 3) +
                scale_shape_manual(name = "Type", values = c("Generalist" = 24, "Cliff specialist" = 21)) +
                theme_minimal() +
                theme(legend.position = "right",
                      legend.title = element_text(face = "bold", size = 10),
                      legend.text = element_text(size = 8),
                      plot.background = element_rect(
                        fill = "white", colour = NA))
              
              # Manual legend for "Pairs"
              pairs_legend1 <- ggplot(data.frame(Pairs = names(coloType), Color = coloType), aes(x = 1, y = Pairs)) +
                geom_point(color = "black", aes(fill = Pairs), shape = 22, size = 4) +
                scale_fill_manual(name = "Pairs", values = coloType) +
                theme_minimal() +
                theme(legend.position = "right",
                      legend.title = element_text(face = "bold", size = 10),
                      legend.text = element_text(face = "italic", size = 8),
                      legend.key.size = unit(0.6, "cm"),    
                      legend.spacing = unit(0.1, "cm"),      
                      legend.margin = margin(0, 0, 0, 0),      
                      legend.box.margin = margin(0, 0, 0, 0),    
                      plot.background = element_rect(
                        fill = "white", colour = NA))
              
              # Manual legend for "elevation"
              elevation_legend1 <- ggplot() +
                geom_raster(
                  data = dem_huesca_df,
                  aes(x = x, y = y, fill = elevation, alpha = 0.6),
                  show.legend = TRUE) +
                scale_fill_gradientn(colors = elevation_colors, na.value = NA, name = "Elevation (m)") +
                guides(alpha = "none") +
                theme_minimal() +
                theme(legend.position = "right",
                      legend.title = element_text(face = "bold", size = 10),
                      legend.text = element_text(size = 8),
                      legend.key.size = unit(0.3, "cm"),    
                      legend.spacing = unit(0.1, "cm"),
                      legend.background = element_rect(
                        fill = "white", colour = NA))
              
              # Extract legends
              type_legend <- get_legend(type_legend1)
              pairs_legend <- get_legend(pairs_legend1)
              elevation_legend <- get_legend(elevation_legend1)
          
              
              
              
            #### --------------------------------------- ####
            #### ----- Combine main map and legends----- ####
            #### --------------------------------------- ####
              
              # For this we are...
              
              # Main map and legends combined
              almost_final_map <- plot_grid(
                map_plot + theme(legend.position = "none"),
                plot_grid(type_legend, pairs_legend, elevation_legend, ncol = 1),
                rel_widths = c(3, 1)
              ) +
                annotate("text",
                         x = 0.44, 
                         y = 0.035,
                         label = "Basemap made with basic layers from IGN",
                         size = 3,
                         color = "grey20") +
                theme(
                panel.background = element_rect(
                  fill = "white", colour = "black",
                  size = 1, linetype = "solid"),
                plot.background = element_rect(
                  fill = "white", colour = "black",
                  size = 1, linetype = "solid"),
                legend.background = element_rect(
                  fill = "white", colour = "black",
                  size = 1, linetype = "solid"),
                plot.caption = element_text(
                  size = 10, color = "grey30",
                  hjust = .5, vjust = 0.1
                )) + 
                annotate(
                  "point",
                  x = 0.272,
                  y = 0.768,
                  shape = 22,  
                  size = 4,
                  color = "black",
                  fill = "white",   
                  stroke = 0.5  
                ) + 
                annotate(
                  "point",
                  x = 0.36,
                  y = 0.758,
                  shape = 22,  
                  size = 4,
                  color = "black",
                  fill = "white",   
                  stroke = 0.5  
                ) + 
                annotate(
                  "point",
                  x = 0.264,
                  y = 0.705,
                  shape = 22,  
                  size = 4,
                  color = "black",
                  fill = "white",   
                  stroke = 0.5  
                ) + 
                annotate(
                  "point",
                  x = 0.468,
                  y = 0.724,
                  shape = 22,  
                  size = 4,
                  color = "black",
                  fill = "white",   
                  stroke = 0.5  
                ) + 
                annotate(
                  "point",
                  x = 0.485,
                  y = 0.686,
                  shape = 22,  
                  size = 4,
                  color = "black",
                  fill = "white",   
                  stroke = 0.5  
                ) + 
                annotate(
                  "point",
                  x = 0.518,
                  y = 0.675,
                  shape = 22,  
                  size = 4,
                  color = "black",
                  fill = "white",   
                  stroke = 0.5  
                ) +
                annotate(
                  "rect",
                  xmin = 0.2, xmax = 0.26,
                  ymin = 0.80, ymax = 0.84,
                  alpha = 0.5,
                  fill = "pink",
                  color = "black"
                ) +
                annotate(
                  "rect",
                  xmin = 0.4, xmax = 0.46,
                  ymin = 0.80, ymax = 0.84,
                  alpha = 0.5,
                  fill = "pink",
                  color = "black"
                )+
                annotate(
                  "rect",
                  xmin = 0.54, xmax = 0.6,
                  ymin = 0.77, ymax = 0.81,
                  alpha = 0.5,
                  fill = "pink",
                  color = "black"
                ) +
                annotate(
                  "rect",
                  xmin = 0.37, xmax = 0.45,
                  ymin = 0.62, ymax = 0.66,
                  alpha = 0.5,
                  fill = "pink",
                  color = "black"
                )+
                annotate(
                  "rect",
                  xmin = 0.56, xmax = 0.62,
                  ymin = 0.58, ymax = 0.62,
                  alpha = 0.5,
                  fill = "pink",
                  color = "black"
                ) +
                annotate(
                  "rect",
                  xmin = 0.16, xmax = 0.24,
                  ymin = 0.6, ymax = 0.64,
                  alpha = 0.5,
                  fill = "pink",
                  color = "black"
                ) +
                annotate(
                  "point",
                  x = 0.42,
                  y = 0.82,
                  shape = 21, 
                  size = 3,
                  color = "black",
                  fill = "black",   # Hypericum nummularium
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.44,
                  y = 0.82,
                  shape = 21,  
                  size = 3,
                  color = "black",
                  fill = "purple",   # Campanula cochleariifolia
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.22,
                  y = 0.82,
                  shape = 24,  
                  size = 3,
                  color = "black",
                  fill = "#ff7f0e",   # Saxifraga hirsuta
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.24,
                  y = 0.82,
                  shape = 24,  
                  size = 3,
                  color = "black",
                  fill = "yellow",   # Hieracium murorum
                  stroke = 0.5 
                ) +
                annotate(
                  "point",
                  x = 0.56,
                  y = 0.79,
                  shape = 21, 
                  size = 3,
                  color = "black",
                  fill = "#2ca02c",   # Lonicera pyrenaica
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.58,
                  y = 0.79,
                  shape = 21,  
                  size = 3,
                  color = "black",
                  fill = "#d62728",   # Antirrhinum sempervirens
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.39,
                  y = 0.64,
                  shape = 21,  
                  size = 3,
                  color = "black",
                  fill = "#2F4F4F",   # Sarcocapnos enneaphylla
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.41,
                  y = 0.64,
                  shape = 21,  
                  size = 3,
                  color = "black",
                  fill = "yellow",   # Hieracium candidum
                  stroke = 0.5 
                ) +
                annotate(
                  "point",
                  x = 0.43,
                  y = 0.64,
                  shape = 24, 
                  size = 3,
                  color = "black",
                  fill = "#00CED1",   # Dioscorea communis
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.58,
                  y = 0.60,
                  shape = 21,  
                  size = 3,
                  color = "black",
                  fill = "#00CED1",   # Dioscorea chouardii
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.60,
                  y = 0.60,
                  shape = 21,  
                  size = 3,
                  color = "black",
                  fill = "#0000CD",   # Androsace cylindrica
                  stroke = 0.5  
                ) +
                annotate(
                  "point",
                  x = 0.20,
                  y = 0.62,
                  shape = 24,  
                  size = 3,
                  color = "black",
                  fill = "#A65628",   # Silene vulgaris
                  stroke = 0.5 
                ) +
                annotate(
                  "point",
                  x = 0.22,
                  y = 0.62,
                  shape = 24,  
                  size = 3,
                  color = "black",
                  fill = "#2F4F4F",   # Fumaria officinalis
                  stroke = 0.5 
                ) +
                annotate(
                  "point",
                  x = 0.18,
                  y = 0.62,
                  shape = 24,  
                  size = 3,
                  color = "black",
                  fill = "black",   # Hypericum perforatum
                  stroke = 0.5 
                ) +
                annotate(
                  "segment",
                  x = 0.272, xend = 0.26,
                  y = 0.778, yend = 0.8,
                  colour = "black",
                  size = 0.5,
                  arrow = arrow(length = unit(0.3, "cm"))
                ) +
                annotate(
                  "segment",
                  x = 0.36, xend = 0.4,
                  y = 0.768, yend = 0.8,
                  colour = "black",
                  size = 0.5,
                  arrow = arrow(length = unit(0.3, "cm"))
                ) +
                annotate(
                  "segment",
                  x = 0.264, xend = 0.24,
                  y = 0.695, yend = 0.64,
                  colour = "black",
                  size = 0.5,
                  arrow = arrow(length = unit(0.3, "cm"))
                ) +
                annotate(
                  "segment",
                  x = 0.468, xend = 0.54,
                  y = 0.734, yend = 0.77,
                  colour = "black",
                  size = 0.5,
                  arrow = arrow(length = unit(0.3, "cm"))
                ) +
                annotate(
                  "segment",
                  x = 0.475, xend = 0.45,
                  y = 0.686, yend = 0.66,
                  colour = "black",
                  size = 0.5,
                  arrow = arrow(length = unit(0.3, "cm"))
                ) +
                annotate(
                  "segment",
                  x = 0.528, xend = 0.56,
                  y = 0.675, yend = 0.62,
                  colour = "black",
                  size = 0.5,
                  arrow = arrow(length = unit(0.3, "cm"))
                ) 
              
              # Show the final map so far
              print(almost_final_map)
          
          
              
              
                  
          ##### ----------------------- #####
          ##### ----- Context map ----- #####
          ##### ----------------------- #####
          
            # I want to do a Europe map to show context
          
            # Download Europe data
            europa <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")
            
            # Create the map
            mapa_europa <- ggplot() +
              geom_sf(
                data = europa,
                fill = "gray85",
                color = "gray20") + 
              geom_sf(
                data = LIM_ADM_sf,
                fill = "red",
                color = "red") + 
              coord_sf(xlim = c(-10, 25), ylim = c(35, 55), expand = FALSE) + 
              theme_minimal() +
              theme(
                axis.title = element_blank(),    # Eliminar títulos de los ejes
                axis.text = element_blank(),     # Eliminar texto de los ejes
                axis.ticks = element_blank(),    # Eliminar marcas de los ejes
                panel.grid = element_blank(),    # Eliminar el grid
                panel.background = element_rect(
                  fill = "white", colour = "black",
                  size = 1, linetype = "solid"), # Fondo del panel
                plot.margin = margin(0, 0, 0, 0) # Eliminar márgenes adicionales
              )
         
            # Show the map
            print(mapa_europa)
          
            
            
            
            
          ##### --------------------- #####
          ##### ----- FINAL MAP ----- #####
          ##### --------------------- ##### 
            
            # Create the inset plot
            inset_plot <- ggplotGrob(mapa_europa)
            
            # Combine main map with inset
            final_map <- almost_final_map +
              annotation_custom(
                grob = inset_plot,
                xmin = 0.13, xmax = 0.34, ymin = 0.13, ymax = 0.32) +
              annotate(
                "rect",
                xmin = 0.137, xmax = 0.34,
                ymin = 0.145, ymax = 0.312,
                fill = NA,
                color = "black")
            
            # Show combined map
            print(final_map)
            
            # Save it as an image
            ggsave(filename = "Figure1_Map.png", 
                   plot = final_map,
                   path = "Figures",
                   width = 16, 
                   height = 16, 
                   units = "cm", 
                   dpi = 300)
            
          
         
        
  
  
  
  
    ######## --------------------------- ########
    ######## ---- Phylogenomic tree ---- ########
    ######## --------------------------- ########

      # The idea here is to build a phylogenetic tree with some species based on the calibrated
      # tree from Jin Y, & Qian H. (2019) V.PhyloMaker: an R package that can generate very
      # large phylogenies for vascular plants. Ecography, 42, 1353–1359. For that you will need
      # to check if your species are already on the tree, and if not, to find the closest
      # relative included on the tree and put it in its place.
  
  
  
  
  
              
              
      ####### --------------------- #######
      ####### ----- Libraries ----- #######
      ####### --------------------- #######
  
        library(ggtree)
        library (adephylo)
        library (V.PhyloMaker2)
        library (phytools)
        library (ape)
        library(ggnewscale) 
        library(deeptime)
  
            
            
            
  
            
                      
      ####### --------------------- #######
      ####### ----- Procedure ----- #######
      ####### --------------------- #######
  
        # First we make a template that we can use to make the tree, then we chose
        # the parameters for the tree, then we add our species to the tree, we make 
        # the tree and at the end we plot it (Supplementary figure 1 in manuscript).
  
  
  
  
            
                        
        ###### ------------------------------------- ######
        ###### -- Documents and selection of tree -- ######
        ###### ------------------------------------- ######
    
          # This should be the only mandatory task for your tree to work. Well, 
          # actually other than introducing the species to their closest relative. 
          # It should be included a species list, some other parameters and the selection 
          # of a tree, based on:
          # https://github.com/jinyizju/V.PhyloMaker2/blob/master/Introduction%20to%20the%20'V.PhyloMaker2'%20package
    

                
    
    
          ##### ---------------------------- #####
          ##### - Species list with format - #####
          ##### ---------------------------- #####
    
            # The aim here is to end up with a template usable for the phylogenomic tree
            # with the list of species, genus, family and "Pairs" (which stands for species
            # partners in my analysis) and some empty columns.
    
            # Get the unique species from your database
            Species <- data.frame("Counter" = NA, "species" = unique(Datos$Species), "genus" = NA, "family" = NA,
                                  "Pairs" = NA, "species.relative" = NA, "genus.relative" = NA,
                                  "close.relative" = NA, "Comentario" = NA, "Nombre" = NA)
    
            # Get the families and "Pairs"
            Datos_reducido <- Datos[, c("Species", "Family", "Pairs")]
            Datos_reducido_unico <- Datos_reducido %>% distinct(Species, .keep_all = TRUE)
            Species$family <- Datos_reducido_unico$Family
            Species$Pairs <- Datos_reducido_unico$Pairs
            rm(Datos_reducido)
            rm(Datos_reducido_unico)
    
            # Transform species to character so strsplit can read them
            Species$species <- as.character(Species$species)
            Species$family <- as.character(Species$family)
            Species$Pairs <- as.character(Species$Pairs)
    
            # Get the genus
            Species$genus <- sapply(strsplit(Species$species, " "), `[`, 1)
    
            # Split genus and species in species name with a "_"
            Species$species <- gsub(" ", "_", Species$species)
    
            
    
    
    
          ##### ---------------------------- #####
          ##### -- Parameters of the tree -- #####
          ##### ---------------------------- #####
    
            # Some parameters have to be chosen for the tree construction, as the name of
            # the tree, the database to use, the type of nodes, or the scenario. All the info in:
            # https://github.com/jinyizju/V.PhyloMaker2/blob/master/Introduction%20to%20the%20'V.PhyloMaker2'%20package
    
            # Tree name
            Tree_name <- "Rocky_Plants"
    
            # Database selection (choose between The Plant List (TPL), Leipzig catalogue of vascular
            # plants (LCVP), or World Plants (WP))
            DB <- "TPL"
            # DB <- "WP"
            # DB <- "LCVP"
            tree_selected <- get (paste0 ("GBOTB.extended.", DB))
    
            # Type of nodes
            node_type <- 2
            # node_type <- 1
            if (node_type == 1) {
              node_info <- "node.info."
            } else {
              node_info <- "nodes.info."
            }
            nodes_selected <- get (paste0 (node_info, node_type, ".", DB))
    
            # Scenario selection (need to chose an scenario between S1, S2 and S3)
            scenario_selected <- "S3"
            # scenario_selected <- S1
            # scenario_selected <- S2
    
            
    
    
    
    
        ###### -------------------------------------- ######
        ###### - Parameters for species in database - ######
        ###### -------------------------------------- ######
    
          # Using the template that we have created with our species (Species) and the database
          # selected, we can complete some other columns (as Counter or species.relative) needed
          # to build our phylogenomic tree. The main idea is that fist we look for the species
          # that are already on the dataframe, and we add them a 1 in counter, as they are the fist
          # ones to get added to the tree. Then we have different options to fit the rest of species,
          # to add them near to a genus that is already on the tree, to look in the database for
          # some other species of its genus and join the tree in its place, and to use the partner
          # "Pairs" to enter the tree as a close relative, if the partner is on the tree.
    
          # database with species
          species_TPL <- data.frame (species = GBOTB.extended.TPL$tip.label)
    
          # check for common species
          common_species <- sort(intersect(species_TPL$species, Species$species))
    
          # Put a 1 in the Species list to the species that are already on the database
          Species$Counter[Species$species %in% common_species] <- 1
    
    
    
    
          
          ##### --------------------------------------------------- #####
          ##### Look for the closest relatives of the other species #####
          ##### --------------------------------------------------- #####
    
            # For each of the species are not on the tree, we look for the closest relative
            # that can be found on the tree
    
    
          
    
            #### ------------------- ####
            #### Androsace vitaliana ####
            #### ------------------- ####
    
              # Set the specie's name
              specie <- "Androsace_vitaliana"
    
              # Extract genus from specie
              specie_genus <- sapply(strsplit(specie, "_"), `[`, 1)
    
              # Extract genus from species_TPL
              species_TPL_genus <- sapply(strsplit(species_TPL$species, "_"), `[`, 1)
    
              # Look for species in species_TPL that have matching genus
              species_with_matching_genus <- species_TPL$species[species_TPL_genus %in% specie_genus]
    
              # Chose the closest relative in the list
              closest_relative <- "Androsace_brevis"
              # Based on: Schneeweiss, G. M., Schönswetter, P., Kelso, S., & Niklfeld, H. (2004).
              # Complex biogeographic patterns in Androsace (Primulaceae) and related genera:
              # evidence from phylogenetic analyses of nuclear internal transcribed spacer and
              # plastid trnL-F sequences. Systematic Biology, 53(6), 856-876.
    
              # Find the index for the Species row
              idx <- which(Species$species == specie)
    
              # Update Counter to 1
              Species$Counter[idx] <- 1
    
              # Add the species from species_TPL to species.relative column in Species
              Species$species.relative[idx] <- closest_relative
    
              # Add "si" in close.relative column
              Species$close.relative[idx] <- "si"
    
              
    
    
            #### ----------------------- ####
            #### Streptocarpus ionanthus ####
            #### ----------------------- ####
    
              # Set the specie's name
              specie <- "Streptocarpus_ionanthus"
    
              # Extract genus from specie
              specie_genus <- sapply(strsplit(specie, "_"), `[`, 1)
    
              # Extract genus from species_TPL
              species_TPL_genus <- sapply(strsplit(species_TPL$species, "_"), `[`, 1)
    
              # Look for species in species_TPL that have matching genus
              species_with_matching_genus <- species_TPL$species[species_TPL_genus %in% specie_genus]
    
              # Chose the closest relative in the list
              closest_relative <- "Streptocarpus_shumensis"
              # Möller, M. (2018). Nuclear DNA C-values are correlated with pollen size at
              # tetraploid but not diploid level and linked to phylogenetic descent in
              # Streptocarpus (Gesneriaceae). South African journal of botany, 114, 323-344.
    
              # Find the index for the Species row
              idx <- which(Species$species == specie)
    
              # Update Counter to 1
              Species$Counter[idx] <- 1
    
              # Add the species from species_TPL to species.relative columne in Species
              Species$species.relative[idx] <- closest_relative
    
              # Add "si" in close.relative column
              Species$close.relative[idx] <- "si"
    
    
              
    
            #### ------------------------ ####
            #### Antirrhinum sempervirens ####
            #### ------------------------ ####
    
              # Set the specie's name
              specie <- "Antirrhinum_sempervirens"
    
              # Extract genus from specie
              specie_genus <- sapply(strsplit(specie, "_"), `[`, 1)
    
              # Extract genus from species_TPL
              species_TPL_genus <- sapply(strsplit(species_TPL$species, "_"), `[`, 1)
    
              # Look for species in species_TPL that have matching genus
              species_with_matching_genus <- species_TPL$species[species_TPL_genus %in% specie_genus]
    
              # Chose the closest relative in the list
              closest_relative <- "Antirrhinum_siculum"
              # It is the only other Antirrhinum on the list
    
              # Find the index for the Species row
              idx <- which(Species$species == specie)
    
              # Update Counter to 1
              Species$Counter[idx] <- 1
    
              # Add the species from species_TPL to species.relative columne in Species
              Species$species.relative[idx] <- closest_relative
    
              # Add "si" in close.relative column
              Species$close.relative[idx] <- "si"
    
              
    
    
            #### ----------------------- ####
            #### Petrocoptis crassifolia ####
            #### ----------------------- ####
    
              # Set the specie's name
              specie <- "Petrocoptis_crassifolia"
    
              # Extract genus from specie
              specie_genus <- sapply(strsplit(specie, "_"), `[`, 1)
    
              # Extract genus from species_TPL
              species_TPL_genus <- sapply(strsplit(species_TPL$species, "_"), `[`, 1)
    
              # Look for species in species_TPL that have matching genus
              species_with_matching_genus <- species_TPL$species[species_TPL_genus %in% specie_genus]
    
              # If there are no plants of the same genus, look for the closest genus
              closest_genus <- "Lychnis"
              # Fior, S., Karis, P. O., Casazza, G., Minuto, L., & Sala, F. (2006).
              # Molecular phylogeny of the Caryophyllaceae (Caryophyllales) inferred from
              # chloroplast matK and nuclear rDNA ITS sequences. American journal of botany,
              # 93(3), 399-411.
    
              # Look for species in species_TPL that have matching genus
              species_with_matching_genus <- species_TPL$species[species_TPL_genus %in% closest_genus]
    
              # Chose the closest relative in the list
              closest_relative <- "Lychnis_ajanensis"
              # It is the only species of the closest genus on the list
    
              # Find the index for the Species row
              idx <- which(Species$species == specie)
    
              # Update Counter to 1
              Species$Counter[idx] <- 1
    
              # Add the species from species_TPL to species.relative columne in Species
              Species$species.relative[idx] <- closest_relative
    
              # Add "si" in close.relative column
              Species$close.relative[idx] <- "si"
    
    
    
              
            #### -------------- ####
            #### Ramonda myconi ####
            #### -------------- ####
    
              # Set the specie's name
              specie <- "Ramonda_myconi"
    
              # Extract genus from specie
              specie_genus <- sapply(strsplit(specie, "_"), `[`, 1)
    
              # Extract genus from species_TPL
              species_TPL_genus <- sapply(strsplit(species_TPL$species, "_"), `[`, 1)
    
              # Look for species in species_TPL that have matching genus
              species_with_matching_genus <- species_TPL$species[species_TPL_genus %in% specie_genus]
    
              # If there are no plants of the same genus, look for the closest genus
              closest_genus <- "Haberlea"
              # Ogutcen, E., Christe, C., Nishii, K., Salamin, N., Möller, M., & Perret, M. (2021).
              # Phylogenomics of Gesneriaceae using targeted capture of nuclear genes.
              # Molecular Phylogenetics and Evolution, 157, 107068.
    
              # Look for species in species_TPL that have matching genus
              species_with_matching_genus <- species_TPL$species[species_TPL_genus %in% closest_genus]
    
              # Chose the closest relative in the list
              closest_relative <- "Haberlea_rhodopensis"
              # It is the only species of the closest genus on the list
    
              # Find the index for the Species row
              idx <- which(Species$species == specie)
    
              # Update Counter to 1
              Species$Counter[idx] <- 1
    
              # Add the species from species_TPL to species.relative columne in Species
              Species$species.relative[idx] <- closest_relative
    
              # Add "si" in close.relative column
              Species$close.relative[idx] <- "si"
    
            # Now all the species in Species are found or the list or have a relative on the list,
            # so the tree can be made
    
    
    
    
    
              
        ###### -------------------------------------- ######
        ###### --------- Phylogenetic Tree ---------- ######
        ###### -------------------------------------- ######
          
          # Here we actually do the phylogenetic Tree. And then we plot it
              
          
              
              
              
          ##### -------------------------------------- #####
          ##### --------- Last preparatives ---------- #####
          ##### -------------------------------------- #####
          
            # Now, with the template completed, is when we are ready to generate the tree.
            # For that we create some data frames (list_A, list_B, list_C and list_D) were we
            # include species and parameters that we need to get into the tree.
        
            # list_A is the template with our species and all the parameters
            list_A <- Species
        
            # First we set the name for the tree
            filename <- Tree_name
            N_sp <- length(list_A$species)
            filename <- paste(filename,N_sp,sep="")
            filename1 <- paste(filename,".txt", sep="")
        
            # Transform Counter to factor
            list_A$Counter <- as.factor(list_A$Counter)
        
            # list_B is a subset of list_A only with the species that are entering the tree instead
            # of another existing species. The ones where close.relative equals to "si".
            list_B <- subset(list_A, close.relative=="si")
            list_B$species <- list_B$species.relative
            list_B$species.relative <- NA
        
            # list_C is a bind of the first ones to enter the tree (Counter == 1) in list_A and list_B
            list_C <- rbind(list_A, list_B)
            list_C <- subset(list_C, Counter=="1")
            list_C <- list_C[, c("species", "genus", "family", "species.relative", "genus.relative")]
        
            # bind.relative binds a list of species to their designated species in the phylogeny
            # and phylo.maker makes the phylogenomic tree.
            rel <- bind.relative (sp.list=list_C, tree=tree_selected, nodes=nodes_selected)
        
            t1 <- phylo.maker(sp.list=rel$species.list,
                              tree=rel$phylo,nodes=rel$nodes.info, scenarios=scenario_selected)
        
            # We save the first tree as T1
            write.tree(t1$scenario.3, "t1.tre")
            T1 <- read.tree(file="t1.tre")
            file.remove("t1.tre")
        
            # Delete species used only as relatives
            T1 <- drop.tip(T1,list_B$species)
        
            # For better visualization, replaze "_" for a space
            T1[["tip.label"]] <- gsub("_", " ", T1[["tip.label"]])
            
            # Delete species used only as relatives
            T1 <- drop.tip(T1, Speciess_a_eliminar_todas)
            
            
            
            
            
          ##### ----------------------------------- #####
          ##### --------- Graph the tree ---------- #####
          ##### ----------------------------------- #####
          
            # Colours for the graphs
            colores_grafico <- c("Generalist" = "#009E73", "Cliff_specialist" = "#E69F00")
            coloType <- c("Ramonda" = "#F781BF",
                          "Antirrhinum" = "#d62728",
                          "Asperula" = "green",
                          "Campanula" = "purple",
                          "Hieracium" = "yellow",
                          "Lonicera" = "#2ca02c",
                          "Androsace" = "#0000CD",
                          "Petrocoptis" = "#A65628",
                          "Saxifraga" = "#ff7f0e",
                          "Hypericum" = "black",
                          "Sarcocapnos" = "#2F4F4F",
                          "Dioscorea" = "#00CED1",
                          "0" = "grey")
            
            # shape 
            formas <- c("Generalist" = 24, "Cliff_specialist" = 21) 
         
            # Add groups to colour lines of the trees
            Datos_m_Species$Species <- as.character(Datos_m_Species$Species)
            
            grp_pairs <- list(Ramonda = Datos_m_Species$Species[Datos_m_Species$Pairs=="Ramonda"],
                        Antirrhinum = Datos_m_Species$Species[Datos_m_Species$Pairs=="Antirrhinum"],
                        Asperula  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Asperula"],
                        Campanula  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Campanula"],
                        Hieracium  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Hieracium"],
                        Lonicera  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Lonicera"],
                        Androsace  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Androsace"],
                        Petrocoptis  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Petrocoptis"],
                        Saxifraga  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Saxifraga"],
                        Hypericum  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Hypericum"],
                        Sarcocapnos  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Sarcocapnos"],
                        Dioscorea  = Datos_m_Species$Species[Datos_m_Species$Pairs=="Dioscorea"])
            
            Datos_m_Species$Species <- factor(Datos_m_Species$Species)
            
            # groupOTU to add the groups
            T1 <- groupOTU(T1, grp_pairs, "Group_Pairs")
           
            # First tree graph
            ptree <- ggtree(T1, aes(color = Group_Pairs), ladderize = FALSE, size = 0.65)
            
            # Rotates nodes to end up as wanted
            ptree <- ggtree::rotate(ptree, 47)
            ptree <- ggtree::rotate(ptree, 44)
            ptree <- ggtree::rotate(ptree, 42)
            ptree <- ggtree::rotate(ptree, 41)
            ptree <- ggtree::rotate(ptree, 38)
            ptree <- ggtree::rotate(ptree, 34)
            
            # Extract data from tree tips
            tip_data <- ptree$data
            
            # Add traits info to tip data
            tip_data <- tip_data %>%
              left_join(Datos_m_Species, by = c("label" = "Species"))
            
            # Plot final tree
            ptree_final <- ptree +
              scale_color_manual(name = "Group_Pairs", values = coloType, guide = "none") +
              new_scale_color() +
              geom_tiplab(data = tip_data, aes(color = factor(Type)), size = 3.5, hjust = -0.1, fontface = "italic") +  # Ajustar hjust
              scale_color_manual(name = "Type", values = colores_grafico, guide = "none") +
              scale_shape_manual(name = "Type", values = formas, guide = "none") +
              geom_tippoint(data = tip_data, color="black", aes(fill = factor(Pairs), shape = factor(Type)), size = 3) +
              scale_fill_manual(name = "Pairs", values = coloType, guide = "none") +
              ggtitle("Phylogenomic tree of 12 pairs of species") +
              scale_x_continuous(
                name = "Divergence time (MY)",
                breaks = c(-4, 16, 36, 56, 76, 96, 116, 136),
                labels = c("140", "120", "100", "80", "60", "40", "20", "0"),
                expand = expansion(mult = c(0.05, 0.5))  # Expande los límites
              ) +
              theme_tree2() +
              theme(
                plot.title = element_text(hjust = 0.5, face = "bold"), 
                legend.position = "none"
              )
            
            # Show tree
            ptree_final
            
            # Save it as an image
            ggsave(filename = "Supplementary_Figure1_Tree.png",
                   path = "Figures",
                   plot = ptree_final, 
                   width = 16, 
                   height = 16, 
                   units = "cm", 
                   dpi = 300)
            
            
            # 2nd option for the plot, I think that I stay with this one
            ptree1 <- ggtree(T1, ladderize = FALSE, size = 0.65)
            
            # Plot final tree
            ptree_final1 <- ptree1 +
              #geom_text(aes(label=node), hjust=-.3) + 
              geom_tiplab(data = tip_data, aes(color = factor(Type)), size = 3.5, hjust = -0.1, fontface = "italic") +  # Ajustar hjust
              scale_color_manual(name = "Type", values = colores_grafico, guide = "none") +
              scale_shape_manual(name = "Type", values = formas, guide = "none") +
              geom_tippoint(data = tip_data, color = "black", aes(fill = factor(Pairs), shape = factor(Type)), size = 3) +
              scale_fill_manual(name = "Pairs", values = coloType, guide = "none") +
              ggtitle("Phylogenomic tree of 12 pairs of species") +
              scale_x_continuous(
                name = "Divergence time (MY)",
                breaks = c(-4, 16, 36, 56, 76, 96, 116, 136),
                labels = c("140", "120", "100", "80", "60", "40", "20", "0"),
                expand = expansion(mult = c(0.05, 0.5))  # Expande los límites
              ) +
              geom_hilight(node = 47, fill = "#00CED1", alpha = 0.3, extend = -136) +
              geom_hilight(node = 47, fill = "#00CED1", alpha = 0.3) +
              geom_hilight(node = 46, fill = "#2F4F4F", alpha = 0.3, extend = -132.5) +
              geom_hilight(node = 46, fill = "#2F4F4F", alpha = 0.3) +
              geom_hilight(node = 45, fill = "#ff7f0e", alpha = 0.3, extend = -122.3) +
              geom_hilight(node = 45, fill = "#ff7f0e", alpha = 0.3) +
              geom_hilight(node = 44, fill = "black", alpha = 0.3, extend = -122.3) +
              geom_hilight(node = 44, fill = "black", alpha = 0.3) +
              geom_hilight(node = 42, fill = "#A65628", alpha = 0.3, extend = -120) +
              geom_hilight(node = 42, fill = "#A65628", alpha = 0.3) +
              geom_hilight(node = 41, fill = "#0000CD", alpha = 0.3, extend = -112) +
              geom_hilight(node = 41, fill = "#0000CD", alpha = 0.3) +
              geom_hilight(node = 40, fill = "green", alpha = 0.3, extend = -90) +
              geom_hilight(node = 40, fill = "green", alpha = 0.3) +
              geom_hilight(node = 39, fill = "#F781BF", alpha = 0.3, extend = -57.5) +
              geom_hilight(node = 39, fill = "#F781BF", alpha = 0.3) +
              geom_hilight(node = 38, fill = "#d62728", alpha = 0.3, extend = -57.5) +
              geom_hilight(node = 38, fill = "#d62728", alpha = 0.3) +
              geom_hilight(node = 35, fill = "#2ca02c", alpha = 0.3, extend = -93.2) +
              geom_hilight(node = 35, fill = "#2ca02c", alpha = 0.3) +
              geom_hilight(node = 34, fill = "purple", alpha = 0.3, extend = -82.7) +
              geom_hilight(node = 34, fill = "purple", alpha = 0.3) +
              geom_hilight(node = 33, fill = "yellow", alpha = 0.3, extend = -82.7) +
              geom_hilight(node = 33, fill = "yellow", alpha = 0.3) +
              theme_tree2() +
              theme(
                plot.title = element_text(hjust = 0.5, face = "bold"), 
                legend.position = "none"
              )
           
            # Show tree
            ptree_final1
            
            # Save it as an image
            ggsave(filename = "Supplementary_Figure1_Tree.png",
                   path = "Figures",
                   plot = ptree_final1, 
                   width = 16, 
                   height = 16, 
                   units = "cm", 
                   dpi = 300)
           
            
            
            
            
            
            
            
             
  ######### ---------------------------------- #########
  ######### ---------------------------------- #########
  #########         FINAL CONSIDERATIONS       ######### 
  ######### ---------------------------------- #########
  ######### ---------------------------------- #########

    # Just to say that even though this script has not been created thinking on
    # replicability of it for other data, I hope some parts of it can work for
    # your study. 
            
    # Also, as the main objetive of this script is to keep track of all the steps
    # on this work to permit replicability, if anything of this script doesnt work
    # dont hesitate to contact cireralberto@gmail.com
    
    # Aaaand that is it, thank you for reading and sorry for the english, I hope
    # everything is understandble :)
          
          