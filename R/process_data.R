rawdf <- read.csv("Data/Reanalysis_results.TXT", 
                  header           = TRUE,
                  na.strings       = c("NA", ""),
                  sep              = "\t",
                  stringsAsFactors = FALSE)

  
# Clean up data-----------------------------------------------------------------
  rawdf <- rawdf[c(-1:-4), ]
  
  # remove columns with all NAs
  rawdf <- rawdf[, !apply(rawdf, 2, function(x) all(is.na(x)))] 
  
  # Sample ID
  rawdf$sample_id <- as.numeric(substr(rawdf$ImageFileName, 7, 8))
  
  # Diameter classes
  diameter_class <- strsplit(unique(rawdf$ClassBoundaries), " ")[[1]]
  Nclass <- unique(rawdf$NumberOfClasses)
  diameter_classification <- data.frame(DiameterClass = 1:Nclass, 
                                        lower_d       = diameter_class,
                                        upper_d       = c(diameter_class[-1], 100))
  
  names(rawdf)[grep("^X|^[.]", names(rawdf))] <- as.vector(t(
                                                  outer(c("L", "SA", "PA", "V", "T"), 
                                                        1:Nclass, paste, sep = "_")))
  
  # remove unnecessary columns
  rmcols <- c("wRHIZO.2015a", "Operator", 
              "ImageAcqDeviceAndSoftwareInfo.DateTimeModified",
              "DevOrder.Criteria", "ImgType.CalibMeth.TPU.Units.PxSizeH.PxSizeV..CalFile.", 
              "PxClassif", "Filters",  "Fractal.PxMin.PxMax", "SoilVol.m3.",
              "RatioHypLeafToRootLen", "FractalDimension", "FractalDeviation",
              "Tips",	"Forks"	,"Crossings", "NofNodules", "NumberOfClasses", 
              "ClassBoundaries", "Left.Top.Right.Bottom..NExclusions", "ImageFileName",
              "AnalysedRegionWidth.cm.", "AnalysedRegionHeight.cm.", "Length.cm.",
              "ProjArea.cm2.", "SurfArea.cm2.", "AvgDiam.mm.", "LenPerVol.cm.m3.", 
              "RootVolume.cm3.", "AnalysedRegionArea.cm2.", "Analysis.Date.Time")
  rawdf <- rawdf[, setdiff(names(rawdf), rmcols)]
  
# Organise data-----------------------------------------------------------------
  rawdf_mlt <- melt(rawdf, id = "sample_id")
  # Diameter classes
  dcdd <- ldply(strsplit(as.character(rawdf_mlt$variable), "_"))
  colnames(dcdd) <- c("measurement", "DiameterClass")
  rawdf_mlt      <- cbind(rawdf_mlt, dcdd)
  rawdf_mlt      <- merge(rawdf_mlt, diameter_classification, 
                          by    = "DiameterClass", 
                          all.x = TRUE)
  
  # Combine with table for treatment applications
  treatdd <- read.csv("Data/sample_id.csv")
  
  root_str_dd <- merge(rawdf_mlt, treatdd, 
                       by    = "sample_id", 
                       all.x = TRUE)
  root_str_dd$DiameterClass <- factor(root_str_dd$DiameterClass,
                                      levels = 1:20)
  
  # remove roots > 3mm diamter (diameter class > 16)
  root_str_dd <- subsetD(root_str_dd, 
                         !DiameterClass %in% c(16:20))
  
  # reshape data  frame
  root_dd     <- dcast(sample_id + DiameterClass + lower_d + upper_d + species + co2 + water + block ~ measurement, 
                       value.var = "value", 
                       data      = root_str_dd) 
  root_dd <- within(root_dd, {
    lower_d <- as.numeric(as.character(lower_d))
    upper_d <- as.numeric(as.character(upper_d))
  })

  # save
  save(root_dd, file = "Output/Data/root_dd.RData")

# Figure-----------------------------------------------------------------------  
  p <- ggplot(subset(root_dd, L != 0),
              aes(x   = factor(lower_d + 0.1), 
                  y   = log10(L), 
                  col = co2))
  p2 <- p + 
    geom_boxplot(size = 0.3) + 
    labs(x = "Diamter (mm)", 
         y = expression(log[10](Root~length(mm)))) +
    theme(axis.text.x = element_text(size = 5)) +
    facet_grid(species ~ water)
  
  ggsavePP(p2, 
         filename = "Output/Figs/root_length_for_each_diameter_classes",
         width    = 6, 
         height   = 6)
