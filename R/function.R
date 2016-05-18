# Subset and droplevels---------------------------------------------------------
  subsetD <- function(...) droplevels(subset(...))
  
  
# Save ggplot in PDF and PNG---------------------------------------------------- 
  ggsavePP <- function(filename, plot, width, height){
    ggsave(filename = paste(filename, ".pdf", sep = ""), 
           plot     = plot, 
           width    = width, 
           height   = height)
    
    ggsave(filename = paste(filename, ".png", sep = ""), 
           plot     = plot, 
           width    = width, 
           height   = height, 
           dpi      = 600)
    }