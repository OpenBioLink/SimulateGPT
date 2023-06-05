source("main_fxns.R")
library(R.utils)
library(stringr)

set.seed(42)
experimentSel = 'gene_essentiality_cancer'

outputList = parseOutputPrompts(file.path('..','..',experimentSel))

##Split into essential and non-essential
essentialGeneFile = file.path('..','data',experimentSel,'CRISPRInferredCommonEssentials.csv')
essentialGene.vect = loadGeneVect(essentialGeneFile)

nonEssentialGeneFile = file.path('..','data',experimentSel,'AchillesNonessentialControls.csv')
nonEssentialGene.vect = loadGeneVect(nonEssentialGeneFile)

getGroundTruth <- function(geneSel,essentialGene.vect,nonEssentialGene.vect){
  if (geneSel %in% essentialGene.vect){ 
    outCome = 'likely'
  }else if (geneSel %in% nonEssentialGene.vect){ 
    outCome='unlikely'
  }else{
    browser()
  }
  return(outCome)
}

allResultsList=list()
for (system_message_sel in names(outputList)){
  geneList = outputList[[system_message_sel]]
  allResultsVect = c()
  for (geneSel in names(geneList)){
    prediction = geneList[[geneSel]]

    reality = getGroundTruth(geneSel,essentialGene.vect,nonEssentialGene.vect)
    print(reality)
    combined = paste(reality,prediction,sep='_')
    allResultsVect = c(allResultsVect,combined)
  }
  allResultsList[[system_message_sel]] = allResultsVect
}


# Function to create confusion matrix from vector of strings
create_confusion_matrix <- function(predictions) {
  # Split ground truth and model predictions
  split_results <- strsplit(predictions, "_")
  ground_truth <- sapply(split_results, "[[", 1)
  model_prediction <- sapply(split_results, "[[", 2)
  
  # Create confusion matrix
  confusion_matrix <- table(ground_truth, model_prediction)
  return(confusion_matrix)
}

# Initialize an empty list to store confusion matrices
confusion_matrices <- list()

# Loop over items in allResultsList and create confusion matrices
for (i in names(allResultsList)) {
  confusion_matrices[[i]] <- create_confusion_matrix(allResultsList[[i]])
}

library(ggplot2)
library(gridExtra)

# Function to convert confusion matrix to data frame for ggplot2
confusion_matrix_to_df <- function(confusion_matrix) {
  df <- as.data.frame(confusion_matrix)
  colnames(df) <- c("ground_truth", "model_prediction", "count")
  return(df)
}

# Function to plot a single confusion matrix using ggplot2
plot_confusion_matrix <- function(confusion_matrix, title, max_count=25) {

  confusion_matrix = confusion_matrices[[tolower(title)]]
  max_count=25
  title = levelSel
  df <- confusion_matrix_to_df(confusion_matrix)
  plot <- ggplot(df, aes(x = ground_truth, y = model_prediction, fill = count)) +
    geom_tile() +
    geom_text(aes(label = count), color = "black", size = 6*7) +
    scale_fill_gradient(low = "white", high = "red", limits = c(0, max_count)) +
    labs(title = title, x = "Ground Truth", y = "Model Prediction") +
    theme_minimal() + theme(plot.title.position = "plot", text=element_text(family="sans",size = 6*7), plot.title = element_text(hjust = 0.5))
  
  return(plot)
}

# Main function to plot confusion matrices in a 2x2 grid
plot_confusion_matrices_grid <- function(confusion_matrices, order, max_count=25) {
  plots <- list()
  
  for (i in seq_along(order)) {
    item_name <- order[i]
    confusion_matrix <- confusion_matrices[[item_name]]
    plots[[i]] <- plot_confusion_matrix(confusion_matrix, item_name,max_count)
  }
  
  grid.arrange(grobs = plots, ncol = 2)
}

# Example usage
#confusion_matrices_order <- c("baseline", "low_complexity", "high_complexity","high5step_complexity")
#p = plot_confusion_matrices_grid(confusion_matrices, confusion_matrices_order, max_count=25)
#print(p)


for (levelSel in names(confusion_matrices)){
plot = plot_confusion_matrix(confusion_matrices, levelSel, max_count=25) 

ggsave(plot, filename = file.path('..','/plots',
                                  paste0('geneEss_',levelSel,'.pdf')),width = (6/2)*(183/2), height = .8*(6/2)*(183/2), units = 'mm')
}

##Calculcate precision recall etc
library(caret) 
baselineStats = confusionMatrix(confusion_matrices$baseline)
highComplexityStats = confusionMatrix(confusion_matrices$high_complexity)

statDf = rbind('Baseline'=c(baselineStats$overall,baselineStats$byClass),'SimulateGPT'=c(highComplexityStats$overall,highComplexityStats$byClass))

library(pander) 
pandoc.table(statDf, plain.ascii=T, style='rmarkdown', split.tables=1000)

library(data.table)
write.csv(x = statDf,
       file=file.path('..','plots','statistics.csv'))