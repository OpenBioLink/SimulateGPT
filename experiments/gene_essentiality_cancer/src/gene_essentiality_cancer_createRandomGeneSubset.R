source("main_fxns.R")
library(R.utils)
experimentSel = 'gene_essentiality_cancer_proliferation'
set.seed(42)
nGenesPerCat = 25

essentialGeneFile = file.path('..','data',experimentSel,'CRISPRInferredCommonEssentials.csv')
essentialGene.vect = loadGeneVect(essentialGeneFile)
essentialGene.vect.subset = sample(essentialGene.vect,nGenesPerCat,replace=FALSE)

nonEssentialGeneFile = file.path('..','data',experimentSel,'AchillesNonessentialControls.csv')
nonEssentialGene.vect = loadGeneVect(nonEssentialGeneFile)
nonEssentialGene.vect.subset = sample(nonEssentialGene.vect,nGenesPerCat,replace=FALSE)

folderForFiles=file.path('..','experiments',experimentSel,'prompts')
mkdirs(folderForFiles)
for (geneSel in c(essentialGene.vect.subset,nonEssentialGene.vect.subset)){
  promptSel = paste0('Is the gene ',geneSel,' likely essential in cancer cell-lines? Provide an outcome of either "likely" or "unlikely".')
  writeLines(text = promptSel, con = file.path(folderForFiles,geneSel))
}