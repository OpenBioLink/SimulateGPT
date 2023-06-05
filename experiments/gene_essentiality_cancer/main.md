# Assessing gene essentiality in cancer cell lines based on DepMap

## Background

DepMap provides a list of 1856 genes that are thought to be essential across cancer cell lines, which can be downloaded from https://depmap.org/portal/download/all/?releasename=DepMap+Public+22Q4&filename=CRISPRInferredCommonEssentials.csv . There is also a list of non-essential controls (https://depmap.org/portal/download/all/?releasename=DepMap+Public+22Q4&filename=AchillesNonessentialControls.csv). 

## Methods

I performed ±8 tests (with randomly selected essential genes) with the baseline and high complexity model.

Here are 3 interesting human prompts (only the genes were changed across runs):

`Is the gene CIT likely essential in cancer cell-lines? Provide an outcome of either "likely" or "unlikely".`
`Is the gene GLRX5 likely essential in cancer cell-lines? Provide an outcome of either "likely" or "unlikely".`
`Is the gene GOT2 likely essential in cancer cell-lines? Provide an outcome of either "likely" or "unlikely".`

## Results

In 3 cases the resukts were different, with the high complexity model correctly predicting "likely" essential, and the baseline model predicting "not likely" essential.

## Conclusions

- In these few experiments, the 'high complexity' system seems more likely to predict essential genes correctly than the 'baseline' model
- It would be good to have another set of negative controls not from just a single paper, to see if the "high complexity" system is just more likely to predict essentiality regardless of the gene

