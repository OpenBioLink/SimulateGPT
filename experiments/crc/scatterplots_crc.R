library(ggplot2)
library(data.table)
library(ggpubr)

##Load exported data
crcAllResults = data.table::fread('all_data_points.csv')

#in-panel annotations (metrics) still mising
#CRC regression plots: add high-complexity in other plots as red dotted line


#simulator
crcAllResults_hc = crcAllResults[which(crcAllResults$simulator=="high_complexity"),]
crcAllResults_bl = crcAllResults[which(crcAllResults$simulator=="baseline"),]

crcResultsList = list()
crcResultsList[['high_complexity__baseline']] = crcAllResults[which(crcAllResults$simulator %in% c("baseline","high_complexity")),]
crcResultsList[['high_complexity__high5step_complexity']] = crcAllResults[which(crcAllResults$simulator %in% c("high5step_complexity","high_complexity")),]
crcResultsList[['high_complexity__low_complexity']] = crcAllResults[which(crcAllResults$simulator %in% c("low_complexity","high_complexity")),]
# p3 <- ggplot(crcAllResults_hc_bl, aes(x=PFS_MONTHS, y=llm_result)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)
# 
# print(p3)

#define colors
COLS = c("blue","red","orange","darkgreen")
names(COLS) = c("baseline","high_complexity","high5step_complexity","low_complexity")
###define scale of y-axis
maxPred = max(crcAllResults[which(crcAllResults$simulator %in% names(COLS)),]$llm_result)
minPred = min(crcAllResults[which(crcAllResults$simulator %in% names(COLS)),]$llm_result)


for (complexityCombi in names(crcResultsList)){
  complexities = strsplit(complexityCombi, split = '__')[[1]]
  COLS_subset = COLS[complexities]
  p1 = ggplot(crcResultsList[[complexityCombi]]) + 
    geom_point(aes(x=PFS_MONTHS, y=llm_result,colour=simulator)) + 
    geom_smooth(aes(x=PFS_MONTHS, y=llm_result,colour=simulator,fill=simulator),  alpha=0.25, method=lm, se=TRUE ) +
    # change name of legend here 
    #
    scale_fill_manual(values=COLS_subset)+
    scale_color_manual(values=COLS_subset) + theme_bw() + 
    stat_cor(aes(x=PFS_MONTHS, y=llm_result, colour=simulator),method="pearson") +
    theme(legend.position = c(0.13, 0.75)) + xlab('Ground-truth PFS (months)') + 
    ylab('LLM estimated PFS (months)') +
    theme(text=element_text(family="sans",size = 14),
          legend.text=element_text(family="sans",size = 14)) +
    ylim(c(minPred-2, maxPred))
  p1
  
  ggsave(p1, filename = file.path('./plots',paste0('CRC_',complexityCombi,'.pdf')),width = 2*(183/4), height = 2*(183/4), units = 'mm')
}

