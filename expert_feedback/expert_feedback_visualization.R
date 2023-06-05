
#### load libraries
library(ggplot2)
library(ggplotify)
library(reshape2)
library(patchwork)
library(dplyr)
library(stringr)
library(tidyr)

#### utility functions
# add new lines in text of plots
addline_format <- function(x){
  x <- gsub('\\.',' ',x)
  return(gsub('(.{1,50})(\\s|\\.|$|_)', '\\1\n', x))
}

# execution from root of repo
# setwd("../")

#### configs
expert_question <- 'I.felt.qualified.to.evaluate.this.topic.'

questions <- list(positive = c('The.output.reflects.the.consensus.in.the.scientific.and.clinical.community.',
                               'The.answer.contains..novel..concepts.that.are.not.typically.known.in.the.scientific.and.clinical.community.',
                               'The.output.contains.evidence.of.correct.recall.of.knowledge.',
                               'The.output.contains.evidence.of.correct.reasoning.steps.',
                               expert_question
),
negative = c('The.output.contains.content.it.shouldn.t.contain.',
             'The.output.omits.content.it.shouldn.t.omit.',
             'The.output.contains.evidence.of.incorrect.recall.of.knowledge.',
             'The.output.contains.evidence.of.incorrect.reasoning.steps.'
)
)

questionsOrder = c(
  'The.output.reflects.the.consensus.in.the.scientific.and.clinical.community.',
  'The.answer.contains..novel..concepts.that.are.not.typically.known.in.the.scientific.and.clinical.community.',
  'The.output.contains.content.it.shouldn.t.contain.',
  'The.output.omits.content.it.shouldn.t.omit.',
  'The.output.contains.evidence.of.correct.recall.of.knowledge.',
  'The.output.contains.evidence.of.incorrect.recall.of.knowledge.',
  'The.output.contains.evidence.of.correct.reasoning.steps.',
  'The.output.contains.evidence.of.incorrect.reasoning.steps.',
  expert_question
)


id_cols <- c('experiment_name', 'prompt_name', 'system_name')

likert_scale <- c('strongly disagree','disagree','slightly disagree','neither','slightly agree','agree','strongly agree')
likert_scale_cols <- colorRampPalette(c("blue", "white", "red"))(7)
names(likert_scale_cols) <- likert_scale

width <- 8
height <- 8
options(repr.plot.width=width, repr.plot.height=height)

# make results directory for plots
dir.create(file.path("expert_feedback/results/plots"), showWarnings = FALSE)

# load results
result_df <- read.csv(file=file.path("expert_feedback/results/expert_feedback_results.csv"))

#### BARPLOTS
width <- 8
height <- 13
options(repr.plot.width=width, repr.plot.height=height)

for(type in unique(result_df$type)){
  for (experiment in c(unique(result_df$experiment_name),"ALL")){
    # only sepsis_treatment has type SIM results
    if(type=="SIM" & experiment!="sepsis_treatment"){
      next
    }
    
    barplots_tmp <- list()
    
    for (question_type in names(questions)){

      questions_tmp <- questions[[question_type]]
      
      if(experiment=="ALL"){
        data_tmp <- result_df[(result_df$type==type),]
      }  else {
        data_tmp <- result_df[(result_df$type==type)&(result_df$experiment_name==experiment),]
      }
      
      result_all <- melt(data = data_tmp, 
                         id.vars = id_cols, 
                         measure.vars = questions_tmp,
                         variable.name = "questions", 
                         value.name = "Likert"
      )
      
      # convert number Likert to string
      result_all$Likert_new <- sapply(X = result_all$Likert, FUN= function(x) likert_scale[x])
      
      # transform result_all DF to have a for each system and questions and Likert value ONE percentage value
      
      # determine percentages of Likert values
      result_plot <- result_all %>%
        group_by(system_name, questions, Likert_new) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)
      
      # add Likert colors
      result_plot$color <- sapply(X = result_plot$Likert_new, FUN= function(x) likert_scale_cols[x])
      
      # divide neither by 2
      result_plot <- result_plot %>%
        mutate(percentage = ifelse(Likert_new == "neither", percentage / 2, percentage))
      
      # split in positive and negative, but keep 'neither' in both
      result_plot_neg <- result_plot %>%
        filter(grepl("disagree|neither", Likert_new))
      
      result_plot_pos <- result_plot %>%
        filter(!grepl("disagree", Likert_new))
      
      # merge question and system
      result_plot_pos$q_s <- paste0(result_plot_pos$questions,"\n",result_plot_pos$system_name)
      result_plot_neg$q_s <- paste0(result_plot_neg$questions,"\n",result_plot_neg$system_name)
      
      result_plot_pos$percentageRound = round(result_plot_pos$percentage, digits = 1)
      result_plot_neg$percentageRound = round(result_plot_neg$percentage, digits = 1)
      
      # barplot
      barplots_tmp[[question_type]] <- ggplot() + 
        geom_bar(data=result_plot_pos, aes(fill=color, x = q_s, y = percentage), position="stack", stat="identity") +
        geom_text(data=result_plot_pos, aes(fill=color, x = q_s, y = percentage, label = percentageRound),
                  size = 3, position = position_stack(vjust = 0.5)) +
        geom_bar(data=result_plot_neg, aes(fill=color, x = q_s, y = -percentage), position="stack", stat="identity") +
        geom_text(data=result_plot_neg, aes(fill=color, x = q_s, y = -percentage, label = percentageRound),
                  size = 3, position = position_stack(vjust = 0.5)) +
        geom_hline(yintercept = 0, color =c("white")) +
        scale_fill_identity("Percent", labels = likert_scale, limits=likert_scale_cols, guide="legend", drop=FALSE) +
        scale_x_discrete(label=addline_format)+
        coord_flip() +
        labs(title=paste0("Questions: ", question_type), y="",x="") +
        theme_bw() +
        theme(plot.title = element_text(size=14, hjust=0.5),
              axis.text.y = element_text(hjust=0),
              legend.position = "bottom") +
        scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) # 101 required otherwise rounding errors remove data!
    }
    
    barplots <- wrap_plots(barplots_tmp, nrow = 2, guides="collect")+
      plot_annotation(title = paste0("Experiment: ", experiment," - Type: ",type),
                      theme = theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
      )
    # save plot
    ggsave(
      paste0(paste0("barplots_",type,"_",experiment,".pdf")),
      plot = barplots,
      path = file.path("expert_feedback/results/plots/"),
      scale = 1,
      #dpi = 300,
      width = width,
      height = height,
      limitsize = FALSE
    )
  }
  
  barplots_tmp <- list()
  barplots_tmp_hc <- list()
  for (question_type in names(questions)){
    
    questions_tmp <- questions[[question_type]]
    
    
    if(experiment=="ALL"){
      data_tmp <- result_df[(result_df$type==type),]
    }  else {
      data_tmp <- result_df[(result_df$type==type)&(result_df$experiment_name==experiment),]
    }
    
    result_all <- melt(data = data_tmp, 
                       id.vars = id_cols, 
                       measure.vars = questions_tmp,
                       variable.name = "questions", 
                       value.name = "Likert"
    )
    
    # convert number Likert to string
    result_all$Likert_new <- sapply(X = result_all$Likert, FUN= function(x) likert_scale[x])
    
    # transform result_all DF to have a for each system and questions and Likert value ONE percentage value
    
    # determine percentages of Likert values
    result_plot <- result_all %>%
      group_by(system_name, questions, Likert_new) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    result_plot$questions = factor(result_plot$questions, levels = rev(questionsOrder))
    
    
    # add Likert colors
    result_plot$color <- sapply(X = result_plot$Likert_new, FUN= function(x) likert_scale_cols[x])
    
    # divide neither by 2
    result_plot <- result_plot %>%
      mutate(percentage = ifelse(Likert_new == "neither", percentage / 2, percentage))
    
    # split in positive and negative, but keep 'neither' in both
    result_plot_neg <- result_plot %>%
      filter(grepl("disagree|neither", Likert_new))
    
    result_plot_pos <- result_plot %>%
      filter(!grepl("disagree", Likert_new))
    
    # merge question and system
    result_plot_pos$q_s <- paste0(result_plot_pos$questions,"\n",result_plot_pos$system_name)
    result_plot_neg$q_s <- paste0(result_plot_neg$questions,"\n",result_plot_neg$system_name)
    
    result_plot_pos$percentageRound = round(result_plot_pos$percentage, digits = 1)
    result_plot_neg$percentageRound = round(result_plot_neg$percentage, digits = 1)
    
    # barplot
    barplots_tmp[[question_type]] <- ggplot() + 
      geom_bar(data=result_plot_pos, aes(fill=color, x = q_s, y = percentage), position="stack", stat="identity") +
      geom_text(data=result_plot_pos, aes(fill=color, x = q_s, y = percentage, label = percentageRound),
                size = 3, position = position_stack(vjust = 0.5)) +
      geom_bar(data=result_plot_neg, aes(fill=color, x = q_s, y = -percentage), position="stack", stat="identity") +
      geom_text(data=result_plot_neg, aes(fill=color, x = q_s, y = -percentage, label = percentageRound),
                size = 3, position = position_stack(vjust = 0.5)) +
      geom_hline(yintercept = 0, color =c("white")) +
      scale_fill_identity("Percent", labels = likert_scale, limits=likert_scale_cols, guide="legend", drop=FALSE) +
      scale_x_discrete(label=addline_format)+
      coord_flip() +
      labs(title=paste0("Questions: ", question_type), y="",x="") +
      theme_bw() +
      theme(plot.title = element_text(size=14, hjust=0.5),
            axis.text.y = element_text(hjust=0),
            legend.position = "bottom") +
      scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) # 101 required otherwise rounding errors remove data!
    
    
    ###Now make a similar plot, but just for high_complexity
    result_plot_pos_hc = result_plot_pos[which(result_plot_pos$system_name=="high_complexity"),]
    result_plot_neg_hc = result_plot_neg[which(result_plot_neg$system_name=="high_complexity"),]
    # barplot
    barplots_tmp_hc[[question_type]] <- ggplot() + 
      geom_bar(data=result_plot_pos_hc, aes(fill=color, x = questions, y = percentage), position="stack", stat="identity") +
      geom_text(data=result_plot_pos_hc, aes(fill=color, x = questions, y = percentage, label = percentageRound),
                size = 3, position = position_stack(vjust = 0.5)) +
      geom_bar(data=result_plot_neg_hc, aes(fill=color, x = questions, y = -percentage), position="stack", stat="identity") +
      geom_text(data=result_plot_neg_hc, aes(fill=color, x = questions, y = -percentage, label = percentageRound),
                size = 3, position = position_stack(vjust = 0.5)) +
      geom_hline(yintercept = 0, color =c("white")) +
      scale_fill_identity("Percent", labels = likert_scale, limits=likert_scale_cols, guide="legend", drop=FALSE) +
      scale_x_discrete(label=addline_format)+
      coord_flip() +
      labs(title=paste0("Questions: ", question_type), y="",x="") +
      theme_bw() +
      theme(plot.title = element_text(size=14, hjust=0.5),
            axis.text.y = element_text(hjust=0),
            legend.position = "bottom") +
      scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) 
    
  }
  
  barplots <- wrap_plots(barplots_tmp, nrow = 2, guides="collect")+
    plot_annotation(title = paste0("Experiment: ", experiment," - Type: ",type),
                    theme = theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
    )
  
  #high complexity
  barplots_hc <- wrap_plots(barplots_tmp_hc, nrow = 2, guides="collect")+
    plot_annotation(title = paste0("Experiment: ", experiment," - Type: ",type),
                    theme = theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
    )
  
  # save plot
  ggsave(
    paste0(paste0("barplots_",type,"_",experiment,".pdf")),
    plot = barplots,
    path = file.path("expert_feedback/results/plots/"),
    scale = 1,
    #dpi = 300,
    width = width,
    height = height,
    limitsize = FALSE
  )
  
  width2 <- 7.3
  height2 <- 5.3
  options(repr.plot.width=width2, repr.plot.height=height2)
  ggsave(
    paste0(paste0("barplots_",type,"_",experiment,"_hc.pdf")),
    plot = barplots_hc,
    path = file.path("expert_feedback/results/plots/"),
    scale = 1,
    width = width2,
    height = height2,
    limitsize = FALSE
  )
}

#### AGGREGATED BARPLOT

# remove expert_question from positive question
questions[["positive"]] <- questions[["positive"]][questions[["positive"]]!=expert_question]

width <- 6
height <- 6
options(repr.plot.width=width, repr.plot.height=height)

for(type in unique(result_df$type)){
  for (experiment in c(unique(result_df$experiment_name),"ALL")){
    # only sepsis_treatment has type SIM results
    if(type=="SIM" & experiment!="sepsis_treatment"){
      next
      
    }
    barplots_tmp <- list()
    
    for (question_type in c(names(questions), "expert_question")){
      if(question_type=="expert_question"){
        questions_tmp <- expert_question
      } else{
        questions_tmp <- questions[[question_type]]
      }
      
      if(experiment=="ALL"){
        data_tmp <- result_df[(result_df$type==type),]
      }  else {
        data_tmp <- result_df[(result_df$type==type)&(result_df$experiment_name==experiment),]
      }

      result_all <- melt(data = data_tmp, 
                         id.vars = id_cols, 
                         measure.vars = questions_tmp,
                         variable.name = "questions", 
                         value.name = "Likert"
      )
      
      # convert number Likert to string
      result_all$Likert_new <- sapply(X = result_all$Likert, FUN= function(x) likert_scale[x])
      
      # transform result_all DF to have a for each system and Likert value ONE percentage value
      
      # determine percentages of Likert values
      result_plot <- result_all %>%
        group_by(system_name, Likert_new) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)
      
      #result_plot$questions = factor(result_plot$questions, levels = rev(questionsOrder))
      
      # add Likert colors
      result_plot$color <- sapply(X = result_plot$Likert_new, FUN= function(x) likert_scale_cols[x])
      
      # divide neither by 2
      result_plot <- result_plot %>%
        mutate(percentage = ifelse(Likert_new == "neither", percentage / 2, percentage))
      
      # split in positive and negative, but keep 'neither' in both
      result_plot_neg <- result_plot %>%
        filter(grepl("disagree|neither", Likert_new))
      
      result_plot_pos <- result_plot %>%
        filter(!grepl("disagree", Likert_new))
      
      
      result_plot_pos$percentageRound = round(result_plot_pos$percentage, digits = 1)
      result_plot_neg$percentageRound = round(result_plot_neg$percentage, digits = 1)
      
      # barplot
      barplots_tmp[[question_type]] <- ggplot() + 
        geom_bar(data=result_plot_pos, aes(fill=color, x = system_name, y = percentage), position="stack", stat="identity") +
        geom_text(data=result_plot_pos, aes(fill=color, x = system_name, y = percentage, label = percentageRound),
                  size = 3, position = position_stack(vjust = 0.5)) +
        geom_bar(data=result_plot_neg, aes(fill=color, x = system_name, y = -percentage), position="stack", stat="identity") +
        geom_text(data=result_plot_neg, aes(fill=color, x = system_name, y = -percentage, label = percentageRound),
                  size = 3, position = position_stack(vjust = 0.5)) +
        geom_hline(yintercept = 0, color =c("white")) +
        scale_fill_identity("", labels = likert_scale, limits=likert_scale_cols, guide="legend", drop=FALSE) +
        coord_flip() +
        labs(title=paste0("Questions: ", question_type), y="",x="") +
        theme_bw() +
        theme(plot.title = element_text(size=14, hjust=0.5),
              axis.text.y = element_text(hjust=1, vjust=0.5),
              legend.position = "bottom") +
        scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) # 101 required otherwise rounding errors remove data!
    }
    
    barplots <- wrap_plots(barplots_tmp, ncol = 1, guides="collect")+
      plot_annotation(title = paste0("Experiment: ", experiment," - Type: ",type),
                      theme = theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")
      )
    
    # save plot
    ggsave(
      paste0(paste0("aggregated_barplots_",type,"_",experiment,".png")),
      plot = barplots,
      device = "png",
      path = file.path("expert_feedback/results/plots/"),
      scale = 1,
      dpi = 300,
      width = width,
      height = height,
      limitsize = FALSE
    )
  }
}


#### BARPLOTS for direct comparison (better/same/worse)

width <- 6
height <- 3
options(repr.plot.width=width, repr.plot.height=height)

decision_scale <- c('worse','same','better')
decision_scale_cols <- colorRampPalette(c("blue", "grey", "red"))(3)
names(decision_scale_cols) <- decision_scale

# remove expert_question from positive question
questions[["positive"]] <- questions[["positive"]][questions[["positive"]]!=expert_question]


# remove unnecessary columns
data_tmp <- result_df[, c(id_cols, 'type', 'expert', unname(unlist(questions)))]
# transform data simultaneously for all into relative comparison
data_tmp <- data_tmp %>% 
  gather(question, Likert,  -experiment_name, -prompt_name, -system_name, -type, -expert) %>% 
  group_by(experiment_name, prompt_name, type, expert, question) %>%
  summarise(baseline = ifelse(type=="SIM", Likert[system_name == "low_complexity"], Likert[system_name == "baseline"]),
            simulator = Likert[system_name == "high_complexity"]) %>% 
  distinct() %>%  # IMPORTANT ADDITION!
  mutate(decision = ifelse(question %in% questions[["positive"]],
                           ifelse(simulator > baseline, "better",
                                  ifelse(simulator < baseline, "worse", 'same')),
                           ifelse(simulator < baseline, "better",
                                  ifelse(simulator > baseline, "worse", 'same'))
  )
  )



aggregate_df <- data.frame()

for(type in unique(result_df$type)){
  for (experiment in c(unique(result_df$experiment_name),"ALL")){
    
    # only sepsis_treatment has type SIM results
    if(type=="SIM" & experiment!="sepsis_treatment"){
      next
    }
  }
  
  # subset data
  if(experiment=="ALL"){
    result_plot <- data_tmp[(data_tmp$type==type),]
  }  else {
    result_plot <- data_tmp[(data_tmp$type==type)&(data_tmp$experiment_name==experiment),]
  }
  
  # add to aggreagte DF
  tmp_aggregate <- result_plot %>%
    group_by(decision) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  tmp_aggregate$name <- paste0(type,"_",experiment)
  aggregate_df <- rbind(aggregate_df, tmp_aggregate)
  
  # determine counts and percentages
  result_plot <- result_plot %>%
    group_by(question, decision) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  result_plot$question = factor(result_plot$question, levels = rev(questionsOrder))
  
  # add colors
  result_plot$color <- sapply(X = result_plot$decision, FUN= function(x) decision_scale_cols[x])
  
  # divide same by 2
  result_plot <- result_plot %>%
    mutate(percentage = ifelse(decision == "same", percentage / 2, percentage))
  
  # split in positive and negative, but keep 'neither' in both
  result_plot_neg <- result_plot %>%
    filter(grepl("worse|same", decision))
  
  result_plot_pos <- result_plot %>%
    filter(!grepl("worse", decision))
  
  # reorder colors for plotting
  result_plot_neg$color <- factor(result_plot_neg$color, levels=unname(decision_scale_cols)[1:2])
  result_plot_pos$color <- factor(result_plot_pos$color, levels=rev(unname(decision_scale_cols)[2:3]))
  
  result_plot_pos$percentageRound = round(result_plot_pos$percentage, digits = 1)
  result_plot_neg$percentageRound = round(result_plot_neg$percentage, digits = 1)
  
  # barplot
  barplot_tmp <- ggplot() + 
    geom_bar(data=result_plot_pos, aes(fill=color, x = question, y = percentage), position="stack", stat="identity") +
    geom_text(data=result_plot_pos, aes(fill=color, x = question, y = percentage, label = percentageRound),
              size = 3, position = position_stack(vjust = 0.5)) +
    geom_bar(data=result_plot_neg, aes(fill=color, x = question, y = -percentage), position="stack", stat="identity") +
    geom_text(data=result_plot_neg, aes(fill=color, x = question, y = -percentage, label = percentageRound),
              size = 3, position = position_stack(vjust = 0.5)) +
    geom_hline(yintercept = 0, color =c("white")) +
    scale_fill_identity("", labels = decision_scale, limits=decision_scale_cols, guide="legend", drop=FALSE) +
    scale_x_discrete(label=addline_format) +
    coord_flip() +
    labs(title=paste(experiment,type), y="",x="") +
    theme_bw() +
    theme(plot.title = element_text(size=14, hjust=0.5),
          axis.text.y = element_text(hjust=1, vjust=0.5),
          legend.position = "bottom") +
    scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) # 101 required otherwise rounding errors remove data!
  
  # save plot
  ggsave(
    paste0(paste0("direct_barplots_",type,"_",experiment,".pdf")),
    plot = barplot_tmp,
    device = "pdf",
    path = file.path("expert_feedback/results/plots/"),
    scale = 1,
    #dpi = 300,
    width = width,
    height = height,
    limitsize = FALSE
  )
}


# plot aggregate DF                                   
result_plot <- aggregate_df

# add colors
result_plot$color <- sapply(X = result_plot$decision, FUN= function(x) decision_scale_cols[x])

# divide same by 2
result_plot <- result_plot %>%
  mutate(percentage = ifelse(decision == "same", percentage / 2, percentage))

# split in positive and negative, but keep 'neither' in both
result_plot_neg <- result_plot %>%
  filter(grepl("worse|same", decision))

result_plot_pos <- result_plot %>%
  filter(!grepl("worse", decision))

# reorder colors for plotting
result_plot_neg$color <- factor(result_plot_neg$color, levels=unname(decision_scale_cols)[1:2])
result_plot_pos$color <- factor(result_plot_pos$color, levels=rev(unname(decision_scale_cols)[2:3]))

result_plot_pos$percentageRound = round(result_plot_pos$percentage, digits = 1)
result_plot_neg$percentageRound = round(result_plot_neg$percentage, digits = 1)


# barplot
barplot_tmp <- ggplot() + 
  geom_bar(data=result_plot_pos, aes(fill=color, x = name, y = percentage), position="stack", stat="identity") +
  geom_text(data=result_plot_pos, aes(fill=color, x = name, y = percentage, label = percentageRound),
            size = 3, position = position_stack(vjust = 0.5)) +
  geom_bar(data=result_plot_neg, aes(fill=color, x = name, y = -percentage), position="stack", stat="identity") +
  geom_text(data=result_plot_neg, aes(fill=color, x = name, y = -percentage, label = percentageRound),
            size = 3, position = position_stack(vjust = 0.5)) +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("", labels = decision_scale, limits=decision_scale_cols, guide="legend", drop=FALSE) +
  coord_flip() +
  labs(title="AGGREGATED direct comparison", y="",x="") +
  theme_bw() +
  theme(plot.title = element_text(size=14, hjust=0.5),
        axis.text.y = element_text(hjust=1, vjust=0.5),
        legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) # 101 required otherwise rounding errors remove data!


# save plot
ggsave(
  paste0(paste0("AGGREGATED_direct_barplots.pdf")),
  plot = barplot_tmp,
  device = "pdf",
  path = file.path("expert_feedback/results/plots/"),
  scale = 1,
  #dpi = 300,
  width = width,
  height = height,
  limitsize = FALSE
)

###Same but filtered for OUT
result_plot_pos_filt = result_plot_pos[which(startsWith(x = result_plot_pos$name, prefix = 'OUT') ),]
result_plot_neg_filt = result_plot_neg[which(startsWith(x = result_plot_neg$name, prefix = 'OUT') ),]
# barplot
barplot_tmp <- ggplot() + 
  geom_bar(data=result_plot_pos_filt, aes(fill=color, x = name, y = percentage), position="stack", stat="identity") +
  geom_text(data=result_plot_pos_filt, aes(fill=color, x = name, y = percentage, label = percentageRound),
            size = 3, position = position_stack(vjust = 0.5)) +
  geom_bar(data=result_plot_neg_filt, aes(fill=color, x = name, y = -percentage), position="stack", stat="identity") +
  geom_text(data=result_plot_neg_filt, aes(fill=color, x = name, y = -percentage, label = percentageRound),
            size = 3, position = position_stack(vjust = 0.5)) +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("", labels = decision_scale, limits=decision_scale_cols, guide="legend", drop=FALSE) +
  coord_flip() +
  labs(title="AGGREGATED filtered direct comparison", y="",x="") +
  theme_bw() +
  theme(plot.title = element_text(size=14, hjust=0.5),
        axis.text.y = element_text(hjust=1, vjust=0.5),
        legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) # 101 required otherwise rounding errors remove data!

# save plot
ggsave(
  paste0(paste0("AGGREGATED_filered_direct_barplots.pdf")),
  plot = barplot_tmp,
  device = "pdf",
  path = file.path("expert_feedback/results/plots/"),
  scale = 1,
  #dpi = 300,
  width = width,
  height = height,
  limitsize = FALSE
)



###Same but filtered for EXP
result_plot_pos_filt = result_plot_pos[which(startsWith(x = result_plot_pos$name, prefix = 'EXP') ),]
result_plot_neg_filt = result_plot_neg[which(startsWith(x = result_plot_neg$name, prefix = 'EXP') ),]
# barplot
barplot_tmp <- ggplot() + 
  geom_bar(data=result_plot_pos_filt, aes(fill=color, x = name, y = percentage), position="stack", stat="identity") +
  geom_text(data=result_plot_pos_filt, aes(fill=color, x = name, y = percentage, label = percentageRound),
            size = 3, position = position_stack(vjust = 0.5)) +
  geom_bar(data=result_plot_neg_filt, aes(fill=color, x = name, y = -percentage), position="stack", stat="identity") +
  geom_text(data=result_plot_neg_filt, aes(fill=color, x = name, y = -percentage, label = percentageRound),
            size = 3, position = position_stack(vjust = 0.5)) +
  
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("", labels = decision_scale, limits=decision_scale_cols, guide="legend", drop=FALSE) +
  coord_flip() +
  labs(title="AGGREGATED filtered direct comparison", y="",x="") +
  theme_bw() +
  theme(plot.title = element_text(size=14, hjust=0.5),
        axis.text.y = element_text(hjust=1, vjust=0.5),
        legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-100,100,25), limits=c(-101,101)) # 101 required otherwise rounding errors remove data!

# save plot
ggsave(
  paste0(paste0("AGGREGATED_EXPfilered_direct_barplots.pdf")),
  plot = barplot_tmp,
  device = "pdf",
  path = file.path("expert_feedback/results/plots/"),
  scale = 1,
  #dpi = 300,
  width = width,
  height = height,
  limitsize = FALSE
)
