#### load libraries
library(dplyr)
library(stringr)
library(tidyr)


### STATISTICAL TESTS

# Wilcox rank sum test (https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/wilcox.test)
wilcox_rank_test <- function(better_count, same_count, worse_count) {
  total_responses <- better_count + same_count + worse_count
  
  # Create response vectors for method A and method B
  observed <- c(rep(1, better_count), rep(0, same_count), rep(-1, worse_count))
#   expected <- rep(0, total_responses)
    
  expected <- c(rep(-1, better_count), rep(0, same_count), rep(1, worse_count))
  
  # Perform the Wilcoxon signed-rank test
  wilcoxon_test <- wilcox.test(observed, expected, paired = TRUE)
  
  return(wilcoxon_test$p.value)
}

# Chi Square test
chi_square_test <- function(better_count, same_count, worse_count) {
  observed <- c(better_count, same_count, worse_count)
  total_responses <- sum(observed)
  #   expected <- rep(total_responses / 3, 3) # EXPECTED ie null hypothesis
  #     expected <- c(0, total_responses, 0) # leads to many NA -> problem?
  expected <- c(0.25/10 * total_responses, 9.5/10 * total_responses, 0.25/10 * total_responses) # weaker EXPECTED ie null hypothesis
  chisq_test <- chisq.test(observed, p = expected / total_responses)
  return(chisq_test$p.value)
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
dir.create(file.path("expert_feedback/results/stats"), showWarnings = FALSE)

# load results
result_df <- read.csv(file=file.path("expert_feedback/results/expert_feedback_results.csv"))


# DIRECT COMPARISON
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

        # subset data
        if(experiment=="ALL"){
        result_plot <- data_tmp[(data_tmp$type==type),]
        }  else {
        result_plot <- data_tmp[(data_tmp$type==type)&(data_tmp$experiment_name==experiment),]
        }

        # add to aggreagte DF (will be built by loop)
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

        # statistics
        result_stats <- result_plot %>%
        group_by(question) %>%
        summarise(better_count = ifelse(length(count[decision == "better"])==0, 0, count[decision == "better"]),
                  same_count = ifelse(length(count[decision == "same"])==0, 0, count[decision == "same"]),
                  worse_count = ifelse(length(count[decision == "worse"])==0, 0, count[decision == "worse"])) %>%
        mutate(across(c(better_count, same_count, worse_count), as.double)) %>%
        rowwise() %>%
        mutate(chi_square_p = chi_square_test(better_count, same_count, worse_count),
              wilcox_rank_p = wilcox_rank_test(better_count, same_count, worse_count))

        write.csv(result_stats, file.path("expert_feedback/results/stats/", paste0(type,"_",experiment,".csv")), row.names=FALSE)
    }
}



# aggregated result statistics
aggregated_stats <- aggregate_df %>%
        group_by(name) %>%
        summarise(better_count = ifelse(length(count[decision == "better"])==0, 0, count[decision == "better"]),
                  same_count = ifelse(length(count[decision == "same"])==0, 0, count[decision == "same"]),
                  worse_count = ifelse(length(count[decision == "worse"])==0, 0, count[decision == "worse"])) %>%
        mutate(across(c(better_count, same_count, worse_count), as.double)) %>%
        rowwise() %>%
        mutate(chi_square_p = chi_square_test(better_count, same_count, worse_count),
              wilcox_rank_p = wilcox_rank_test(better_count, same_count, worse_count))

write.csv(aggregated_stats, file.path("expert_feedback/results/stats/AGGREGATED.csv"), row.names=FALSE)
