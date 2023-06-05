loadGeneVect <- function(filePathSel){
  gene.vect = readLines(filePathSel)
  gene.vect = gene.vect[2:length(gene.vect)]
  gene.vect = gsub('\\ \\([0-9]+)$','',gene.vect)
  return(gene.vect)
}

parseOutputPrompts <- function(simulatorGithubLocalDir,experimentSel){
  prompts = list.files(file.path(simulatorGithubLocalDir,'experiments',experimentSel,'prompts'))
  #system_messages = setdiff(list.files(file.path(simulatorGithubLocalDir,'system_messages'), include.dirs = F),'archive')
  system_messages = unique(str_split_fixed(list.files(file.path(simulatorGithubLocalDir,'experiments',experimentSel,'ai_messages')), '--',2)[,1])
  
  outputList = list()
  for (system_message_sel in system_messages){
    print(system_message_sel)
    outputList[[system_message_sel]] = list()
    aiFiles = list.files(path=file.path(simulatorGithubLocalDir,'experiments',experimentSel,'ai_messages'), pattern = paste0('^',system_message_sel,'.*'))
    completedPrompts = gsub(paste0(system_message_sel,'\\-\\-'),'',aiFiles)
    print(paste0('uncompleted runs for ',paste(setdiff(prompts,completedPrompts),collapse = ' ')))
    for (fileSel in aiFiles){
      promptSel=gsub(paste0(system_message_sel,'\\-\\-'),'',fileSel)
      rawData = readLines(con = file.path(simulatorGithubLocalDir,'experiments',experimentSel,'ai_messages',fileSel), warn=FALSE)
      outComeRow = grep('^[ ]+outcome: .*',rawData)
      outcome = gsub('^[ ]+outcome:[ ]+','',rawData[[outComeRow]])
      #if (outcome=='likely or unlikely'){browser()}
      outputList[[system_message_sel]][[promptSel]] =outcome
    }
  }
  return(outputList)
}