#### SETUP ####

  # clear environment & change language to english
  rm(list=ls())
  Sys.setenv(LANG = "en")
  
  # set working directory to current file's directory
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # load custom functions
  source("Functions/printReport.R")
  source("Functions/libs.R")
  
  
  # load libraries
  libraries <- c(
    "dplyr",
    "tidyr",
    "stringr",
    "rstatix",
    "BayesFactor",
    "ggplot2")
  
  libs(libraries)
  rm(libraries)


#### SCRIPT VARIABLES ####

  # Corrected data available? (1 = yes)
  corrected <- 1
  
  # New data collected? (1 = yes)
  newData <-  0
  
  # Print report? (1 = yes)
  report <- 1


#### IMPORTING DATA ####
if (corrected == 0) {  
  if (newData == 1) {
    
    fullTable <- read.delim("Data/Untitled.txt", skip = 0, skipNul = T, fileEncoding="UCS-2LE")
    
    relevantTable <- fullTable %>% 
      filter(BlockCondition %in% c("long", "short")) %>% 
      select(Subject,
             ProlificNo,
             Age, 
             Sex,
             check.RESP,
             BlockCondition,
             UnrelAcc = TarRecallU.ACC.LogLevel6.,
             RelAcc = TarRecallR.ACC,
             TarRecallR.RESP,
             TarRecallR.CRESP,
             TarRecallU.RESP.LogLevel6.,
             TarRecallU.CRESP.LogLevel6.,
             RESP_LTM = RecallDisplay.RESP,
             CRESP_LTM = RecallDisplay.CRESP,
             TypeLTM = Type) %>% 
      filter(!is.na(RelAcc) | !is.na(UnrelAcc) | CRESP_LTM != "")
    
    saveRDS(relevantTable, "Data/relevantTable.RDS")
  
  }else{
    
    relevantTable <- readRDS("Data/relevantTable.RDS")
    
  }
  
  
  table1 <- relevantTable %>%
    # Adding 'NA' to empty cells
    mutate(TarRecallR.RESP = ifelse(is.na(RelAcc), NA, TarRecallR.RESP),
           TarRecallR.CRESP = ifelse(is.na(RelAcc), NA, TarRecallR.CRESP),
           TarRecallU.RESP.LogLevel6. = ifelse(is.na(UnrelAcc), NA, TarRecallU.RESP.LogLevel6.),
           TarRecallU.CRESP.LogLevel6. = ifelse(is.na(UnrelAcc), NA, TarRecallU.CRESP.LogLevel6.),
    # Adding "TypeSTM column
           TypeSTM = case_when(
             !is.na(UnrelAcc) ~ "Unrelated",
             !is.na(RelAcc) ~ "Related"),
    # Merging Related and Unrelated columns together 
           RESP_STM = coalesce(TarRecallR.RESP, TarRecallU.RESP.LogLevel6.),
           CRESP_STM = coalesce(TarRecallR.CRESP, TarRecallU.CRESP.LogLevel6.)
    ) %>% 
    # Removing redundant columns
    select(-c(UnrelAcc, 
              RelAcc, 
              TarRecallR.RESP,
              TarRecallR.CRESP,
              TarRecallU.RESP.LogLevel6.,
              TarRecallU.CRESP.LogLevel6.)
    ) 
  
  
  # Removing superfluous characters from the inputs 
  table1 <- table1 %>%  
    mutate(nWords = str_count(RESP_STM, "SPACE") + 1,
           CRESP_STM = str_remove(CRESP_STM, "\\{ENTER\\}"),
           RESP_STM = str_remove_all(RESP_STM, 
          "\\{ENTER\\}|\\{SHIFT\\}|\\{CAPSLOCK\\}|\\{LEFTARROW\\}|\\{\\?\\}|\\{CONTROL\\}|\\{RIGHTARROW\\}|\\{UPARROW\\}|\\{ALT\\}|\\{INSERT\\}"
           ),
           CRESP_LTM = str_remove_all(CRESP_LTM, "\\{ENTER\\}"),
           RESP_LTM = str_remove_all(RESP_LTM, 
          "\\{ENTER\\}|\\{SHIFT\\}|\\{CAPSLOCK\\}|\\{LEFTARROW\\}|\\{\\?\\}|\\{CONTROL\\}|\\{RIGHTARROW\\}|\\{UPARROW\\}|\\{ALT\\}|\\{INSERT\\}|\\{SPACE\\}"
           )
    )
  
  
  # Breaking up the inputs into separate columns for each serial position
  table2 <- table1 %>% 
    separate(col = RESP_STM, 
             into = paste0("RESP", 1:max(table1$nWords, na.rm = TRUE)),
             sep = "{SPACE}") %>% 
    separate(col = CRESP_STM, 
             into = paste0("CRESP", 1:4),
             sep = "{SPACE}"
    )
  
  # saving the file for manual corrections
  write.csv(table2, "Data/tbl2.csv")

}else{
  
  # read the file with corrected outputs
  table2 <- read.csv("Data/tbl2_corrected.csv", sep = ";")
  
  
}  
  

#### ACCURACY CALCULATION ####

table3 <- table2 %>%
  # Calculating LTM accuracy
  mutate( AccuracyLTM = case_when(
    is.na(nWords) ~ as.integer(RESP_LTM == CRESP_LTM)),
  # Calculating STM Serial Score
    AccuracySTM = (
       ifelse(is.na(RESP1 == CRESP1), 0, RESP1 == CRESP1) +
       ifelse(is.na(RESP2 == CRESP2), 0, RESP2 == CRESP2) +
       ifelse(is.na(RESP3 == CRESP3), 0, RESP3 == CRESP3) +
       ifelse(is.na(RESP4 == CRESP4), 0, RESP4 == CRESP4)
    )/4,
  # Calculating STM Item Memory
    AccuracySTMItem = (
          RESP1 %in% c(CRESP1, CRESP2, CRESP3, CRESP4) +
          RESP2 %in% c(CRESP1, CRESP2, CRESP3, CRESP4) +
          RESP3 %in% c(CRESP1, CRESP2, CRESP3, CRESP4) +
          RESP4 %in% c(CRESP1, CRESP2, CRESP3, CRESP4)
      )/4,
  # Calculating STM Serial Memory (conditionalized)
    AccuracySTMSerial = ifelse(AccuracySTM == 0, 0, AccuracySTM/AccuracySTMItem),
  # Transforming variables into factors
    Subject = factor(Subject),
    BlockCondition = factor(BlockCondition),
    TypeSTM = factor(TypeSTM),
    TypeLTM = factor(TypeLTM)
  )


# Counting the number of subjects
N = length(levels(table3$Subject))

# Calculating mean accuracy for individual subjects
Subjects <- table3 %>% 
  group_by(Subject, ProlificNo) %>%
  summarise(LTM = mean(AccuracyLTM, na.rm = T),
            STM = mean(AccuracySTM, na.rm = T),
            STM_s = mean(AccuracySTMSerial, na.rm = T),
            STM_i = mean(AccuracySTMItem, na.rm = T)) %>% 
  arrange(LTM)



#### STM: ANALYSES & PLOTS ####

# Calculating mean accuracy by condition for individual subjects
tableSTM <- table3 %>% 
  select(Subject, BlockCondition, TypeSTM, AccuracySTM, AccuracySTMSerial, AccuracySTMItem) %>% 
  filter(!is.na(TypeSTM)) %>% 
  group_by(Subject, BlockCondition, TypeSTM) %>% 
  summarise(meanAccuracy = mean(AccuracySTM),
            meanSerial = mean(AccuracySTMSerial),
            meanItem = mean(AccuracySTMItem)) %>% 
  ungroup()


# Calculating mean accuracy by condition for the whole sample
STMmeans <- tableSTM %>% 
  group_by(BlockCondition, TypeSTM) %>% 
  summarise(mean = mean(meanAccuracy, na.rm = T),
            sd = sd(meanAccuracy),
            se = sd/sqrt(N-1))


STMmeans_i <- tableSTM %>% 
  group_by(BlockCondition, TypeSTM) %>% 
  summarise(mean = mean(meanItem, na.rm = T),
            sd = sd(meanItem),
            se = sd/sqrt(N-1))

STMmeans_s <- tableSTM %>% 
  group_by(BlockCondition, TypeSTM) %>% 
  summarise(mean = mean(meanSerial, na.rm = T),
            sd = sd(meanSerial),
            se = sd/sqrt(N-1))


# 2-way anova: raw accuracy
anova2waySTM <- anova_test(tableSTM, dv = meanAccuracy, wid = Subject, within = c(BlockCondition, TypeSTM))

# Bayesian anova
anovaBF(meanAccuracy ~ TypeSTM * BlockCondition, data = tableSTM)

# 2-way anova: serial memory
anova2waySTMserial <- anova_test(tableSTM, dv = meanSerial, wid = Subject, within = c(BlockCondition, TypeSTM))

# 2-way anova: item memory
anova2waySTMitem <- anova_test(tableSTM, dv = meanItem, wid = Subject, within = c(BlockCondition, TypeSTM))



# Generating plots
plot_STM <- STMmeans %>% 
  ggplot(aes(BlockCondition, mean)) +
  geom_line(aes(group=TypeSTM, color = TypeSTM), linewidth = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  ggtitle("STM: raw accuracy") +
  ylim(.5,1)

plot_STM_s <- STMmeans_s %>% 
  ggplot(aes(BlockCondition, mean)) +
  geom_line(aes(group=TypeSTM, color = TypeSTM), linewidth = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  ggtitle("STM: serial memory") +
  ylim(.5,1)

plot_STM_i <- STMmeans_i %>% 
  ggplot(aes(BlockCondition, mean)) +
  geom_line(aes(group=TypeSTM, color = TypeSTM), linewidth = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  ggtitle("STM: item memory") +
  ylim(.5,1)

plot_STM
plot_STM_s
plot_STM_i



#### LTM: ANALYSES & PLOTS ####

# Calculating mean accuracy by condition for individual subjects
tableLTM <- table3 %>% 
  select(Subject, BlockCondition, TypeLTM, AccuracyLTM) %>% 
  filter(TypeLTM != "") %>% 
  group_by(Subject, BlockCondition, TypeLTM) %>% 
  summarise(meanAccuracy = mean(AccuracyLTM)) %>% 
  ungroup()


# Calculating mean accuracy by condition for the whole sample
LTMmeans <- tableLTM %>% 
  group_by(BlockCondition, TypeLTM) %>% 
  summarise(mean = mean(meanAccuracy),
            sd = sd(meanAccuracy),
            se = sd/sqrt(N-1))


# 2-way anova
anova2wayLTM <- anova_test(tableLTM, dv = meanAccuracy, wid = Subject, within = c(BlockCondition, TypeLTM))

# Bayesian anovas
anovaBF(meanAccuracy ~ TypeLTM * BlockCondition, 
        data = tableLTM)

anovaBF(meanAccuracy ~ TypeLTM * BlockCondition + Subject, 
        data = tableLTM,
        whichRandom = "Subject")




# Generating plots
plot_LTM <- LTMmeans %>% 
  ggplot(aes(BlockCondition, mean)) +
  geom_line(aes(group=TypeLTM, color = TypeLTM), linewidth = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  ggtitle("LTM: raw accuracy") +
  ylim(.3,.8)


plot_LTM

#### PRINT REPORT ####

# Run custom function
printReport(report)
