### this code will not run as-is, because we are unable to share the data


library(table1)
library(reshape2)
library(survival)
# library(rstanarm)
library(tidyverse)
library(readxl)
library(cmprsk)
library(survminer)
library(gtsummary)
library(gt)

## save out package versions
packages <- as.data.frame(installed.packages()) %>%
  filter(Package %in% c("table1", "reshape2", "survival", "tidyverse", "readxl",
                        "cmprik", "survminer", "gtsummary", "gt",
                        "rstan", "rstanarm", "lme4"))

write.csv(packages, "my_packages.csv")

dir <- "~/Library/CloudStorage/Box-Box/pooling camostat - shared data/"
demo_aarhus <- read_excel(paste0(dir,"Aarhus camosat data/aarhus_camostat_characteristics_data_shell.xlsx"), sheet = 2) %>%
  dplyr::mutate(participantID = as.character(participantID)) #%>% dplyr::select(trial, participantID,treatment,age,sex)
symp_aarhus <- read_excel(paste0(dir,"Aarhus camosat data/aarhus_camostat_symptom_data_shell_v2.xlsx"), sheet = 2) %>%
  dplyr::mutate(participantID = as.character(participantID)) %>% 
  dplyr::mutate(study_day = as.numeric(study_day))

demo_cruk <- read_excel(paste0(dir,"CRUK camosat data/CRUK_camostat_data.xlsx"), sheet = 1) %>% 
  dplyr::mutate(participantID=as.character(participantid)) %>% dplyr::select(-c(participantid)) #%>% dplyr::select(trial, participantID,treatment,age,sex)
symp_cruk <- read_excel(paste0(dir,"CRUK camosat data/CRUK_camostat_data.xlsx"), sheet = 2) %>% 
  dplyr::mutate(participantID=as.character(participantid)) %>% dplyr::select(-c(participantid))  %>% 
  dplyr::mutate(study_day = as.numeric(study_day))

demo_ghent <- read_excel(paste0(dir,"Ghent camostat data/Characteristics_age corrected.xlsx"), sheet = 2)  %>% 
  dplyr::filter(!is.na(treatment)) %>% 
  dplyr::filter(!participantID %in% c(6,12,13,14,19,52,55,57,8,10,17,59,20,108)) %>% 
  dplyr::mutate(participantID = as.character(participantID)) #%>% dplyr::select(trial, participantID,treatment,age,sex)
symp_ghent <- read_excel(paste0(dir,"Ghent camostat data/Symptoms_shell2_complete.xlsx"), sheet = 2) %>% 
  dplyr::filter(!participantID %in% c(6,12,13,14,19,52,55,57,8,10,17,59,20,108)) %>% 
  dplyr::mutate(participantID = as.character(participantID)) %>% 
  dplyr::mutate(study_day = as.numeric(study_day))

demo_incmnsz <- read_excel(paste0(dir,"INCMNSZ camostat data/camostat_characteristics_data_shell.xlsx"), sheet = 2) %>% 
  dplyr::mutate(participantID = as.character(participantID)) #%>% dplyr::select(trial, participantID,treatment,age,sex)
symp_incmnsz <- read_excel(paste0(dir,"INCMNSZ camostat data/camostat_symptom_data_shell.xlsx"), sheet = 2) %>% 
  dplyr::mutate(participantID = as.character(participantID)) %>% 
  dplyr::mutate(study_day = as.numeric(study_day))#%>% dplyr::select(trial, participantID, study_day,
viro_incmnsz <- read_excel(paste0(dir,"INCMNSZ camostat data/camostat_virology_data_shell.xlsx"), sheet = 2) %>% 
  dplyr::mutate(participantID = as.character(participantID))

camostat.id <- read_csv("~/camo-IDs.csv")

stanford <- read_csv("~/CSV_export_all_2021-11-05-200821840.csv")
stanford$participant_id <- ifelse(stanford$participant_id == 'Camo 024', 'CAMO-024', stanford$participant_id)

# ### added in the "|01-" to gsub and now get 51
stanford <- stanford %>% 
  dplyr::filter(participant_id != 'CAMO-016_v1') %>%
  #filter(grepl('CAMO', participant_id)) %>%
  dplyr::mutate(participantID = gsub("_.*|01-", "", participant_id))
# stanford1 <- inner_join(stanford, camostat.id, by = c("participantID" = "x"))
stanford1 <- inner_join(stanford, camostat.id, by = c("participantID" = "x"))
length(unique(stanford1$participantID)) ### n =51


demo1 <- stanford1 %>% 
  dplyr::filter(redcap_event_name == 'Screening')  %>% 
  dplyr::arrange(participantID, rowSums(is.na(.))) %>% 
  dplyr::distinct(participantID, .keep_all = T)

demo_stanford <- stanford1 %>% 
  dplyr::filter(redcap_event_name == 'Screening') %>% 
  dplyr::mutate(age = age_c, trial = 'Stanford') %>% dplyr::select(participantID, age, sex, trial) %>% unique() %>% 
  dplyr::mutate(sex = ifelse(sex == 0, 'M','F'))


trt_stanford <- read.csv("~/camo-treatment-codes-for-IMA.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(participantID = Used.By,
                treatment = case_when(Random.Group=="Placebo" ~ 0,
                                      Random.Group=="Camostat" ~ 1))

demo_stanford1 <- merge(demo_stanford, trt_stanford, by="participantID", all.x = TRUE)

demo_paris <- read_delim(paste0(dir, "Paris camostat data/camostat_characteristics_France.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  dplyr::mutate(participantID = as.character(participantID))

symp_paris <- read_delim(paste0(dir,"Paris camostat data/camostat_symptoms_France.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  dplyr::mutate(participantID = as.character(participantID)) %>% 
  dplyr::mutate(study_day = as.numeric(study_day))

demo <- dplyr::bind_rows(demo_aarhus,demo_cruk,demo_ghent,demo_incmnsz,demo_stanford1,demo_paris) %>% dplyr::select(-c("...19","...21"))
demo$hospitalized <- ifelse(demo$trial == 'Stanford' & demo$participantID != 'CAMO-033',0,
                            ifelse(demo$participantID == 'CAMO-033',1,demo$hospitalized))
demo$hosp_day <- ifelse(is.na(demo$hosp_day), demo$hosp_LOS, demo$hosp_day)
demo$death <- ifelse(demo$trial == 'Stanford',0,demo$death)
table1(~ age + sex + as.factor(treatment)| trial, data = demo)
table1(~ age + sex| trial + treatment, data = demo)
table1(~ as.factor(hospitalized) + as.factor(supp_ox) + as.factor(death)| trial + treatment, data = demo)
table1(~ as.factor(hospitalized) + as.factor(death)| trial + treatment, data = demo)
table1(~ as.factor(hospitalized) + as.factor(death)| trial, data = demo)
demo_missing <- as.data.frame(colMeans(is.na(demo))*100)
rm(demo_aarhus, demo_cruk, demo_ghent, demo_incmnsz, demo_paris,demo_stanford, demo_stanford1)

symptom <- c("nasal_congestion", "cough",
             "fatigue","temp","sob","nausea","sore_throat","diarrhea","ache","taste","smell")


daily_sp <- stanford1 %>% 
  dplyr::filter(grepl(pattern = ("Daily Symptom Survey"), redcap_event_name, ignore.case = T)) %>% 
  dplyr::select("participantID","redcap_event_name",grep(paste(symptom, collapse = "|"), colnames(stanford), ignore.case = T))
symp_var <- colnames(daily_sp[-c(1:2)])
ace.daily <-melt(daily_sp, id= c("participantID","redcap_event_name"), measure.vars = symp_var)
ace.daily$value[ace.daily$value == ""] <- NA
ace.daily <- ace.daily %>% dplyr::filter(!is.na(value))
ace.daily$study_day <- parse_number(ace.daily$redcap_event_name)
ace.daily$variable <- gsub("\\_.*","",ace.daily$variable)
ace.daily <- ace.daily %>% dplyr::select(-c(redcap_event_name))

tmp <- ace.daily %>% dplyr::filter(variable == 'Nasal') %>% dplyr::select(-c(variable))
colnames(tmp) <- c('participantID','nasal_congestion','study_day')
tmp1 <- ace.daily %>% dplyr::filter(variable == 'aches') %>% dplyr::select(-c(variable))
colnames(tmp1) <- c('participantID','body_ache','study_day')
tmp2 <- ace.daily %>% dplyr::filter(variable == 'cough') %>% dplyr::select(-c(variable))
colnames(tmp2) <- c('participantID','cough','study_day')
tmp3 <- ace.daily %>% dplyr::filter(variable == 'diarrhea') %>% dplyr::select(-c(variable))
colnames(tmp3) <- c('participantID','diarrhea','study_day')
tmp8 <- ace.daily %>% dplyr::filter(variable == 'fatigue') %>% dplyr::select(-c(variable))
colnames(tmp8) <- c('participantID','fatigue','study_day')
tmp4 <- ace.daily %>% dplyr::filter(variable == 'nausea') %>% dplyr::select(-c(variable))
colnames(tmp4) <- c('participantID','nausea','study_day')
tmp5 <- ace.daily %>% dplyr::filter(variable == 'sob') %>% dplyr::select(-c(variable))
colnames(tmp5) <- c('participantID','sob','study_day')
tmp6 <- ace.daily %>% dplyr::filter(variable == 'Sore') %>% dplyr::select(-c(variable))
colnames(tmp6) <- c('participantID','sore_throat','study_day')
tmp7 <- ace.daily %>% dplyr::filter(variable == 'temp') %>% dplyr::select(-c(variable))
colnames(tmp7) <- c('participantID','temp_F','study_day')
tmp9 <- ace.daily %>% dplyr::filter(variable == 'taste') %>% dplyr::select(-c(variable))
colnames(tmp9) <- c('participantID','taste','study_day')
tmp10 <- ace.daily %>% dplyr::filter(variable == 'smell') %>% dplyr::select(-c(variable))
colnames(tmp10) <- c('participantID','smell','study_day')


df_list <- list(tmp,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9, tmp10)
symp_stanford <- df_list %>% purrr::reduce(full_join, by=c('participantID','study_day'))
symp_stanford$trial <- 'Stanford'

symp_stanford1 <- symp_stanford %>% 
  dplyr::mutate(short_of_breath = sob) %>%
  dplyr::select(trial,participantID,study_day,nasal_congestion,cough,short_of_breath,
                sore_throat,fatigue,diarrhea,nausea,body_ache,temp_F,taste,smell)
symp_stanford1$study_day <- as.numeric(symp_stanford1$study_day)

symp <- dplyr::bind_rows(symp_aarhus,symp_cruk,symp_ghent,symp_incmnsz,symp_stanford1,symp_paris) %>% 
  dplyr::mutate(sob = ifelse(is.na(short_of_breath), short_of_breatch, short_of_breath)) %>% 
  dplyr::mutate(myalgia = ifelse(is.na(myalgia), body_ache, myalgia)) %>%
  dplyr::mutate(sore_throat = ifelse(is.na(sore_throat),sore_throast, sore_throat)) %>% 
  dplyr::mutate(taste1 = ifelse(is.na(abnormal_taste),loss_of_taste, abnormal_taste)) %>%
  dplyr::mutate(smell1 = ifelse(is.na(abnormal_smell),loss_of_smell, abnormal_smell)) %>%
  dplyr::mutate(taste0 = ifelse(is.na(taste),taste1, taste)) %>% 
  dplyr::mutate(smell0 = ifelse(is.na(smell),smell1, smell )) %>% 
  mutate(taste_smell = ifelse(taste0-smell0<=0, taste0,smell0)) %>% 
  dplyr::mutate(abnormal_taste_smell1 = ifelse(is.na(taste_smell), abnormal_taste_smell, taste_smell)) %>% 
  dplyr::filter(trial != 'INCMNSZ-MX') %>% filter(trial != 'France') %>% 
  #filter(study_day == 1) %>%
  dplyr::select(-c(short_of_breath,short_of_breatch, body_ache,sore_throast, taste0, taste1, smell0, smell1, abnormal_taste, abnormal_smell, abnormal_taste_smell,taste_smell))


table(symp$trial)
symp_missing <- as.data.frame(colMeans(is.na(symp))*100)
colnames(symp_missing) <- "Missing (%)"
#rm(list=ls()[!ls() %in% c('demo','symp','symp_survival')])

symptom1 <- c("nasal_congestion","cough","sob",
              "sore_throat","fatigue","diarrhea","nausea","myalgia","abnormal_taste_smell1")
symp_survival1 <-reshape2::melt(symp, id= c("trial","participantID","study_day"), measure.vars = symptom1)

# rm(list=ls()[!ls() %in% c('demo','symp','symptom1','symp_survival1','trt_check')])

### symptom at 1st day 
symp1d <- NULL
for (i in 1:9){
  tmp <- symp_survival1 %>% dplyr::filter(variable == symptom1[i])
  tmp <- tmp[order(tmp$trial,tmp$participantID, as.numeric(as.character(tmp$study_day))),]
  tmp <- tmp %>% dplyr::filter(study_day <= 14) %>% 
    dplyr::group_by(trial, participantID) %>%
    dplyr::mutate(value = as.numeric(value)) #%>% filter(study_day !=1)
  df <- as.data.frame(spread(tmp, key = study_day, value = value))
  df[, "max"] <- do.call(pmax, c(df[5:15], list(na.rm=TRUE)))
  df$baseline <- ifelse(df$`1` >1, 1,0)
  df$other <- ifelse(df$`1` <=1 & df$max >1, 1,0)
  df$total <- df$baseline + df$other
  table(df$baseline, df$other)
  rownames(df) <- df$participantID
  df1 <- df %>% dplyr::select(c(baseline,other,total)) 
  #dplyr::select(-c(trial, participantID, variable))
  colnames(df1) <- paste0(colnames(df1),"_",symptom1[i])
  df1$participantID <- rownames(df1)
  symp1d[[i]] <- df1
  #colnames(symp1[[i]]) <- c("participantID", paste0('symp_',symptom1[i]), paste0('time_',symptom1[i]))
}

survival_symp1d <- symp1d %>% purrr::reduce(full_join, by=c('participantID'))
rownames(survival_symp1d) <- survival_symp1d$participantID
#survival_symp1d <- survival_symp1d %>% dplyr::select(-(participantID))
#colSums(survival_symp1d, na.rm = T)

### symptom improvement 
symp1 <- NULL
for (i in 1:length(symptom1)){
  tmp <- symp_survival1 %>% dplyr::filter(variable == symptom1[i])
  tmp <- tmp[order(tmp$trial,tmp$participantID, as.numeric(as.character(tmp$study_day))),]
  tmp <- tmp %>% dplyr::filter(study_day <= 14) %>% 
    dplyr::group_by(trial, participantID) %>%
    dplyr::mutate(Diff = as.numeric(value - lag(value))) %>% dplyr::filter(study_day !=1) %>% dplyr::select(-c(value))
  df <- as.data.frame(spread(tmp, key = study_day, value = Diff))
  rownames(df) <- df$participantID
  df1 <- as.data.frame(df[-(1:3)])
  df1[df1>0] <- 8
  df1[df1<=-2] <- 1
  df1[df1==-1] <- 0
  df1[is.na(df1)] <- 9
  mm<-(apply(df1,1,paste,collapse=""))
  toMatch <- c("10","19")
  df1$time <- as.numeric(as.data.frame(str_locate(mm,paste0(toMatch, collapse = "|")))$start + 1)
  df1$c <- as.numeric(ifelse(!is.na(df1$time),1,0))
  df1$time[is.na(df1$time)] <- 14
  df1$participantID <- rownames(df1)
  df0 <- as.data.frame(df[c(1:3)])
  df2 <- left_join(df0,df1, by = 'participantID')
  print(table(df2$trial, df2$time))
  symp1[[i]] <- df2 %>% dplyr::select(c(participantID,c,time))
  colnames(symp1[[i]]) <- c("participantID", paste0('symp_',symptom1[i]), paste0('time_',symptom1[i]))
}

### symptom resolution
symp_res1 <- c("nasal_congestion","sob",
               "sore_throat","diarrhea",
               "nausea","myalgia")
symp_res <- NULL
for (i in 1:length(symp_res1)){
  tmp <- symp_survival1 %>% dplyr::filter(variable == symp_res1[i])
  tmp <- tmp[order(tmp$trial,tmp$participantID, as.numeric(as.character(tmp$study_day))),]
  tmp1 <- tmp %>% dplyr::filter(study_day <= 14) %>% 
    group_by(trial, participantID)
  df_res <- as.data.frame(spread(tmp1, key = study_day, value = value))
  rownames(df_res) <- df_res$participantID
  df1 <- as.data.frame(df_res[-(1:3)])
  df1[is.na(df1)] <- 9
  mm<-(apply(df1,1,paste,collapse=""))
  toMatch <- c("00")
  df1$time <- as.numeric(as.data.frame(str_locate(mm,paste0(toMatch, collapse = "|")))$start + 1)
  df1$c <- as.numeric(ifelse(!is.na(df1$time),1,0))
  df1$time[is.na(df1$time)] <- 15
  df1$participantID <- rownames(df1)
  df0 <- as.data.frame(df_res[c(1:3)])
  df2 <- left_join(df0,df1, by = 'participantID')
  print(table(df2$trial, df2$time))
  symp_res[[i]] <- df2 %>% dplyr::select(c(participantID,c,time))
  colnames(symp_res[[i]]) <- c("participantID", paste0('sympres_',symp_res1[i]), paste0('timeres_',symp_res1[i]))
}

symp_res2 <- c("cough","fatigue")
symp_res0 <- NULL
for (i in 1:length(symp_res2)){
  tmp <- symp_survival1 %>% dplyr::filter(variable == symp_res2[i])
  tmp <- tmp[order(tmp$trial,tmp$participantID, as.numeric(as.character(tmp$study_day))),]
  tmp1 <- tmp %>% dplyr::filter(study_day <= 14) %>% 
    group_by(trial, participantID)
  df_res <- as.data.frame(spread(tmp1, key = study_day, value = value))
  rownames(df_res) <- df_res$participantID
  df1 <- as.data.frame(df_res[-(1:3)])
  df1[is.na(df1)] <- 9
  mm<-(apply(df1,1,paste,collapse=""))
  toMatch <- c("00","11","01","10")
  df1$time <- as.numeric(as.data.frame(str_locate(mm,paste0(toMatch, collapse = "|")))$start + 1)
  df1$c <- as.numeric(ifelse(!is.na(df1$time),1,0))
  df1$time[is.na(df1$time)] <- 15
  df1$participantID <- rownames(df1)
  df0 <- as.data.frame(df_res[c(1:3)])
  df2 <- left_join(df0,df1, by = 'participantID')
  print(table(df2$trial, df2$time))
  symp_res0[[i]] <- df2 %>% dplyr::select(c(participantID,c,time))
  colnames(symp_res0[[i]]) <- c("participantID", paste0('sympres_',symp_res2[i]), paste0('timeres_',symp_res2[i]))
}


survival_symp <- symp1 %>% purrr::reduce(full_join, by=c('participantID'))
rownames(survival_symp) <- survival_symp$participant_id

survival_symp <- survival_symp %>%
  dplyr::mutate(any_symp = rowSums(dplyr::select(., starts_with('symp')),na.rm=TRUE),
                time_symp =invoke(pmin, dplyr::select(., starts_with('time')), na.rm = TRUE)
  )

survival_symp$symp_symp = ifelse(survival_symp$any_symp==0, 0, 1)


survival_symp1d <- inner_join(demo, survival_symp1d, by = 'participantID')

survival_symp <- inner_join(survival_symp, survival_symp1d, by = 'participantID')
survival_symp$total_symp <- ifelse(survival_symp$any_symp == 0, 0,1)


survival_res <- symp_res %>% purrr::reduce(full_join, by=c('participantID'))
rownames(survival_res) <- survival_res$participant_id
survival_res1 <- symp_res0 %>% purrr::reduce(full_join, by=c('participantID'))
rownames(survival_res1) <- survival_res1$participant_id
survival_res2 <- left_join(survival_res, survival_res1, by = 'participantID')
class(survival_res2)

survival_res2 <- inner_join(survival_res2, demo[, c("participantID", "trial", "treatment")], by = 'participantID')
table(survival_res2$trial)
table(survival_res2$treatment)

survival_res2 <- survival_res2 %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(starts_with("sympres")), na.rm = T)) %>% 
  dplyr::mutate(res = ifelse(sum == 8, 1, 
                             ifelse(sum ==7 & trial == 'CRUK',1,0))) %>% 
  dplyr::mutate(timeres_diarrhea = ifelse(trial == 'CRUK', NA, timeres_diarrhea)) %>% 
  dplyr::mutate(time_res = max(across(starts_with("timeres")), na.rm = T))




#rm(list=ls()[!ls() %in% c('demo','symp','symptom1','trt_check','trt','survival_symp1','survival_res2')])

table(survival_res2$res)




table(survival_symp$time_cough, survival_symp$treatment)
t1 <- table(survival_symp$time_abnormal_taste_smell1,survival_symp$trial)
t(round(prop.table(t1,2),3))
table(survival_symp$time_fatigue)
table(survival_symp$time_myalgia)
table(survival_symp$time_nasal_congestion)
table(survival_symp$time_nausea)
table(survival_symp$time_sob)
table(survival_symp$time_sore_throat)
table(survival_symp$time_symp)
table(survival_symp$treatment, survival_symp$trial)

symptom1 <- c("nasal_congestion","cough","sob",
              "sore_throat","fatigue","diarrhea","nausea","myalgia","abnormal_taste_smell1","symp")
symptom_label <- c("Nasal congestion","Cough","Short of Breath",
                   "Sore throat","Fatigue","Diarrhea","Nausea","Myalgia","Abnormal taste/smell","Any Symptom")

survival_symp1 <- as.data.frame(survival_symp)



data1 <- list()
for (i in 1:length(symptom1)){
  test_symp <- paste0("symp_",symptom1[i]) 
  ttest_symp <- paste0("time_",symptom1[i])
  data1[[i]] <- survival_symp1 %>% dplyr::select(ttest_symp, test_symp)
  colnames(data1[[i]]) <- c("time","status")
  dir <- "~/camo_pool/output/"
  jpeg(filename = paste0(dir,symptom1[i],"_cuml_c.jpeg"),width = 12, height =12, units = 'in', res = 600)
  plot(survfit(Surv(time, status) ~ 1, data = data1[[i]]), 
       fun = function(x) 1-x, ylab = "Cumulative Incidence",xlab = "Days",
       main = paste0("Symptom Improvement for ", symptom_label[i]), conf.int = FALSE) + ylim(0,1)
  dev.off()
}



data1 <- survival_res2 %>% dplyr::select(time_res, res)
colnames(data1) <- c("time","status")
dir <- "~/camo_pool/output/"
jpeg(filename = paste0(dir,"res_cuml_c.jpeg"),width = 12, height =12, units = 'in', res = 600)
plot(survfit(Surv(time, status) ~ 1, data = data1), 
     fun = function(x) 1-x, ylab = "Cumulative Incidence",xlab = "Days",
     main = paste0("Symptom Resolution"), conf.int = FALSE) + ylim(0,1)
dev.off()



surv_plot <- as.data.frame(survival_res2[,c("time_res", "trial")]) %>% group_by_all() %>% summarise(N=n())
surv_plot <- subset(surv_plot, surv_plot[1]<=14)
surv_plot <- surv_plot %>% dplyr::mutate(freq = ifelse(trial == 'aarhus',round(N/69*100,1),
                                                       ifelse(trial == 'CRUK', round(N/33*100,1),
                                                              ifelse(trial == 'Stanford', round(N/48*100,1),
                                                                     ifelse(trial == 'UZ Ghent', round(N/90*100,1),NA)))))




colnames(surv_plot) <- c("symptom","trial","N","freq")



dir <- "~/camo_pool/output/"
jpeg(filename = paste0(dir,"res_barplot_c.jpeg"),width = 12, height = 12, units = 'in', res = 600)
ggplot(surv_plot, aes(x=symptom,y=N,fill=trial))+
  geom_bar(stat="identity",position = "stack",aes(y = N)) +
  scale_x_continuous(breaks=seq(2,14,1)) + 
  geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
  scale_fill_manual(name = "Trial",labels = c("AARHUS", "CRUK","Stanford","UZ Ghent"), values = c("#DF536B", "#61D04F", "#2297E6", "#28E2E5")) +
  theme_bw() + labs(title= paste0("Symptom Resolution "), x="Symptom Resolution Day", y = "N (%)")
dev.off()

p2 <- list()
require(cmprsk)
for (i in 1:length(symptom1)){
  test_symp <- paste0("symp_",symptom1[i]) 
  ttest_symp <- paste0("time_",symptom1[i])
  ci_ulcer <- 
    cuminc(
      ftime = survival_symp1[,c(ttest_symp)], 
      fstatus = survival_symp1[,c(test_symp)], 
      group = survival_symp1$treatment,
      cencode = 0
    )
  
  ciplotdat1 <- 
    ci_ulcer %>% 
    list_modify("Tests" = NULL) %>% 
    map_df(`[`, c("time", "est"), .id = "id") %>% 
    #filter(id %in% c("3 1", "4 1")) %>% 
    dplyr::mutate(Ulceration = recode(
      id, 
      "0 1" = "Placebo", 
      "1 1" = "Treatment")
    )
  
  p2[[i]] <- ggplot(ciplotdat1, aes(x = time, y = est, color = Ulceration)) +
    geom_step(lwd = 1.2)  +
    ylim(c(0, 1)) +
    xlim(c(1,15)) +
    theme_classic() +
    theme(plot.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "bottom") +
    labs(x = "Days", 
         y = "Cumulative incidence",
         title = paste0("Symptom Resolution for ",symptom_label[i])) +
    annotate("text", x = 1, y = 1, hjust = 0,
             label = paste0(
               "p-value = ", 
               ifelse(ci_ulcer$Tests[1, 2] < .001, 
                      "<.001", 
                      round(ci_ulcer$Tests[1, 2], 3))))
  dir <- "~/camo_pool/output/"
  jpeg(filename = paste0(dir,symptom1[i],"_cuml_trtc.jpeg"),width = 12, height =12, units = 'in', res = 600)
  plot(p2[[i]])
  dev.off()
}


p3 <- list()
for (i in 1:length(symptom1)){
  if(symptom1[i] != 'diarrhea') {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp[,c(ttest_symp, "trial")]) %>% group_by_all() %>% summarise(N=n())
    surv_plot <- subset(surv_plot, surv_plot[1]<14)
    n <- as.data.frame(survival_symp[,c(paste0("total_",symptom1[i]), "trial")] %>% group_by_all() %>% summarise(total=n()))
    n <- n[ which(n[1] == 1),]
    surv_plot <- inner_join(surv_plot, n, by = 'trial')
    surv_plot <- surv_plot %>% dplyr::mutate(freq = round(N/total*100,2))
    colnames(surv_plot) <- c("symptom","trial","N","total_nasal_congestion", "total","freq")
    p3[[i]] <- ggplot(surv_plot, aes(x=symptom,y=N,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = N)) +
      scale_x_continuous(breaks=seq(2,13,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      scale_fill_manual(name = "Trial",labels = c((paste0("AARHUS ","( n = ", (subset(n, trial == 'aarhus')$total), " )")), 
                                                  (paste0("CRUK ","( n = ", (subset(n, trial == 'CRUK')$total), " )")),
                                                  (paste0("Stanford ","( n = ", (subset(n, trial == 'Stanford')$total), " )")),
                                                  (paste0("UZ Ghent ","( n = ", (subset(n, trial == 'UZ Ghent')$total), " )"))), 
                        values = c("#DF536B", "#61D04F", "#2297E6", "#28E2E5")) +
      theme_bw() + labs(title= paste0("Symptom Improvement for ",symptom_label[i]), x="Symptom Improvment Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_c1.jpeg"),width = 15, height = 12, units = 'in', res = 300)
    plot(p3[[i]])
    dev.off()
  } else {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp[,c(ttest_symp, "trial")]) %>% group_by_all() %>% summarise(N=n())
    surv_plot <- subset(surv_plot, surv_plot[1]<14)
    n <- as.data.frame(survival_symp[,c(paste0("total_",symptom1[i]), "trial")] %>% group_by_all() %>% summarise(total=n()))
    n <- n[ which(n[1] == 1),]
    surv_plot <- inner_join(surv_plot, n, by = 'trial')
    surv_plot <- surv_plot %>% dplyr::mutate(freq = round(N/total*100,2))
    colnames(surv_plot) <- c("symptom","trial","N","total_nasal_congestion", "total","freq")
    p3[[i]] <- ggplot(surv_plot, aes(x=symptom,y=N,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = N)) +
      scale_x_continuous(breaks=seq(2,13,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      scale_fill_manual(name = "Trial",labels = c((paste0("AARHUS ","( n = ", (subset(n, trial == 'aarhus')$total), " )")), 
                                                  #(paste0("CRUK ","( n = ", (subset(n, trial == 'CRUK')$total), " )")),
                                                  (paste0("Stanford ","( n = ", (subset(n, trial == 'Stanford')$total), " )")),
                                                  (paste0("UZ Ghent ","( n = ", (subset(n, trial == 'UZ Ghent')$total), " )"))), 
                        values = c("#DF536B", "#2297E6", "#28E2E5")) +
      theme_bw() + labs(title= paste0("Symptom Improvement for ",symptom_label[i]), x="Symptom Improvment Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_c1.jpeg"),width = 15, height = 12, units = 'in', res = 300)
    plot(p3[[i]])
    dev.off()
  }
}




p4 <- list()
for (i in 1:length(symptom1)){
  if(symptom1[i] != 'diarrhea') {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp1[,c(ttest_symp, "trial","treatment")]) %>% group_by_all() %>% summarise(N=n())
    surv_plot <- subset(surv_plot, surv_plot[1]<14)
    surv_plot <- surv_plot %>%
      dplyr::mutate(freq = ifelse(treatment == 0 & trial == 'aarhus', round(N/30*100,1),
                                  ifelse(treatment == 1 & trial == 'aarhus',round(N/39*100,1),
                                         ifelse(treatment == 0 & trial == 'CRUK', round(N/18*100,1),
                                                ifelse(treatment == 1 & trial == 'CRUK',round(N/15*100,1),
                                                       ifelse(treatment == 0 & trial == 'Stanford', round(N/26*100,1),
                                                              ifelse(treatment == 1 & trial == 'Stanford',round(N/22*100,1),
                                                                     ifelse(treatment == 0 & trial == 'UZ Ghent', round(N/29*100,1),
                                                                            ifelse(treatment == 1 & trial == 'UZ Ghent',round(N/61*100,1), NA))))))))) %>% 
      dplyr::mutate(new_value = ifelse(treatment == 0, -1*N, N))
    colnames(surv_plot) <- c("symptom","trial","treatment","N","freq","new_value")
    p4[[i]] <- ggplot(surv_plot, aes(x=symptom,y=new_value,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = new_value)) +
      geom_hline(yintercept=0) + 
      scale_x_continuous(breaks=seq(2,13,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      geom_label(aes(x=13, y=8, label="Camostat"),fill = "white") + 
      geom_label(aes(x=13, y=-8, label="Placebo"),fill = "white") +
      scale_fill_manual(name = "Trial",labels = c("AARHUS", "CRUK","Stanford","UZ Ghent"), values = c("#DF536B", "#61D04F", "#2297E6", "#28E2E5")) +
      theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      labs(title= paste0("Symptom Improvement for ",symptom_label[i]), x="Symptom Improvment Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_bytrt_c.jpeg"),width = 12, height = 16, units = 'in', res = 600)
    plot(p4[[i]])
    dev.off()
  } else {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp1[,c(ttest_symp, "trial","treatment")]) %>% group_by_all() %>% summarise(N=n())
    surv_plot <- subset(surv_plot, surv_plot[1]<14)
    surv_plot <- surv_plot %>%
      dplyr::mutate(freq = ifelse(treatment == 0 & trial == 'aarhus', round(N/30*100,1),
                                  ifelse(treatment == 1 & trial == 'aarhus',round(N/39*100,1),
                                         ifelse(treatment == 0 & trial == 'Stanford', round(N/26*100,1),
                                                ifelse(treatment == 1 & trial == 'Stanford',round(N/22*100,1),
                                                       ifelse(treatment == 0 & trial == 'UZ Ghent', round(N/29*100,1),
                                                              ifelse(treatment == 1 & trial == 'UZ Ghent',round(N/61*100,1), NA))))))) %>% 
      dplyr::mutate(new_value = ifelse(treatment == 0, -1*N, N))
    colnames(surv_plot) <- c("symptom","trial","treatment","N","freq","new_value")
    p4[[i]] <- ggplot(surv_plot, aes(x=symptom,y=new_value,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = new_value)) +
      geom_hline(yintercept=0) + 
      scale_x_continuous(breaks=seq(2,13,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      geom_label(aes(x=13, y=8, label="Camostat"),fill = "white") + 
      geom_label(aes(x=13, y=-8, label="Placebo"),fill = "white") +
      scale_fill_manual(name = "Trial",labels = c("AARHUS","Stanford","UZ Ghent"), values = c("#DF536B", "#2297E6", "#28E2E5")) +
      theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      labs(title= paste0("Symptom Improvement for ",symptom_label[i]), x="Symptom Improvment Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_bytrt_c.jpeg"),width = 12, height = 16, units = 'in', res = 600)
    plot(p4[[i]])
    dev.off()
  }
}  

ci_ulcer <- 
  cuminc(
    ftime = survival_res2$time_res, 
    fstatus = survival_res2$res, 
    group = survival_res2$treatment,
    cencode = 0
  )

ciplotdat1 <- 
  ci_ulcer %>% 
  list_modify("Tests" = NULL) %>% 
  map_df(`[`, c("time", "est"), .id = "id") %>% 
  #filter(id %in% c("3 1", "4 1")) %>% 
  dplyr::mutate(Ulceration = recode(
    id, 
    "0 1" = "Placebo", 
    "1 1" = "Treatment")
  )

dir <- "~/camo_pool/output/"
jpeg(filename = paste0(dir,"cuml_res.jpeg"),width = 12, height =12, units = 'in', res = 600)
ggplot(ciplotdat1, aes(x = time, y = est, color = Ulceration)) +
  geom_step(lwd = 1.2)  +
  ylim(c(0, 1)) +
  xlim(c(1,15)) +
  theme_classic() +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(x = "Days", 
       y = "Cumulative incidence",
       title = "Symptom Resolution") +
  annotate("text", x = 1, y = 1, hjust = 0,
           label = paste0(
             "p-value = ", 
             ifelse(ci_ulcer$Tests[1, 2] < .001, 
                    "<.001", 
                    round(ci_ulcer$Tests[1, 2], 3))))
dev.off()


p3 <- list()
for (i in 1:length(symptom1)){
  if(symptom1[i] != 'diarrhea') {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp1[,c(ttest_symp, "trial")]) %>% group_by_all() %>% summarise(N=n())
    #surv_plot <- subset(surv_plot, surv_plot[1]<14)
    surv_plot <- surv_plot %>% dplyr::mutate(freq = ifelse(trial == 'aarhus',round(N/69*100,1),
                                                           ifelse(trial == 'CRUK', round(N/33*100,1),
                                                                  ifelse(trial == 'Stanford', round(N/48*100,1),
                                                                         ifelse(trial == 'UZ Ghent', round(N/90*100,1),NA)))))
    colnames(surv_plot) <- c("symptom","trial","N","freq")
    p3[[i]] <- ggplot(surv_plot, aes(x=symptom,y=N,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = N)) +
      scale_x_continuous(breaks=seq(1,15,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      scale_fill_manual(name = "Trial",labels = c("AARHUS", "CRUK","Stanford","UZ Ghent"), values = c("#DF536B", "#61D04F", "#2297E6", "#28E2E5")) +
      theme_bw() + labs(title= paste0("Symptom Resolution for ",symptom_label[i]), x="Symptom Resolution Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_res.jpeg"),width = 12, height = 12, units = 'in', res = 600)
    plot(p3[[i]])
    dev.off()
  } else {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp1[,c(ttest_symp, "trial")]) %>% group_by_all() %>% summarise(N=n())
    #surv_plot <- subset(surv_plot, surv_plot[1]<14)
    surv_plot <- surv_plot %>% dplyr::mutate(freq = ifelse(trial == 'aarhus',round(N/69*100,1),
                                                           ifelse(trial == 'Stanford', round(N/48*100,1),
                                                                  ifelse(trial == 'UZ Ghent', round(N/90*100,1),NA))))
    colnames(surv_plot) <- c("symptom","trial","N","freq")
    p3[[i]] <- ggplot(surv_plot[which(surv_plot$trial!="CRUK"),], aes(x=symptom,y=N,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = N)) +
      scale_x_continuous(breaks=seq(1,15,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      scale_fill_manual(name = "Trial",labels = c("AARHUS","Stanford","UZ Ghent"), values = c("#DF536B", "#2297E6", "#28E2E5")) +
      theme_bw() + labs(title= paste0("Symptom Resolution for ",symptom_label[i]), x="Symptom Resolution Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_res.jpeg"),width = 12, height = 12, units = 'in', res = 600)
    plot(p3[[i]])
    dev.off()
  }
}



p4 <- list()
for (i in 1:length(symptom1)){
  if(symptom1[i] != 'diarrhea') {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp1[,c(ttest_symp, "trial","treatment")]) %>% group_by_all() %>% summarise(N=n())
    #surv_plot <- subset(surv_plot, surv_plot[1]<14)
    surv_plot <- surv_plot %>%
      dplyr::mutate(freq = ifelse(treatment == 0 & trial == 'aarhus', round(N/30*100,1),
                                  ifelse(treatment == 1 & trial == 'aarhus',round(N/39*100,1),
                                         ifelse(treatment == 0 & trial == 'CRUK', round(N/18*100,1),
                                                ifelse(treatment == 1 & trial == 'CRUK',round(N/15*100,1),
                                                       ifelse(treatment == 0 & trial == 'Stanford', round(N/26*100,1),
                                                              ifelse(treatment == 1 & trial == 'Stanford',round(N/22*100,1),
                                                                     ifelse(treatment == 0 & trial == 'UZ Ghent', round(N/29*100,1),
                                                                            ifelse(treatment == 1 & trial == 'UZ Ghent',round(N/61*100,1), NA))))))))) %>% 
      dplyr::mutate(new_value = ifelse(treatment == 0, -1*N, N))
    colnames(surv_plot) <- c("symptom","trial","treatment","N","freq","new_value")
    p4[[i]] <- ggplot(surv_plot, aes(x=symptom,y=new_value,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = new_value)) +
      geom_hline(yintercept=0) + 
      scale_x_continuous(breaks=seq(1,15,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      geom_label(aes(x=13, y=8, label="Camostat"),fill = "white") + 
      geom_label(aes(x=13, y=-8, label="Placebo"),fill = "white") +
      scale_fill_manual(name = "Trial",labels = c("AARHUS", "CRUK","Stanford","UZ Ghent"), values = c("#DF536B", "#61D04F", "#2297E6", "#28E2E5")) +
      theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      labs(title= paste0("Symptom Resolution for ",symptom_label[i]), x="Symptom Resolution Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_bytrt_res.jpeg"),width = 12, height = 16, units = 'in', res = 600)
    plot(p4[[i]])
    dev.off()
  } else {
    ttest_symp <- paste0("time_",symptom1[i])
    surv_plot <- as.data.frame(survival_symp1[,c(ttest_symp, "trial","treatment")]) %>% group_by_all() %>% summarise(N=n())
    #surv_plot <- subset(surv_plot, surv_plot[1]<14)
    surv_plot <- surv_plot %>%
      dplyr::mutate(freq = ifelse(treatment == 0 & trial == 'aarhus', round(N/30*100,1),
                                  ifelse(treatment == 1 & trial == 'aarhus',round(N/39*100,1),
                                         ifelse(treatment == 0 & trial == 'Stanford', round(N/26*100,1),
                                                ifelse(treatment == 1 & trial == 'Stanford',round(N/22*100,1),
                                                       ifelse(treatment == 0 & trial == 'UZ Ghent', round(N/29*100,1),
                                                              ifelse(treatment == 1 & trial == 'UZ Ghent',round(N/61*100,1), NA))))))) %>% 
      dplyr::mutate(new_value = ifelse(treatment == 0, -1*N, N))
    colnames(surv_plot) <- c("symptom","trial","treatment","N","freq","new_value")
    p4[[i]] <- ggplot(surv_plot[which(surv_plot$trial!="CRUK"),], aes(x=symptom,y=new_value,fill=trial))+
      geom_bar(stat="identity",position = "stack",aes(y = new_value)) +
      geom_hline(yintercept=0) + 
      scale_x_continuous(breaks=seq(1,15,1)) + 
      geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
      geom_label(aes(x=13, y=8, label="Camostat"),fill = "white") + 
      geom_label(aes(x=13, y=-8, label="Placebo"),fill = "white") +
      scale_fill_manual(name = "Trial",labels = c("AARHUS","Stanford","UZ Ghent"), values = c("#DF536B", "#2297E6", "#28E2E5")) +
      theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      labs(title= paste0("Symptom Resolution for ",symptom_label[i]), x="Symptom Resolution Day", y = "N (%)")
    dir <- "~/camo_pool/output/"
    jpeg(filename = paste0(dir,symptom1[i],"_barplot_bytrt_res.jpeg"),width = 12, height = 16, units = 'in', res = 600)
    plot(p4[[i]])
  }
}

###  drop CRUK from symptom1[i] == 'diarrhea'

surv_plot <- as.data.frame(survival_res2[,c("time_res", "trial","treatment")]) %>% group_by_all() %>% summarise(N=n())
surv_plot <- subset(surv_plot, surv_plot[1]<=14)
surv_plot <- surv_plot %>%
  dplyr::mutate(freq = ifelse(treatment == 0 & trial == 'aarhus', round(N/30*100,1),
                              ifelse(treatment == 1 & trial == 'aarhus',round(N/39*100,1),
                                     ifelse(treatment == 0 & trial == 'CRUK', round(N/18*100,1),
                                            ifelse(treatment == 1 & trial == 'CRUK',round(N/15*100,1),
                                                   ifelse(treatment == 0 & trial == 'Stanford', round(N/26*100,1),
                                                          ifelse(treatment == 1 & trial == 'Stanford',round(N/22*100,1),
                                                                 ifelse(treatment == 0 & trial == 'UZ Ghent', round(N/29*100,1),
                                                                        ifelse(treatment == 1 & trial == 'UZ Ghent',round(N/61*100,1), NA))))))))) %>% 
  dplyr::mutate(new_value = ifelse(treatment == 0, -1*N, N))
colnames(surv_plot) <- c("symptom","trial","treatment","N","freq","new_value")
dir <- "~/camo_pool/output/"
jpeg(filename = paste0(dir,"res_barplot_bytrt_res.jpeg"),width = 12, height = 16, units = 'in', res = 600)
ggplot(surv_plot, aes(x=symptom,y=new_value,fill=trial))+
  geom_bar(stat="identity",position = "stack",aes(y = new_value)) +
  geom_hline(yintercept=0) + 
  scale_x_continuous(breaks=seq(1,15,1)) + 
  geom_text(aes(label = paste0(N,"\n","(",freq,"%)")),position = position_stack(vjust = .5)) + 
  geom_label(aes(x=13, y=8, label="Camostat"),fill = "white") + 
  geom_label(aes(x=13, y=-8, label="Placebo"),fill = "white") +
  scale_fill_manual(name = "Trial",labels = c("AARHUS", "CRUK","Stanford","UZ Ghent"), values = c("#DF536B", "#61D04F", "#2297E6", "#28E2E5")) +
  theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  labs(title= paste0("Symptom Resolution"), x="Symptom Resolution Day", y = "N (%)")
dev.off()

symp_surv <- survival_symp

p2 <- list()
for (i in 1:length(symptom1)){
  test_symp <- paste0("symp_",symptom1[i]) 
  ttest_symp <- paste0("time_",symptom1[i])
  ci_ulcer <- 
    cuminc(
      ftime = symp_surv[,c(ttest_symp)], 
      fstatus = symp_surv[,c(test_symp)], 
      group = symp_surv$treatment,
      cencode = 0
    )
  
  ciplotdat1 <- 
    ci_ulcer %>% 
    list_modify("Tests" = NULL) %>% 
    map_df(`[`, c("time", "est"), .id = "id") %>% 
    #filter(id %in% c("3 1", "4 1")) %>% 
    dplyr::mutate(Ulceration = recode(
      id, 
      "0 1" = "Placebo", 
      "1 1" = "Trt")
    )
  
  p2[[i]] <- ggplot(ciplotdat1, aes(x = time, y = est, color = Ulceration)) +
    geom_step(lwd = 1.2)  +
    ylim(c(0, 1)) +
    theme_classic() +
    theme(plot.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "bottom") +
    labs(x = "Days", 
         y = "Cumulative incidence",
         title = paste0("Symptom Improvement for ",symptom1[i])) +
    annotate("text", x = 0, y = 1, hjust = 0,
             label = paste0(
               "p-value = ", 
               ifelse(ci_ulcer$Tests[1, 2] < .001, 
                      "<.001", 
                      round(ci_ulcer$Tests[1, 2], 3))))
  dir <- "~/camo_pool/output/"
  jpeg(filename = paste0(dir,symptom1[i],"_cuml1.jpeg"),width = 5000, height =5000, units = 'px', res = 500)
  plot(p2[[i]])
  dev.off()
}


library(plyr)
# Break up d by state, then fit the specified model to each piece and
# return a list
demo$sex <- as.numeric(as.factor(demo$sex))


## using strata()
res.cox.strata <- coxph(Surv(hosp_day,hospitalized) ~ treatment + age + sex + strata(trial), data = demo)
summary(res.cox.strata) 

res.cox.strata %>%
  tbl_regression(exponentiate = TRUE)

## check proportional hazards assumptions:
cox.zph(res.cox.strata)

## save this out, can't use ggsave with par() use just jpeg
jpeg("~/output/hosp_coxph_assumption.jpeg",width = 3000, height =3000, units = 'px', res = 500)
par(mfrow=c(2,2))
plot(cox.zph(res.cox.strata))
dev.off()



### https://github.com/stan-dev/rstanarm/issues/588):
## run in terminal:
# git clone -b feature/survival https://github.com/stan-dev/rstanarm.git
devtools::document("rstanarm")

# install.packages("rstan")
library(rstan)
library(rstanarm)
library(lme4)


survival_symp1$treatment <- as.factor(survival_symp1$treatment)

surv_mod <- rstanarm::stan_surv(Surv(time_symp, symp_symp) ~ treatment + age + sex + (treatment|trial),
                                data = survival_symp1,
                                chains = 1,
                                seed = 1234)


surv_exp <- update(surv_mod, basehaz = "exp")
surv_weibull <- update(surv_mod, basehaz = "weibull")
surv_gomp <- update(surv_mod, basehaz = "gompertz")
#surv_bspline <- update(surv_mod, basehaz = "bs")
#surv_mspline1 <- update(surv_mod, basehaz = "ms")
#surv_mspline2 <- update(surv_mod, basehaz = "ms", basehaz_ops = list(df=9))
surv_weibullaft <- update(surv_mod, basehaz = "weibull-aft")

plotfun <- function(model, title) {
  plot(model, plotfun = "basehaz") +
    coord_cartesian(ylim = c(0,0.4)) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5))
}

p_exp <- plotfun(surv_exp, "Exponential")
p_weibull <- plotfun(surv_weibull, "Weibull")
p_gompertz <- plotfun(surv_gomp, "Gompertz")
#p_bspline <- plotfun(surv_bspline, "B-splines with\ntwo internal knots")
#p_mspline1 <- plotfun(surv_mspline1, "M-splines with\ntwo internal knots")
#p_mspline2 <- plotfun(surv_mspline2, "M-splines with\nfive internal knots")
p_weibullaft <- plotfun(surv_weibullaft, "Weibull-AFT")


library(cowplot)

dir <- "~/camo_pool/output/"
jpeg(filename = paste0(dir,"model_fit.jpeg"),width = 5000, height =5000, units = 'px', res = 500)
plot_grid(p_exp,
          p_weibull,
          p_gompertz,
          #p_bspline,
          #p_mspline1,
          #p_mspline2,
          p_weibullaft,
          ncol = 2)
dev.off()

compare_models(loo(surv_exp),
               loo(surv_weibull),
               loo(surv_gomp),
               #loo(surv_bspline),
               #loo(surv_mspline1),
               #loo(surv_mspline2),
               loo(surv_weibullaft))

surv_weibullaft4 <- update(surv_mod,chains = 4, basehaz = "weibull-aft")
surv_weibull4 <- update(surv_mod,chains = 4, basehaz = "weibull")


compare_models(loo(surv_exp),
               loo(surv_weibull),
               loo(surv_gomp),
               #loo(surv_bspline),
               #loo(surv_mspline1),
               #loo(surv_mspline2),
               loo(surv_weibullaft),
               loo(surv_weibullaft4),
               loo(surv_weibull4))


surv_mod1 <- update(surv_weibullaft4,
                    prior_intercept = normal(0,20),
                    prior = normal(),
                    prior_aux = normal())

## shortcut...check if same results as surv_mod1 above
surv_mod1jl <- rstanarm::stan_surv(Surv(time_symp, symp_symp) ~ treatment + age + sex + (treatment|trial),
                                   data = survival_symp1,
                                   chains = 4,
                                   seed = 1234,
                                   basehaz = "weibull-aft",
                                   prior_intercept = normal(0,20),
                                   prior = normal(),
                                   prior_aux = normal())

## surv_mod1 and surv_mod1jl look to be the same
# surv_mod1 <- surv_mod1jl
## save out surv_mod1jl as an rdata file so we don't have to run this again
# save.image(file="camo_pool_data_20240416.RData")
# load(file="camo_pool_data_20240416.RData")


### try a different seed to see how much the results vary --  still same interpretation
# surv_mod1jl_seed4321 <- rstanarm::stan_surv(Surv(time_symp, symp_symp) ~ treatment + age + sex + (treatment|trial),
#                                             data = survival_symp1,
#                                             chains = 4,
#                                             seed = 4321,
#                                             basehaz = "weibull-aft",
#                                             prior_intercept = normal(0,20),
#                                             prior = normal(),
#                                             prior_aux = normal())
# 
# ### try a different seed to see how much the results vary
# surv_mod1jl_seed1220 <- rstanarm::stan_surv(Surv(time_symp, symp_symp) ~ treatment + age + sex + (treatment|trial),
#                                             data = survival_symp1,
#                                             chains = 4,
#                                             seed = 1220,
#                                             basehaz = "weibull-aft",
#                                             prior_intercept = normal(0,20),
#                                             prior = normal(),
#                                             prior_aux = normal())



## this is to compare with surv_mod1
# surv_fixed <- stan_surv(Surv(time_symp, symp_symp) ~ treatment + age + sex, 
#                         data = survival_symp1, 
#                         basehaz = "weibull",
#                         chains = 1, 
#                         seed = 1234)
# summary(surv_fixed, digits = 5)
# 
# surv_randint <- stan_surv(Surv(time_symp, symp_symp) ~ treatment + age + sex + (1|trial), 
#                           data = survival_symp1, 
#                           basehaz = "weibull",
#                           chains = 1, 
#                           seed = 1234)
# summary(surv_randint, digits = 5)
# loo_fixed <- loo(surv_fixed)
# loo_randint <- loo(surv_randint)
# loo_randtrt <- loo(surv)
# 
# loo_compare(loo_fixed, loo_randint, loo_randtrt)

## output surv_mod1
summary(surv_mod1, digits = 5)
print(surv_mod1, digits = 2)

a<- surv_mod1 %>%
  as_tibble() %>%
  transmute(AARHUS = exp(treatment1 + `b[treatment1 trial:aarhus]`),
            CRUK = exp(treatment1 + `b[treatment1 trial:CRUK]`),
            Stanford = exp(treatment1 + `b[treatment1 trial:Stanford]`),
            UZ_Ghent = exp(treatment1 + `b[treatment1 trial:UZ_Ghent]`),
            Overall = exp(treatment1),
  ) %>%
  bayesplot::mcmc_intervals(prob_outer = .95)
a

bayes_final_r <- as.data.frame(cbind(a$data$m,a$data$ll,a$data$hh))
colnames(bayes_final_r) <- c("hr","ci_lower","ci_upper")
bayes_final_r$trial <- c("AARHUS","CRUK", "Stanford", "UZ_Ghent", "Overall")

## create vars in bayes_final_r:
bayes_final_r$ci_lower_lab = c(0.50, 0.47, 0.49, 0.74, 0.51)
bayes_final_r$ci_upper_lab = c(1.10, 1.33, 1.27, 1.73, 1.55)



### FORESTPLOT CODE ###
library(forestplot)
library(dplyr)
dir <- "~/camo_pool/drafts/"
## get values from bayes_final_r:
## add N obs from survival_symp1:
trial_obs <- survival_symp1 %>%
  group_by(trial) %>%
  dplyr::summarise(n_obs=n()) %>%
  ungroup() %>%
  dplyr::select(trial, n_obs)

trial_obs <- rbind(trial_obs, cbind(trial="Overall", n_obs=sum(trial_obs$n_obs)))
## clean up trial names to match bayes_final_r:
trial_obs$trial[which(trial_obs$trial=="UZ Ghent")] <- "UZ_Ghent"
trial_obs$trial[which(trial_obs$trial=="aarhus")] <- "AARHUS"

## merge trial_obs into bayes_final_r
forest_out <- merge(bayes_final_r, trial_obs, by="trial", all.x = TRUE) %>%
  ## make trial a factor to order it:
  dplyr::mutate(trial=factor(trial, levels=c("AARHUS", "CRUK", "Stanford", "UZ_Ghent", "Overall"),
                             labels=c("AARHUS", "CRUK", "Stanford", "UZ Ghent", "Pooled"))
  ) %>%
  dplyr::arrange(trial)

# Create confidence interval column to display
forest_out$hr_ci <- sprintf("%.2f (%.2f, %.2f)",
                            round(forest_out$hr,2), round(forest_out$ci_lower_lab,2), round(forest_out$ci_upper_lab,2))


forestdat <- structure(list(mean  = c(NA, forest_out$hr),
                            lower = c(NA, forest_out$ci_lower_lab),
                            upper = c(NA, forest_out$ci_upper_lab)),
                       .Names = c("mean", "lower", "upper"),
                       row.names = c(NA, -6L),
                       class = "data.frame")

foresttext <- cbind(c("", as.character(forest_out$trial)),
                    c("N",forest_out$n_obs),
                    c("HR (95% CI)", forest_out$hr_ci)
)
jpeg(filename = paste0(dir,"forest_sympres_20240416.jpeg"),width = 15, height =10, units = 'in', res = 300)
forestdat %>%
  forestplot(labeltext = foresttext,
             # is.summary = c(rep(FALSE, 6), TRUE),
             xlim = c(0, 2.0),
             xticks = c(0.5, 1.0, 1.5,2.0),
             xlab = "Hazard Ratio (95% CI)",
             zero = 1,
             txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=1.2),
                              ticks = gpar(fontfamily = "", cex=0.8),
                              xlab  = gpar(fontfamily = "", cex = 1.2),
                              legend = gpar(fontfamily = "", cex=0.8)),
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             colgap = unit(10,"mm"),
             arrow_lab = c("Favor Placebo", "Favor Treatment")
  ) %>%
  fp_decorate_graph(graph.pos=2,
                    ## use those blank spaces to get the position right, but haven't found a better way
                    left_bottom_txt = fp_txt_gp("                                                                                   Favor Treatment", gp = gpar(cex=1.2)),
                    right_bottom_txt = fp_txt_gp("Favor Placebo                                                                                                                                                ", gp = gpar(cex=1.2))
  ) %>%
  fp_set_style(box=c(rep("black",5), "dodgerblue"),
               line=rep("black",6))
dev.off()






######################### BAYESIAN MODELING ######################### 
### first fit the frequentist model
model_freq <- lmer(ct_slope ~ tx + age_c + male+(1+tx|site), data=simdat)
summary(model_freq)
#txCamostat  -0.208998   0.242350  -0.862

#### assumptions
## Rate of change in the shedding of SARS-CoV-2 virus for Days 1-5 will be compared between the two arms 
##   using a linear mixed effects model with a random intercept and slope for trial to account for correlation 
##   between observations from the same trial.  We will test whether ??=0 in the model below where ???slope???_i 
##   is the derived slope for Days 1-5 for participant i, ??_trial is a random intercept for trial, ??_trial is a 
##   random slope for trial, and X represents the m regression covariates listed in Section 2.3. The dependent 
##   variable, ???slope???_i, will be estimated for each participant with at least two measurements available 
##   using least squares regression. 
## model: slope ~ N(a_trial + theta_trial*arm + betax, sigma^2)
## random intercept for trial (a_trial) ~ N(0,10)
## random slope for trial (theta_trial) ~ N(delta, tao^2)
## rate of change (delta) ~ N(0,10)
## tao ~ N(0, 2.5)
## beta ~ N(0, 2.5)
## sigma ~ N(0, 2.5)

# help from: https://towardsdatascience.com/a-bayesian-approach-to-linear-mixed-models-lmm-in-r-python-b2f1378c3ac8
# and from: https://www.r-bloggers.com/2019/09/bayesian-linear-mixed-models-random-intercepts-slopes-and-missing-data/
# and: https://www.r-bloggers.com/2020/04/bayesian-linear-regression/


### now the bayesian model
## provides options for which parameters priors can be set:
get_prior(ct_slope ~ tx + age_c + male + (1+tx|site), data=simdat)

# Set priors
priors <- c(prior(normal(0,10), class = Intercept), # intercept prior
            prior(normal(0,2.5), class = b), # slope prior
            # prior(lkj_corr_cholesky(1.5), class = cor), # correlation between intercept and beta -- try as sensitivity
            prior(normal(0, 2.5), class = sigma), # population variance
            prior(normal(0, 2.5), class = sd) # tau0, group variance
)

# Fit model
model_bayes <- brm(ct_slope ~ tx + age_c + male + (tx|site),
                   data = simdat,
                   prior = priors,
                   family = gaussian(),
                   warmup = 1000, # burn-in
                   iter = 5000, # number of iterations
                   chains = 2,  # number of MCMC chains
                   control = list(adapt_delta = 0.95)) # advanced MC settings
print(model_bayes)
#txCamostat    -0.21      0.33    -0.85     0.40 1.00     4469     3060

## 	Posterior (or prior) predictive checks
pp_check(model_bayes)

summary(model_bayes)
posterior_summary(model_bayes)

ranef(model_bayes)

## markov chains -- trace plot of linear regression
mcmc_trace(model_bayes,  pars = c("b_Intercept", "sigma"), 
           n_warmup = 500, facet_args = list(ncol = 2, labeller = label_parsed))

mcmc_dens(model_bayes)+
  vline_at(-0.015, col="red")

describe_posterior(model_bayes)



#### compare CT values between box files and redcap values 
library(dplyr)
camostat_studyday <- stanford1 %>% dplyr::filter(!is.na(r_time_l)) %>% select_if(~all(!is.na(.))) #%>% 
#select(participant_id, date_visit)

camostat_nasal <- stanford1 %>% dplyr::filter(redcap_event_name == 'Nasal Swab Results') 
# %>% mutate(nasal_ct_id = participant_id, nasal_ct_y = y)

camostat_nasal1 <- camostat_nasal %>% dplyr::select(., c(participantID,starts_with('nasal_ct')))
camostat_nasal2 <- inner_join(camostat_nasal1, trt_stanford, by = 'participantID') %>% 
  dplyr::select(., c(participantID,starts_with('nasal_ct')))

# camostat_nasal3 <- full_join(camostat_nasal2, camostat.id, by = c('nasal_ct_y' ='y'))

library(lubridate)

X_CT_results_2_17_21_ <- read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/ CT results 2-17-21 .xlsx")
colnames(X_CT_results_2_17_21_) <- c("study","id","Lab recv'd","Specimen ID","Requisition","Date collected","Time collected",
                                     "Result","CT Value","Rnase Value","Date - Time Process")
X_CT_results_2_17_21_ <-X_CT_results_2_17_21_ %>% select(-c("Time collected","Date - Time Process"))
X_CT_results_3_3_21_ <- read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/ CT results 3-3-21 .xlsx")
colnames(X_CT_results_3_3_21_) <- c("study","id","Lab recv'd","Specimen ID","Requisition","Date collected","Time collected",
                                    "Result","CT Value","Rnase Value","Date - Time Process")
X_CT_results_3_3_21_$`CT Value` <- as.numeric(X_CT_results_3_3_21_$`CT Value`)
X_CT_results_3_3_21_ <- X_CT_results_3_3_21_ %>% select(-c("Time collected","Date - Time Process"))
CT_results_5_14_21 <- read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/CT results  5-14-21.xlsx")
colnames(CT_results_5_14_21) <- c("study","id","Lab recv'd","Specimen ID","Requisition","Date collected","Time collected",
                                  "Result","CT Value","Rnase Value")
CT_results_1_30_21 <- as.data.frame(read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/CT results 1-30-21.xlsx"))
colnames(CT_results_1_30_21) <- c("study","id","Lab recv'd","Specimen ID","Requisition","Date collected","Time collected",
                                  "Result","CT Value","Rnase Value","Date - Time Process")
CT_results_1_30_21[,6] <- ifelse(CT_results_1_30_21[,6] == 'Missing',NA,CT_results_1_30_21[,6])
CT_results_1_30_21[,6] <- as.numeric(CT_results_1_30_21[,6])
CT_results_1_30_21[,6] <- openxlsx::convertToDate(CT_results_1_30_21[,6])
CT_results_1_30_21[,9] <- as.numeric(CT_results_1_30_21[,9])

CT_results_2_9_21 <- read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/CT results 2-9-21.xlsx")
colnames(CT_results_2_9_21) <- c("study","id","Lab recv'd","Specimen ID","Requisition","Date collected","Time collected",
                                 "Result","CT Value","Rnase Value","Date - Time Process")
CT_results_2_9_21$`Date collected` <- as.numeric(CT_results_2_9_21$`Date collected`)
CT_results_2_9_21$`Date collected` <- openxlsx::convertToDate(CT_results_2_9_21$`Date collected`)

CT_results_4_28_21 <- read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/CT results 4-28-21.xlsx")
colnames(CT_results_4_28_21) <- c("study","id","Lab recv'd","Specimen ID","Requisition","Date collected","Time collected",
                                  "Result","CT Value","Rnase Value")
CT_results_4_28_21 <- CT_results_4_28_21 %>% select(-c("Time collected"))

CT_results_first_batch <- as.data.frame(read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/CT results first batch.xlsx")) %>% select(-c("...3"))
colnames(CT_results_first_batch) <- c("study","id","Lab recv'd","Requisition","Date collected","Time collected",
                                      "Result","Specimen ID","CT Value","Rnase Value","Date - Time Process")
CT_results_first_batch[,5] <- as.numeric(CT_results_first_batch[,5])
CT_results_first_batch[,5] <- openxlsx::convertToDate(CT_results_first_batch[,5])


CTs_results_3_26_21 <- read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/CTs results 3-26-21.xlsx")
colnames(CTs_results_3_26_21) <- c("study","id","Lab recv'd","Specimen ID","Requisition","Date collected","Time collected",
                                   "Result","CT Value","Rnase Value")
XCAM_VIRO_MAY_report_1_ <- read_excel("~/Library/CloudStorage/Box-Box/QSU COVID Projects/camostat/Ct values/CT results-selected/XCAM - VIRO - MAY report[1].xlsx", 
                                      col_names = T, skip = 1)
colnames(XCAM_VIRO_MAY_report_1_) <- c("Specimen ID","studyid","Date collected","Requisition",
                                       "CT Value","Rnase Value","Result")
XCAM_VIRO_MAY_report_1_$id <- parse_number(sub(".*,", "", XCAM_VIRO_MAY_report_1_$studyid))

ct_value <- dplyr::bind_rows(X_CT_results_2_17_21_,X_CT_results_3_3_21_,CT_results_5_14_21, CT_results_1_30_21,CT_results_2_9_21,
                             CT_results_4_28_21, CT_results_first_batch,CTs_results_3_26_21,XCAM_VIRO_MAY_report_1_)
colnames(ct_value)
ct_value1 <- na.omit(ct_value %>% dplyr::select(id, "Date collected", "CT Value"))
length(unique(ct_value1$id)) ### 47

ct_value3 <- ct_value1 %>% select(id, "Date collected", "CT Value")
id <- ct_value3 %>% group_by(id, `Date collected`) %>% count() %>% dplyr::filter(n>1) ### 65, 67, 68, 69, 3,4, 73
ct_value3d <- ct_value3 %>% dplyr::filter(!id %in% c(3,4,65,67,68,69,73))
ct_value3d <- spread(ct_value3d, key = "Date collected", value = `CT Value`)


camostat_studyday1 <- camostat_studyday %>% select(participant_id,r_time_l) %>% dplyr::mutate(r_date = as.Date(as.character(as.POSIXct(r_time_l)))) %>% 
  dplyr::mutate(id = str_replace(participant_id, pattern = "-\\d+\\-", replacement = "-")) %>% 
  dplyr::mutate(id = abs(parse_number(sub("-*,", "", id))))

ct_value2 <- left_join(camostat_studyday1, ct_value1, by = 'id') %>% dplyr::mutate(collect = as.Date(as.character(as.POSIXct(`Date collected`))))

ct_value2 <- ct_value2 %>% dplyr::mutate(study_day = as.numeric(collect - r_date) +1)
ct_value3d1 <- ct_value2 %>% dplyr::filter(!id %in% c(3,4,65,67,68,69,73)) %>% dplyr::filter(study_day>0) %>% select("id", "study_day", "CT Value")
ct_value3d1 <- spread(ct_value3d1, key = "study_day", value = `CT Value`)
camostat_nasal2$id <- abs(parse_number(sub("-*,", "", str_replace(camostat_nasal2$participantID, pattern = "-\\d+\\-", replacement = "-"))))
ct_check1 <- full_join(ct_value3d1, camostat_nasal2, by = 'id')

ct_check1 <- ct_check1 %>% dplyr::mutate(cd1 = `1` - nasal_ct_1,
                                         cd2 = `2` - nasal_ct_2,
                                         cd3 = `3` - nasal_ct_3,
                                         cd4 = `4` - nasal_ct_4,
                                         cd5 = `5` - nasal_ct_5,
                                         cd6 = `6` - nasal_ct_6,
                                         cd7 = `7` - nasal_ct_7,
                                         cd8 = `8` - nasal_ct_8,
                                         cd9 = `9` - nasal_ct_9,
                                         cd10 = `10` - nasal_ct_10) %>%
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%
  group_by(id) %>%
  dplyr::mutate(cd=sum(across(starts_with("cd")), na.rm = TRUE)) %>%
  ungroup()


ct_check1_d <- ct_check1 %>% dplyr::filter(cd !=0)
ct_check1_s <- ct_check1 %>% dplyr::filter(cd ==0) %>% dplyr::mutate(d1 = ifelse(is.na(nasal_ct_1), `1`, nasal_ct_1),
                                                                     d2 = ifelse(is.na(nasal_ct_2), `2`, nasal_ct_2),
                                                                     d3 = ifelse(is.na(nasal_ct_3), `3`, nasal_ct_3),
                                                                     d4 = ifelse(is.na(nasal_ct_4), `4`, nasal_ct_4),
                                                                     d5 = ifelse(is.na(nasal_ct_5), `5`, nasal_ct_5),
                                                                     d6 = ifelse(is.na(nasal_ct_6), `6`, nasal_ct_6),
                                                                     d7 = ifelse(is.na(nasal_ct_7), `7`, nasal_ct_7),
                                                                     d8 = ifelse(is.na(nasal_ct_8), `8`, nasal_ct_8),
                                                                     d9 = ifelse(is.na(nasal_ct_9), `9`, nasal_ct_9),
                                                                     d10 = ifelse(is.na(nasal_ct_10), `10`, nasal_ct_10))

ct_check1_s$check <- ifelse(is.na(ct_check1_s$d1) & is.na(ct_check1_s$d2) & is.na(ct_check1_s$d3), 'check','ok')
ct_check1_s1 <- full_join(camostat_studyday1, ct_check1_s, by = 'id') %>% 
  select("id","participant_id","r_date","d1","d2","d3","d4","d5","d6","d7",
         "d8","d9","d10","14","17","20","21","23","24","27", "28","29","check")

library(data.table)
ct_full1 <- melt(setDT(ct_check1_s1), id.vars = c("id","participant_id","r_date","check"), variable.name = "study_day")
ct_full1$study_day1 <- parse_number(as.character(ct_full1$study_day))
ct_full1$study_calendar <- ct_full1$r_date + ct_full1$study_day1-1

colnames(ct_full1)

ct_full1_1 <- ct_full1 %>% select("id","participant_id","r_date","study_calendar","value","check")
ct_final <- spread(ct_full1_1, key = study_calendar, value = value)
ct_final$check <-ifelse(ct_final$id %in% ct_check1_d$id, 'discrepant',
                        ifelse(is.na(ct_final$check) | ct_final$check == 'check' ,'missing',
                               ifelse(ct_final$id %in% id$id,'multiple',ct_final$check)))
ct_final$study <- 'CAMO'

ct_valued5 <- ct_value2 %>% dplyr::filter(study_day <=5 & study_day>0) %>% select(id, study_day,`CT Value`)
ct_valued5 <- ct_valued5[ ! ( ( ct_valued5$id ==3 & ct_valued5$study_day==4 & ct_valued5$`CT Value` == 31.83)) , ]
ct_valued51 <- spread(ct_valued5, key = study_day, value = `CT Value`)
# ct_valued51 <- ct_valued5 %>%
#   arrange(id, study_day) %>%
#   pivot_wider(names_from = study_day, values_from = 'CT Value')

## id 4 study_day 7 has 2 values... take the average
ct_valued7 <- ct_value2 %>% dplyr::filter(study_day <=7 & study_day>0) %>% select(id, study_day,`CT Value`)
ct_valued7 <- ct_valued7[ ! ( ( ct_valued7$id ==3 & ct_valued7$study_day==4 & ct_valued7$`CT Value` == 31.83)) , ] %>%
  group_by(id, study_day) %>%
  dplyr::mutate(`CT Value`=mean(`CT Value`, na.rm = TRUE)) %>%
  dplyr::filter(row_number()==1) %>%
  ungroup()

ct_valued71 <- spread(ct_valued7, key = study_day, value = `CT Value`)
# ct_valued71 <- ct_valued7 %>%
#   arrange(id, study_day) %>%
#   pivot_wider(names_from = study_day, values_from = 'CT Value')












library(data.table)
viro_stanford <- ct_full1 %>% dplyr::filter(check == 'ok') %>% dplyr::select("participant_id", "study_day1","value")
viro_stanford$trial <- 'Stanford'

viro_stanford <- viro_stanford %>% dplyr::mutate(participantID = participant_id, study_day = study_day1) %>% select(participantID, study_day,value, trial) %>% na.omit() 
# %>%
#   filter(study_day <=5)

dir <- "~/pooling camostat - shared data/"
viro_ghent <- read_excel(paste0(dir,"Ghent camostat data/Virology_shell2_complete.xlsx"), sheet = 2)
colnames(viro_ghent) <- viro_ghent[1,]
viro_ghent <- viro_ghent[-(1),]
viro_ghent <- viro_ghent %>% dplyr::filter(!participantID %in% c(6,12,13,14,19,52,55,57,8,10,17,59,20,108))%>% 
  dplyr::mutate(participantID = as.character(participantID)) %>% dplyr::mutate(study_day = as.numeric(study_day))
viro_ghent <- viro_ghent %>%
  dplyr::filter(study_day <=5) %>%
  ## if missing ct6 and not missing ct, use ct:
  dplyr::mutate(value = case_when(!is.na(ct6) ~ ct6,
                                  is.na(ct6) & !is.na(ct) ~ ct)
  ) %>%
  dplyr::select(participantID,study_day,value, trial) 



viro1 <- rbind(viro_stanford,viro_ghent)
viro1$value[is.na(viro1$value)] = 40
viro1$study_day2 <- as.numeric(as.factor(viro1$study_day))

length(unique(viro1$participantID))
length(unique(viro_stanford$participantID)) ### 26
length(unique(viro_ghent$participantID)) ### 93
table(viro1$trial)

viro1 <- as.data.frame(viro1)
viro1$value <- as.numeric(viro1$value)


library(broom)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
### fit the model for ct slope

mods <- viro1 %>%
  dplyr::filter(study_day %in% c(1,2,3,4,5)) %>%
  nest(data = -participantID) %>%
  dplyr::mutate(
    fit = map(data, ~ lm(value~study_day, data = .x)),
    tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  dplyr::filter(term=='study_day') %>%
  ## fix participantID for Stanford patients:
  ## drop the -01- and the _v2
  dplyr::mutate(participantID = gsub("01-|_v2", "", participantID)) %>%
  dplyr::select(participantID, estimate) #%>%
#filter(!is.na(viro1))

demo_viral <- demo %>%
  dplyr::filter(trial %in% c("Stanford", "UZ Ghent")) 

ctslope <- inner_join(mods,demo_viral, by = 'participantID')

library(lme4)
### first fit the frequentist model
model_freq <- lmer(estimate ~ treatment + age + sex + (1+treatment|trial), data=ctslope)
summary(model_freq) ## treatment   -0.016578   0.525667
# -0.016578 - 1.96*0.525667 (-1.04)
# -0.016578 + 1.96*0.525667 (1.01)

## by site
coef(model_freq)

#install.packages('brms')
library(brms)
## set a seed:
set.seed(1234)
# get_prior(b1 ~ treatment + age + sex + (1+treatment|trial), data=ctslope)
get_prior(estimate ~ treatment + age + sex + (1+treatment|trial), data=ctslope)

# Set priors
priors <- c(prior(normal(0,10), class = Intercept), # intercept prior
            prior(normal(0,2.5), class = b), # slope prior
            # prior(lkj_corr_cholesky(1.5), class = cor), # correlation between intercept and beta -- try as sensitivity
            prior(normal(0, 2.5), class = sigma), # population variance
            prior(normal(0, 2.5), class = sd) # tau0, group variance
)

# Fit model
model_bayes <- brm(estimate ~ treatment + age + sex + (1+treatment|trial), data=ctslope,
                   prior = priors,
                   family = gaussian(),
                   warmup = 1000, # burn-in
                   iter = 5000, # number of iterations
                   chains = 2,  # number of MCMC chains
                   control = list(adapt_delta = 0.95)) # advanced MC settings
print(model_bayes) ## -0.01 1.00 -2.05     2.13

## by site
coef(model_bayes)


bayes_viral<- model_bayes %>%
  as_tibble() %>%
  rename("b_treatmentStanford"="r_trial[Stanford,treatment]",
         "b_treatmentUZGhent"="r_trial[UZ.Ghent,treatment]") %>%
  transmute(Stanford = b_treatment + b_treatmentStanford,
            UZ_Ghent = b_treatment + b_treatmentUZGhent,
            Overall = b_treatment,
  ) %>%
  bayesplot::mcmc_intervals(prob_outer = .95)

bayes_viral

bayes_viral_out <- as.data.frame(cbind(bayes_viral$data$m,bayes_viral$data$ll,bayes_viral$data$hh))
colnames(bayes_viral_out) <- c("est","ci_lower","ci_upper")
bayes_viral_out$trial <- c("Stanford", "UZ_Ghent", "Overall")

## create vars in bayes_viral_out to match the figure output:
bayes_viral_out$est_lab = c(0.31, -0.07, 0.11)
bayes_viral_out$ci_lower_lab = c(-1.05, -1.25, -2.04)
bayes_viral_out$ci_upper_lab = c(1.80, 1.09, 2.23)

### viral endpoint forestplot:
library(forestplot)
library(dplyr)
dir <- "~/camo_pool/drafts/"
## get values from bayes_final_r:
## add N obs from survival_symp1:
viral_obs <- ctslope %>%
  ## only those included in model:
  dplyr::filter(!is.na(estimate)) %>%
  group_by(trial) %>%
  dplyr::summarise(n_obs=n()) %>%
  ungroup() %>%
  select(trial, n_obs)

viral_obs <- rbind(viral_obs, cbind(trial="Overall", n_obs=sum(viral_obs$n_obs)))
viral_obs

## clean up trial names to match bayes_viral_out:
viral_obs$trial[which(viral_obs$trial=="UZ Ghent")] <- "UZ_Ghent"


## merge viral_obs into bayes_final_r
forest_viral_out <- merge(bayes_viral_out, viral_obs, by="trial", all.x = TRUE) %>%
  ## make trial a factor to order it:
  dplyr::mutate(trial=factor(trial, levels=c("Stanford", "UZ_Ghent", "Overall"),
                             labels=c("Stanford", "UZ Ghent", "Pooled"))
  ) %>%
  arrange(trial)

# Create confidence interval column to display
forest_viral_out$est_ci <- sprintf("%.2f (%.2f, %.2f)",
                                   round(forest_viral_out$est_lab,2), round(forest_viral_out$ci_lower_lab,2), round(forest_viral_out$ci_upper_lab,2))


forestdat_viral <- structure(list(mean  = c(NA, forest_viral_out$est_lab),
                                  lower = c(NA, forest_viral_out$ci_lower_lab),
                                  upper = c(NA, forest_viral_out$ci_upper_lab)),
                             .Names = c("mean", "lower", "upper"),
                             row.names = c(NA, -3L),
                             class = "data.frame")

foresttext_viral <- cbind(c("", as.character(forest_viral_out$trial)),
                          c("N",forest_viral_out$n_obs),
                          c("Estimate (95% CI)", forest_viral_out$est_ci)
)
jpeg(filename = paste0(dir,"forest_viral_20240416.jpeg"),width = 15, height =10, units = 'in', res = 300)
forestdat_viral %>%
  forestplot(labeltext = foresttext_viral,
             # is.summary = c(rep(FALSE, 6), TRUE),
             xlim = c(-3, 3),
             xticks = c(-2, -1, 0, 1, 2),
             xlab = "Treatment effect (95% CI)",
             zero = 0,
             txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=1.2),
                              ticks = gpar(fontfamily = "", cex=0.8),
                              xlab  = gpar(fontfamily = "", cex = 1.2),
                              legend = gpar(fontfamily = "", cex=0.8)),
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             colgap = unit(10,"mm"),
             arrow_lab = c("Favor Placebo", "Favor Treatment")
  ) %>%
  fp_decorate_graph(graph.pos=2,
                    ## use blank spaces to get the position right, but haven't found a better way
                    left_bottom_txt = fp_txt_gp("                                                                                                               Favor Treatment", gp = gpar(cex=1.2)),
                    right_bottom_txt = fp_txt_gp("Favor Placebo                                                                                                                            ", gp = gpar(cex=1.2))
  ) %>%
  fp_set_style(box=c(rep("black",3), "dodgerblue"),
               line=rep("black",4))
dev.off()




### AEs
ae_aarhus = readxl::read_xlsx("Aarhus camosat data/camostat_AE_data_shell.xlsx",
                              sheet = "characteristics_data")

ae_cruk = readxl::read_xlsx("CRUK_camostat_AE_data.xlsx",
                            sheet = "AE_data")

ae_ghent = readxl::read_xlsx("camostat_AE_data_shell.xlsx",
                             sheet = "characteristics_data") %>%
  ## these were excluded for other UZ Ghent analyses
  dplyr::filter(!participantID %in% c(6,12,13,14,19,52,55,57,8,10,17,59,20,108))

ae_incmnsz = readxl::read_xlsx("camostat_AE_data_shell INCMNSZ.xlsx",
                               sheet = "characteristics_data")

ae_stanford = stanford[which(stanford$redcap_event_name %in% stanford$redcap_event_name[grepl("Adverse", stanford$redcap_event_name)] &
                               stanford$participantID %in% camostat.id$x),
                       c("participantID", names(stanford)[grepl("ae", names(stanford))])] %>%
  select(participantID, aeterm, aeser, aerel, aestdat) %>%
  ## look for aeterm in the ctcae_4_with_lay_terms.pdf file to categorize them
  dplyr::mutate(SOC = case_when(aeterm %in% c("ALT", "ALT elevation", "Borderline hypoxemia on pulse oximeter",
                                              "lactic dehydrogenase", "LDH elevatioin", "liver function tests", "Liver function tests") ~ "Investigations",
                                aeterm %in% c("diarrhea", "Diarrhea", "nausea") ~ "Gastrointestinal disorders",
                                aeterm %in% c("Dry Cough", "dyspnea", "Dyspnea") ~ "Respiratory, Thoracic and mediastinal disorders",
                                aeterm %in% c("Headache") ~ "Nervous System Disorders",
                                aeterm %in% c("Pruritis of genital organ", "rash", "Rash", "Skin rash") ~ "Skin and subcutaneous tissue disorders",
                                aeterm %in% c("vertigo") ~ "Ear and labyrinth disorders",
                                aeterm %in% c("Hypotension") ~ "Vascular disorders",
                                aeterm %in% c("suicidal ideation") ~ "Psychiatric disorders"
  )
  ) %>%
  ## create same vars as the other ae dataframes; AEID (AE count by participantID):
  group_by(participantID) %>%
  dplyr::mutate(AEID = seq(1:n()),
                trial = "Stanford") %>%
  ungroup() %>%
  ## rename vars to match other ae dataframes:
  rename("serious"="aeser",
         "related"="aerel") %>%
  ## keep and order only same vars as other ae dataframes:
  select(trial, participantID, AEID, serious, related, SOC)

ae_all = do.call("rbind", list(ae_aarhus, ae_cruk, ae_ghent, ae_incmnsz, ae_stanford)) %>%
  dplyr::mutate(participantID=case_when(trial=="INCMNSZ-MX" ~ gsub("[^0-9.-]|-", "", participantID),
                                        TRUE ~ participantID)
  )
## get treatment arms from demo df:
demo_fix = demo %>%
  dplyr::mutate(participantID=case_when(trial=="INCMNSZ-MX" ~ gsub("[^0-9.-]|-", "", participantID),
                                        TRUE ~ participantID)
  )


ae_all = merge(ae_all, demo_fix[,c("trial", "participantID", "treatment")], by=c("trial", "participantID"), all.x = TRUE) %>%
  ## drop any NA rows.
  dplyr::filter(!is.na(participantID) & !is.na(AEID)) %>%
  ## fix some more of these AEs that were not categorized:
  dplyr::mutate(SOC = case_when(SOC %in% c("Back pain", "musculoskeletal and connective tissue disorders") ~ "Musculoskeletal and connective tissue disorders",
                                SOC %in% c("Bacteremia", "infections and infestations", "Lung infection", "Otitis media") ~ "Infections and infestations",
                                SOC %in% c("cardiac disorders") ~ "Cardiac disorders",
                                SOC %in% c("Death", "general disorders and administration site conditions", "Fever") ~ "General disorders and administration site conditions",
                                SOC %in% c("Dry cough", "respiratory, thoracic and mediastinal disorders",
                                           "Respiratory, Thoracic and mediastinal disorders", "Dyspnea",
                                           "Productive cough") ~ "Respiratory, thoracic and mediastinal disorders",
                                SOC %in% c("ear and labyrinth disorders") ~ "Ear and labyrinth disorders",
                                SOC %in% c("Enterocolitis", "gastrointestinal disorders", "Nausea", "Stomach pain") ~ "Gastrointestinal disorders",
                                SOC %in% c("eye disorders", "Watering eyes") ~ "Eye disorders",
                                SOC %in% c("investigations") ~ "Investigations",
                                SOC %in% c("metabolism and nutrition disorders") ~ "Metabolism and nutrition disorders",
                                SOC %in% c("nervous system disorders", "Nervous System Disorders", "Paresthesia", "Anosmia") ~ "Nervous system disorders",
                                SOC %in% c("psychiatric disorders", "Psychiatric Disorders") ~ "Psychiatric disorders",
                                SOC %in% c("skin and subcutaneous tissue disorders") ~ "Skin and subcutaneous tissue disorders",
                                
                                
                                TRUE ~ SOC
  ),
  ## create a treatment factor for the table:
  treatment_lab = factor(case_when(treatment==0 ~ "Placebo",
                                   treatment==1 ~ "Camostat")
                         , levels = c("Camostat", "Placebo"))
  )

require(gtsummary)
### try using gtsummary -- 
reset_gtsummary_theme()
# theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

## AEs
tab_ae <- ae_all %>%
  select(SOC, treatment_lab) %>%
  tbl_summary(
    by = treatment_lab,
    # type = list(c("age_mom", "age_gestational") ~ "continuous"),
    # statistic = list(c("age_mom", "age_gestational") ~ "{median} [{p25}, {p75}]"),
    missing_text = "Missing",
    label = list(SOC ~ "MedDRA system organ class"
    )) %>%
  as_gt()

## save html file:
tab_ae %>%
  gt::gtsave("tab_ae_20240126.html", inline_css = TRUE)


## code here for day7 and day14 PCR positive:
# Odds  of testing positive on Day 14 were evaluated by treatment arm using a generalized linear mixed-effects model with a random effect for trial fit using frequentist methods.

#   -   also include data from Jilg et al Figure 2A <https://academic.oup.com/cid/article/77/7/941/7190261?guestAccessKey=>
#   -   Day 7: 31/87 positive in camostat and 29/91 in placebo
# -   Day 14: 11/85 positive in camostat and 10/86 positive in placebo

library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(gtsummary)
library(gt)
library(readxl)
library(readr)
library(stringr)


## Pool ct data for Stanford, Paris, INCMNSZ:
ct_stanford <- readRDS("ct_stanford.rds")
ct_paris <- readRDS("ct_paris.rds")
ct_mexico <- readRDS("ct_mexico.rds")


ct_all <- plyr::rbind.fill(list(ct_stanford, ct_paris, ct_mexico))


### for Stanford data use threshold of 40: <40 is positive; >40 is negative:
ct_all$rt_pcr_result[which(ct_all$trial=="Stanford")] <- ifelse(ct_all$ct[which(ct_all$trial=="Stanford")]<40, 1, 0)


## mexico
demo_incmnsz <- read_excel("INCMNSZ camostat data/camostat_characteristics_data_shell.xlsx", sheet = 2) %>% 
  mutate(participantID = as.character(participantID)) #%>% dplyr::select(trial, participantID,treatment,age,sex)

## stanford
demo_stanford <- read.csv("camo-treatment-codes-for-IMA.csv", stringsAsFactors = FALSE) %>%
  mutate(participantID = Used.By,
         treatment = case_when(Random.Group=="Placebo" ~ 0,
                               Random.Group=="Camostat" ~ 1))

## paris:
demo_paris <- read_delim("camostat_characteristics_France.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% mutate(participantID = as.character(participantID))

trt_all <- do.call("rbind", list(demo_incmnsz[, c("participantID", "treatment")],
                                 demo_stanford[, c("participantID", "treatment")],
                                 demo_paris[, c("participantID", "treatment")]))

ct_all <- merge(ct_all, trt_all, by = "participantID", all.x = TRUE)

## add rows for Jilg et al Fig2 https://academic.oup.com/cid/article/77/7/941/7190261
## created trial, participantID, study_day, rt_pcr_result for:
## day 7: 31 positive, 56 negative in camostat; 29 positive, 62 negative in placebo
## day 14: 11 positive, 74 negative in camostat, 10 positive, 76 negative in placebo
ct_jilg = data.frame(cbind(trial=rep("jilg", 349),
                           participantID=c(1:349),
                           study_day=c(rep(7, 178), rep(14, 171)),
                           rt_pcr_result=c(rep(1, 31), rep(0, 56),
                                           rep(1, 29), rep(0, 62),
                                           rep(1, 11), rep(0, 74),
                                           rep(1, 10), rep(0, 76)),
                           ct=NA, ct2=NA,
                           treatment=c(rep("Camostat",87), rep("Placebo",91), rep("Camostat", 85), rep("Placebo", 86))
))

ct_all = rbind(ct_all, ct_jilg)

ct_all <- ct_all %>%
  mutate(rt_pcr_result = as.numeric(rt_pcr_result),
         treatment=factor(recode(treatment,
                                 "0" = "Placebo",
                                 "1" = "Camostat"),
                          levels = c("Placebo", "Camostat")),
         study_day=factor(recode(study_day,
                                 "6"="7",
                                 "7"="7",
                                 "8"="7",
                                 "13"="14",
                                 "14"="14",
                                 "15"="14")
                          , levels = c("7", "14"))
  )





with(ct_all, table(rt_pcr_result, study_day, useNA = "ifany"))

## for Supplemental Table how many from each site contribute data?
with(ct_all[which(ct_all$treatment=="Camostat"),], table(study_day, trial, useNA = "ifany"))
with(ct_all[which(ct_all$treatment=="Placebo"),], table(study_day, trial, useNA = "ifany"))

## Day 7 rt_pcr_result:
## day 7
df_mod7 <- ct_all %>%
  filter(study_day %in% c(7)) 
# %>%
#   filter(trial!="jilg")

df_mod7 %>%
  mutate(rt_pcr_lab=case_when(rt_pcr_result==0 ~ "Negative",
                              rt_pcr_result==1 ~ "Positive",
                              TRUE ~ NA_character_)) %>%
  tbl_strata(strata = trial,
             .tbl_fun = 
               ~ .x %>% tbl_summary(include = c("treatment", "rt_pcr_lab"),
                                    by=treatment,
                                    missing_text = "Missing",
                                    label = list(rt_pcr_lab ~ "Day 7 RT PCR Result, n (%)")
               )
  ) %>%
  as_gt()

## day 7
mod7 <- glmer(rt_pcr_result ~ treatment + (1|trial), data=df_mod7, family = "binomial") %>%
  tbl_regression(exponentiate=TRUE,
                 label = list("treatment" ~ "Treatment"),
                 pvalue_fun = label_style_pvalue(digits=3)
  ) %>%
  add_n() %>%
  add_nevent()

mod7


### DEBUG ###
# mod <- glmer(rt_pcr_result ~ treatment + (1|trial), data=df_mod7, family = "binomial")
# summary(mod)

## Day 14 rt_pcr_result:
### day 14
df_mod14 <- ct_all %>%
  filter(study_day %in% c(14))

df_mod14 %>%
  mutate(rt_pcr_lab=case_when(rt_pcr_result==0 ~ "Negative",
                              rt_pcr_result==1 ~ "Positive",
                              TRUE ~ NA_character_)) %>%
  tbl_strata(strata = trial,
             .tbl_fun = 
               ~ .x %>% tbl_summary(include = c("treatment", "rt_pcr_lab"),
                                    by=treatment,
                                    missing_text = "Missing",
                                    label = list(rt_pcr_lab ~ "Day 14 RT PCR Result, n (%)")
               )
  ) %>%
  as_gt()

### day 14
mod14 <- glmer(rt_pcr_result ~ treatment + (1|trial), data=df_mod14, family = "binomial") %>%
  tbl_regression(exponentiate=TRUE,
                 label = list("treatment" ~ "Treatment"),
                 pvalue_fun = label_style_pvalue(digits=3)
  ) %>%
  add_n() %>%
  add_nevent()

mod14


### DEBUG ###
# mod <- glmer(rt_pcr_result ~ treatment + (1|trial), data=df_mod14, family = "binomial")
# summary(mod)