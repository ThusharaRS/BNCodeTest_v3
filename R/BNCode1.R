#'First 6 Line  Code Testing
#'
#'@export
#'@return data
#'

#Packages <- c("plyr", "dplyr", "tidyr", "bnlearn", "reshape", "bnviewer","rjson","readr")
#lapply(Packages, library, character.only = TRUE)

display <-function(){

  RLM_Data_4vs5_0 <- filter(RLM_Data, RLM_Data$KO_Consumption >= 4)[,c(201,26:39,194:200,209:358)]
  RLM_Data_4vs5_1 <- sapply(RLM_Data_4vs5_0,as.factor)
  RLM_Data_4vs5_2 <- as.data.frame(RLM_Data_4vs5_1)
  RLM_Data_4vs5_3 <- RLM_Data_4vs5_2[,c(107,23,12,16,121,151,163,47,21,78,74,1)]
  RLM_Data_4vs5_4 <- rename(RLM_Data_4vs5_3, c("ST_MainSecOccasionNets17_KO_Prop_4vs5" = "KO during media consumption at leisure","ST_CompanionNets1_KO_Prop_4vs5" = "KO alone or by myself","Q28Q30Loop_11_Q28Q3001" = "Imagery KO is more refreshing than other soft drinks","QCNets_New" = "Age Nets","ST_Q26_10_TB_Prop_4vs5" = "Any Bev to renew my energy","ST_Q26_2_TB_Prop_4vs5" = "Any Bev to wake me up","ST_Q26_6_TB_Prop_4vs5" = "Any Bev to ensure i drink enough each day","ST_DaypartHighLevelNets_4_KO_Prop_4vs5" = "KO in evening","QF_New" = "Income Level","ST_MainSecOccasionNets07_SSD_Regular_Prop_4vs5" = "SSD eating dinner away","ST_MainSecOccasionNets06_KO_Prop_4vs5" = "KO eating lunch away","KO_Consumption_4_5" = "KO Consumption"))
  numrows<- nrow(RLM_Data_4vs5_4)
  return(numrows)

}

#display()
