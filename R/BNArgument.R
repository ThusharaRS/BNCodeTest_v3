#'Creating json object for BN

#'@return value
#'@export



getData <- function(MY_BL2,MY_WL2){
  # install.packages('bnlearn', dependencies=TRUE, repos='http://cran.rstudio.com/')
  # install.packages("GGally")
  # install.packages("network")
  # install.packages("sna")
  # install.packages("RColorBrewer")
  # install.packages("intergraph")
  # install.packages("qgraph")
  # install.packages("bnviewer")
  # install.packages("devtools")
  # devtools::install_github("robson-fernandes/bnviewer")
  # install.packages(c('devtools','bnviewer'))
  # install.packages("lazyeval")
  Packages <- c("plyr", "dplyr", "tidyr", "bnlearn", "reshape", "bnviewer","rjson","readr")
  lapply(Packages, library, character.only = TRUE)

  MY_WL<-unlist(MY_WL2)
  MY_BL<-unlist(MY_BL2)

  BL <- matrix(c(MY_BL
  ),
  ncol = 2, byrow = TRUE)

  WL <- matrix(c(MY_WL
  ),
  ncol = 2, byrow = TRUE)


  data(RLM_Data)

  RLM_Data_4vs5_0 <- filter(RLM_Data, RLM_Data$KO_Consumption >= 4)[,c(201,26:39,194:200,209:358)]
  RLM_Data_4vs5_1 <- sapply(RLM_Data_4vs5_0,as.factor)
  RLM_Data_4vs5_2 <- as.data.frame(RLM_Data_4vs5_1)
  RLM_Data_4vs5_3 <- RLM_Data_4vs5_2[,c(107,23,12,16,121,151,163,47,21,78,74,1)]
  RLM_Data_4vs5_4 <- rename(RLM_Data_4vs5_3, c("ST_MainSecOccasionNets17_KO_Prop_4vs5" = "KO during media consumption at leisure","ST_CompanionNets1_KO_Prop_4vs5" = "KO alone or by myself","Q28Q30Loop_11_Q28Q3001" = "Imagery KO is more refreshing than other soft drinks","QCNets_New" = "Age Nets","ST_Q26_10_TB_Prop_4vs5" = "Any Bev to renew my energy","ST_Q26_2_TB_Prop_4vs5" = "Any Bev to wake me up","ST_Q26_6_TB_Prop_4vs5" = "Any Bev to ensure i drink enough each day","ST_DaypartHighLevelNets_4_KO_Prop_4vs5" = "KO in evening","QF_New" = "Income Level","ST_MainSecOccasionNets07_SSD_Regular_Prop_4vs5" = "SSD eating dinner away","ST_MainSecOccasionNets06_KO_Prop_4vs5" = "KO eating lunch away","KO_Consumption_4_5" = "KO Consumption"))







  BN_RLM_HC_9 <- hc(RLM_Data_4vs5_4, score = "aic", whitelist = WL, blacklist = BL)
  BN_RLM_HC_9

  Score_BN <- score(BN_RLM_HC_9,RLM_Data_4vs5_4)
  Score_BN

  acyclic(BN_RLM_HC_9, directed = FALSE, debug = FALSE)
  directed(BN_RLM_HC_9)
  Arcs_DF <- as.data.frame(arcs(BN_RLM_HC_9))
  Arcs_DF$Unique <- paste(Arcs_DF$from,Arcs_DF$to,sep = "-")

  Boot_Strength_8 <- boot.strength(RLM_Data_4vs5_4, cluster = NULL, R = 100, m = 50, algorithm = "hc", algorithm.args = list(), cpdag = TRUE, debug = FALSE)


  Boot_Strength_DF <- data.frame(From = Boot_Strength_8$from, To = Boot_Strength_8$to,
                                 strength = Boot_Strength_8$strength,
                                 direction = Boot_Strength_8$direction
  )

  Boot_Strength_DF$Unique <- paste(Boot_Strength_DF$From,Boot_Strength_DF$To,sep = "-")
  Arcs_BN <- merge(Arcs_DF,Boot_Strength_DF,by = "Unique")
  request.body <- toJSON(Arcs_BN[,-c(1:3,7)])


  return(request.body )
}



#getData(MY_BL1,MY_WL1)
