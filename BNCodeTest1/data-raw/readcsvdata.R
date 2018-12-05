#devtools::use_data_raw()


RLM_Data <-read.csv('RLData.csv')

devtools::use_data(RLM_Data)
