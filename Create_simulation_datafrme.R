

##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)
  
##### Load Packages #####
  library(tidyverse)

##### Create annotation data #####
  Anno.df <- data.frame(TestID = c("Predict1","Predict2","Predict3",
                                   "Predict4","Predict5","Predict6"),
                        Tool = c("ToolA","ToolA","ToolA","ToolB","ToolB","ToolB"),
                        Type = c("Type1","Type2","Type3","Type1","Type2","Type3"))
  
##### Create simulation datafrme #####
  #### Binary data ####
  Check_Bi.df <- data.frame(Actual = sample(c(0,1), 100, replace = TRUE),
                            Predict1 = sample(c(0,1), 100, replace = TRUE),
                            Predict2 = sample(c(0,1), 100, replace = TRUE),
                            Predict3 = sample(c(0,1), 100, replace = TRUE),
                            Predict4 = sample(c(0,1), 100, replace = TRUE),
                            Predict5 = sample(c(0,1), 100, replace = TRUE),
                            Predict6 = sample(c(0,1), 100, replace = TRUE))
    
  #### Discrete data: Multiple data ####
  Check_DisMult.df <- data.frame(Actual = sample(c(0:10), 100, replace = TRUE),
                                 Predict1 = sample(c(0:10), 100, replace = TRUE),
                                 Predict2 = sample(c(0:10), 100, replace = TRUE),
                                 Predict3 = sample(c(0:10), 100, replace = TRUE),
                                 Predict4 = sample(c(0:10), 100, replace = TRUE),
                                 Predict5 = sample(c(0:10), 100, replace = TRUE),
                                 Predict6 = sample(c(0:10), 100, replace = TRUE))

  #### Continuous data ####
  Check_Conti.df <- data.frame(Actual = sample(c(0:100), 100, replace = TRUE),
                                 Predict1 = sample(c(0:100), 100, replace = TRUE),
                                 Predict2 = sample(c(0:100), 100, replace = TRUE),
                                 Predict3 = sample(c(0:100), 100, replace = TRUE),
                                 Predict4 = sample(c(0:100), 100, replace = TRUE),
                                 Predict5 = sample(c(0:100), 100, replace = TRUE),
                                 Predict6 = sample(c(0:100), 100, replace = TRUE))
  
##### Save the RData #####
  save.image("D:/Dropbox/##_GitHub/##_CAESAR/CellCheck/Create_simulation_datafrme.RData")
  
  
  
  