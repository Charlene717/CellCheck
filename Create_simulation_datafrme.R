

##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  library(tidyverse)

##### Create annotation data #####
  Anno.df <- data.frame(TestID = c("Predict1","Predict2","Predict3","Predict4","Predict5",
                                   "Predict6","Predict7","Predict8","Predict9","Predict10",
                                   "Predict11","Predict12","Predict13","Predict14","Predict15"),
                        Tool = c("ToolA","ToolA","ToolA","ToolB","ToolB","ToolB","ToolC","ToolC","ToolC",
                                 "ToolD","ToolD","ToolD","ToolE","ToolE","ToolE"),
                        Type = c("Type1","Type2","Type3","Type1","Type2","Type3",
                                 "Type1","Type2","Type3","Type1","Type2","Type3",
                                 "Type1","Type2","Type3"))

##### Create simulation datafrme #####
  #### Binary data ####
  Actual1_1 <-sample(c(0,1), 100, replace = TRUE)
  Check_Bi.df <- data.frame(Actual = Actual1_1,
                            Predict1 = Actual1_1,
                            Predict2 = sample(c(0,1), 100, replace = TRUE),
                            Predict3 = sample(c(0,1), 100, replace = TRUE),
                            Predict4 = sample(c(0,1), 100, replace = TRUE),
                            Predict5 = sample(c(0,1), 100, replace = TRUE),
                            Predict6 = sample(c(0,1), 100, replace = TRUE),
                            Predict7 = sample(c(0,1), 100, replace = TRUE),
                            Predict8 = sample(c(0,1), 100, replace = TRUE),
                            Predict9 = sample(c(0,1), 100, replace = TRUE),
                            Predict10 = sample(c(0,1), 100, replace = TRUE),
                            Predict11 = sample(c(0,1), 100, replace = TRUE),
                            Predict12 = sample(c(0,1), 100, replace = TRUE),
                            Predict13 = sample(c(0,1), 100, replace = TRUE),
                            Predict14 = sample(c(0,1), 100, replace = TRUE),
                            Predict15 = sample(c(0,1), 100, replace = TRUE))
  Word.set <- c("A","B")
  Actual1_2 <-sample(Word.set, 100, replace = TRUE)
  Check_Bi2.df <- data.frame(Actual = Actual1_2,
                            Predict1 = Actual1_2,
                            Predict2 = sample(Word.set, 100, replace = TRUE),
                            Predict3 = sample(Word.set, 100, replace = TRUE),
                            Predict4 = sample(Word.set, 100, replace = TRUE),
                            Predict5 = sample(Word.set, 100, replace = TRUE),
                            Predict6 = sample(Word.set, 100, replace = TRUE),
                            Predict7 = sample(Word.set, 100, replace = TRUE),
                            Predict8 = sample(Word.set, 100, replace = TRUE),
                            Predict9 = sample(Word.set, 100, replace = TRUE),
                            Predict10 = sample(Word.set, 100, replace = TRUE),
                            Predict11 = sample(Word.set, 100, replace = TRUE),
                            Predict12 = sample(Word.set, 100, replace = TRUE),
                            Predict13 = sample(Word.set, 100, replace = TRUE),
                            Predict14 = sample(Word.set, 100, replace = TRUE),
                            Predict15 = sample(Word.set, 100, replace = TRUE))
  #### Discrete data: Multiple data ####
  Word.set <- c("A1","A2","B1","B2","C1","C2","D1","D2","E1","E2")
  Actual2_2 <- sample(Word.set, 500, replace = TRUE)
  Check_DisMult.df <- data.frame(Actual = Actual2_2,
                                 Predict1 = Actual2_2,
                                 Predict2 = sample(Word.set, 500, replace = TRUE),
                                 Predict3 = sample(Word.set, 500, replace = TRUE),
                                 Predict4 = sample(Word.set, 500, replace = TRUE),
                                 Predict5 = sample(Word.set, 500, replace = TRUE),
                                 Predict6 = sample(Word.set, 500, replace = TRUE),
                                 Predict7 = sample(Word.set, 500, replace = TRUE),
                                 Predict8 = sample(Word.set, 500, replace = TRUE),
                                 Predict9 = sample(Word.set, 500, replace = TRUE),
                                 Predict10 = sample(Word.set, 500, replace = TRUE),
                                 Predict11 = sample(Word.set, 500, replace = TRUE),
                                 Predict12 = sample(Word.set, 500, replace = TRUE),
                                 Predict13 = sample(Word.set, 500, replace = TRUE),
                                 Predict14 = sample(Word.set, 500, replace = TRUE),
                                 Predict15 = sample(Word.set, 500, replace = TRUE))

  Actual2_1 <- sample(c(0:10), 100, replace = TRUE)
  Check_DisMult2.df <- data.frame(Actual = Actual2_1,
                                  Predict1 = Actual2_1,
                                  Predict2 = sample(c(0:10), 100, replace = TRUE),
                                  Predict3 = sample(c(0:10), 100, replace = TRUE),
                                  Predict4 = sample(c(0:10), 100, replace = TRUE),
                                  Predict5 = sample(c(0:10), 100, replace = TRUE),
                                  Predict6 = sample(c(0:10), 100, replace = TRUE),
                                  Predict7 = sample(c(0:10), 100, replace = TRUE),
                                  Predict8 = sample(c(0:10), 100, replace = TRUE),
                                  Predict9 = sample(c(0:10), 100, replace = TRUE),
                                  Predict10 = sample(c(0:10), 100, replace = TRUE),
                                  Predict11 = sample(c(0:10), 100, replace = TRUE),
                                  Predict12 = sample(c(0:10), 100, replace = TRUE),
                                  Predict13 = sample(c(0:10), 100, replace = TRUE),
                                  Predict14 = sample(c(0:10), 100, replace = TRUE),
                                  Predict15 = sample(c(0:10), 100, replace = TRUE))
  #### Continuous data ####
  Actual3 <- sample(c(0:100), 100, replace = TRUE)
  Check_Conti.df <- data.frame(Actual = Actual3,
                               Predict1 = Actual3,
                               Predict2 = sample(c(0:100), 100, replace = TRUE),
                               Predict3 = sample(c(0:100), 100, replace = TRUE),
                               Predict4 = sample(c(0:100), 100, replace = TRUE),
                               Predict5 = sample(c(0:100), 100, replace = TRUE),
                               Predict6 = sample(c(0:100), 100, replace = TRUE),
                               Predict7 = sample(c(0:100), 100, replace = TRUE),
                               Predict8 = sample(c(0:100), 100, replace = TRUE),
                               Predict9 = sample(c(0:100), 100, replace = TRUE),
                               Predict10 = sample(c(0:100), 100, replace = TRUE),
                               Predict11 = sample(c(0:100), 100, replace = TRUE),
                               Predict12 = sample(c(0:100), 100, replace = TRUE),
                               Predict13 = sample(c(0:100), 100, replace = TRUE),
                               Predict14 = sample(c(0:100), 100, replace = TRUE),
                               Predict15 = sample(c(0:100), 100, replace = TRUE))

##### Save the RData #####
  save.image("D:/Dropbox/##_GitHub/##_CAESAR/CellCheck/Create_simulation_datafrme.RData")



