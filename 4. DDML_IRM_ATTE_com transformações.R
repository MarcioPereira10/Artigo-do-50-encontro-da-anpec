################################################################################
#################### Double/Debiased Machine Learning - IRM (ATTE) ##############
################################################################################

# Limpando o environment 
rm(list=ls())  # Apagando todos os objetos


# Carregando dados
load("C:/Users/ma/Desktop/Bullying/Dados e do-file/Dados R/Vers?o final/Principal/Com transforma??es.RData")



# Instala??o dos pacotes necess?rios
# install.packages("data.table")
# install.packages("DoubleML")
# install.packages("glmnet")
# install.packages("ranger")


# Caregando pacotes
library(data.table) # extens?o do data.frame
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(DoubleML) # Estima??o do DML
library(glmnet)
library(ranger)


# Pode ser que um dos processos necessite de mais mem?ria
memory.limit(9999999999) # Aumentando a mem?ria de forma que n?o trave, aqui 
# al? de usar a mem?ria f?sica do computados, utiliza a mem?oria virtual, 





# Criando os data.table

## Agora, necessita-se a transformar a data.table em um formato que o comando con-
## siga ler, assim, desenha-se o DoubleMLData  
dt1_math = DoubleMLData$new(x3,
                            y_col = "PV1MATH",
                            d_cols = "t1",
                            x_cols = c(paste(colnames(xe), sep = ", ")))
# x6 listas os principais objetos e m?todos de um DoubleMLData
# View(x6$data) Para ver os dados da data.table


## Criando para os demais tratamentos para Matem?tica
dt2_math = DoubleMLData$new(x3,
                            y_col = "PV1MATH",
                            d_cols = "t2",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt3_math = DoubleMLData$new(x3,
                            y_col = "PV1MATH",
                            d_cols = "t3",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt4_math = DoubleMLData$new(x3,
                            y_col = "PV1MATH",
                            d_cols = "t4",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt5_math = DoubleMLData$new(x3,
                            y_col = "PV1MATH",
                            d_cols = "t5",
                            x_cols = c(paste(colnames(xe), sep = ", ")))





## data.table para leitura
dt1_read = DoubleMLData$new(x3,
                            y_col = "PV1READ",
                            d_cols = "t1",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt2_read = DoubleMLData$new(x3,
                            y_col = "PV1READ",
                            d_cols = "t2",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt3_read = DoubleMLData$new(x3,
                            y_col = "PV1READ",
                            d_cols = "t3",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt4_read = DoubleMLData$new(x3,
                            y_col = "PV1READ",
                            d_cols = "t4",
                            x_cols = c(paste(colnames(xe), sep = ", ")))


dt5_read = DoubleMLData$new(x3,
                            y_col = "PV1READ",
                            d_cols = "t5",
                            x_cols = c(paste(colnames(xe), sep = ", ")))





## data.table para ci?ncias
dt1_scie = DoubleMLData$new(x3,
                            y_col = "PV1SCIE",
                            d_cols = "t1",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt2_scie = DoubleMLData$new(x3,
                            y_col = "PV1SCIE",
                            d_cols = "t2",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt3_scie = DoubleMLData$new(x3,
                            y_col = "PV1SCIE",
                            d_cols = "t3",
                            x_cols = c(paste(colnames(xe), sep = ", ")))

dt4_scie = DoubleMLData$new(x3,
                            y_col = "PV1SCIE",
                            d_cols = "t4",
                            x_cols = c(paste(colnames(xe), sep = ", ")))


dt5_scie = DoubleMLData$new(x3,
                            y_col = "PV1SCIE",
                            d_cols = "t5",
                            x_cols = c(paste(colnames(xe), sep = ", ")))










## Estima??o IRM por Random forest para matem?tica

set.seed(31415) # necess?rio para a sample split (divis?o da amostra)

### Ajustando as fun??es inc?modo
learner_g = lrn("regr.ranger", num.trees = 500, min.node.size = 2, max.depth = 5)
learner_classif_m = lrn("classif.ranger", num.trees = 500, min.node.size = 2, max.depth = 5)


### Estima??o IRM

#### Para t1
doubleml_math_t1 = DoubleMLIRM$new(dt1_math,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_math_t1$fit() # Ajustando o modelo
doubleml_math_t1$summary() # imprimindo o resultado


#### Para t2
doubleml_math_t2 = DoubleMLIRM$new(dt2_math,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_math_t2$fit() # Ajustando o modelo
doubleml_math_t2$summary() # imprimindo o resultado


#### Para t3
doubleml_math_t3 = DoubleMLIRM$new(dt3_math,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_math_t3$fit() # Ajustando o modelo
doubleml_math_t3$summary() # imprimindo o resultado


#### Para t4
doubleml_math_t4 = DoubleMLIRM$new(dt4_math,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_math_t4$fit() # Ajustando o modelo
doubleml_math_t4$summary() # imprimindo o resultado


#### Para t5
doubleml_math_t5 = DoubleMLIRM$new(dt5_math,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_math_t5$fit() # Ajustando o modelo
doubleml_math_t5$summary() # imprimindo o resultado





## Estima??o PLR por Random forest para leitura

#### Para t1
doubleml_read_t1 = DoubleMLIRM$new(dt1_read,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_read_t1$fit() # Ajustando o modelo
doubleml_read_t1$summary() # imprimindo o resultado


#### Para t2
doubleml_read_t2 = DoubleMLIRM$new(dt2_read,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_read_t2$fit() # Ajustando o modelo
doubleml_read_t2$summary() # imprimindo o resultado


#### Para t3
doubleml_read_t3 = DoubleMLIRM$new(dt3_read,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_read_t3$fit() # Ajustando o modelo
doubleml_read_t3$summary() # imprimindo o resultado


#### Para t4
doubleml_read_t4 = DoubleMLIRM$new(dt4_read,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_read_t4$fit() # Ajustando o modelo
doubleml_read_t4$summary() # imprimindo o resultado


#### Para t5
doubleml_read_t5 = DoubleMLIRM$new(dt5_read,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_read_t5$fit() # Ajustando o modelo
doubleml_read_t5$summary() # imprimindo o resultado




## Estima??o PLR por Random forest para ci?ncias

#### Para t1
doubleml_scie_t1 = DoubleMLIRM$new(dt1_scie,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_scie_t1$fit() # Ajustando o modelo
doubleml_scie_t1$summary() # imprimindo o resultado


#### Para t2
doubleml_scie_t2 = DoubleMLIRM$new(dt2_scie,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_scie_t2$fit() # Ajustando o modelo
doubleml_scie_t2$summary() # imprimindo o resultado


#### Para t3
doubleml_scie_t3 = DoubleMLIRM$new(dt3_scie,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_scie_t3$fit() # Ajustando o modelo
doubleml_scie_t3$summary() # imprimindo o resultado


#### Para t4
doubleml_scie_t4 = DoubleMLIRM$new(dt4_scie,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_scie_t4$fit() # Ajustando o modelo
doubleml_scie_t4$summary() # imprimindo o resultado


#### Para t5
doubleml_scie_t5 = DoubleMLIRM$new(dt5_scie,
                                   ml_m = learner_classif_m,
                                   ml_g = learner_g,
                                   score = "ATTE",
                                   dml_procedure = "dml2",
                                   n_folds = 5,
                                   n_rep = 1)
doubleml_scie_t5$fit() # Ajustando o modelo
doubleml_scie_t5$summary() # imprimindo o resultado
