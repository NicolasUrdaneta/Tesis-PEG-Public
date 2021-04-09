################################################################################
##                                                                            ##
##                          Causal trees                                      ##
##                                                                            ##
################################################################################
##                                                                            ##
################################################################################

prof_controls<-c("antiguedad_prof_adj",
                 "categ_prof2","age_prof","VA")
est_controls<-c("age","num_materias_v2_nota","gpa_sem","gpa_acum",
                "estrato","educ_madre","ocup_madre","facultad_est","nota_curso",
                "creditos_curso","situacion_academ","tiempo_rta")
other_controls<-c("total_estudiantes","fraccion_mujeres_es","tot_profs_curso")

db.surveys<-read_dta(paste0(modified,"/Final Surveys Dataset.dta")) %>% 
  dplyr::select(sexo_prof,sexo_prof_res,score_8_res,score_sem_prof_res, score_sem_curso_res,
         positive_res, negative_res,all_of(prof_controls),all_of(est_controls),
         all_of(other_controls),common_sample, sexo_est, idcursoseccion) %>% 
  filter(common_sample==1 & is.na(sexo_est)==F)

prof_controls<-c("antiguedad_prof_adj",
                 "factor(categ_prof2)","age_prof","VA")
est_controls<-c("age","num_materias_v2_nota","gpa_sem","gpa_acum","factor(estrato)",
                "factor(educ_madre)","nota_curso",
                "creditos_curso","tiempo_rta")
other_controls<-c("total_estudiantes","fraccion_mujeres_es","tot_profs_curso")
  

fmla_8 <- paste("score_8_res ~ ",
                paste(prof_controls,collapse = " + "),
                " + ",paste(est_controls, collapse = " + "),
                " + ",paste(other_controls, collapse = " + "))

fmla_prof <- paste("score_sem_prof_res ~ ",
                   paste(prof_controls,collapse = " + "),
                   " + ",paste(est_controls, collapse = " + "),
                   " + ",paste(other_controls, collapse = " + "))

fmla_curso <- paste("score_sem_curso_res ~ ",
                    paste(prof_controls,collapse = " + "),
                    " + ",paste(est_controls, collapse = " + "),
                    " + ",paste(other_controls, collapse = " + "))

fmla_positive <- paste("positive_res ~ ",
                       paste(prof_controls,collapse = " + "),
                   " + ",paste(est_controls, collapse = " + "),
                   " + ",paste(other_controls, collapse = " + "))

fmla_negative <- paste("negative_res ~ ",
                       paste(prof_controls,collapse = " + "),
                    " + ",paste(est_controls, collapse = " + "),
                    " + ",paste(other_controls, collapse = " + "))

df_comments<-db.surveys[!is.na(db.surveys$positive_res),]

set.seed(1)
sample_train<-sample(1:nrow(db.surveys),0.5*nrow(db.surveys))
sample_train_comments<-sample(1:nrow(df_comments),0.5*nrow(df_comments))


df_train <- db.surveys[sample_train,]
df_test <- db.surveys[-sample_train,]

df_train_com <- df_comments[sample_train_comments,]
df_test_com <- df_comments[-sample_train_comments,]

ct_unpruned_8 <- honest.causalTree(
  formula = fmla_8, # Define the model
  data = df_train, # Subset used to create tree structure
  est_data = df_test, # Which data set to use to estimate effects
  treatment = df_train$sexo_prof, # Splitting sample treatment variable
  est_treatment = df_test$sexo_prof, # Estimation sample treatment variable
  split.Rule = "CT", # Define the splitting option
  cv.option = "TOT", # Cross validation options
  cp = 0, # Complexity parameter
  split.Honest = TRUE, # Use honesty when splitting
  cv.Honest = TRUE, # Use honesty when performing cross-validation
  minsize = 2500, # Min. number of treatment and control cases in each leaf
  HonestSampleSize = nrow(df_test)) # Num obs used in estimation after building the tree

ct_unpruned_prof <- honest.causalTree(
  formula = fmla_prof, # Define the model
  data = df_train, # Subset used to create tree structure
  est_data = df_test, # Which data set to use to estimate effects
  treatment = df_train$sexo_prof, # Splitting sample treatment variable
  est_treatment = df_test$sexo_prof, # Estimation sample treatment variable
  split.Rule = "CT", # Define the splitting option
  cv.option = "TOT", # Cross validation options
  cp = 0, # Complexity parameter
  split.Honest = TRUE, # Use honesty when splitting
  cv.Honest = TRUE, # Use honesty when performing cross-validation
  minsize = 2500) # Min. number of treatment and control cases in each leaf

ct_unpruned_curso <- honest.causalTree(
  formula = fmla_curso, # Define the model
  data = df_train, # Subset used to create tree structure
  est_data = df_test, # Which data set to use to estimate effects
  treatment = df_train$sexo_prof, # Splitting sample treatment variable
  est_treatment = df_test$sexo_prof, # Estimation sample treatment variable
  split.Rule = "CT", # Define the splitting option
  cv.option = "TOT", # Cross validation options
  cp = 0, # Complexity parameter
  split.Honest = TRUE, # Use honesty when splitting
  cv.Honest = TRUE, # Use honesty when performing cross-validation
  minsize = 2500) # Min. number of treatment and control cases in each leaf

ct_unpruned_positive <- honest.causalTree(
  formula = fmla_positive, # Define the model
  data = df_train_com, # Subset used to create tree structure
  est_data = df_test_com, # Which data set to use to estimate effects
  treatment = df_train_com$sexo_prof, # Splitting sample treatment variable
  est_treatment = df_test_com$sexo_prof, # Estimation sample treatment variable
  split.Rule = "CT", # Define the splitting option
  cv.option = "TOT", # Cross validation options
  cp = 0, # Complexity parameter
  split.Honest = TRUE, # Use honesty when splitting
  cv.Honest = TRUE, # Use honesty when performing cross-validation
  minsize = 1500) # Min. number of treatment and control cases in each leaf

ct_unpruned_negative <- honest.causalTree(
  formula = fmla_negative, # Define the model
  data = df_train_com, # Subset used to create tree structure
  est_data = df_test_com, # Which data set to use to estimate effects
  treatment = df_train_com$sexo_prof, # Splitting sample treatment variable
  est_treatment = df_test_com$sexo_prof, # Estimation sample treatment variable
  split.Rule = "CT", # Define the splitting option
  cv.option = "TOT", # Cross validation options
  cp = 0, # Complexity parameter
  split.Honest = TRUE, # Use honesty when splitting
  cv.Honest = TRUE, # Use honesty when performing cross-validation
  minsize = 1500) # Min. number of treatment and control cases in each leaf
  

ct_cptable_8 <- as.data.frame(ct_unpruned_8$cptable)
ct_cptable_prof <- as.data.frame(ct_unpruned_prof$cptable)
ct_cptable_curso <- as.data.frame(ct_unpruned_curso$cptable)
ct_cptable_positive <- as.data.frame(ct_unpruned_positive$cptable)
ct_cptable_negative <- as.data.frame(ct_unpruned_negative$cptable)


# Obtain optimal complexity parameter to prune tree from cross validated error
selected_cp_8 <- which.min(ct_cptable_8$xerror)
selected_cp_prof <- which.min(ct_cptable_prof$xerror)
selected_cp_curso <- which.min(ct_cptable_curso$xerror)
selected_cp_positive <- which.min(ct_cptable_positive$xerror)
selected_cp_negative <- which.min(ct_cptable_negative$xerror)

optim_cp_ct_8 <- ct_cptable_8[selected_cp_8, "CP"]
optim_cp_ct_prof <- ct_cptable_prof[selected_cp_prof, "CP"]
optim_cp_ct_curso <- ct_cptable_curso[selected_cp_curso, "CP"]
optim_cp_ct_positive <- ct_cptable_positive[selected_cp_positive, "CP"]
optim_cp_ct_negative <- ct_cptable_negative[selected_cp_negative, "CP"]

# Prune the tree at optimal complexity parameter.
ct_pruned_8 <- prune(tree = ct_unpruned_8, cp = optim_cp_ct_8)
ct_pruned_prof <- prune(tree = ct_unpruned_prof, cp = optim_cp_ct_prof)
ct_pruned_curso <- prune(tree = ct_unpruned_curso, cp = optim_cp_ct_curso)
ct_pruned_positive <- prune(tree = ct_unpruned_positive, cp = optim_cp_ct_positive)
ct_pruned_negative <- prune(tree = ct_unpruned_negative, cp = optim_cp_ct_negative)

ct_pruned_8

#df_test_8<-df_test[is.na(df_test$score_stand_8_res)==F,]
tauhat_ct_8 <- predict(ct_pruned_8, newdata = df_test)
tauhat_ct_prof <- predict(ct_pruned_prof, newdata = df_test)
tauhat_ct_curso <- predict(ct_pruned_curso, newdata = df_test)
tauhat_ct_positive <- predict(ct_pruned_positive, newdata = df_test_com)
tauhat_ct_negative <- predict(ct_pruned_negative, newdata = df_test_com)


rpart.plot(
  x = ct_pruned_8, # Pruned tree
  type = 3, # Draw separate split labels for the left and right directions
  fallen = TRUE, # Position the leaf nodes at the bottom of the graph
  leaf.round = 1, # Rounding of the corners of the leaf node boxes
  extra = 100, # Display the percentage of observations in the node
  branch = 0.3, # Shape of the branch lines
  box.palette = "RdBu",
  tweak=2) # Palette for coloring the node

rpart.plot(
  x = ct_pruned_prof, # Pruned tree
  type = 3, # Draw separate split labels for the left and right directions
  fallen = TRUE, # Position the leaf nodes at the bottom of the graph
  leaf.round = 1, # Rounding of the corners of the leaf node boxes
  extra = 100, # Display the percentage of observations in the node
  branch = 1, # Shape of the branch lines
  box.palette = "RdBu",
  tweak=2) # Palette for coloring the node

rpart.plot(
  x = ct_pruned_curso, # Pruned tree
  type = 2, # Draw separate split labels for the left and right directions
  fallen = FALSE, # Position the leaf nodes at the bottom of the graph
  leaf.round = 1, # Rounding of the corners of the leaf node boxes
  extra = 100, # Display the percentage of observations in the node
  branch = 0.3, # Shape of the branch lines
  box.palette = "RdBu",
  tweak=2) # Palette for coloring the node

rpart.plot(
  x = ct_pruned_positive, # Pruned tree
  type = 2, # Draw separate split labels for the left and right directions
  fallen = TRUE, # Position the leaf nodes at the bottom of the graph
  leaf.round = 1, # Rounding of the corners of the leaf node boxes
  extra = 100, # Display the percentage of observations in the node
  branch = 0.3, # Shape of the branch lines
  box.palette = "RdBu",
  tweak=2) # Palette for coloring the node

rpart.plot(
  x = ct_pruned_negative, # Pruned tree
  type = 2, # Draw separate split labels for the left and right directions
  fallen = TRUE, # Position the leaf nodes at the bottom of the graph
  leaf.round = 1, # Rounding of the corners of the leaf node boxes
  extra = 100, # Display the percentage of observations in the node
  branch = 0.3, # Shape of the branch lines
  box.palette = "RdBu",
  tweak=2) # Palette for coloring the node


# Create a factor column 'leaf' indicating leaf assignment
num_leaves <- length(unique(tauhat_ct_8)) #There are as many leaves as there are predictions
df_test$leaf_8 <- factor(tauhat_ct_8, labels = seq(num_leaves))
num_leaves <- length(unique(tauhat_ct_prof)) #There are as many leaves as there are predictions
df_test$leaf_prof <- factor(tauhat_ct_prof, labels = seq(num_leaves))
num_leaves <- length(unique(tauhat_ct_curso)) #There are as many leaves as there are predictions
df_test$leaf_curso <- factor(tauhat_ct_curso, labels = seq(num_leaves))
num_leaves <- length(unique(tauhat_ct_positive)) #There are as many leaves as there are predictions
df_test$leaf_positive <- factor(tauhat_ct_positive, labels = seq(num_leaves))
num_leaves <- length(unique(tauhat_ct_negative)) #There are as many leaves as there are predictions
df_test$leaf_negative <- factor(tauhat_ct_negative, labels = seq(num_leaves))

df_test$predict_8 <- tauhat_ct_8

# num_leaves <- length(unique(tauhat_ct_prof)) #There are as many leaves as there are predictions
# df_test$leaf <- factor(tauhat_ct_prof, labels = seq(num_leaves))
# df_test$predict <- tauhat_ct_prof

# Run the regression
ols_ct <- felm(as.formula("score_8_res ~ 0 + leaf_8 + sexo_prof_res:leaf_8 | 0 | 0 | idcursoseccion"), data= df_test)
ols_ct_summary <- summary(ols_ct)
ols_ct_summary


ols_ct <- felm(as.formula("score_sem_prof_res ~ 0 + leaf_prof + sexo_prof_res:leaf_prof | 0 | 0 | idcursoseccion"), data= df_test)
ols_ct_summary <- summary(ols_ct)
ols_ct_summary

ols_ct <- felm(as.formula("score_sem_curso_res ~ 0 + leaf_curso + sexo_prof_res:leaf_curso | 0 | 0 | idcursoseccion"), data= df_test)
ols_ct_summary <- summary(ols_ct)
ols_ct_summary


##