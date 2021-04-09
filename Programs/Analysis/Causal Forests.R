################################################################################
##                                                                            ##
##                          Causal forest                                     ##
##                                                                            ##
################################################################################
##                                                                            ##
################################################################################


prof_controls<-c("antiguedad_prof_adj",
                 "categ_prof2","age_prof","tiempo_rta","VA")
est_controls<-c("age","num_materias_v2_nota","gpa_sem","gpa_acum",
                "educ_madre","facultad_curso","nota_curso",
                "creditos_curso","tiempo_rta")
other_controls<-c("total_estudiantes","fraccion_mujeres_es","tot_profs_curso")

db.surveys<-read_dta(paste0(modified,"/Final Surveys Dataset.dta")) %>% 
  dplyr::select(sexo_prof,sexo_prof_res,score_8_res,score_sem_prof_res, score_sem_curso_res,
         positive_res, negative_res, all_of(prof_controls),all_of(est_controls),
         all_of(other_controls),common_sample, sexo_est, idcursoseccion) %>% 
  filter(common_sample==1 & is.na(sexo_est)==F)

## Adjust factor variables
db.surveys$educ_madre<-ifelse(db.surveys$educ_madre==1 |
                              db.surveys$educ_madre==3 | 
                              db.surveys$educ_madre==5,1,0)

## Set train and test datasets

set.seed(1)
sample_train<-sample(1:nrow(db.surveys),0.5*nrow(db.surveys))

df_train <- db.surveys[sample_train,]
df_test <- db.surveys[-sample_train,]

## Choose outcome & remove from data
outcome<-"score_sem_prof_res"
nonoutcomes<-setdiff(c("idcursoseccion","score_8_res","score_sem_prof_res",
                       "score_sem_curso_res","positive_res","negative_res"),
                     outcome)
df_train<-df_train[,!(names(df_train) %in% nonoutcomes)]
df_test<-df_test[,!(names(df_test) %in% nonoutcomes)]

## Remove any missing values from any variable

df_train$missing<-apply(df_train, MARGIN=1, FUN=function(x){sum(is.na(x))})
df_test$missing<-apply(df_test, MARGIN=1, FUN=function(x){sum(is.na(x))})

df_train<-df_train[df_train$missing==0,]
df_test<-df_test[df_test$missing==0,]

## Estimate causal forests
## Estimate causal forests
factors<-c("categ_prof2","educ_madre","facultad_curso")
db.controls_train<-df_train[,!(names(df_train) %in% c("sexo_prof_res","sexo_prof",
                                                      "sexo_est","missing","common_sample",outcome))]

db.controls_test<-df_test[,!(names(df_test) %in% c("sexo_prof_res","sexo_prof",
                                                "sexo_est","missing","common_sample",outcome))]
db.controls_train[,factors]<-lapply(db.controls_train[,factors], function(x){as.factor(x)})
db.controls_test[,factors]<-lapply(db.controls_test[,factors], function(x){as.factor(x)})

db.controls_train<-fastDummies::dummy_cols(db.controls_train, select_columns = factors, remove_selected_columns = T)
db.controls_test<-fastDummies::dummy_cols(db.controls_test, select_columns = factors, remove_selected_columns = T)

X=as.matrix(db.controls_train)
Y=as.matrix(df_train[,outcome])
W=as.matrix(df_train[,"sexo_prof_res"])

trees<-100
forest<-causal_forest(X=X,Y=Y,W=W,
                      tune.parameters=c("all"),
                      num.trees=trees,
                      seed=123)
#SaveRds(forest, "Forest 200", output.dir)
test_calibration(forest)
# forest<-causal_forest(X=X,Y=Y,W=W,
#                       tune.parameters=c("imbalance.penalty"),
#                       num.trees=trees, 
#                       min.node.size = 1000,
#                       honesty=TRUE,
#                       honesty.fraction=0.5,
#                       alpha=0.5,
#                       honesty.prune.leaves=TRUE)

#SaveRds(forest,"Causal Forest Output", output.dir)

df_predict<-df_test

df_predict$forest_hat<-predict(forest,db.controls_test)$predictions
df_predict$forest_hat_se<-sqrt(predict(forest,db.controls_test, 
                       estimate.variance = TRUE)$variance.estimates)

df_predict$forest_hat_tstat<-df_predict$forest_hat/df_predict$forest_hat_se
df_predict$forest_hat_sig<-ifelse(abs(df_predict$forest_hat_tstat)>=1.96,1,0)

df_predict$categs<-as.factor(ifelse(abs(df_predict$forest_hat)>0.1,1,
                   ifelse(abs(df_predict$forest_hat)<0.1 & abs(df_predict$forest_hat)>=0.05,2,3)))
                   

summary(df_predict$forest_hat)

slackr_bot("Finished causal forest",incoming_webhook_url="https://hooks.slack.com/services/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")
slackr_bot(test_calibration(forest),incoming_webhook_url="https://hooks.slack.com/services/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")
slackr_bot(summary(df_predict$forest_hat),incoming_webhook_url="https://hooks.slack.com/services/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")
slackr_bot(summary(df_predict$forest_hat_tstat),incoming_webhook_url="https://hooks.slack.com/services/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")


summary(df_predict$forest_hat_tstat)
summary(df_predict$forest_hat_sig)

density_F<-density(df_predict[df_predict$sexo_est==1,]$forest_hat, n = 2^12, adjust=1)
density.data_F<-data.frame(x=density_F$x, y=density_F$y)
density_H<-density(df_predict[df_predict$sexo_est==0,]$forest_hat, n = 2^12, adjust=1)
density.data_H<-data.frame(x=density_H$x, y=density_H$y)

data.quants_F<-data.frame(quant=quantile(df_predict[df_predict$sexo_est==1,]$forest_hat,
                                       c(.1, .2, .3, .4, .5, .6, .7, .8, .9)))
data.quants_H<-data.frame(quant=quantile(df_predict[df_predict$sexo_est==0,]$forest_hat,
                                         c(.1, .2, .3, .4, .5, .6, .7, .8, .9)))

data.quants_F$y_quants<-sapply(data.quants_F$quant,function(z){
  mean(density.data_F[round(z,3)==round(density.data_F$x,3),]$y)})
data.quants_H$y_quants<-sapply(data.quants_H$quant,function(z){
  mean(density.data_H[round(z,2)==round(density.data_H$x,2),]$y)})



mean(df_predict[df_predict$sexo_est==0,]$forest_hat)
mean(df_predict[df_predict$sexo_est==1,]$forest_hat)


forest_density_table<-rbind(t(as.matrix(data.quants_F$quant)),
                            t(as.matrix(data.quants_H$quant)))
colnames(forest_density_table)<-rownames(data.quants_F)
rownames(forest_density_table)<-c("Estudiante mujer", "Estudiante hombre")


table_raw<-stargazer(forest_density_table, type="latex", summary=F, digits=3,
                     table.layout = "t",out.header=F, header=F)

idx0 <- grep("begin{tabular}", table_raw, fixed = T)[1] # Start of \begin{tabular}
idx1 <- grep("end{tabular}", table_raw, fixed = T)[1] # End of \begin{tabular}

tex_header <- c(table_raw[idx0], "\\toprule \\toprule")
tex_footer <- c("\\bottomrule \\bottomrule", table_raw[idx1])

# Remove [-1.8ex] and get the inside of the tabular
tex_inner <- gsub("\\\\[-[\\.0-9]+ex]", "", table_raw[(idx0+1):(idx1-1)])
tex_inner <-tex_inner[3:6]


latex_output <- c(tex_header,tex_inner,tex_footer)

# Write to file
write(latex_output,paste0(git_tables, "/Forest deciles.tex"))



### Density plot


l_green<-rgb(30,150,30, alpha = 255, max=255)
l_blue<-rgb(50,100,200, alpha = 255, max=255)

l_green2<-rgb(30,150,30, alpha = 180, max=255)
l_blue2<-rgb(50,100,200, alpha = 180, max=255)

ggplot(data.frame(x = density.data_F[abs(density.data_F$x)<0.8,]$x, 
                  y = density.data_F[abs(density.data_F$x)<0.8,]$y), aes(x, y)) + 
  geom_line(size=0) + 
  geom_segment(aes(xend = x, yend = 0, colour = x)) + 
  scale_colour_gradientn(colours = c(l_blue, l_blue2,"Grey",l_green2,l_green),
                         values=c(0,0.45,0.55,1)  )+xlim(-1,1)+
  theme_minimal()+
  geom_segment(x=0.05,y=0, xend=0.05, yend=3.2,linetype="dashed", size=1.05)+
  geom_segment(x=-0.05,y=0, xend=-0.05, yend=3.2,linetype="dashed", size=1.05)+
  geom_point(data=data.quants_F,aes(x = quant, y= y_quants))+
  geom_label(label="10%",x=data.quants_F$quant[1]-0.05,y=data.quants_F$y_quants[1],label.size = 0.2,color = "black")+
  geom_label(label="20%",x=data.quants_F$quant[2]-0.05,y=data.quants_F$y_quants[2],label.size = 0.2,color = "black")+
  geom_label(label="30%",x=data.quants_F$quant[3]-0.05,y=data.quants_F$y_quants[3],label.size = 0.2,color = "black")+
  geom_label(label="40%",x=data.quants_F$quant[4]-0.05,y=data.quants_F$y_quants[4],label.size = 0.2,color = "black")+
  geom_label(label="50%",x=data.quants_F$quant[5]-0.05,y=data.quants_F$y_quants[5],label.size = 0.2,color = "black")+
  geom_label(label="60%",x=data.quants_F$quant[6]+0.05,y=data.quants_F$y_quants[6],label.size = 0.2,color = "black")+
  geom_label(label="70%",x=data.quants_F$quant[7]+0.05,y=data.quants_F$y_quants[7]+0.1,label.size = 0.2,color = "black")+
  geom_label(label="80%",x=data.quants_F$quant[8]+0.05,y=data.quants_F$y_quants[8],label.size = 0.2,color = "black")+
  geom_label(label="90%",x=data.quants_F$quant[9]+0.05,y=data.quants_F$y_quants[9],label.size = 0.2,color = "black")


ggplot(data.frame(x = density.data_H[abs(density.data_H$x)<0.8,]$x, 
                  y = density.data_H[abs(density.data_H$x)<0.8,]$y), aes(x, y)) + 
  geom_line(size=0) + 
  geom_segment(aes(xend = x, yend = 0, colour = x)) + 
  scale_colour_gradientn(colours = c(l_blue, l_blue2,"Grey",l_green2,l_green),
                         values=c(0,0.45,0.55,1)  )+xlim(-1,1)+
  theme_minimal()+
  geom_segment(x=0.05,y=0, xend=0.05, yend=3.2,linetype="dashed", size=1.05)+
  geom_segment(x=-0.05,y=0, xend=-0.05, yend=3.2,linetype="dashed", size=1.05)+
  geom_point(data=data.quants_H,aes(x = quant, y= y_quants))+
  geom_label(label="10%",x=data.quants_H$quant[1]-0.05,y=data.quants_H$y_quants[1],label.size = 0.2,color = "black")+
  geom_label(label="20%",x=data.quants_H$quant[2]-0.05,y=data.quants_H$y_quants[2],label.size = 0.2,color = "black")+
  geom_label(label="30%",x=data.quants_H$quant[3]-0.05,y=data.quants_H$y_quants[3],label.size = 0.2,color = "black")+
  geom_label(label="40%",x=data.quants_H$quant[4]-0.05,y=data.quants_H$y_quants[4],label.size = 0.2,color = "black")+
  geom_label(label="50%",x=data.quants_H$quant[5]-0.05,y=data.quants_H$y_quants[5],label.size = 0.2,color = "black")+
  geom_label(label="60%",x=data.quants_H$quant[6]+0.05,y=data.quants_H$y_quants[6],label.size = 0.2,color = "black")+
  geom_label(label="70%",x=data.quants_H$quant[7]+0.05,y=data.quants_H$y_quants[7]+0.1,label.size = 0.2,color = "black")+
  geom_label(label="80%",x=data.quants_H$quant[8]+0.05,y=data.quants_H$y_quants[8],label.size = 0.2,color = "black")+
  geom_label(label="90%",x=data.quants_H$quant[9]+0.05,y=data.quants_H$y_quants[9],label.size = 0.2,color = "black")+
  labs(y="Densidad", x="Coeficiente profesor mujer", fill="")+
  theme(axis.text=element_text(size=3, family="serif"),
        legend.text = element_text(size=3,family="serif"),
        text=element_text(family="serif"))


