################################################################################
##                                                                            ##
##                          Organize Lasso results                            ##
##                                                                            ##
################################################################################
##                                                                            ##
##                                                                            ##
################################################################################


#lasso.root<-LoadRds("Lasso_root_results",paste0(output.dir,"/Regs/Lasso"))
lasso.coefs<-LoadRds("Coefs_lasso", paste0(output.dir,"/Regs/Lasso/Feb 2021"))
lasso.coefs.res<-LoadRds("Coefs_lasso_res", paste0(output.dir,"/Regs/Lasso/Feb 2021"))


lasso.coefs$word<-rownames(lasso.coefs)
lasso.coefs<-lasso.coefs %>% 
  filter(word!="(Intercept)") %>% 
  dplyr::select(-word)☻
colnames(lasso.coefs)[1]<-"coef"


lasso.coefs.res$word<-rownames(lasso.coefs.res)
lasso.coefs.res<-lasso.coefs.res %>% 
  filter(word!="(Intercept)" & substr(lasso.coefs.res$word,1,15)!="facultad_curso_") %>% 
  dplyr::select(-word)
colnames(lasso.coefs.res)[1]<-"coef"

size<-25

## Female predictors
female.pred1<-order(lasso.coefs.res$coef, decreasing = T)[1:size]
female.words1<-rownames(lasso.coefs.res)[female.pred1]  
female.coefs1<-round(lasso.coefs.res[female.pred1,"coef"], digits = 3)

female.pred2<-order(lasso.coefs.res$coef, decreasing = T)[(size+1):(size*2)]
female.words2<-rownames(lasso.coefs.res)[female.pred2]  
female.coefs2<-round(lasso.coefs.res[female.pred2,"coef"], digits = 3)


## Male predictors
male.pred1<-order(lasso.coefs.res$coef, decreasing = F)[1:size]
male.words1<- rownames(lasso.coefs.res)[male.pred1] 
male.coefs1<-round(lasso.coefs.res[male.pred1,"coef"], digits = 3)

male.pred2<-order(lasso.coefs.res$coef, decreasing = F)[(size+1):(size*2)]
male.words2<- rownames(lasso.coefs.res)[male.pred2] 
male.words2[male.words2=="hi"]<-"him"
male.words2[male.words2=="materno"]<-"materno-infantil"
male.words2[male.words2=="chinar"]<-"rhino"
male.coefs2<-round(lasso.coefs.res[male.pred2,"coef"], digits = 3)

## Dataset:

predictors<-data.frame(Palabra1_1=female.words1,Coefs1_1=female.coefs1,
                       Palabra1_2=female.words2,Coefs1_2=female.coefs2,
                       space = NA,
                       Palabra2_1=male.words1,Coefs2_1=male.coefs1,
                       Palabra2_2=male.words2,Coefs2_2=male.coefs2)

table_raw<-stargazer(predictors, type="latex", summary=F, digits=3,
                     table.layout = "t",out.header=F, header=F)

idx0 <- grep("begin{tabular}", table_raw, fixed = T)[1] # Start of \begin{tabular}
idx1 <- grep("end{tabular}", table_raw, fixed = T)[1] # End of \begin{tabular}

tex_header <- c(table_raw[idx0], "\\toprule \\toprule")
tex_footer <- c("\\bottomrule \\bottomrule", table_raw[idx1])

# Remove [-1.8ex] and get the inside of the tabular
tex_inner <- gsub("\\\\[-[\\.0-9]+ex]", "", table_raw[(idx0+1):(idx1-1)])
tex_inner <-c(" & \\multicolumn{4}{c}{Mujeres} & & \\multicolumn{4}{c}{Hombres} \\\\ \\cline{2-5} \\cline{7-10} ",tex_inner[5:29])


latex_output <- c(tex_header,tex_inner,tex_footer)

# Write to file
write(latex_output,paste0(git_tables, "/Lasso words table unadj.tex"))



## Number of coefs different from 0:
lasso.coefs.res %>% filter(coef!=0)  %>% dim()
lasso.coefs %>% filter(coef!=0)  %>% dim()


# Word cloud --------------------------------------------------------------

library(wordcloud)
library(wordcloud2)
#webshot::install_phantomjs()
library(webshot)
library(htmlwidgets)

lasso.coefs.res$word<-rownames(lasso.coefs.res)
lasso.coefs.res[lasso.coefs.res$word=="hi",]$word<-"him"
lasso.coefs.res[lasso.coefs.res$word=="materno",]$word<-"materno-infantil"
lasso.coefs.res[lasso.coefs.res$word=="chinar",]$word<-"rhino"

lasso.coefs.mujer<-lasso.coefs.res %>% filter(coef>0) %>% 
  mutate(freq = round(coef*1000,digits=1)) %>% 
  dplyr::select(-coef)%>% 
  filter(freq>12.8)
lasso.coefs.hombre<-lasso.coefs.res %>% filter(coef<0)%>% 
  mutate(freq = -round(coef*1000,digits=1)) %>% 
  dplyr::select(-coef) %>% 
  filter(freq>=11.05)


color_pa_gr <- RColorBrewer::brewer.pal(5, "Greens")[3:5]
color_pa_bl <- RColorBrewer::brewer.pal(5, "Blues")[3:5]


set.seed(123)
lasso_words_hombre<-wordcloud2(data=lasso.coefs.hombre, size=1.4, 
           color=rep_len( color_pa_gr, nrow(lasso.coefs.hombre)),
           fontFamily = 'Bookman', shape="circle", gridSize=0)

set.seed(1234)
lasso_words_mujer<-wordcloud2(data=lasso.coefs.mujer, size=0.8, 
           color=rep_len( color_pa_bl, nrow(lasso.coefs.mujer)),
           fontFamily = 'Bookman', shape="circle", gridSize=0)

htmlwidgets::saveWidget(lasso_words_hombre,"1.html",selfcontained = F)
webshot("1.html", paste0(git_figures,"/Lasso hombres.pdf"), delay = 60,
        vwidth = 1200, vheight=1000)

htmlwidgets::saveWidget(lasso_words_mujer,"2.html",selfcontained = F)
webshot("2.html", paste0(git_figures,"/Lasso mujer.pdf"), delay = 60,
        vwidth = 1200, vheight=1000)
