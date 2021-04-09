################################################################################
##                                                                            ##
##                       Causal trees plot for prof score                     ##
##                                                                            ##
################################################################################

node_main_prof<-Node$new("Variable dependiente: Puntaje del profesor")

A<-node_main_prof$AddChild("Edad profesor >= 59")
  R_A<-A$AddChild("(9%) \n Profesor mujer: 0.004")

B<-node_main_prof$AddChild("Edad profesor < 59")
  C<-B$AddChild("Antigüedad profesor >= 7.7")
    C1<-C$AddChild("Valor agregado < 0.19")
      R_C1<-C1$AddChild("(11%) \n Profesor mujer: 0.198***")
    C2<-C$AddChild("Valor agregado >= 0.19")
      D1<-C2$AddChild("N. estudiantes >= 46")
        R_D1<-D1$AddChild("(7%) \n Profesor mujer: 0.156***")
      D2<-C2$AddChild("N. estudiantes < 46")
        R_D2<-D2$AddChild("(5%) \n Profesor mujer: -0.067*")

  E<-B$AddChild("Antigüedad profesor < 7.7")
    E1<-E$AddChild("% alumnas >= 71%")
      R_E1<-E1$AddChild("(5%) \n Profesor mujer: 0.074") 
    
    E2<-E$AddChild("% alumnas < 71%")
      
      G<-E2$AddChild("Antigüedad profesor < 3.6")
        G1<-G$AddChild("N. Profesores > 1")
          R_G1<-G1$AddChild("(7%) \n Profesor mujer: -0.076**")
        G2<-G$AddChild("N. Profesores = 1")
          H<-G2$AddChild("% alumnas < 30%")
            R_H<-H$AddChild("(12%) \n Profesor mujer: -0.020")
          I<-G2$AddChild("% alumnas >= 30%")
            I1<-I$AddChild("Promedio acumulado >= 4.3")
              R_I1<-I1$AddChild("(4%) \n Profesor mujer: 0.022")
            I2<-I$AddChild("Promedio acumulado < 4.3")
            
              J<-I2$AddChild("N. estudiantes >= 50")
                R_J<-J$AddChild("(6%) \n Profesor mujer: -0.060")
              K<-I2$AddChild("N. estudiantes < 50")
                K1<-K$AddChild("Valor agregado >= 0.75")
                  R_K1<-K1$AddChild("(5%) \n Profesor mujer: -0.084***")
                K2<-K$AddChild("Valor agregado < 0.75")
                  
                  M<-K2$AddChild("% alumnas >= 39%")
                    
                    M2<-M$AddChild("Edad profesor < 35")
                      N<-M2$AddChild("N. estudiantes >= 23")
                        R_N<-N$AddChild("(5%) \n Profesor mujer: -0.079**")
                      O<-M2$AddChild("N. estudiantes < 23")
                        R_O<-O$AddChild("(4%) \n Profesor mujer: 0.064**")
                        M1<-M$AddChild("Edad profesor >= 35")
                        R_M1<-M1$AddChild("(4%) \n Profesor mujer: 0.068")
                        
                        L<-K2$AddChild("% alumnas < 39%")
                        R_L<-L$AddChild("(6%) \n Profesor mujer: -0.0004")
                        
                        F<-E2$AddChild("Antigüedad profesor >= 3.6")
                        F1<-F$AddChild("Valor agregado >= 0.34")
                        R_F1<-F1$AddChild("(6%) \n Profesor mujer: 0.087**") 
                        F2<-F$AddChild("Valor agregado < 0.34")
                        R_F2<-F2$AddChild("(4%) \n Profesor mujer: 0.011") 



l_green<-rgb(30,150,30, alpha = 140, max=255)
l_blue<-rgb(50,100,200, alpha = 140, max=255)

l_green2<-rgb(30,150,30, alpha = 80, max=255)
l_blue2<-rgb(50,100,200, alpha = 80, max=255)

## General outline
SetNodeStyle(node_main_prof, shape = "box", style="filled", fillcolor="transparent", fontcolor="black", inherit=FALSE, fontsize=20)
SetNodeStyle(node_main_prof$`Edad profesor < 59`, shape = "box", style="filled,rounded", fillcolor="transparent", fontcolor="black", inherit=T, fontsize=14)
SetNodeStyle(node_main_prof$`Edad profesor >= 59`, shape = "box", style="filled,rounded", fillcolor="transparent", fontcolor="black", inherit=T, fontsize=14)

plot(node_main_prof)



## Color green for women penalization
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor >= 7.7`$`Valor agregado < 0.19`$`(11%) 
 Profesor mujer: 0.198***`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor >= 7.7`$`Valor agregado >= 0.19`$`N. estudiantes < 46`$`(5%) 
 Profesor mujer: -0.067*`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor >= 7.7`$`Valor agregado >= 0.19`$`N. estudiantes >= 46`$`(7%) 
 Profesor mujer: 0.156***`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores > 1`$`(7%) 
 Profesor mujer: -0.076**`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas >= 30%`$`Promedio acumulado < 4.3`$`N. estudiantes < 50`$`Valor agregado >= 0.75`$`(5%) 
 Profesor mujer: -0.084***`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas >= 30%`$`Promedio acumulado < 4.3`$`N. estudiantes < 50`$`Valor agregado < 0.75`$`% alumnas >= 39%`$`Edad profesor < 35`$`N. estudiantes >= 23`$`(5%) 
 Profesor mujer: -0.079**`, shape = "box", fillcolor=l_green, penwidth = "3px")

## Color blue when evaluations favor women
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas >= 30%`$`Promedio acumulado < 4.3`$`N. estudiantes < 50`$`Valor agregado < 0.75`$`% alumnas >= 39%`$`Edad profesor < 35`$`N. estudiantes < 23`$`(4%) 
 Profesor mujer: 0.064**`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor >= 3.6`$`Valor agregado >= 0.34`$`(6%) 
 Profesor mujer: 0.087**`, shape = "box", fillcolor=l_blue, penwidth = "3px")


## Light green for non significant negative bias 
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas >= 30%`$`Promedio acumulado < 4.3`$`N. estudiantes >= 50`$`(6%) 
 Profesor mujer: -0.060`, shape = "box", fillcolor=l_green2, penwidth = "3px")


## Lighter blue for non significant positive bias
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas >= 71%`$`(5%) 
 Profesor mujer: 0.074`, shape = "box", fillcolor=l_blue2, penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas >= 30%`$`Promedio acumulado < 4.3`$`N. estudiantes < 50`$`Valor agregado < 0.75`$`% alumnas >= 39%`$`Edad profesor >= 35`$`(4%) 
 Profesor mujer: 0.068`, shape = "box", fillcolor=l_blue2, penwidth = "3px")

## Grey for coefficients really close to zero
SetNodeStyle(node_main_prof$`Edad profesor >= 59`$`(9%) 
 Profesor mujer: 0.004`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas < 30%`$`(12%) 
 Profesor mujer: -0.020`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas >= 30%`$`Promedio acumulado >= 4.3`$`(4%) 
 Profesor mujer: 0.022`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor >= 3.6`$`Valor agregado < 0.34`$`(4%) 
 Profesor mujer: 0.011`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_prof$`Edad profesor < 59`$`Antigüedad profesor < 7.7`$`% alumnas < 71%`$`Antigüedad profesor < 3.6`$`N. Profesores = 1`$`% alumnas >= 30%`$`Promedio acumulado < 4.3`$`N. estudiantes < 50`$`Valor agregado < 0.75`$`% alumnas < 39%`$`(6%) 
 Profesor mujer: -0.0004`, shape = "box", fillcolor="LightGrey", penwidth = "3px")


## Plot save
plot<-plot(node_main_prof)
plot
saveWidget(plot, paste0(git_figures,"/Causal Tree prof.html"))

webshot::webshot(url=paste0(git_figures,"/Causal Tree prof.html"), 
                 file=paste0(git_figures,"/Causal Tree prof.pdf"),
                 selector="#graph0", zoom = 0.75)


