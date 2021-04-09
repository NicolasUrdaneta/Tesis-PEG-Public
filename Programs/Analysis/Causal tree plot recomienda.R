################################################################################
##                                                                            ##
##                       Causal trees plot for recomendation                  ##
##                                                                            ##
################################################################################

node_main<-Node$new("Variable dependiente: Recomienda a su profesor/a")

A<-node_main$AddChild("% alumnas >= 0.71")
  A1<-A$AddChild("Edad profesor >= 45")
    R_A1<-A1$AddChild("(3%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
  A2<-A$AddChild("Edad profesor <= 45")
    R_A1<-A2$AddChild("(5%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
  
B<-node_main$AddChild("% alumnas < 0.71")
  D<-B$AddChild("% alumnas < 0.20")
    R_D<-D$AddChild("(8%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
  C<-B$AddChild("% alumnas >= 0.20")
    C1<-C$AddChild("N. estudiantes >= 62")
      D<-C1$AddChild("Edad profesor >= 35")
        D1<-D$AddChild("Valor agregado < -0.39")
          R_D1<-D1$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
        D2<-D$AddChild("Valor agregado >= -0.39")
          H1<-D2$AddChild("N. estudiantes < 78")
            R_H1<-H1$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
          H2<-D2$AddChild("N. estudiantes >= 78")
            R_H2<-H2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
      E<-C1$AddChild("Edad profesor < 35")
        R_E<-N2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
        
    C2<-C$AddChild("N. estudiantes < 62")
      F<-C2$AddChild("Edad profesor < 34")
        F1<-F$AddChild("Valor agregado >= 0.74")
          R_F1<-F1$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
        F2<-F$AddChild("Valor agregado < 0.74")
          I1<-F2$AddChild("% alumnas < 0.53")
            K1<-I1$AddChild("Edad estudiante < 19")
              R_K1<-K1$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
            K2<-I1$AddChild("Edad estudiante >= 19")
              R_K2<-K2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
          I2<-F2$AddChild("% alumnas >= 0.53")
            R_I2<-I2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
      G<-C2$AddChild("Edad profesor >= 34")
        G1<-G$AddChild("Créditos curso >= 3")
          J1<-G1$AddChild("Antigüedad >= 13")
            R_J1<-J1$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
          J2<-G1$AddChild("Antigüedad < 13")
            L1<-J2$AddChild("Valor agregado >= -0.57")
              M1<-L1$AddChild("N. estudiantes < 43")
                N1<-M1$AddChild("Antigüedad < 1.2")
                  O1<-N1$AddChild("N. estudiantes >= 24")
                    R_O1<-O1$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
                  O2<-N1$AddChild("N. estudiantes < 24")
                    R_O2<-O2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
                N2<-M1$AddChild("Antigüedad >= 1.2")
                  R_N2<-N2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
              M2<-L1$AddChild("N. estudiantes >= 43")
                R_M2<-M2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
            L2<-J2$AddChild("Valor agregado < -0.57")
             R_L2<-L2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
        G2<-G$AddChild("Créditos curso < 3")
          R_G2<-G2$AddChild("(%) \n Prof. M.:  \n Prof. M. X Est. M.: ")
      


l_green<-rgb(30,150,30, alpha = 140, max=255)
l_blue<-rgb(50,100,200, alpha = 140, max=255)

l_green2<-rgb(30,150,30, alpha = 80, max=255)
l_blue2<-rgb(50,100,200, alpha = 80, max=255)

## General outline
SetNodeStyle(node_main, shape = "box", style="filled", fillcolor="transparent", fontcolor="black", inherit=FALSE, fontsize=20)
SetNodeStyle(node_main$`% alumnas >= 0.71`, shape = "box", style="filled,rounded", fillcolor="transparent", fontcolor="black", inherit=T, fontsize=14)
SetNodeStyle(node_main$`% alumnas < 0.71`, shape = "box", style="filled,rounded", fillcolor="transparent", fontcolor="black", inherit=T, fontsize=14)

plot(node_main)


## Color blue for women penalization
SetNodeStyle(node_main$`% alumnas < 0.23`$`(10%) 
 Prof. M.: -0.049*** 
 Prof. M. X Est. M.: -0.010`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes >= 38`$`Antigüedad < 15`$`Créditos del curso < 3`$`(4%) 
 Prof. M. : -0.127*** 
 Prof. M. X Est. M.: 0.020`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes >= 38`$`Antigüedad < 15`$`Créditos del curso >= 3`$`Valor agregado < -0.36`$`(8%) 
 Prof. M.: -0.039** 
 Prof. M. X Est. M.: -0.025*`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes >= 38`$`Antigüedad < 15`$`Créditos del curso >= 3`$`Valor agregado >= -0.36`$`Antigüedad >= 7.7`$`(5%) 
 Prof. M.: -0.050** 
 Prof. M. X Est. M.: -0.011`,shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes < 38`$`% alumnas >= 0.49`$`Antigüedad < 0.29`$`(6%) 
 Prof. M.: 0.002 
 Prof. M. X Est. M.: -0.023**`, shape = "box", fillcolor=l_blue, penwidth = "3px")

## Color green when evaluations favor women
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes < 38`$`% alumnas >= 0.49`$`Antigüedad >= 0.29`$`(4%) 
 Prof. M.: 0.027* 
 Prof. M. X Est. M.: -0.014`, shape = "box", fillcolor=l_green, penwidth = "3px")

SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas >= 0.63`$`Edad profesor < 52`$`% alumnas >= 0.76`$`(4%) 
 Prof. M.: 0.035* 
 Prof. M. X Est. M.: 0.012`, shape = "box", fillcolor=l_green, penwidth = "3px")

## Light blue for non significant negative bias 
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes >= 38`$`Antigüedad < 15`$`Créditos del curso >= 3`$`Valor agregado >= -0.36`$`Antigüedad < 7.7`$`Antigüedad < 0.84`$`(6%) 
 Prof. M.: -0.017 
 Prof. M. X Est. M.: -0.011`, shape = "box", fillcolor=l_blue2, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes < 38`$`% alumnas < 0.49`$`N. Estudiantes >= 23`$`(6%) 
 Prof. M.: -0.020 
 Prof. M. X Est. M.: 0.002`, shape = "box", fillcolor=l_blue2, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas >= 0.63`$`Edad profesor >= 52`$`(4%) 
 Prof. M.: 0.000 
 Prof. M. X Est. M.: -0.024`, shape = "box", fillcolor=l_blue2, penwidth = "3px")

## Lighter green for non significant positive bias
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas < 0.36`$`Tiempo respuesta < 3.6 mins`$`(10%) 
 Prof. M.: 0.009 
 Prof. M. X Est. M.: 0.001`, shape = "box", fillcolor=l_green2, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas >= 0.63`$`Edad profesor < 52`$`% alumnas < 0.76`$`(9%) 
 Prof. M.: 0.027 
 Prof. M. X Est. M.: -0.014`, shape = "box", fillcolor=l_green2, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes >= 38`$`Antigüedad >= 15`$`0.017 (6%) 
 Prof. M.: 0.017 
 Prof. M. X Est. M.: -0.002`, shape = "box", fillcolor=l_green2, penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes >= 38`$`Antigüedad < 15`$`Créditos del curso >= 3`$`Valor agregado >= -0.36`$`Antigüedad < 7.7`$`Antigüedad >= 0.84`$`(6%) 
 Prof. M.: 0.012 
 Prof. M. X Est. M.: -0.003`, shape = "box", fillcolor=l_green2, penwidth = "3px")

## Grey for coefficients really close to zero
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas < 0.36`$`Tiempo respuesta >= 3.6 mins`$`(7%) 
 Prof. M.: 0.004 
 Prof. M. X Est. M.: -0.005`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main$`% alumnas >= 0.23`$`% alumnas < 0.63`$`% alumnas >= 0.36`$`N. Estudiantes < 38`$`% alumnas < 0.49`$`N. Estudiantes < 23`$`(5%) 
 Prof. M.: 0.010 
 Prof. M. X Est. M.: -0.015`, shape = "box", fillcolor="LightGrey", penwidth = "3px")



## Plot
plot(node_main)

export_graph(ToDiagrammeRGraph(node_main), paste0(git_figures,"/Causal Tree recomienda.png"), file_type = "png")
webshot::webshot(url="https://rpubs.com/Nurdnaneta/725351", file=paste0(git_figures,"/Causal Tree recomienda.pdf"), cliprect="viewport")
