################################################################################
##                                                                            ##
##                       Causal trees plot for course score                   ##
##                                                                            ##
################################################################################

node_main_curso<-Node$new("Variable dependiente: Puntaje del curso")

A1<-node_main_curso$AddChild("Edad profesor >= 59")
  R_A1<-A1$AddChild("(9%) \n Prof. M.: 0.052 \n Prof. M. X Est. M.: 0.008")

A2<-node_main_curso$AddChild("Edad profesor < 59")
  B<-A2$AddChild("Antigüedad >= 7.7")
    B1<-B$AddChild("Valor agregado >= -0.17")
      D1<-B1$AddChild("Nota < 4.1")
        R_D1<-D1$AddChild("(7%) \n Prof. M.: -0.063** \n Prof. M. X Est. M.: -0.035")
      D2<-B1$AddChild("Nota >= 4.1")
        E1<-D2$AddChild("Antigüedad >= 13")
          R_E1<-E1$AddChild("(5%) \n Prof. M.: 0.039 \n Prof. M. X Est. M.: -0.016")
        E2<-D2$AddChild("Antigüedad < 13")
          R_E2<-E2$AddChild("(4%) \n Prof. M.: -0.112*** \n Prof. M. X Est. M.: 0.061")
    B2<-B$AddChild("Valor agregado < -0.17")
      R_B2<-B2$AddChild("(9%) \n Prof. M.: -0.165*** \n Prof. M. X Est. M.: -0.015")
      
  C<-A2$AddChild("Antigüedad < 7.7")
    
    C2<-C$AddChild("% alumnas < 0.71")
      F<-C2$AddChild("% alumnas < 0.36")
        F1<-F$AddChild("N. estudiantes >= 50")
          R_F1<-F1$AddChild("(5%) \n Prof. M.: 0.053 \n Prof. M. X Est. M.: 0.031")
        F2<-F$AddChild("N. estudiantes < 50")
          O1<-F2$AddChild("Nota < 3.4")
            R_O1<-O1$AddChild("(5%) \n Prof. M.: 0.039 \n Prof. M. X Est. M.: -0.016")
          O2<-F2$AddChild("Nota >= 3.4")
            P1<-O2$AddChild("Valor agregado < -0.076")
              R_P1<-P1$AddChild("(7%) \n Prof. M.: -0.004 \n Prof. M. X Est. M.: 0.010")
            P2<-O2$AddChild("Valor agregado >= -0.076")
              R_P2<-P2$AddChild("(5%) \n Prof. M.: 0.056*** \n Prof. M. X Est. M.: 0.014")
              
              C1<-C$AddChild("% alumnas >= 0.71")
              N1<-C1$AddChild("Edad profesor < 36")
              R_N1<-N1$AddChild("(2%) \n Prof. M.: 0.105** \n Prof. M. X Est. M.: -0.001")
              N2<-C1$AddChild("Edad profesor >= 36")
              R_N2<-N2$AddChild("(3%) \n Prof. M.: 0.103** \n Prof. M. X Est. M.: -0.055")
            
                      
      G<-C2$AddChild("% alumnas >= 0.36")
        G1<-G$AddChild("Edad profesor < 27")
          H1<-G1$AddChild("N. estudiantes < 22")
            R_H1<-H1$AddChild("(3%) \n Prof. M.: -0.005 \n Prof. M. X Est. M.: 0.060")
          H2<-G1$AddChild("N. estudiantes >= 22")
            R_H2<-H2$AddChild("(6%) \n Prof. M.: -0.040 \n Prof. M. X Est. M.: -0.035")
        G2<-G$AddChild("Edad profesor >= 27")
      
          J<-G2$AddChild("Edad profesor < 41")
            J1<-J$AddChild("N. estudiantes >= 47")
              K1<-J1$AddChild("Valor agregado < 0.2")
                R_K1<-K1$AddChild("(5%) \n Prof. M.: -0.104** \n Prof. M. X Est. M.: 0.033")
              K2<-J1$AddChild("Valor agregado >= 0.2")
                R_K2<-K2$AddChild("(4%) \n Prof. M.: -0.024 \n Prof. M. X Est. M.: -0.001")
            J2<-J$AddChild("N. estudiantes < 47")
              L1<-J2$AddChild("% alumnas < 0.46")
                R_K1<-L1$AddChild("(4%) \n Prof. M.: 0.098*** \n Prof. M. X Est. M.: -0.0002")
              L2<-J2$AddChild("% alumnas >= 0.46")
                M1<-L2$AddChild("% alumnas < 0.55")
                  R_M1<-M1$AddChild("(4%) \n Prof. M.: 0.037 \n Prof. M. X Est. M.: 0.007")
                M2<-L2$AddChild("% alumnas >= 0.55")
                  R_M2<-M2$AddChild("(5%) \n Prof. M.: 0.042 \n Prof. M. X Est. M.: 0.004")
                
          I<-G2$AddChild("Edad profesor >= 41")
            I1<-I$AddChild("Edad profesor < 44")
              R_I1<-I1$AddChild("(3%) \n Prof. M.: -0.109*** \n Prof. M. X Est. M.: 0.017")
            I2<-I$AddChild("Edad profesor >= 44")
              N1<-I2$AddChild("Valor agregado >= 0.3")
                R_N1<-N1$AddChild("(3%) \n Prof. M.: -0.033 \n Prof. M. X Est. M.: 0.045")
              N2<-I2$AddChild("Valor agregado < 0.3")
                R_N2<-N2$AddChild("(3%) \n Prof. M.: 0.086** \n Prof. M. X Est. M.: 0.019")
      

l_green<-rgb(30,150,30, alpha = 140, max=255)
l_blue<-rgb(50,100,200, alpha = 140, max=255)

l_green2<-rgb(30,150,30, alpha = 80, max=255)
l_blue2<-rgb(50,100,200, alpha = 80, max=255)

## General outline
SetNodeStyle(node_main_curso, shape = "box", style="filled", fillcolor="transparent", fontcolor="black", inherit=FALSE, fontsize=20)
SetNodeStyle(node_main_curso$`Edad profesor < 59`, shape = "box", style="filled,rounded", fillcolor="transparent", fontcolor="black", inherit=T, fontsize=14)
SetNodeStyle(node_main_curso$`Edad profesor >= 59`, shape = "box", style="filled,rounded", fillcolor="transparent", fontcolor="black", inherit=T, fontsize=14)

plot(node_main_curso)


## Color blue for women penalization
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad >= 7.7`$`Valor agregado >= -0.17`$`Nota < 4.1`$`(7%) 
 Prof. M.: -0.063** 
 Prof. M. X Est. M.: -0.035`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad >= 7.7`$`Valor agregado >= -0.17`$`Nota >= 4.1`$`Antigüedad < 13`$`(4%) 
 Prof. M.: -0.112*** 
 Prof. M. X Est. M.: 0.061`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad >= 7.7`$`Valor agregado < -0.17`$`(9%) 
 Prof. M.: -0.165*** 
 Prof. M. X Est. M.: -0.015`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor >= 41`$`Edad profesor < 44`$`(3%) 
 Prof. M.: -0.109*** 
 Prof. M. X Est. M.: 0.017`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor < 41`$`N. estudiantes >= 47`$`Valor agregado < 0.2`$`(5%) 
 Prof. M.: -0.104** 
 Prof. M. X Est. M.: 0.033`, shape = "box", fillcolor=l_blue, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor < 27`$`N. estudiantes >= 22`$`(6%) 
 Prof. M.: -0.040 
 Prof. M. X Est. M.: -0.035`, shape = "box", fillcolor=l_blue, penwidth = "3px")

## Color green when evaluations favor women
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas >= 0.71`$`Edad profesor < 36`$`(2%) 
 Prof. M.: 0.105** 
 Prof. M. X Est. M.: -0.001`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas >= 0.71`$`Edad profesor >= 36`$`(3%) 
 Prof. M.: 0.103** 
 Prof. M. X Est. M.: -0.055`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor >= 41`$`Edad profesor >= 44`$`Valor agregado < 0.3`$`(3%) 
 Prof. M.: 0.086** 
 Prof. M. X Est. M.: 0.019`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor < 41`$`N. estudiantes < 47`$`% alumnas < 0.46`$`(4%) 
 Prof. M.: 0.098*** 
 Prof. M. X Est. M.: -0.0002`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas < 0.36`$`N. estudiantes < 50`$`Nota >= 3.4`$`Valor agregado >= -0.076`$`(5%) 
 Prof. M.: 0.056*** 
 Prof. M. X Est. M.: 0.014`, shape = "box", fillcolor=l_green, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas < 0.36`$`N. estudiantes >= 50`$`(5%) 
 Prof. M.: 0.053 
 Prof. M. X Est. M.: 0.031`, shape = "box", fillcolor=l_green, penwidth = "3px")

## Light blue for non significant negative bias 
  ## Not in this plot

## Lighter green for non significant positive bias
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor < 41`$`N. estudiantes < 47`$`% alumnas >= 0.46`$`% alumnas >= 0.55`$`(5%) 
 Prof. M.: 0.042 
 Prof. M. X Est. M.: 0.004`, shape = "box", fillcolor=l_green2, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor < 41`$`N. estudiantes < 47`$`% alumnas >= 0.46`$`% alumnas < 0.55`$`(4%) 
 Prof. M.: 0.037 
 Prof. M. X Est. M.: 0.007`, shape = "box", fillcolor=l_green2, penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor >= 59`$`(9%) 
 Prof. M.: 0.052 
 Prof. M. X Est. M.: 0.008`, shape = "box", fillcolor=l_green2, penwidth = "3px")

## Grey for coefficients really close to zero
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad >= 7.7`$`Valor agregado >= -0.17`$`Nota >= 4.1`$`Antigüedad >= 13`$`(5%) 
 Prof. M.: 0.039 
 Prof. M. X Est. M.: -0.016`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas < 0.36`$`N. estudiantes < 50`$`Nota < 3.4`$`(5%) 
 Prof. M.: 0.039 
 Prof. M. X Est. M.: -0.016`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas < 0.36`$`N. estudiantes < 50`$`Nota >= 3.4`$`Valor agregado < -0.076`$`(7%) 
 Prof. M.: -0.004 
 Prof. M. X Est. M.: 0.010`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor < 27`$`N. estudiantes < 22`$`(3%) 
 Prof. M.: -0.005 
 Prof. M. X Est. M.: 0.060`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor < 41`$`N. estudiantes >= 47`$`Valor agregado >= 0.2`$`(4%) 
 Prof. M.: -0.024 
 Prof. M. X Est. M.: -0.001`, shape = "box", fillcolor="LightGrey", penwidth = "3px")
SetNodeStyle(node_main_curso$`Edad profesor < 59`$`Antigüedad < 7.7`$`% alumnas < 0.71`$`% alumnas >= 0.36`$`Edad profesor >= 27`$`Edad profesor >= 41`$`Edad profesor >= 44`$`Valor agregado >= 0.3`$`(3%) 
 Prof. M.: -0.033 
 Prof. M. X Est. M.: 0.045`, shape = "box", fillcolor="LightGrey", penwidth = "3px")

plot(node_main_curso)

export_graph(ToDiagrammeRGraph(node_main), paste0(git_figures,"/Causal Tree curso.pdf"), file_type = "pdf")