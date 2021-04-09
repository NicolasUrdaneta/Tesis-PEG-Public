library(DiagrammeR)
# Create a node data frame
grViz('
  digraph flow {

       # set characteristics 
       node[shape=rectangle, 
            color=skyblue, 
            penwidth=2,
            fillcolor=lavender, 
            style=filled,
            fontsize=15, 
            fontcolor=grey46,
            fontname = "helvetica" # this may be OS dependent
            ];
       edge[minlen=2, 
            color=grey,
            penwidth=2
            ];
       nodesep=0.1; #hack
       
       A[label="Variable dependiente: Recomienda a su profesor/a"]
        B[label="% alumnas < 0.23"]
          R_AB[label="(10%) \n Prf. Mujer: \n Prf. Muj. x Est. Muj:"]
          
        C[label="% alumnas >= 0.23"]
          D[label="% alumnas >= 0.63"]
            D1[label="% alumnas >= 0.36"]
              F[label="Edad profesor >= 52"]
                R_F[label="(4%) \n Prf. Mujer: 0.000 \n Prf. Muj. x Est. Muj: -0.024"]
              G[label="Edad profesor < 52"]
                G1[label="% alumnas >= 0.76"]
                  R_G1[label="(4%) \n Prf. Mujer: 0.035* \n Prf. Muj. x Est. Muj: 0.012"]
                G2[label="% alumnas < 0.76"]
                  R_G2[label="(9%) \n Prf. Mujer: 0.027 \n Prf. Muj. x Est. Muj: -0.014"]
            
            D2[label="% alumnas >= 0.36"]
              H[label="N. estudiantes >= 38"]
                H1[label="Antigüedad >= 15"]
                  R_H1[label="(6%) \n Prf. Mujer: 0.017 \n Prf. Muj. x Est. Muj: -0.002"]
                H2[label="Antigüedad < 15"]
                  J[label="Créditos del curso < 3"]
                    R_J[label="(4%) \n Prf. Mujer: -0.127*** \n Prf. Muj. x Est. Muj:0.020"]
                  
                  K[label="Créditos del curso >= 3"]
                    K1[label="Valor agregado >= -0.36"]
                      L1[label="Antigüedad >= 7.7"]
                        R_L1[label="(5%)"]
                      L2[label="Antigüedad >= 7.7"]
                        M1[label="Antigüedad >= 0.84"]
                          R_M1[label="(6%)"]
                        M2[label="Antigüedad < 0.84"]
                          R_M2[label="(6%)"]
                    K2[label="Valor agregado < -0.36"]
                      R_K2[label="(8%)"]
                
              I[label="N. estudiantes < 38"]
                I1[label="% alumnas >= 0.49"]
                  N1[label="Antigüedad >= 0.29"]
                    R_N1[label="(4%)"]
                  N2[label="Antigüedad < 0.29"]
                    R_N2[label="(6%) \n Prf. Mujer: 0.002 \n Prf. Muj. x Est. Muj: -0.023"]
                I2[label="% alumnas < 0.49"]
                  O1[label="N. estudiantes < 23"]
                    R_O1[label="0.004 (5%) \n Prf. Mujer: 0.010 \n Prf. Muj. x Est. Muj: -0.015"]
                  O2[label="N. estudiantes >= 23"]
                    R_O2[label="(6%) \n Prf. Mujer: -0.020 \n Prf. Muj. x Est. Muj: 0.002"]
              
          E[label="% alumnas < 0.63"]
            E1[label="Tiempo respuesta < 3.6 mins"]
              R_E1[label="(8%) \n Prf. Mujer: 0.009 \n Prf. Muj. x Est. Muj: 0.001"]
            E2[label="Tiempo respuesta >= 3.6 mins"]
              R_E2[label="(11%) \n Prf. Mujer: 0.004 \n Prf. Muj. x Est. Muj: -0.005"];
       
       A->C
        C->D
           D->D1
              D1->F->R_F
              D1->G->G1->R_G1
                  G->G2->R_G2
           D->D2
              D2->H->H1->R_H1
                  H->H2->J->R_J
                     H2->K->K1->L1->R_L1
                            K1->L2->M1->R_M1
                                L2->M2->R_M2
                         K->K2->R_K2
              D2->I->I1->N1->R_N1
                     I1->N2->R_N2
                  I->I2->O1->R_O1
                     I2->O2->R_O2
        C->E
           E->E1->R_E1
           E->E2->R_E2
       
       A->B->R_AB
       
       
       # Graph
       {rank=same ;B  C };
       {rank=same ;D  E  };
       {rank=same ;D1 D2 E1 E2  }
       {rank=same ;F G H I };
       {rank=same ;G1 G2 H1 H2 I1 I2};
       {rank=same ;J K N1 N2 O1 O2 };
       {rank=same ;L1 L2 };
       {rank=same ;M1 M2 };
       {rank=same ;R_AB R_E2 R_G1 R_H1 R_K2 R_M1 R_N1 R_O1};
       {rank=same ; R_E1 R_F R_G2 R_J R_L1 R_M2 R_N2 R_O2};
  }

')

