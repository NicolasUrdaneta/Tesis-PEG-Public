********************************************************************************
**																			  **
**							Main regressions table							  **
**																			  **
********************************************************************************
**																			  **
**		This program estimates the main regressions with scores				  **
**																			  **
********************************************************************************

clear all

cap log close

log using "Logs/Main regressions `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

encode idestudiante, gen(id_est)

bys idcursoseccion: egen sexo_prof_sd_curso = sd(sexo_prof)
bys idestudiante: egen sexo_prof_sd_est = sd(sexo_prof)


*	____________________________________________________________________________
*
*							 1. Main results table
*	____________________________________________________________________________

///


foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{

	*local yvar  score_sem_prof
	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 , baselevels absorb(idestudiante) vce(cluster i.id_curso) 

	estimates store reg_`yvar'
	estadd ysumm
	if "`yvar'"=="score_prof" | "`yvar'"=="score_curso" {
		estadd scalar ymean = 0, replace 
	}

	estadd local FEest "Sí", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace

	*local yvar  score_sem_prof
	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 & (inrange(categ_prof2,6,8) | categ_prof2==4), baselevels absorb(idestudiante) vce(cluster i.id_curso) 

	estimates store reg_`yvar'_rest
	estadd ysumm
	if "`yvar'"=="score_prof" | "`yvar'"=="score_curso" {
		estadd scalar ymean = 0, replace 
	}

	estadd local FEest "Sí", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace



}  

///

#delimit ;
estout reg_score_sem_prof reg_score_sem_curso reg_score_8 reg_positive reg_negative using "${git_documento}/Tablas/main_regs.tex",  
	style(tex) label
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N r2 ymean betas_sum,
		labels("E.F. estudiante" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) 
	keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor) varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) ) 
	numbers(\multicolumn{@span}{c}{( )}) 
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{2cm}}}"
		"\toprule \toprule"
		"& \multirow{2}{2cm}{\centering{Puntaje del profesor}} & \multirow{2}{2cm}{\centering{Puntaje del curso}} &\multirow{2}{2cm}{\centering{Recomienda al profesor}} &\multirow{2}{2cm}{\centering{Comentario positivo}} &\multirow{2}{2cm}{\centering{Comentario negativo}} \\ [5mm]")
	posthead("\cline{2-@span}")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en el semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando, nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre. Observaciones varían por columna debido a que no todos los estudiantes deben responder las mismas preguntas y no todos deciden comentar." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr

// Two panel version

#delimit ;
estout reg_score_sem_prof reg_score_sem_curso reg_score_8 reg_positive reg_negative using "${git_documento}/Tablas/main_regsA.tex",  
	style(tex) label
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N r2 ymean betas_sum,
		labels("E.F. estudiante" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) 
	keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor) varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) ) 
	numbers(\multicolumn{@span}{c}{( )}) 
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{2cm}}}"
		"\toprule \toprule"
		"& \multirow{2}{2cm}{\centering{Puntaje del profesor}} & \multirow{2}{2cm}{\centering{Puntaje del curso}} &\multirow{2}{2cm}{\centering{Recomienda al profesor}} &\multirow{2}{2cm}{\centering{Comentario positivo}} &\multirow{2}{2cm}{\centering{Comentario negativo}} \\ [5mm]")
	posthead("\cline{2-@span}"
		"& \multicolumn{@E}{c}{Panel A: Muestra completa} \\")
	postfoot("\toprule");
#delimit cr

#delimit ;
estout reg_score_sem_prof_rest reg_score_sem_curso_rest reg_score_8_rest reg_positive_rest reg_negative_rest using "${git_documento}/Tablas/main_regsB.tex",  
	style(tex) label
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N r2 ymean betas_sum,
		labels("E.F. estudiante" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) 
	keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor) varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) ) 
	numbers(\multicolumn{@span}{c}{( )}) 
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("& \multicolumn{@E}{c}{Panel B: Profesores de planta} \\")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en el semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando, nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre. Observaciones varían por columna debido a que no todos los estudiantes deben responder las mismas preguntas y no todos deciden comentar." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr


*	____________________________________________________________________________
*
*						2. Main results table only medicina
*	____________________________________________________________________________

	reghdfe score_sem_prof i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof ///	
	if common_sample==1  & facultad_est==12, baselevels noabsorb 	///
	vce(cluster i.id_curso) 

foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{

	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1  & facultad_est==12, baselevels absorb(idestudiante) 	///
	vce(cluster i.id_curso) 

	estimates store reg_`yvar'_med
	estadd ysumm
	estadd local FEest "Sí", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace

}  

#delimit ;
estout reg_score_sem_prof_med reg_score_sem_curso_med reg_score_8_med reg_positive_med reg_negative_med using "${git_documento}/Tablas/main_regs_medicina.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N r2 ymean betas_sum,
		labels("E.F. estudiante" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title("Diferencias en las evaluaciones por género del profesor \\ Solamente estudiantes de medicina") varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Profesor mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	numbers(\multicolumn{@span}{c}{( )})
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{2cm}}}"
		"\toprule \toprule"
		"& \multirow{2}{2cm}{\centering{Puntaje del profesor}} & \multirow{2}{2cm}{\centering{Puntaje del curso}} &\multirow{2}{2cm}{\centering{Recomienda al profesor}} &\multirow{2}{2cm}{\centering{Comentario positivo}} &\multirow{2}{2cm}{\centering{Comentario negativo}} \\ [5mm]")
	posthead("\cline{2-@span}")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en scriptsize semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando y nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre. Observaciones varían por columna debido a que no todos los estudiantes deben responder las mismas preguntas y no todos deciden comentar." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr


*	____________________________________________________________________________
*
*					 4. Robustez a interés de responder la encuesta
*	____________________________________________________________________________


foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{

	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 & sd_id_score!=0, baselevels absorb(idestudiante) 		///
	vce(cluster i.id_curso) 

	estimates store reg_`yvar'_nosis
	estadd ysumm
	estadd local FEest "Sí", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace

}  

foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{

	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 & Nombre!=., baselevels absorb(idestudiante) 	///
	vce(cluster i.id_curso) 

	estimates store reg_`yvar'_sicom
	estadd ysumm
	if "`yvar'"=="score_prof" | "`yvar'"=="score_curso" {
		estadd scalar ymean = 0, replace 
	}
	estadd local FEest "Sí", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace

}  


#delimit ;
estout reg_score_sem_prof reg_score_sem_curso reg_score_8 reg_positive reg_negative using "${git_documento}/Tablas/regs_interes_1.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N r2 ymean betas_sum,
		labels("E.F. estudiante" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor) varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	numbers(\multicolumn{@span}{c}{( )})
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{2cm}}}"
		"\toprule \toprule"
		"& \multirow{2}{2cm}{\centering{Puntaje del profesor}} & \multirow{2}{2cm}{\centering{Puntaje del curso}} &\multirow{2}{2cm}{\centering{Recomienda al profesor}} &\multirow{2}{2cm}{\centering{Comentario positivo}} &\multirow{2}{2cm}{\centering{Comentario negativo}} \\ [5mm]")
	posthead("\cline{2-@span}"
		"& \multicolumn{@E}{c}{Panel A: Muestra completa} \\")
	postfoot("\toprule");
#delimit cr

#delimit ;
estout reg_score_sem_prof_sicom reg_score_sem_curso_sicom reg_score_8_sicom reg_positive_sicom reg_negative_sicom using "${git_documento}/Tablas/regs_interes_2.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N r2 ymean betas_sum,
		labels("E.F. estudiante" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor)
	varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("& \multicolumn{@E}{c}{Panel B: Estudiantes que también comentan} \\")
	postfoot("\toprule");
#delimit cr

#delimit ;
estout reg_score_sem_prof_nosis reg_score_sem_curso_nosis reg_score_8_nosis reg_positive_nosis reg_negative_nosis using "${git_documento}/Tablas/regs_interes_3.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N r2 ymean betas_sum,
		labels("E.F. estudiante" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor)
	varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("& \multicolumn{@E}{c}{Panel C: Evaluaciones sin respuestas sistemáticas} \\")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en el semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando y nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre. Observaciones varían por columna debido a que no todos los estudiantes deben responder las mismas preguntas y no todos deciden comentar." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr


*	____________________________________________________________________________
*
*					 4. Robustez a versiones sin controles y sin E.F.
*	____________________________________________________________________________


foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{

	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof i.semestre		///
	if common_sample==1, baselevels noabsorb 									///
	vce(cluster i.id_curso) 

	estimates store reg_`yvar'_OLS
	estadd ysumm
	estadd local FEest "No", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace


	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 				///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1, baselevels noabsorb								 	///
	vce(cluster i.id_curso) 

	estimates store reg_`yvar'_noFE
	estadd ysumm
	if "`yvar'"=="score_prof" | "`yvar'"=="score_curso" {
		estadd scalar ymean = 0, replace 
	}
	estadd local FEest "No", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace

	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 				///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 & sexo_prof_sd_curso!=0, baselevels 					///
	absorb(idestudiante id_curso)	vce(cluster i.id_curso) 

	cap drop sample
	gen sample=e(sample)

	estimates store reg_`yvar'_allFE
	estadd ysumm
	if "`yvar'"=="score_prof" | "`yvar'"=="score_curso" {
		estadd scalar ymean = 0, replace 
	}
	estadd local FEest "Sí", replace
	estadd local FEcurso "Sí", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace

	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 				///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 & sexo_prof_sd_curso!=0 & sample==1, baselevels 					///
	absorb(idestudiante)	vce(cluster i.id_curso) 

	estimates store reg_`yvar'_allFE1
	estadd ysumm
	if "`yvar'"=="score_prof" | "`yvar'"=="score_curso" {
		estadd scalar ymean = 0, replace 
	}
	estadd local FEest "Sí", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace




}  



#delimit ;
estout reg_score_sem_prof_OLS reg_score_sem_curso_OLS reg_score_8_OLS reg_positive_OLS reg_negative_OLS using "${git_documento}/Tablas/regs_OLS1.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest FEcurso r2 betas_sum,
		labels("E.F. estudiante" "E.F. curso" "R$^2$" "$\beta_1+\beta_2$")
		fmt(%~12s %~12s %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof 1.sexo_est)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor) varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)" 1.sexo_est "Estudiante mujer", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	numbers(\multicolumn{@span}{c}{( )})
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{2cm}}}"
		"\toprule \toprule"
		"& \multirow{2}{2cm}{\centering{Puntaje del profesor}} & \multirow{2}{2cm}{\centering{Puntaje del curso}} &\multirow{2}{2cm}{\centering{Recomienda al profesor}} &\multirow{2}{2cm}{\centering{Comentario positivo}} &\multirow{2}{2cm}{\centering{Comentario negativo}} \\ [5mm]")
	posthead("\cline{2-@span}"
		"& \multicolumn{@E}{c}{Panel A: Sin controles} \\")
	postfoot("\toprule");
#delimit cr

#delimit ;
estout reg_score_sem_prof_noFE reg_score_sem_curso_noFE reg_score_8_noFE reg_positive_noFE reg_negative_noFE using "${git_documento}/Tablas/regs_OLS2.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest FEcurso r2 betas_sum,
		labels("E.F. estudiante" "E.F. curso" "R$^2$" "$\beta_1+\beta_2$")
		fmt(%~12s %~12s %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof 1.sexo_est)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor)
	varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)" 1.sexo_est "Estudiante mujer", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("& \multicolumn{@E}{c}{Panel B: Con controles} \\")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. En la estimación de efectos fijos de estudiante el coeficiente de sexo del estudiante es igual a cero por colinealidad con los efectos fijos. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en el semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando y nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre. Observaciones varían por columna debido a que no todos los estudiantes deben responder las mismas preguntas y no todos deciden comentar." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr

#delimit ;
estout reg_score_sem_prof_allFE1 reg_score_sem_curso_allFE1 reg_score_8_allFE1 reg_positive_allFE1 reg_negative_allFE1 using "${git_documento}/Tablas/regs_cursoFE1.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest FEcurso N r2 ymean betas_sum,
		labels("E.F. estudiante" "E.F. curso" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$" )
		fmt(%~12s %~12s %9.0fc %9.2fc %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor)
	varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{2cm}}}"
		"\toprule \toprule"
		"& \multirow{2}{2cm}{\centering{Puntaje del profesor}} & \multirow{2}{2cm}{\centering{Puntaje del curso}} &\multirow{2}{2cm}{\centering{Recomienda al profesor}} &\multirow{2}{2cm}{\centering{Comentario positivo}} &\multirow{2}{2cm}{\centering{Comentario negativo}} \\ [5mm]")
	posthead("\cline{2-@span}"
		"& \multicolumn{@E}{c}{Panel A: Modelo principal en cursos con profesores hombres y mujeres} \\")
	postfoot("\toprule");
#delimit cr

#delimit ;
estout reg_score_sem_prof_allFE reg_score_sem_curso_allFE reg_score_8_allFE reg_positive_allFE reg_negative_allFE using "${git_documento}/Tablas/regs_cursoFE2.tex",
	style(tex) label notype
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest FEcurso N r2 ymean betas_sum,
		labels("E.F. estudiante" "E.F. curso" "Obs" "R$^2$" "Prom. var. dep." "$\beta_1+\beta_2$" )
		fmt(%~12s %~12s %9.0fc %9.2fc %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor)
	varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)", elist(1.sexo_est#1.sexo_prof \addlinespace) )  
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("& \multicolumn{@E}{c}{Panel B: Modelo principal con efectos fijos de curso} \\")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Panel A es igual a la Tabla \ref{main_table} pero se limita a las observaciones con variación por género del profesor intra-curso para ser comparable con el panel B. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en el semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando y nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre. Observaciones varían por columna debido a que no todos los estudiantes deben responder las mismas preguntas y no todos deciden comentar." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr


** Message when done
sendtoslack, url(https://hooks.slack.com/services/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) message("Finished running main regs")

cap log close
