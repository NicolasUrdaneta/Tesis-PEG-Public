********************************************************************************
**																			  **
**					  Response score model regressions						  **
**																			  **
********************************************************************************
**																			  **
**		This program estimates the regressions for response score model       **
** 		outcomes				  											  **
**																			  **
********************************************************************************

clear all


cap log close

log using "Logs/Regressions RSM outcomes `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear


foreach yvar in score_rsm_std_prof score_rsm_std_curso{

	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 , baselevels absorb(idestudiante) vce(cluster i.id_curso) 

	estimates store reg_`yvar'
	estadd ysumm
	if "`yvar'"=="score_stand_prof" | "`yvar'"=="score_stand_curso" {
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
estout reg_score_rsm_std_prof reg_score_rsm_std_curso  using "${git_documento}/Tablas/regs_rsm.tex",  
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
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{3.5cm}}}"
		"\toprule \toprule"
		"& \multirow{2}{3.5cm}{\centering{Puntaje del profesor}} & \multirow{2}{3.5cm}{\centering{Puntaje del curso}}  \\ [5mm]")
	posthead("\cline{2-@span}")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en el semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando, nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre. Observaciones varían por columna debido a que no todos los estudiantes deben responder las mismas preguntas. Puntajes calculados con un moelo de respuesta al ítem de escala de calificación (RSM). Variable dependiente estandarizada." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr
