********************************************************************************
**																			  **
**							Heckman main regressions table					  **
**																			  **
********************************************************************************
**																			  **
**		This program estimates the main regressions using a Heckman 		  **
**		selection model to account for comments								  **
**																			  **
********************************************************************************

clear all

cap log close

log using "Logs/Heckman regressions `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

encode idestudiante, gen(id_est_num)

foreach yvar in positive negative{

	// Especificación principal con FE de estudiante
	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 , baselevels absorb(idestudiante) vce(cluster i.idprof) 

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


	// Heckman en muestra aleatoria con FE de estudiante

		* Muestra aleatoria del 40% de estudiantes (0.25)
		* Muestra aleatoria del 20% de estudiantes (0.84)
	preserve
		keep if common_sample==1
		keep idestudiante common_sample
		duplicates drop
		set seed 1234
		gen random = cond(rnormal(0,1)>=0.84,1,0) if common_sample==1
		drop common_sample

		tempfile random_sample
		save `random_sample'
	restore

	merge m:1 idestudiante using `random_sample', gen(_merge_sample_heckman)
	drop _merge

	local yvar positive
	heckman `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 				///
	i.semestres_cursado i.semestre $est_controls $prof_controls 				///
	$other_controls i.id_est_num if random==1 , select(frac_comenta_semestre_sin  			///
	i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof i.semestres_cursado  			///
	i.semestre $est_controls $prof_controls $other_controls i.id_est_num) vce(cl idprof)

	estimates store reg_`yvar'_heckman_FE
	estadd ysumm
	estadd local FEest "No", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["`yvar':1.sexo_prof","`yvar':1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<].1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace


	// Especificación sin FE de estudiante y curso
	reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 				///
	i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
	if common_sample==1 , baselevels noabsorb vce(cluster i.idprof) 

	estimates store reg_`yvar'_v2
	estadd ysumm
	estadd local FEest "No", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["1.sexo_prof","1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<0.1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace


	// Especificación heckman sin FE de estudiante
	heckman `yvar' i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 				///
	i.semestres_cursado i.semestre $est_controls $prof_controls 				///
	$other_controls	if common_sample==1 , select(frac_comenta_semestre_sin  	///
	i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof i.semestres_cursado  			///
	i.semestre $est_controls $prof_controls $other_controls) vce(cl idprof)

	estimates store reg_`yvar'_heckman
	estadd ysumm
	estadd local FEest "No", replace
	estadd local FEcurso "No", replace

	mat V = e(V)
	local bsum = _b[1.sexo_prof]+_b[1.sexo_prof#1.sexo_est]
	local sesum = sqrt(_se[1.sexo_prof]^2+_se[1.sexo_prof#1.sexo_est]^2+2*V["`yvar':1.sexo_prof","`yvar':1.sexo_prof#1.sexo_est"])
	local pval = ttail(e(N),abs(`bsum'/`sesum'))
	estadd local betas_sum "`= cond(`pval'<0.01,"`:di %5.3f `bsum''***", cond(`pval'<0.05,"`:di %5.3f `bsum''**",						cond(`pval'<].1,"`:di %5.3f `bsum''*", "`:di %5.3f `bsum''")))'", 		replace


} 


#delimit ;
estout reg_positive reg_positive_heckman_FE reg_positive_v2 reg_positive_heckman  reg_negative reg_negative_heckman_FE reg_negative_v2 reg_negative_heckman using "${git_documento}/Tablas/regs_heckman.tex",  
	style(tex) label
	cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
	stats(FEest N ymean betas_sum,
		labels("E.F. estudiante" "Obs" "Prom. var. dep." "$\beta_1+\beta_2$")
		fmt(%~12s %9.0fc %9.2fc %9.2fc %~12s)) keep(1.sexo_prof 1.sexo_est 1.sexo_est#1.sexo_prof /:athrho /:lnsigma, relax) drop(select:1.sexo_prof select:1.sexo_est	select:1.sexo_est#1.sexo_prof)
	replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
	title(Diferencias en las evaluaciones por género del profesor) varlabels(1.sexo_prof "Profesor mujer ($\beta_1$)" 1.sexo_est "Estudiante mujer" 1.sexo_est#1.sexo_prof "Est. mujer $\times$ Prof. mujer ($\beta_2$)" athrho "$\rho$" lnsigma "$\ln(\sigma)", elist(1.sexo_est#1.sexo_prof \addlinespace) ) 
	numbers(\multicolumn{@span}{c}{( )}) 
	collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(__ \_)
	prehead("\begin{threeparttable}" "\begin{tabular}{l*{@E}{C{2cm}}}"
		"\toprule \toprule"
		"& \multicolumn{3}{c}{Comentario positivo} & \multicolumn{3}{c}{Comentario negativo} \\ [-2mm]"
		"& \multirow{2}{2cm}{\centering{EF}} & \multirow{2}{2cm}{\centering{MCO}} &\multirow{2}{2cm}{\centering{Heckman}} &\multirow{2}{2cm}{\centering{EF}} &\multirow{2}{2cm}{\centering{MCO}}
			&\multirow{2}{2cm}{\centering{Heckman}} \\  [2mm]")
	posthead("\cline{2-@span}")
	postfoot("\bottomrule \bottomrule" "\end{tabular}" "\begin{tablenotes}" "\scriptsize" "\item Errores estándar con cluster a nivel de profesor. * p$<$0.10, ** p$<$0.05, *** p$<$0.01. En la estimación de efectos fijos de estudiante el coeficiente de sexo del estudiante es igual a cero por colinealidad con los efectos fijos. Controles incluidos en la regresión a nivel de estudiante: edad, edad$^2$, número de materias inscritas en el semestre, promedio del semestre, promedio acumulado, situación académica, semestre que está cursando y nota obtenida en el curso. A nivel de profesor: edad, edad$^2$, antigüedad del contrato del profesor, categoría del profesor, polinomio grado 3 del valor agregado. A nivel de curso: facultad del curso, cantidad de estudiantes, fracción de estudiantes mujeres, cantidad de profesores que dictan el curso, créditos y un indicador de curso magistral/complementario. Se incluyen efectos fijos de semestre." "\end{tablenotes}" "\end{threeparttable}");
#delimit cr



/*


** Two stage Heckman using fixed effects **

// Crear variable de si comenta o no

gen comments = 1 if positive!=. & common_sample==1
replace comments = 0 if positive==. & common_sample==1

// Partial out a los outcomes y a commentar o no
foreach yvar in comments{

	reghdfe `yvar' i.sexo_est i.semestre i.semestres_cursado $est_controls 		///
		$prof_controls $other_controls	if common_sample==1, baselevels 		///
		absorb(idestudiante) res(`yvar'_res)

}  

// Two stage heckman
foreach yvar in positive{

	probit comments frac_comenta_semestre_sin sexo_prof_res sexo_prof_est_res	///
	if common_sample==1, baselevels

	cap drop comments_hat
	predict comments_hat,xb
	sum 	comments_hat,d

	* Construir el IMR predicho
	cap drop pdf 
	cap drop cdf
	cap drop IMR_`yvar'
	gen pdf = normalden(comments_hat) /*generar densidad*/
	gen cdf = normal(comments_hat) /*generar acumulada*/
	gen IMR_`yvar' = pdf/cdf /*generar inverso de la razón de mills*/

	reghdfe `yvar'_res IMR_`yvar' sexo_prof_res sexo_prof_est_res	///
	if common_sample==1, baselevels noabsorb

}

gen interact = sexo_prof*sexo_est

reg positive sexo_prof interact tiempo_rta sexo_est  nota_curso
reg positive tiempo_rta sexo_est nota_curso if e(sample)
cap drop y_res
cap drop x_res
cap drop x2_res
predict y_res, res 
reg sexo_prof tiempo_rta sexo_est nota_curso if e(sample)
predict x_res, res 
reg interact tiempo_rta sexo_est nota_curso if e(sample)
predict x2_res, res 

reg y_res x_res x2_res
reg positive tiempo_rta sexo_prof interact sexo_est nota_curso

reghdfe score_prof i.sexo_est i.sexo_prof i.sexo_est#i.sexo_prof 	///
i.semestres_cursado i.semestre $est_controls $prof_controls $other_controls	///
if common_sample==1 , baselevels absorb(idestudiante) vce(cluster i.idprof) 

reg score_prof_res sexo_prof_res sexo_prof_est_res if common_sample==1
