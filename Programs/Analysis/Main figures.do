********************************************************************************
**																			  **
**									Main figures							  **
**																			  **
********************************************************************************
**																			  **
**		This program estimates the main regressions to create figures		  **
**																			  **
********************************************************************************

cap log close

log using "Logs/Regs by semestre `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

// First: baseline regression

reghdfe score_sem_prof i.sexo_est i.sexo_prof  					i.semestres_cursado i.semestre $est_controls $prof_controls 				$other_controls  if common_sample==1, baselevels absorb(idestudiante) 						vce(cluster i.id_curso)

local baseline = _b[1.sexo_prof]


// globals for regressions


foreach yvar in  score_sem_prof {

	if "`yvar'"!="score_8" local ytitle = "Desviaciones estándar"
	if "`yvar'"=="score_sem_prof" local title = "Puntaje profesor"
	if "`yvar'"=="score_sem_curso" local title = "Puntaje curso"
	

reghdfe `yvar' i.sexo_est i.sexo_prof i.sexo_prof#ib1.semestres_cursado 		///
	i.semestres_cursado i.semestre $prof_controls $est_controls $other_controls ///
	if common_sample==1, baselevels	///
		absorb(idestudiante)  nocons vce(cluster i.id_curso) 

matrix V=e(V)

matrix coefs_sem=J(10,2,.)
matrix sds_sem=J(10,2,.)

forvalues t=1/10{

	matrix coefs_sem[`t',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`t'.semestres_cursado"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`t'.semestres_cursado"]

	matrix sds_sem[`t',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`t'.semestres_cursado"]^2+2*`cov1')

}  

matrix rownames coefs_sem = 1 2 3 4 5 6 7 8 9 10

coefplot (matrix(coefs_sem[.,1]), se(sds_sem[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) , sch(tufte) name(By_sem_`yvar', replace) yline(0, lcol(red)) xlabel(,labsize(small )) ylabel(,labsize(small)) ytitle("`ytitle'", size(small)) yscale(range(-0.2 0.1)) ylabel(-0.2(0.05)0.1) yline(`baseline' , lcol(navy) lp(shortdash)) vertical  xtitle("Semestre cursado del estudiante") 
gr display, xsize(20) ysize(14)
gr export "${git_documento}/Graficas/Regs_by_semestre_`yvar'.pdf", replace

}
/*
grc1leg EST_FE_8 EST_FE_prof EST_FE_curso, sch(tufte)
gr export "${output_git}\Main_regs_semestre.pdf", replace 

