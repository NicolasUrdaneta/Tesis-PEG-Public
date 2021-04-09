********************************************************************************
**																			  **
**						Estimación principal por estrato					  **
**																			  **
********************************************************************************

cap log close

log using "Logs/Regs por estrato `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

// First: baseline regression

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls  if common_sample==1, baselevels absorb(idestudiante) 						vce(cluster i.id_curso)

local baseline = _b[1.sexo_prof]


// Por estrato

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.sexo_prof#i.estrato 				i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls if common_sample==1, baselevels absorb(idestudiante) 		vce(cluster i.id_curso)

matrix V=e(V)

tab estrato
local size=r(r)
levelsof estrato, local(est)
matrix coefs_estrato=J(`size',2,.)
matrix sds_estrato=J(`size',2,.)

local j =1
foreach p of local est{

	matrix coefs_estrato[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.estrato"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.estrato"]

	matrix sds_estrato[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.estrato"]^2+2*`cov1')

	local j = `j'+1
}  

matrix rownames coefs_estrato = 1 2 3 4 5 6 

*local baseline = -0.03
coefplot (matrix(coefs_estrato[.,1]), se(sds_estrato[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap)))  , sch(tufte) name(Estrato_est, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ylabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall)) xtitle("Estrato socioeconómico", size(medsmall)) yscale(range(-0.1 0.05)) ylabel(-0.1(0.05)0.05) yline(`baseline' , lcol(navy) lp(shortdash)) vertical xscale(titlegap(*5))
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_estrato.pdf", replace

test (_b[1.sexo_prof#1.estrato]=_b[1.sexo_prof#2.estrato]=_b[1.sexo_prof#3.estrato]=_b[1.sexo_prof#4.estrato]=_b[1.sexo_prof#5.estrato]=_b[1.sexo_prof#6.estrato])

/*
test (_b[1.sexo_prof#3.categ_edad_prof]=_b[1.sexo_prof#4.categ_edad_prof])
test (_b[1.sexo_prof#3.categ_edad_prof]=_b[1.sexo_prof#5.categ_edad_prof])
test (_b[1.sexo_prof#4.categ_edad_prof]=_b[1.sexo_prof#5.categ_edad_prof])
*/

// Por educación de la madre

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.sexo_prof#i.educ_madre 				i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls if common_sample==1, baselevels absorb(idestudiante) 		vce(cluster i.id_curso)

matrix V=e(V)

tab educ_madre
local size=r(r)
levelsof educ_madre, local(est)
matrix coefs_educmadre=J(`size',2,.)
matrix sds_educmadre=J(`size',2,.)

local j =1
foreach p of local est{

	matrix coefs_educmadre[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.educ_madre"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.educ_madre"]

	matrix sds_educmadre[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.educ_madre"]^2+2*`cov1')

	local j = `j'+1
}  
matrix rownames coefs_educmadre = "Bachillerato" "Posgrado" "Primaria" "Profesional" "Técnico"

*local baseline = -0.03
coefplot (matrix(coefs_educmadre[.,1]), se(sds_educmadre[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap)))  , sch(tufte) name(Educ_madre, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ylabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall)) xtitle("Nivel educativo de la madre del estudiante", size(medsmall)) yscale(range(-0.1 0.05)) ylabel(-0.1(0.05)0.05) yline(`baseline' , lcol(navy) lp(shortdash)) vertical xscale(titlegap(*5)) order(Primaria Bachillerato Técnico Profesional Posgrado)
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_educ_madre.pdf", replace

test (_b[1.sexo_prof#1.educ_madre]=_b[1.sexo_prof#2.educ_madre]=_b[1.sexo_prof#3.educ_madre]=_b[1.sexo_prof#4.educ_madre]=_b[1.sexo_prof#5.educ_madre])

// Por ocupación de la madre

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.sexo_prof#i.ocup_madre 				i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls if common_sample==1, baselevels absorb(idestudiante) 		vce(cluster i.id_curso)

matrix V=e(V)

tab ocup_madre
local size=r(r)
levelsof ocup_madre, local(est)
matrix coefs_ocupmadre=J(`size',2,.)
matrix sds_ocupmadre=J(`size',2,.)

local j =1
foreach p of local est{

	matrix coefs_ocupmadre[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.ocup_madre"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.ocup_madre"]

	matrix sds_ocupmadre[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.ocup_madre"]^2+2*`cov1')

	local j = `j'+1
}  
matrix rownames coefs_ocupmadre = "Desempleada" "Empleada" "Empresaria" "Independiente" "Otro"

*local baseline = -0.03
coefplot (matrix(coefs_ocupmadre[.,1]), se(sds_ocupmadre[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap)))  , sch(tufte) name(Ocup_madre, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ylabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall)) xtitle("Oucpación de la madre del estudiante", size(medsmall)) yscale(range(-0.1 0.05)) ylabel(-0.1(0.05)0.05) yline(`baseline' , lcol(navy) lp(shortdash)) vertical xscale(titlegap(*5))
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_ocup_madre.pdf", replace

test (_b[1.sexo_prof#1.ocup_madre]=_b[1.sexo_prof#2.ocup_madre]=_b[1.sexo_prof#3.ocup_madre]=_b[1.sexo_prof#4.ocup_madre]=_b[1.sexo_prof#5.ocup_madre])
