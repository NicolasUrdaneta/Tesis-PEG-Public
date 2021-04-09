********************************************************************************
**																			  **
**					Estimación principal por categorías				          **
**																			  **
********************************************************************************

cap log close

log using "Logs/Regs por categoría `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

cap drop categ_prof3 
gen categ_prof3  = 1 if categ_prof2==2
replace categ_prof3  = 2 if categ_prof2==4
replace categ_prof3  = 3 if categ_prof2==6
replace categ_prof3  = 4 if categ_prof2==7 | categ_prof2==8 
replace categ_prof3  = 5 if categ_prof2==1 | categ_prof2==3  


// First: baseline regression

foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{

reghdfe `yvar' i.sexo_est i.sexo_prof i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls  if common_sample==1, baselevels absorb(idestudiante) 						vce(cluster i.id_curso)

local baseline = _b[1.sexo_prof]


// By tipo de profesor

reghdfe `yvar' i.sexo_est i.sexo_prof i.categ_prof3						i.sexo_prof#i.categ_prof3 i.semestres_cursado i.semestre $est_controls 	$prof_controls2 $other_controls if common_sample==1, baselevels 				absorb(idestudiante) vce(cluster i.id_curso)

matrix V=e(V)

tab categ_prof3
local size=r(r)
levelsof categ_prof3 if categ_prof3!=5, local(age)
matrix coefs_edad_prof=J(`size',2,.)
matrix sds_edad_prof=J(`size',2,.)

local j =1
foreach p of local age{

	matrix coefs_edad_prof[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.categ_prof3"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.categ_prof3"]

	matrix sds_edad_prof[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.categ_prof3"]^2+2*`cov1')

	local j = `j'+1
}  


matrix rownames coefs_edad_prof = "Asistente graduado" "Cátedra" "Prof Asistente" "Asociado y titular"

*local baseline = -0.03
coefplot (matrix(coefs_edad_prof[.,1]), se(sds_edad_prof[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap)))  , sch(tufte) name(Categ_`yvar', replace) yline(0, lcol(red)) xlabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall))  yscale(range(-0.15 0.1)) ylabel(-0.15(0.05)0.1, labsize(small)) yline(`baseline' , lcol(navy) lp(shortdash)) vertical yscale(titlegap(*-10)) xscale(titlegap(*5)) legend(off)
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_categ_`yvar'.pdf", replace

test (_b[1.sexo_prof#2.categ_prof3]=_b[1.sexo_prof#4.categ_prof3])

}

// Con un coeficiente por género del estudiante

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.categ_prof3 i.sexo_prof#i.categ_prof3 i.sexo_est#i.categ_prof3 i.sexo_est#i.sexo_prof#i.categ_prof3 i.semestres_cursado i.semestre $est_controls 	$prof_controls2 $other_controls if common_sample==1, baselevels 				absorb(idestudiante) vce(cluster i.id_curso)

matrix V=e(V)

tab categ_prof3
local size=r(r)
levelsof categ_prof3 if categ_prof3!=5, local(age)
matrix coefs_edad_prof=J(`size',2,.)
matrix sds_edad_prof=J(`size',2,.)

local j =1
foreach p of local age{

	matrix coefs_edad_prof[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.categ_prof3"]
	matrix coefs_edad_prof[`j',2] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.categ_prof3"]+_b["1.sexo_est#1.sexo_prof#`p'.categ_prof3"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.categ_prof3"]
	local cov2 = V["1.sexo_prof","1.sexo_est#1.sexo_prof#`p'.categ_prof3"]
	local cov3 = V["1.sexo_prof#`p'.categ_prof3","1.sexo_est#1.sexo_prof#`p'.categ_prof3"]

	matrix sds_edad_prof[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.categ_prof3"]^2+2*`cov1')
	matrix sds_edad_prof[`j',2] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.categ_prof3"]^2+_se["1.sexo_est#1.sexo_prof#`p'.categ_prof3"]^2+2*`cov1'+2*`cov2'+2*`cov3')

	local j = `j'+1
}  


matrix rownames coefs_edad_prof = "Asistente graduado" "Cátedra" "Prof Asistente" "Asociado y titular" //"Otros"

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls  if common_sample==1, baselevels absorb(idestudiante) 						vce(cluster i.id_curso)

local baseline = _b[1.sexo_prof]

*local baseline = -0.03
coefplot (matrix(coefs_edad_prof[.,1]), se(sds_edad_prof[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) (matrix(coefs_edad_prof[.,2]), se(sds_edad_prof[.,2]) msize(small) mc(gs6) m(S) ciopts(color(gs6) recast(rcap)))  , sch(tufte) name(Categ_prof_v2, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall))  yscale(range(-0.15 0.1)) ylabel(-0.15(0.05)0.1, labsize(small)) yline(`baseline' , lcol(navy) lp(shortdash)) vertical yscale(titlegap(*-10)) xscale(titlegap(*5)) legend(off)
gr display, xsize(20) ysize(16)

