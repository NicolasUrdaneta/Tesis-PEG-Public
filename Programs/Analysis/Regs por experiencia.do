********************************************************************************
**																			  **
**					Estimación principal por carrera/facultad				  **
**																			  **
********************************************************************************

cap log close

log using "Logs/Regs por experiencia `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

cap drop categ_edad_prof 
gen categ_edad_prof  = 1 if inrange(age_prof,18,25)
replace categ_edad_prof  = 2 if inrange(age_prof,25,35)
replace categ_edad_prof  = 3 if inrange(age_prof,35,45)
replace categ_edad_prof  = 4 if inrange(age_prof,45,55)
replace categ_edad_prof  = 5 if inrange(age_prof,55,90)

// First: baseline regression

foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{


reghdfe `yvar' i.sexo_est i.sexo_prof i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls  if common_sample==1, baselevels absorb(idestudiante) 						vce(cluster i.id_curso)

local baseline = _b[1.sexo_prof]


// By tipo de profesor

reghdfe `yvar' i.sexo_est i.sexo_prof i.categ_edad_prof 						i.sexo_prof#i.categ_edad_prof i.semestres_cursado i.semestre $est_controls 	$prof_controls2 $other_controls if common_sample==1, baselevels 				absorb(idestudiante) vce(cluster i.id_curso)

matrix V=e(V)

tab categ_edad_prof
local size=r(r)
levelsof categ_edad_prof, local(age)
matrix coefs_edad_prof=J(`size',2,.)
matrix sds_edad_prof=J(`size',2,.)

local j =1
foreach p of local age{

	matrix coefs_edad_prof[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.categ_edad_prof"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.categ_edad_prof"]

	matrix sds_edad_prof[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.categ_edad_prof"]^2+2*`cov1')

	local j = `j'+1
}  

** % profesoras en cada categoría
local j =1 
forvalues age=1/5{
	sum sexo_prof if categ_edad_prof==`age' & e(sample)
	local age`age' = round(r(mean),0.01)*100
	mat coefs_edad_prof[`j',2] = `age`age''
	mat sds_edad_prof[`j',2] = 0
	local j = `j'+1
}


matrix rownames coefs_edad_prof = "19-25" "25-35" "35-45" "45-55" "55+"

*local baseline = -0.03
coefplot (matrix(coefs_edad_prof[.,1]), se(sds_edad_prof[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap)))  , sch(tufte) name(Edad_prof, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall))  xtitle("Edad", size(medsmall)) yscale(range(-0.15 0.1)) ylabel(-0.15(0.05)0.1, labsize(small)) yline(`baseline' , lcol(navy) lp(shortdash)) vertical yscale(titlegap(*-10)) xscale(titlegap(*5)) legend(off)
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_age_`yvar'.pdf", replace
coefplot (matrix(coefs_edad_prof[.,1]), se(sds_edad_prof[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) (matrix(coefs_edad_prof[.,2]), se(sds_edad_prof[.,2]) msize(small) mc(gs6) m(Oh) noci axis(2))  , sch(tufte) name(Edad_prof_perc, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall)) ytitle("Porcentaje de profesoras (%)", axis(2) size(medsmall)) xtitle("Edad", size(medsmall)) yscale(range(-0.15 0.1)) ylabel(20(5)40, axis(2) labsize(small)) ylabel(-0.15(0.05)0.1, labsize(small)) yline(`baseline' , lcol(navy) lp(shortdash)) vertical yscale(titlegap(*-10)) xscale(titlegap(*5)) legend(r(1) label(2 "Coeficiente de regresión") label(3 "% profesoras"))
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_age_`yvar'_porcent.pdf", replace

}

test (_b[1.sexo_prof#3.categ_edad_prof]=_b[1.sexo_prof#4.categ_edad_prof])
test (_b[1.sexo_prof#3.categ_edad_prof]=_b[1.sexo_prof#5.categ_edad_prof])
test (_b[1.sexo_prof#4.categ_edad_prof]=_b[1.sexo_prof#5.categ_edad_prof])

// Edades más desagregadas
cap drop categ_edad_prof 
gen categ_edad_prof  = 1 if inrange(age_prof,18,25)
replace categ_edad_prof  = 2 if inrange(age_prof,25,30)
replace categ_edad_prof  = 3 if inrange(age_prof,30,35)
replace categ_edad_prof  = 4 if inrange(age_prof,35,40)
replace categ_edad_prof  = 5 if inrange(age_prof,40,45)
replace categ_edad_prof  = 6 if inrange(age_prof,45,50)
replace categ_edad_prof  = 7 if inrange(age_prof,50,55)
replace categ_edad_prof  = 8 if inrange(age_prof,55,60)
replace categ_edad_prof  = 9 if inrange(age_prof,60,90)



// First: baseline regression

foreach yvar in score_8 score_sem_prof score_sem_curso positive negative{


reghdfe `yvar' i.sexo_est i.sexo_prof i.semestres_cursado i.semestre $est_controls $prof_controls2 				$other_controls  if common_sample==1, baselevels absorb(idestudiante) 						vce(cluster i.id_curso)

local baseline = _b[1.sexo_prof]


// By tipo de profesor

reghdfe `yvar' i.sexo_est i.sexo_prof i.categ_edad_prof 						i.sexo_prof#i.categ_edad_prof i.semestres_cursado i.semestre $est_controls 	$prof_controls2 $other_controls if common_sample==1, baselevels 				absorb(idestudiante) vce(cluster i.id_curso)

matrix V=e(V)

tab categ_edad_prof
local size=r(r)
levelsof categ_edad_prof, local(age)
matrix coefs_edad_prof=J(`size',2,.)
matrix sds_edad_prof=J(`size',2,.)

local j =1
foreach p of local age{

	matrix coefs_edad_prof[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.categ_edad_prof"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.categ_edad_prof"]

	matrix sds_edad_prof[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.categ_edad_prof"]^2+2*`cov1')

	local j = `j'+1
}  

** % profesoras en cada categoría
local j =1 
forvalues age=1/5{
	sum sexo_prof if categ_edad_prof==`age' & e(sample)
	local age`age' = round(r(mean),0.01)*100
	mat coefs_edad_prof[`j',2] = `age`age''
	mat sds_edad_prof[`j',2] = 0
	local j = `j'+1
}


matrix rownames coefs_edad_prof = "19-25" "25-30" "30-35" "35-40" "40-45" "45-50" "50-55" "55-60" "60+" 

*local baseline = -0.03
coefplot (matrix(coefs_edad_prof[.,1]), se(sds_edad_prof[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap)))  , sch(tufte) name(Edad_prof, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall))  xtitle("Edad", size(medsmall)) yscale(range(-0.15 0.1)) ylabel(-0.15(0.05)0.1, labsize(small)) yline(`baseline' , lcol(navy) lp(shortdash)) vertical yscale(titlegap(*-10)) xscale(titlegap(*5)) legend(off)
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_age_`yvar'_disag.pdf", replace
coefplot (matrix(coefs_edad_prof[.,1]), se(sds_edad_prof[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) (matrix(coefs_edad_prof[.,2]), se(sds_edad_prof[.,2]) msize(small) mc(gs6) m(Oh) noci axis(2))  , sch(tufte) name(Edad_prof_perc, replace) yline(0, lcol(red)) xlabel(,labsize(small)) ytitle("Desviaciones estándar", size(medsmall)) ytitle("Porcentaje de profesoras (%)", axis(2) size(medsmall)) xtitle("Edad", size(medsmall)) yscale(range(-0.15 0.1)) ylabel(20(5)40, axis(2) labsize(small)) ylabel(-0.15(0.05)0.1, labsize(small)) yline(`baseline' , lcol(navy) lp(shortdash)) vertical yscale(titlegap(*-10)) xscale(titlegap(*5)) legend(r(1) label(2 "Coeficiente de regresión") label(3 "% profesoras"))
gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_age_`yvar'_porcent_disag.pdf", replace

}
