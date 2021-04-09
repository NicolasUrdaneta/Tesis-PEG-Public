********************************************************************************
**																			  **
**					Estimación principal por carrera/facultad				  **
**																			  **
********************************************************************************

cap log close

log using "Logs/Regs por carreras `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

replace programa_adm = proper(programa_adm)
replace programa_adm = subinstr(programa_adm,"Ingenieria","Ingeniería",.)
replace programa_adm = subinstr(programa_adm,"Administracion","Administración",.)
replace programa_adm = subinstr(programa_adm,"gia","gía",.)
replace programa_adm = subinstr(programa_adm,"Politica","Política",.)
replace programa_adm = subinstr(programa_adm,"Contaduria","Contaduría",.)
replace programa_adm = subinstr(programa_adm,"Diseno","Diseño",.)
replace programa_adm = subinstr(programa_adm,"Economia","Economía",.)
replace programa_adm = subinstr(programa_adm,"Filosofia","Filosofía",.)
replace programa_adm = subinstr(programa_adm,"Fisica","Física",.)
replace programa_adm = subinstr(programa_adm,"Publicos","Públicos",.)
replace programa_adm = subinstr(programa_adm,"Biomedica","Biomédica",.)
replace programa_adm = subinstr(programa_adm,"Electrica","Eléctrica",.)
replace programa_adm = subinstr(programa_adm,"Electronica","Electrónica",.)
replace programa_adm = subinstr(programa_adm,"Mecanica","Mecánica",.)
replace programa_adm = subinstr(programa_adm,"Quimica","Química",.)
replace programa_adm = subinstr(programa_adm,"Matematicas","Matemáticas",.)
replace programa_adm = subinstr(programa_adm,"Musica","Música",.)
replace programa_adm = subinstr(programa_adm,"Y","y",.)

replace programa_adm = "Ing de Sistemas y Computación" if programa_adm=="Ing. De Sistemas y Computacion"
replace programa_adm = "Lenguaje y Est Sociocultural" if programa_adm=="Lenguajes y Estudios Sociocult"

cap drop programa_adm_num
encode programa_adm, gen(programa_adm_num)

foreach var in est curso{

	labvalch3 facultad_`var' , strfcn(proper(`"@"'))
	labvalch3 facultad_`var' , subst(ÓN ón)
	labvalch3 facultad_`var' , subst(ÑO ño)
	labvalch3 facultad_`var' , subst(ÍA ía)
	labvalch3 facultad_`var' , subst(ÉM ém)
	labvalch3 facultad_`var' , subst(ÑO ño)
	labvalch3 facultad_`var' , subst(Alberto "")
	labvalch3 facultad_`var' , subst(Lleras "")
	labvalch3 facultad_`var' , subst("  " "")
	labvalch3 facultad_`var' , subst(- "")
	labvalch3 facultad_`var' , subst(R r)
	labvalch3 facultad_`var' , subst(Y y)
	labvalch3 facultad_`var' , subst("Esc De Gobierno" "Escuela De Gobierno")

}

labvalch3 facultad_est , subst(cion ción)
labvalch3 facultad_est , subst(Economia Economía)
labvalch3 facultad_est , subst(Ingenieria Ingeniería)

// First: baseline regression

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.programa_adm_num 					i.semestres_cursado i.semestre $est_controls $prof_controls 				$other_controls  if common_sample==1 & programa_adm_num!=24 & 				programa_adm_num!=36  , baselevels absorb(idestudiante) 						vce(cluster i.id_curso)

local baseline = _b[1.sexo_prof]

// By carrera


tab programa_adm_num if programa_adm_num!=24 & programa_adm_num!=30 & programa_adm_num!=36
local size=r(r)
matrix coefs=J(`size',2,.)
matrix sds=J(`size',2,.)

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.programa_adm_num 					i.sexo_prof#i.programa_adm_num i.semestres_cursado i.semestre 				$est_controls $prof_controls $other_controls  if common_sample==1 & 		programa_adm_num!=24 & programa_adm_num!=36 , baselevels 					absorb(idestudiante) vce(cluster i.id_curso)

estimate store regs_carrera

matrix V=e(V)


levelsof programa_adm_num if programa_adm_num!=24 & programa_adm_num!=30 & programa_adm_num!=36, local(prog)
local j = 1
foreach p of local prog{

	matrix coefs[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.programa_adm_num"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.programa_adm_num"]

	matrix sds[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.programa_adm_num"]^2+2*`cov1')

	unique idprofesor if sexo_prof==1 & programa_adm_num==`p' & e(sample)
	local muj = r(unique)
	unique idprofesor if sexo_prof==0 & programa_adm_num==`p' & e(sample)
	local hom = r(unique)
	matrix coefs[`j',2] = `muj'*100/(`muj'+`hom')

	local j = `j'+1
}

levelsof programa_adm if programa_adm_num!=24 & programa_adm_num!=30 & programa_adm_num!=36 & programa_adm_num!=., local(progname)
mat rownames coefs = `progname'

mat l coefs
mat l sds

*local baseline =  -0.03

coefplot (matrix(coefs[.,1]), se(sds[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) , sch(tufte) sort name(Carrera, replace) xline(0, lcol(red)) xline(`baseline' , lcol(navy) lp(shortdash))  xlabel(,labsize(small )) ylabel(,labsize(small)) xtitle("Desviaciones estándar", size(small))
gr display, xsize(17) ysize(20)
gr export "${git_documento}/Graficas/Regs_by_carrera.pdf", replace

coefplot (matrix(coefs[.,1]), se(sds[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) (matrix(coefs[.,2]), se(sds[.,1]) msize(small) mc(gs6) m(Oh) noci axis(2)) , sch(tufte) sort(1) name(Carrera_perc, replace) xline(0, lcol(red)) xlabel(,labsize(small )) ylabel(,labsize(small)) xtitle("Desviaciones estándar", size(small)) xtitle("Porcentaje de profesoras (%)", axis(2) size(small)) xlabel(, labsize(small) axis(2)) xscale(range(-0.4 0.2)) xlabel(-0.4(0.2)0.2, labsize(small)) xline(`baseline' , lcol(navy) lp(shortdash)) legend(r(1) label(2 "Coeficiente de regresión") label(3 "% profesoras"))
gr display, xsize(17) ysize(20)
gr export "${git_documento}/Graficas/Regs_by_carrera_porcent.pdf", replace

// By facultad estudiante

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.facultad_est 						i.sexo_prof#i.facultad_est i.semestres_cursado i.semestre $est_controls 	$prof_controls $other_controls if common_sample==1 & facultad_est!=6 & facultad_est!=9 , baselevels 				absorb(idestudiante) vce(cluster i.id_curso)

estimate store regs_facultad

matrix V=e(V)

tab facultad_est
local size=r(r)
levelsof facultad_est if facultad_est!=6 & facultad_est!=9, local(fac)
matrix coefs_fac=J(`size',2,.)
matrix sds_fac=J(`size',2,.)


local j =1
foreach p of local fac{

	matrix coefs_fac[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.facultad_est"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.facultad_est"]

	matrix sds_fac[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.facultad_est"]^2+2*`cov1')

	unique idprofesor if sexo_prof==1 & facultad_est==`p' & e(sample)
	local muj = r(unique)
	unique idprofesor if sexo_prof==0 & facultad_est==`p' & e(sample)
	local hom = r(unique)
	matrix coefs_fac[`j',2] = `muj'*100/(`muj'+`hom')

	local j = `j'+1
}  


sdecode facultad_est	, gen(facultad_est_str)
levelsof facultad_est_str if facultad_est!=6 & facultad_est!=9, local(fac_names)
matrix rownames coefs_fac = `fac_names'

*local baseline = -0.03
coefplot (matrix(coefs_fac[.,1]), se(sds_fac[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) , sch(tufte) sort name(Facultad, replace) xline(0, lcol(red)) xlabel(,labsize(small )) ylabel(,labsize(small)) xtitle("Desviaciones estándar", size(small)) xscale(range(-0.2 0.1)) xlabel(-0.2(0.05)0.1) xline(`baseline' , lcol(navy) lp(shortdash))
gr display, xsize(18) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_facultad_est.pdf", replace
*local baseline = -0.03
coefplot (matrix(coefs_fac[.,1]), se(sds_fac[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) (matrix(coefs_fac[.,2]), se(sds_fac[.,1]) msize(small) mc(gs6) m(Oh) noci axis(2)) , sch(tufte) sort(1) name(Facultad_perc, replace) xline(0, lcol(red)) xlabel(,labsize(small )) ylabel(,labsize(small)) xtitle("Desviaciones estándar", size(small)) xtitle("Porcentaje de profesoras (%)", axis(2) size(small)) xlabel(, labsize(small) axis(2)) xscale(range(-0.2 0.1)) xlabel(-0.2(0.05)0.1, labsize(small)) xline(`baseline' , lcol(navy) lp(shortdash)) legend(r(1) label(2 "Coeficiente de regresión") label(3 "% profesoras"))
gr display, xsize(18) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_facultad_est_porcent.pdf", replace




// By facultad curso

reghdfe score_sem_prof i.sexo_est i.sexo_prof i.facultad_curso 						i.sexo_prof#i.facultad_curso i.semestres_cursado i.semestre $est_controls 	$prof_controls $other_controls if common_sample==1 & facultad_curso !=4  & facultad_curso!=10 & facultad_curso!=13, baselevels 				absorb(idestudiante) vce(cluster i.id_curso)

estimate store regs_facultad2

matrix V=e(V)

tab facultad_curso
local size=r(r)
levelsof facultad_curso if facultad_curso !=4  & facultad_curso!=10 & facultad_curso!=13, local(fac)
matrix coefs_fac_curso=J(`size',2,.)
matrix sds_fac_curso=J(`size',2,.)

local j =1
foreach p of local fac{

	matrix coefs_fac_curso[`j',1] = _b["1.sexo_prof"]+_b["1.sexo_prof#`p'.facultad_curso"]

	local cov1 = V["1.sexo_prof","1.sexo_prof#`p'.facultad_curso"]

	matrix sds_fac_curso[`j',1] = sqrt(_se["1.sexo_prof"]^2+_se["1.sexo_prof#`p'.facultad_curso"]^2+2*`cov1')

	unique idprofesor if sexo_prof==1 & facultad_curso==`p' & e(sample)
	local muj = r(unique)
	unique idprofesor if sexo_prof==0 & facultad_curso==`p' & e(sample)
	local hom = r(unique)
	matrix coefs_fac_curso[`j',2] = `muj'*100/(`muj'+`hom')

	local j = `j'+1
}  


sdecode facultad_curso	, gen(facultad_curso_str)
levelsof facultad_curso_str  if facultad_curso !=4  & facultad_curso!=10 & facultad_curso!=13, local(fac_names)
matrix rownames coefs_fac_curso = `fac_names'

*local baseline = -0.03
coefplot (matrix(coefs_fac_curso[.,1]), se(sds_fac_curso[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) , sch(tufte) sort name(Facultad_curso, replace) xline(0, lcol(red)) xlabel(,labsize(small)) ylabel(,labsize(small)) xtitle("Desviaciones estándar", size(small)) xscale(range(-0.25 0.15)) xlabel(-0.25(0.05)0.15) xline(`baseline' , lcol(navy) lp(shortdash))
gr display, xsize(18) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_facultad_curso.pdf", replace


coefplot (matrix(coefs_fac_curso[.,1]), se(sds_fac_curso[.,1]) msize(small) mc(black) m(S) ciopts(color(black) recast(rcap))) (matrix(coefs_fac_curso[.,2]), se(sds_fac_curso[.,1]) msize(small) mc(gs6) m(Oh) noci axis(2)) , sch(tufte) sort(1) name(Facultad_curso_perc, replace) xline(0, lcol(red)) xlabel(,labsize(small )) ylabel(,labsize(small)) xtitle("Desviaciones estándar", size(small)) xtitle("Porcentaje de profesoras (%)", axis(2) size(small)) xlabel(, labsize(small) axis(2)) xscale(range(-0.25 0.15)) xlabel(-0.25(0.05)0.15, labsize(small)) xline(`baseline' , lcol(navy) lp(shortdash)) legend(r(1) label(2 "Coeficiente de regresión") label(3 "% profesoras"))
gr display, xsize(18) ysize(16)
gr export "${git_documento}/Graficas/Regs_by_facultad_curso_porcent.pdf", replace
