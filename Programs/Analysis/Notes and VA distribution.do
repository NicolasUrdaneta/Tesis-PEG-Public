********************************************************************************
**																			  **
**						Grades and VA distributions							  **
**																			  **
********************************************************************************
**																			  **
**		This program creates the figures with the distributions of grades	  **
**		and value added														  **
**																			  **
********************************************************************************

cap log close

log using "Logs/Notes and VA distributions `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

ksmirnov nota_curso if nota_curso>1.5, by(sexo_prof)
ttest nota_curso if nota_curso>1.5, by(sexo_prof)
local m_m = round(r(mu_2),0.01)
local m_h = round(r(mu_1),0.01)

if "$fig_title"=="ON"{
	local title = "Distribución de las notas de los estudiantes por género del profesor"
	local note1 = "Promedio mujeres `m_m'. Promedio hombres `m_h'. (Diferencia significativa al 1%)."
	local note2 = "Diferencia de las distribuciones significativa al 1% de acuerdo al test de Kolmogorov-Smirnov."
}

tw (kdensity nota_curso if sexo_prof==1 & nota_curso>1.5, lp(solid) lc(black))	///
 (kdensity nota_curso if sexo_prof==0 & nota_curso>1.5, lp(dash) lc(navy)), 	///
 legend(order(1 "Mujeres" 2 "Hombres" ) r(1)) name(notas, replace) 				///
 title("")						///
 ytitle("Densidad") xtitle("Nota") sch(tufte)				///
 note("`note1'" "`note2'")
 gr display, ysize(13) xsize(20)
gr export "${git_documento}/Graficas/Notas_distributions.pdf", replace

preserve

keep VA sexo_prof semestre idprofesor facultad_curso count_prof_sem
keep if count_prof_sem==1

reg VA i.facultad_curso i.semestre
sum VA
local mean=r(mean)

predict VA_res, res

_pctile VA_res, p(0.05 99.95)
local min=r(r1)
local max=r(r2)

ksmirnov VA_res , by(sexo_prof)
ttest VA_res , by(sexo_prof)
local m_m = round(r(mu_2),0.01)
local m_h = round(r(mu_1),0.01)
if "$fig_title"=="ON"{
	local title = "Distribución del valor agregado de los profesores por género"
	local note1 = "Promedio mujeres `m_m'. Promedio hombres `m_h'. (Diferencia significativa al 1%)."
	local note2 = "Diferencia de las distribuciones significativa al 1% de acuerdo al test de Kolmogorov-Smirnov."
}

tw (kdensity VA_res if sexo_prof==1 & VA>=`min',	lp(solid) lc(black))  (kdensity VA_res if sexo_prof==0  & VA>=`min', lp(dash) lc(navy)), legend(order(1 "Mujeres" 2 "Hombres" ) r(1)) name(VA, replace) sch(tufte) title("`title'") ytitle("Densidad") xtitle("Valor agregado normalizado") note("`note1'" "`note2'")
 gr display, ysize(13) xsize(20)
gr export "${git_documento}/Graficas\VA_distributions.pdf", replace 

restore
