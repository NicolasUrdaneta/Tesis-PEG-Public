********************************************************************************
**																			  **
**						Correlaciones entre variables principales   		  **
**																			  **
********************************************************************************

cap log close

log using "Logs/Corrs main vars `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

 gen tiempo_rta_win = tiempo_rta if inrange(tiempo_rta,1,180)

local depvars score_sem_prof score_sem_curso score_8 positive negative 
local indepvars nota_curso creditos_curso tiempo_rta_win VA age_prof antiguedad_prof_adj total_estudiantes tot_profs_curso

matrix corr_regs = J(8,5,.)
matrix corr_regs_se = J(8,5,.)
matrix corr_regs_t = J(8,5,.)

local col = 1
foreach dep of local depvars {	

	local row = 1
	foreach indep of local indepvars{

		reg `dep' `indep' i.facultad_curso i.semestre if common_sample==1, vce(cluster idcursoseccion)

		mat corr_regs[`row', `col'] = _b[`indep']
		mat corr_regs_se[`row', `col'] = _se[`indep']
		mat corr_regs_t[`row', `col'] = _b[`indep']/_se[`indep']

		local row = 1 + `row'

	}

	local col = 1 + `col'

}

*matrix colnames corr_regs = `depvars'
matrix rownames corr_regs = `indepvars'

mat l corr_regs
mat l corr_regs_t

clear 
svmat corr_regs, names(col)
svmat corr_regs_t


forvalues var=1/5{

	gen Var`var'=string(c`var',"%7.3f")
	replace Var`var'=Var`var'+"*" if abs(corr_regs_t`var')>1.64
	replace Var`var'=Var`var'+"*" if abs(corr_regs_t`var')>1.96
	replace Var`var'=Var`var'+"*" if abs(corr_regs_t`var')>2.69

	drop c`var' corr_regs_t`var'

}

gen row=_n 
gen variable=""
local j = 1
foreach x in "Nota del curso" "Créditos del curso" "Tiempo de respuesta (mins)" "Valor agregado profesor" "Edad profesor" "Antigüedad del profesor" "N. de estudiantes" "N. de profesores"{
	replace var = "`x'" in `j'
	local j = `j'+1
}

drop row 

order variable Var*

foreach var of varlist variable Var1 Var2 Var3 Var4{
	replace `var'=`var'+" & "
}

replace Var5 = Var5 + " \\ "


set obs `=_N+1'
replace variable = "\bottomrule \bottomrule" in `=_N'
set obs `=_N+1'
replace variable="\end{tabular}" in `=_N'
set obs `=_N+1'
replace variable="\begin{tablenotes}"  in `=_N'
set obs `=_N+1'
replace variable="\footnotesize" in `=_N'
set obs `=_N+1'
replace variable="\item Correlaciones entre variables de resultado y características del estudiante, del curso y del profesor. Coeficientes obtenidos de una regresión entre la variable de resultado correspondiente, la característica y efectos fijos de facultad del curso y de semestre. Pruebas de hipótesis con cluster a nivel de curso. * p$<$0.10, ** p$<$0.05, *** p$<$0.01." in `=_N'
set obs `=_N+1'
replace variable="\end{tablenotes}" in `=_N'
set obs `=_N+1'
replace variable="\end{threeparttable}"  in `=_N'

tempfile lower
save `lower'

clear 
set obs 5
gen variable=""
replace variable="\begin{threeparttable}" in 1
replace variable="\begin{tabular}{l*{5}{C{2.5cm}}}" in 2
replace variable="\toprule \toprule" in 3
replace variable="Variable & Puntaje del profesor & Puntaje del curso & Recomienda al profesor & Prob. comentario positivo & Prob. comentario negativo \\ [5mm]" in 4
replace variable="\cline{2-6}" in 5

append using `lower'

outsheet variable Var* using "$git_documento/Tablas/Tabla Descriptiva Calidad Datos.tex" , replace nonames delim("") noquote

cap log close
