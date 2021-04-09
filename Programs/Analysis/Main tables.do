********************************************************************************
**																			  **
**							Descriptive stats table							  **
**																			  **
********************************************************************************
**																			  **
**		This program creates the descriptive stats table					  **
**																			  **
********************************************************************************

clear all

cap log close

log using "Logs/Descriptive stats table `c(current_date)'.txt", replace text

use "Processed/Final Surveys Dataset.dta", clear

// Adjust tiempo de respuesta

gen tiempo_rta_adj = tiempo_rta if tiempo_rta<=180

global Table_prof antiguedad_prof_adj age_prof VA activo
global Table_est sexo_est age semestres_cursado STEM cred_insc_sem gpa_sem nota_curso creditos_curso tiempo_rta_adj comenta
global Table_others total_estudiantes fraccion_mujeres_es

cap program drop MeanDiff
program define MeanDiff

	syntax varlist(min=1 numeric) [if] , group(varname) [cluster(varname) absorb(varlist) mname(string)]

	qui levelsof `group'
	if r(r)!=2{
	 dis as error "Error: Groupvar must contain two categories"
	 exit 198
	}
	if "`cluster'"!=""{
		local clust_loc = "cl `cluster'"
	}
	else{
		local clust_loc = "unadjusted"
	}

	if "`absorb'"!=""{
		local t=1
		foreach var in `varlist' {
			dis "`var'"
		qui reghdfe `var' `group' `if', absorb(`absorb') vce(`clust_loc')
		mat v=e(V)
		matrix Temp=(e(N),_b[_cons],sqrt(v[2,2]),	///
			_b[_cons]+_b[`group'],									///
			sqrt(v[2,2]+v[1,1]+2*v[1,2]),							///
			_b[`group'],											///
			(ttail(e(df_r),abs(_b[`group'])/_se[`group']))*2)
		matrix rown Temp = "`var'"
		if `t'==1 matrix Diff = (Temp)
		if `t'!=1 matrix Diff = (Diff \ Temp)
		local t=0
		}
	}
	else{
		local t=1
		foreach var in `varlist' {
			dis "`var'"
		qui reghdfe `var' `group' `if', noabsorb vce(`clust_loc')
		mat v=e(V)
		matrix Temp=(e(N),_b[_cons],sqrt(v[2,2]),	///
			_b[_cons]+_b[`group'],									///
			sqrt(v[2,2]+v[1,1]+2*v[1,2]),							///
			_b[`group'],											///
			(ttail(e(df_r),abs(_b[`group'])/_se[`group']))*2)
		matrix rown Temp = "`var'"
		if `t'==1 matrix Diff = (Temp)
		if `t'!=1 matrix Diff = (Diff \ Temp)
		local t=0
		}
	}

	mat coln Diff= "N" "Media H" "DE H" "Media M" "DE M" 				///
		"Diferencia (H-M)" "P-valor" 
	if "`mname'"!=""{
		mat `mname' = Diff
		mat l `mname'
	}
	else{
		mat list Diff
	}

end

gen comenta = 1 if merge_comments==3
replace comenta = 0 if merge_comments==1

gen activo = 1 if estado_prof==1
replace activo = 0 if estado_prof==2
tab categ_prof2, gen(cat_prof_dum)

/*
MeanDiff $Table_est $Table_prof cat_prof_dum2 cat_prof_dum4 cat_prof_dum6 cat_prof_dum7 cat_prof_dum8 $Table_others if common_sample==1 & sexo_est*VA*tiempo_rta!=., group(sexo_prof) mname(simple_diff)
MeanDiff $Table_est $Table_prof cat_prof_dum2 cat_prof_dum4 cat_prof_dum6 cat_prof_dum7 cat_prof_dum8 $Table_others if common_sample==1 & sexo_est*VA*tiempo_rta!=., group(sexo_prof) cluster(id_curso) mname(cluster_diff)
*/
MeanDiff $Table_est $Table_prof cat_prof_dum2 cat_prof_dum4 cat_prof_dum6 cat_prof_dum7 cat_prof_dum8 $Table_others if common_sample==1 & sexo_est*VA*tiempo_rta!=., group(sexo_prof) cluster(id_curso) absorb(facultad_curso) mname(facultadFE_diff)



count if common_sample==1 & sexo_est*VA*tiempo_rta!=. & sexo_prof==1
local Num_muj: display %12.0f `=r(N)'

clear 
svmat float facultadFE_diff

format facultadFE_diff2-facultadFE_diff7 %9.3f

sum facultadFE_diff1
local Num_obs : display %12.0f `=r(mean)'
local Num_hom = `Num_obs'-`Num_muj'

drop facultadFE_diff1 facultadFE_diff3 facultadFE_diff5

ren (facultadFE_diff*) (Media_H Media_M Diferencia Pval)

gen Diferencia_s=string(Diferencia,"%7.3f")

foreach var in Media_H Media_M{

	gen `var'_s=string(`var',"%7.2f")

}

replace Diferencia_s=Diferencia_s+"*" if Pval<0.1
replace Diferencia_s=Diferencia_s+"*" if Pval<0.05
replace Diferencia_s=Diferencia_s+"*" if Pval<0.01

drop Pval

gen row=_n 
gen variable=""
local j = 1
foreach x in "Sexo estudiante" "Edad estudiante" "Semestres cursados" "Carrera STEM" "Créditos inscritos" "Promedio del semestre" "Nota del curso" "Créditos del curso" "Tiempo de respuesta (mins)" "Escribe comentario" "Antigüedad del profesor" "Edad profesor" "Valor agregado profesor" "Profesor activo" "Asistente graduado" "Profesor de cátedra" "Profesor asistente" "Profesor asociado" "Profesor titular" "N. de estudiantes" "\% estudiantes mujeres"{
	replace var = "`x'" in `j'
	local j = `j'+1
}

drop row Diferencia

order variable Media_H Media_M Diferencia
set obs 22
replace variable="Observaciones (Totales)" in `=_N'
replace Media_M_s="`Num_muj'" in `=_N'
replace Media_H_s="`Num_hom'" in `=_N'
replace Diferencia_s="`Num_obs'" in `=_N'

drop Media_H Media_M
ren (Media_H_s Media_M_s Diferencia_s) (Media_H Media_M Diferencia)

order variable Media_H Media_M Diferencia

foreach var in variable Media_H Media_M{
	replace `var'=`var'+" & "
}

preserve
	keep in 12/22

	rename (variable Media_H Media_M Diferencia) (variable2 Media_H2 Media_M2 Diferencia2)

	gen row=_n

	tempfile mitad2
	save `mitad2'
restore

keep in 1/11

gen space=" & "

gen row=_n
merge 1:1 row using `mitad2'

drop _merge row

replace Diferencia=Diferencia+" & "
replace Diferencia2=Diferencia2+" \\ "

set obs `=_N+1'
replace variable = "\bottomrule \bottomrule" in `=_N'
set obs `=_N+1'
replace variable="\end{tabular}" in `=_N'
set obs `=_N+1'
replace variable="\begin{tablenotes}"  in `=_N'
set obs `=_N+1'
replace variable="\footnotesize" in `=_N'
set obs `=_N+1'
replace variable="\item Diferencia de medias obtenida de una regresión lineal con efectos fijos de facultad del curso. Pruebas de hipótesis realizadas con errores estándar con cluster a nivel de curso. La diferencia de medias del tiempo de respuesta restringe las evaluaciones que se respondieron en menos de 3 horas debido a que es común que muchos estudiantes comiencen a responder un día y terminen en otro. Aunque el promedio de tiempo de respuesta es de 16 horas, el  85\% de las encuestas se responden en menos de media hora y el 87.5\% en menos de 3 horas. * p$<$0.10, ** p$<$0.05, *** p$<$0.01." in `=_N'
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
replace variable="\begin{tabular}{l*{3}{C{1.8cm}}C{0.4cm}l*{3}{C{1.8cm}}}" in 2
replace variable="\toprule \toprule" in 3
replace variable="& Prof. Hombres & Prof. Mujeres & Diferencia & & & Prof. Hombres & Prof. Mujeres & Diferencia \\ [5mm]" in 4
replace variable="\cline{1-4} \cline{6-9}" in 5

append using `lower'

outsheet variable Media_H Media_M Diferencia space variable2 Media_H2 Media_M2 Diferencia2 using "$git_documento/Tablas/Tabla Descriptiva.tex" , replace nonames delim("") noquote
