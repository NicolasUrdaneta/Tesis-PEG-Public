********************************************************************************
**																			  **
**						Sentiment analysis descriptive figure				  **
**																			  **
********************************************************************************
**																			  **
**		This program creates the sentiment analysis descriptive stats		  **
**																			  **
********************************************************************************

**** Tabla de probabilidad promedio de sentimiento por categoría de comentarios
**** Esta tabla es para validar que la forma de sacar los sentimientos funciona
**** muy bien

import delimited "Processed\Comments sentiments.csv", encoding(UTF-8) clear 

drop periodo unnamed0 v1

gen id_pregunta = "_curs_pos" if id=="P216"
replace id_pregunta = "_curs_mej" if id=="P217"
replace id_pregunta = "_prof_pos" if id=="P216V01"
replace id_pregunta = "_prof_mej" if id=="P217V01"

matrix results_avg = J(4,3,.)
matrix results_se = J(4,3,.)

matrix rownames results_avg = Curs_pos_avg Curs_mej_avg Prof_pos_avg Prof_mej_avg
matrix rownames results_se = Curs_pos_sd Curs_mej_sd Prof_pos_sd Prof_mej_sd
matrix colnames results_avg = positive_avg negative_avg neutral_avg
matrix colnames results_se = positive_sd negative_sd neutral_sd


local j=1
foreach var in positive negative neutral{
	local k=1
	foreach preg in _curs_pos _curs_mej _prof_pos _prof_mej{

		sum `var' if id_pregunta=="`preg'",d
		matrix results_avg[`k',`j']=r(mean)
		matrix results_se[`k',`j']=r(sd)/sqrt(r(N))

	local k = `k'+1
	}
local j = `j'+1
} 

ttest negative=neutral if id_pregunta=="_prof_mej"



preserve

clear
svmat results_avg , names(col)
svmat results_se , names(col)

gen row=_n
label define vals 1 "Aspectos positivos del curso" 2 "Aspectos por mejorar del curso" 3 "Aspectos positivos del profesor" 4 "Aspectos por mejorar del profesor"
label values row vals

rename (positive_* negative_* neutral_*) (*_positive *_negative *_neutral)

reshape long avg sd, i(row) j(sentiment) string

encode sentiment, gen(sents) 

if "$fig_title"=="ON"	local title1 = "Probabilidad promedio que un comentario sea positivo, neutro"
if "$fig_title"=="ON"	local title2 = "o negativo por categorías de comentario"

graph bar avg, by(row, title("`title1'" "`title2'") note("")) over(sents) sch(tufte) legend(r(1) label(1 "Negativo") label(2 "Neutro") label(3 "Positivo"))  asyvars bar(1, fcolor(black) lc(black)) bar(2, fcolor(gs4) lc(black)) bar(3, fcolor(gs8)) ytitle(Probabilidad) name(probs, replace) blabel(total, format(%9.2f) size(vsmall))

gr display, xsize(20) ysize(16)
gr export "${git_documento}/Graficas/Sentiment analysis averages.pdf", replace

restore


preserve

clear
svmat results_avg , names(col)


gen row=_n


rename (positive_* negative_* neutral_*) (*_positive *_negative *_neutral)

reshape long avg, i(row) j(sentiment) string
reshape wide avg, j(row) i(sentiment)

replace sentiment="Prob. positivo" if sentiment=="_positive"
replace sentiment="Prob. neutro" if sentiment=="_neutral"
replace sentiment="Prob. negativo" if sentiment=="_negative"

gsort -sentiment

foreach var of varlist avg1 avg2 avg3 avg4{

	gen `var'_s=string(`var',"%7.2f")
	if "`var'"!="avg4" replace `var'_s=`var'_s+" & "
	if "`var'"=="avg4" replace `var'_s=`var'_s+" \\ "
	drop `var'
	

}

replace sentiment = sentiment + " & "

set obs `=_N+1'
replace sentiment = "\bottomrule \bottomrule" in `=_N'
set obs `=_N+1'
replace sentiment ="\end{tabular}" in `=_N'
set obs `=_N+1'
replace sentiment ="\begin{tablenotes}"  in `=_N'
set obs `=_N+1'
replace sentiment="\footnotesize" in `=_N'
set obs `=_N+1'
replace sentiment="\item Probabilidad promedio de clasificar un comentario como positivo, neutro o negativo dependiendo de las categorías de comentarios. Promedios estadísticamente distintos entre sí." in `=_N'
set obs `=_N+1'
replace sentiment="\end{tablenotes}" in `=_N'
set obs `=_N+1'
replace sentiment="\end{threeparttable}"  in `=_N'

tempfile lower
save `lower'

clear 
set obs 5
gen sentiment=""
replace sentiment="\begin{threeparttable}" in 1
replace sentiment="\begin{tabular}{l*{4}{C{2.5cm}}}" in 2
replace sentiment="\toprule \toprule" in 3
replace sentiment="& Aspectos positivos del curso & Aspectos por mejorar del curso & Aspectos positivos del profesor & Aspectos por mejorar del profesor \\ [5mm]" in 4
replace sentiment="\cline{2-5}" in 5

append using `lower'

outsheet sentiment avg* using "$git_documento/Tablas/Tabla Descriptiva Sentiments.tex" , replace nonames delim("") noquote


restore


