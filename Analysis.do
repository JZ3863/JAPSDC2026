cd "output directory"

/*******************************************************
* This file used older version (1.8) of csdid: 
cap ado uninstall csdid

net install csdid, ///
    from("https://raw.githubusercontent.com/friosavila/stpackages/main") replace
********************************************************/
use an, clear

encode ID, gen(id)
encode provincename, gen(province)
* relative time
g t = 1 if year == 2013
replace t = 2 if year == 2015
replace t = 3 if year == 2018
replace t = 4 if year == 2020

tsset id t

**# variables
**### sex
recode sex (1 = 0 "Female") (2 = 1 "Male"), gen(sex_)
drop sex
ren sex_ sex
**### uban
recode urban (1 = 0 "Rural") (2 = 1 "Urban"), gen(urban_)
drop urban
ren urban_ urban
**### marital
recode maritalc (3/6 = 0 "Single") ( 1/2= 1 "Married/cohabiting"), gen(marital)

**### education
recode educ (1 = 0 "Illiterate") (2/5 = 1 "Elementary/middle school") (6/11 = 2 "High school and above"), gen(edu3)

**### individual income
g pcincome = hhincome / hhsize

**### ADL & IADL
recode ADL (0 = 0 "No") (1/6 = 1 "Yes"), gen(dADL)
recode IADL (0 = 0 "No") (1/6 = 1 "Yes"), gen(dIADL)

foreach i of var dADL dIADL {
	bys ID (year): g temp_`i'1 = `i' if year == 2011
	bys ID: egen `i'1 = max(temp_`i'1)
	bys ID (year): g temp_`i'2 = `i' if year == 2013
	bys ID: egen `i'2 = max(temp_`i'2)
	bys ID (year): g temp_`i'3 = `i' if year == 2015
	bys ID: egen `i'3 = max(temp_`i'3)
	bys ID (year): g temp_`i'4 = `i' if year == 2018
	bys ID: egen `i'4 = max(temp_`i'4)
	bys ID (year): g temp_`i'5 = `i' if year == 2020
	bys ID: egen `i'5 = max(temp_`i'5)
}
drop temp_dADL* temp_dIADL*

**### life satisfaction, care total, depress
recode life (1/2 = 0 "Dissatisfied") (3/5 = 1 "Satisfied"), gen(life1)

foreach i of var caretotal carehourc carehourd caredayc caredayd {
	g ln`i' = log(`i') if `i' != 0
	replace ln`i' = log(1) if `i' == 0
}

recode depress (0/9 = 0 "No depressed") (10/30 = 1 "depressed"), g(depress1) 

**### unmet needs
recode unmetADL (0 = 0 "No") (1/6 = 1 "Yes"), gen(dunmetADL)
recode unmetIADL (0 = 0 "No") (1/6 = 1 "Yes"), gen(dunmetIADL)

**### health insurance
* create consistent medical insurance across years
bys ID: egen hinsurancemode = mode(hinsurance), min 
label define hinsurancemode_lbl ///
    1  "UEBMI"   /* Urban Employee Basic Medical Insurance */ ///
    2  "URRBMI"  /* Urban & Rural Resident Basic Medical Insurance */ ///
    3  "URBMI"   /* Urban Resident Basic Medical Insurance */ ///
    4  "NRCMS"   /* New Rural Cooperative Medical Scheme */ ///
    5  "Gongfei" /* Government medical insurance */ ///
    6  "Aid"     /* Medical aid */ ///
    7  "Dabing"  /* Urban non-employed persons' health insurance */ ///
    8  "LTCI"    /* Long-term care insurance */ ///
    9  "Other"   /* Other */ ///
    10 "No insurance" /* No insurance */
lab val hinsurancemode hinsurancemode_lbl
ta hinsurancemode, m

**# Sample selection
**## All 5 waves (balanced panel)
distinct ID // No. of unique ID = 25882; No. of obs = 129410
* remove wave 1 (2011)
drop if year == 2011
drop ID_w1 householdID_w1 r1iwm
* remove not interviewed
drop if inw == "No"
distinct ID // No. of unique ID = 24776; No. of obs = 79705

**## policy timing & eligibility
**### policy timing
* year
g first_year_treat = 2016 if inlist(cityname, "承德市", "上饶市")
replace first_year_treat = 2017 if inlist(cityname, "安庆市", "荆门市", "广州市", ///
		"成都市", "徐州市", "齐齐哈尔市") | inlist(cityname, "上海市","苏州市", ///
		"济南市", "临沂市", "聊城市", "重庆市")
replace first_year_treat = 2018 if inlist(cityname, "德州市", "滨州市", "枣庄市", ///
		"威海市")
replace first_year_treat = 2015 if inlist(cityname, "潍坊市")
replace first_year_treat = 2012 if inlist(cityname, "青岛市")

* month
g first_month_treat = 1 if inlist(cityname, "安庆市", "荆门市", "德州市", "潍坊市")
replace first_month_treat = 11 if inlist(cityname, "承德市","上饶市")
replace first_month_treat = 8 if inlist(cityname, "广州市", "济南市","临沂市")
replace first_month_treat = 7 if inlist(cityname, "成都市","徐州市","威海市", "青岛市")
replace first_month_treat = 10 if inlist(cityname, "齐齐哈尔市", "聊城市")
replace first_month_treat = 6 if inlist(cityname, "枣庄市")
replace first_month_treat = 12 if inlist(cityname, "重庆市")

**### create never treated based on city and eligibility criteria
* disability
g disability = (ADL > 0 & ADL < .) | (IADL > 0 & IADL < .) 
lab var disability "Eligibility: IADL + ADL + cognition"

* cognition
g min_cog = cognition < 6 & ///
    (cityname == "青岛市" | cityname == "上海市" | cityname == "成都市" | ///
     cityname == "广州市" | cityname == "上饶市" | cityname == "济南市" | ///
     cityname == "威海市")
replace disability = 1 if disability == 0 & min_cog == 1

* health insurance eligibility
**Qingdao and Shangrao residents
replace first_year_treat = 2015 if cityname=="青岛市" & hinsurancemode == 4 
replace first_month_treat = 1 if cityname=="青岛市" & hinsurancemode == 4
replace first_year_treat = 2017 if cityname=="上饶市" & hinsurancemode != 1 & ///
	hinsurancemode != 5 
replace first_month_treat = 1 if cityname=="上饶市" & hinsurancemode != 1 & ///
	hinsurancemode != 5 

* old: exclude shanghai age < 60
bys ID: egen max_age = max(age)
replace first_year_treat = . if cityname == "上海市" & max_age < 60

*replace first time to treat to missing 
foreach i of var first_year_treat first_month_treat {    
replace `i' = . if hinsurancemode > 5
replace `i' = . if (cityname == "承德市" | cityname == "安庆市" | cityname == "广州市" | ///
                   cityname == "成都市" | cityname == "德州市" | cityname == "滨州市" | ///
                   cityname == "齐齐哈尔市" | cityname == "济南市" | cityname == "枣庄市" | ///
                   cityname == "潍坊市" | cityname == "威海市" | cityname == "临沂市" | ///
                   cityname == "聊城市" | cityname == "重庆市") ///
                   & hinsurancemode != 1 & hinsurancemode != 5
}

* create first_year_treat if id disability first becomes 1
bys ID: egen temp_first_year = min(cond(disability == 1 & first_year_treat != ., year, .))
replace first_year_treat = temp_first_year if temp_first_year != . & temp_first_year > first_year_treat
replace first_month_treat = . if first_year_treat == .

* never treated based on never treated cities
g never_treat = (first_month_treat == .)
drop temp*

**### Eligibility: Self-directed care vs. agency-directed care
g treat = first_year_treat != .

* SDC
replace treat = 2 if treat == 1 & ///
    (cityname == "承德市" | cityname == "安庆市" | cityname == "上饶市" | ///
	cityname == "荆门市" | cityname == "广州市" | cityname == "成都市" | ///
	cityname == "徐州市" | cityname == "德州市" | cityname == "滨州市")

lab def treat_lbl ///
	0 "Never treated" ///
	1 "ADC" ///
	2 "SDC"
lab val treat treat_lbl

* group (relative to t)
g group = 3 if first_year_treat >=2016 & first_year_treat != .
replace group = 0 if group == .

**## trim sample
**### Step 1: Keep IDs with insurance = UEBMI, URRBMI, URBMI, NRCMS, Gongfei
distinct ID if hinsurancemode > 5 // 769; 1567
drop if hinsurancemode > 5
distinct ID // 24007; 78138
**### Step 2: IDs who appear only once in the panel
bys ID: g id_count = _N
g single_obs = (id_count == 1)
distinct ID if single_obs == 1 // n = 2098
drop if single_obs == 1
drop id_count single_obs
distinct ID //21909;76040
**### Step 3: delete those age < 45 and missing age Pei (2024)
distinct ID if max_age < 45 | age > 200 //No. of unique ID = 162; No. of obs = 452
drop if max_age < 45 | age > 200

**### Step 4: delete those without disability after 2015
bys ID: egen max_disability15 = max(cond(year > 2015, disability, .))
distinct ID if max_disability15 != 1 //No. of unique ID = 12788; No. of obs = 42452
drop if max_disability15 != 1 

**#### step 4.2: replace unmetADL with missing if ADL = . 
distinct ID if ADL ==. & unmetADL != . // n = 7
replace unmetADL =. if ADL == . & unmetADL != .

**### Step 5: delete individuals who receive LTCI before 2015
distinct ID if first_year_treat <=2015 //No. of unique ID = 22; No. of obs = 84
drop if first_year_treat <= 2015

**### Step 6: final sample
distinct ID // No. of unique ID = 8937; No. of obs = 33052

* treat variables
recode treat (0 = 0 "Never treated") (1 = .) (2 = 1 "SDC"), gen(treat_sc)
lab var treat_sc "SDC vs control"
recode treat (0 = 0 "Never treated") (1 = 1 "ADC") (2 = .), gen(treat_ac)
lab var treat_sc "ADC vs control"

**# csdid: SDC vs control
cap drop sc_xtpccon* sample_sc
gl C0 age i.sex i.marital i.edu3 i.urban hhsize child ADL dIADL i.selfhealth
gl C $C0 i.sc_xtpcconsume

qui {
csdid life $C0 if treat != 1, ivar(id) time(t) gvar(group) cl(city)
g sample_sc = e(sample)
egen sc_xtpcconsume_ = xtile(pcconsumption) if sample_sc == 1, n(4) by(year)
recode sc_xtpcconsume_ (. = 5 "Missing") if sample_sc == 1, gen(sc_xtpcconsume)
}

qui teffects aipw (life $C) (treat_sc $C) if e(sample) & year < 2018, atet vce(cl ID)
tebalance summarize
mat balance = r(table)
coefplot (matrix(balance[,1]), offset(-.01) msymbol(Oh) mcolor(black)) ///
	(matrix(balance[,2]), msymbol(O) mcolor(black)), ///
	xline(0 -.1 .1, lpattern(dash) lwidth(thin)) xtitle("Standardized difference") ///
	legend(order(1 "Raw" 2 "Weighted") pos(6) ring(1) cols(2)) noci ///
	xlabel(-.2 "-0.2" -.1 "-0.1"  0 "0" .1 "0.1" .2 "0.2" .4 "0.4" .6 "0.6" .8 "0.8", nogrid) ///
	ylabel( ///
        1  "Age" ///
        2  "Gender" ///
        3  "Marital status" ///
        4  "Education: elementary/middle school" ///
        5  "Education: high school and above" ///
        6  "Residence" ///
        7  "Household size" ///
        8  "Number of children" ///
        9  "ADLs" ///
        10 "IADLs" ///
        11 "Health: poor" ///
        12 "Health: fair" ///
        13 "Health: good" ///
        14 "Health: very good" ///
        15 "Expenditure: 2nd quartile" ///
        16 "Expenditure: 3rd quartile" ///
        17 "Expenditure: 4th quartile" ///
        18 "Expenditure: missing", angle(horizontal)) grid(none) ///
		yline(1/18, lwidth(vthin) lcolor(gs12))

csdid unmetADL $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
ta treat if e(sample)
ta city if e(sample)
distinct city if e(sample) // 122
distinct city if e(sample) & group == 3 //9
matrix b_unmet = e(b)[1, "Tm1".."Tp1"]
matrix V_unmet = e(V)["Tm1".."Tp1", "Tm1".."Tp1"]
est store sdc_unmetADL

csdid lncaretotal $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
matrix b_care = e(b)[1, "Tm1".."Tp1"] 
matrix V_care = e(V)["Tm1".."Tp1", "Tm1".."Tp1"] 
est store sdc_caretotal

csdid life1 $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
matrix b_care = e(b)[1, "Tm1".."Tp1"]
matrix V_care = e(V)["Tm1".."Tp1", "Tm1".."Tp1"] 
est store sdc_life1

csdid depress1 $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
matrix b_care = e(b)[1, "Tm1".."Tp1"] 
matrix V_care = e(V)["Tm1".."Tp1", "Tm1".."Tp1"] 
est store sdc_depress1

**# csdid: ADC vs control
cap drop ac_xtpccon* sample_ac
g sample_ac = 1 if (sample_sc == 1 & treat ! = 2) | treat == 1
egen ac_xtpcconsume_ = xtile(pcconsumption) if sample_ac == 1, n(4) by(year)
recode ac_xtpcconsume_ (. = 5 "Missing"), gen(ac_xtpcconsume)
gl Ca $C0 i.ac_xtpcconsume

qui teffects aipw (life $Ca) (treat_ac $Ca) if sample_ac == 1 & year < 2018, pstolerance(1e-6) atet vce(cl ID) 
tebalance summarize
mat balance = r(table)
coefplot (matrix(balance[,1]), offset(-.01) msymbol(Oh) mcolor(black)) ///
	(matrix(balance[,2]), msymbol(O) mcolor(black)), ///
	xline(0 -.1 .1, lpattern(dash) lwidth(thin))  xtitle("Standardized difference") ///
	legend(order(1 "Raw" 2 "Weighted") pos(6) ring(1) cols(2)) noci ///
	xlabel(-.5 "-0.5" -.1 "-0.1" 0 "0" .1 "0.1" .5 "0.5" 1 "1", nogrid) ///
	ylabel( ///
        1  "Age" ///
        2  "Gender" ///
        3  "Marital status" ///
        4  "Education: elementary/middle school" ///
        5  "Education: high school and above" ///
        6  "Residence" ///
        7  "Household size" ///
        8  "Number of children" ///
        9  "ADLs" ///
        10 "IADLs" ///
        11 "Health: poor" ///
        12 "Health: fair" ///
        13 "Health: good" ///
        14 "Health: very good" ///
        15 "Expenditure: 2nd quartile" ///
        16 "Expenditure: 3rd quartile" ///
        17 "Expenditure: 4th quartile" ///
        18 "Expenditure: missing", angle(horizontal)) grid(none) ///
		yline(1/18, lwidth(vthin) lcolor(gs12))

csdid unmetADL $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
ta treat if e(sample) //410
matrix b_unmet = e(b)[1, "Tm1".."Tp1"] 
matrix V_unmet = e(V)["Tm1".."Tp1", "Tm1".."Tp1"]
est store adc_unmetADL

csdid lncaretotal $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
matrix b_unmet = e(b)[1, "Tm1".."Tp1"] 
matrix V_unmet = e(V)["Tm1".."Tp1", "Tm1".."Tp1"]
est store adc_caretotal

csdid life1 $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
matrix b_unmet = e(b)[1, "Tm1".."Tp1"] 
matrix V_unmet = e(V)["Tm1".."Tp1", "Tm1".."Tp1"]
est store adc_life1

csdid depress1 $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
matrix b_unmet = e(b)[1, "Tm1".."Tp1"] 
matrix V_unmet = e(V)["Tm1".."Tp1", "Tm1".."Tp1"]
est store adc_depress1

**## plot trends
coefplot (sdc_unmetADL, msymbol(O) mcolor(blue) ciopts(recast(rcap) lcolor(blue) lpattern(solid)) lwidth(thin)) /// 
         (sdc_caretotal, msymbol(T) mcolor(dkgreen) ciopts(recast(rcap) lcolor(dkgreen) lpattern(solid)) lwidth(thin)) ///
		 (sdc_life1, msymbol(S) mcolor(red) ciopts(recast(rcap) lcolor(red) lpattern(solid)) lwidth(thin)) ///
		 (sdc_depress1, msymbol(D) mcolor(purple) ciopts(recast(rcap) lcolor(purple) lpattern(solid)) lwidth(thin)), ///
    keep(Tm1 Tp0 Tp1) vert graphregion(color(white)) /// 
    yline(0, lcolor(black) lwidth(thin) lpattern(dash)) ///
    xline(1.4, lcolor(black) lwidth(thin)) ///
    coeflabels(Tm1 = "2015" Tp0 = "2018" Tp1 = "2020") ///
	legend(order(2 "Unmet needs" 4 "Care hours" 6 "Life satisfaction" 8 "Depression") position(6) ///
			symxsize(*0.5) keygap(*0.5) rows(1)) ///
    ytitle("ATT", size(medsmall) margin(r=-1)) /// 
	ylabel( -.6 "-0.6" -.4 "-0.4" -.2 "-0.2" 0 "0" .2 "0.2",labsize(medsmall) nogrid) ///
    text(-.71 1.4 "2016", placement(s) size(medsmall)) ///
	text(-.71 3.4 "Year", placement(s) size(medsmall) justification(right)) ///
	title("(a) Self-directed care versus untreated", size(medsmall))
graph save "SDC.gph", replace

coefplot (adc_unmetADL, msymbol(O) mcolor(blue) ciopts(recast(rcap) lcolor(blue) lpattern(solid)) lwidth(thin)) /// 
         (adc_caretotal, msymbol(T) mcolor(dkgreen) ciopts(recast(rcap) lcolor(dkgreen) lpattern(solid)) lwidth(thin)) ///
		 (adc_life1, msymbol(S) mcolor(red) ciopts(recast(rcap) lcolor(red) lpattern(solid)) lwidth(thin)) ///
		 (adc_depress1, msymbol(D) mcolor(purple) ciopts(recast(rcap) lcolor(purple) lpattern(solid)) lwidth(thin)), ///
    keep(Tm1 Tp0 Tp1) vert  graphregion(color(white)) /// 
    yline(0, lcolor(black) lwidth(thin) lpattern(solid)) ///
    xline(1.4, lcolor(black) lwidth(thin)) ///
    coeflabels(Tm1 = "2015" Tp0 = "2018" Tp1 = "2020") ///
	legend(order(2 "Unmet needs" 4 "Care hours" 6 "Life satisfaction" 8 "Depression") position(6) ///
			symxsize(*.5) keygap(*.5) rows(1)) ///
    ytitle("ATT", size(medsmall) margin(r=-1)) /// 
	ylabel(-.5 "-0.5" 0 "0" .5 "0.5", labsize(medsmall) nogrid) ///
    text(-.71 1.4 "2016", placement(s) size(medsmall)) ///
	text(-.71 3.4 "Year", placement(s) size(medsmall) justification(right)) ///
	title("(b) Agency-directed care versus untreated", size(medsmall))
graph save "ADC.gph", replace
grc1leg "SDC" "ADC", rows(2)

**# sensitivity analysis
**## placebo effects
* SDC
cap drop treatcdc_pla group_pla max_treatcdc_pla
g treatcdc_pla = 1 if sample_sc == 1 & inlist(cityname, "北京", "天津", "昆明市", "呼和浩特市", "汉中市")
replace treatcdc_pla = 0 if sample_sc ==1 & inlist(cityname, "天津", "昆明市", "汉中市") & (hinsurancemode != 1 & hinsurancemode != 5)
replace treatcdc_pla = 1 if cityname == "昆明市" & min_cog == 1 & (hinsurancemode == 1 | hinsurancemode == 5)
replace treatcdc_pla = 0 if sample_sc == 1 & treatcdc_pla != 1
bys id: egen max_treatcdc_pla = max(treatcdc_pla)
g group_pla = 3 if max_treatcdc_pla == 1
replace group_pla = 0 if group_pla == .

csdid unmetADL $C if sample_sc == 1, ivar(id) time(t) gvar(group_pla) cl(city) agg(event)
csdid lncaretotal $C if sample_sc == 1, ivar(id) time(t) gvar(group_pla) cl(city) agg(event)
csdid life $C if sample_sc == 1, ivar(id) time(t) gvar(group_pla) cl(city) agg(event) 
csdid depress1 $C if sample_sc == 1, ivar(id) time(t) gvar(group_pla) cl(city) agg(event)

* ADC
cap drop treatadc_pla group_pla2 max_treatadc_pla
g treatadc_pla = 1 if sample_ac == 1 & inlist(cityname, "吉林市", "宁波市", "福州市", "南宁市")
replace treatadc_pla = 0 if sample_ac == 1 & inlist(cityname, "宁波市", "福州市", "南宁市") & (hinsurancemode != 1 & hinsurancemode != 5)
replace treatadc_pla = 0 if sample_ac == 1 & treatadc_pla ! = 1
bys id: egen max_treatadc_pla = max(treatadc_pla)
g group_pla2 = 3 if max_treatadc_pla == 1
replace group_pla2 = 0 if group_pla2 == .

csdid unmetADL $Ca if sample_ac == 1, ivar(id) time(t) gvar(group_pla2) cl(city) agg(event)
csdid lncaretotal $Ca if sample_ac == 1, ivar(id) time(t) gvar(group_pla2) cl(city) agg(event)
csdid life $Ca if sample_ac == 1, ivar(id) time(t) gvar(group_pla2) cl(city) agg(event)
csdid depress1 $Ca if sample_ac == 1, ivar(id) time(t) gvar(group_pla2) cl(city) agg(event)

**## csdid: all vs control
g sample_full = sample_sc == 1 | sample_ac == 1
egen fu_xtpcconsume_ = xtile(pcconsumption) if sample_full == 1, n(4) by(year)
recode fu_xtpcconsume_ (. = 5 "Missing"), gen(fu_xtpcconsume)
gl Cb $C0 i.fu_xtpcconsume

qui csdid unmetADL $Cb if sample_fu == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
cap drop sample_des
g sample_des = e(sample)

**## additional outcomes
recode helpspousem (2 = 1 "Yes") (else = 0 "No"), gen(helpspouse)
recode helpchildm (2 = 1 "Yes") (else = 0 "No"), gen(helpchid)
* SDC
csdid dunmetADL $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid carehourd $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid lncaredayc $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid livein $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid helpspouse $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid helpchid $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid life $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid depress $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)

csdid dunmetIADL $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)

* ADC
csdid dunmetADL $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid carehourd $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid lncaredayc $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid livein $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid helpspouse $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid helpchid $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid life $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid depress $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid dunmetIADL $Ca if sample_ac == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)

**## other counfounding policy (https://doi.org/10.1016/j.aggp.2024.100082)
g integrate = 0
replace integrate = 1 if inlist(cityname, "上海市", "保定市", "兰州市", "北京", "南宁市")
replace integrate = 1 if inlist(cityname, "南昌市", "呼和浩特市", "哈尔滨", "哈尔滨市", "嘉兴市")
replace integrate = 1 if inlist(cityname, "大连市", "天津", "威海市", "岳阳市", "广州市")
replace integrate = 1 if inlist(cityname, "昆明市", "杭州市", "江门市", "洛阳市", "海东地区")
replace integrate = 1 if inlist(cityname, "深圳市", "漳州市", "濮阳市", "石家庄市", "苏州市")
replace integrate = 1 if inlist(cityname, "赣州市", "郑州市", "重庆市", "长沙市", "青岛市")
replace integrate = 1 if cityname == "齐齐哈尔市"


csdid unmetADL $C integrate if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid life1 $C integrate if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid lncaretotal $C integrate if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)
csdid depress1 $C integrate if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event)


**## Placebo test
compress
save temp, replace

use temp, clear
keep if sample_sc == 1

cap program drop InSpacePlaceboTest
program define InSpacePlaceboTest, rclass
    preserve

    cap drop group_sc_new

    xtshuffle group, id(city) time(t) gen(group_sc_new)

    qui csdid unmetADL $C, ///
        ivar(id) time(t) gvar(group_sc_new) cl(city) agg(simple)

    return scalar pbo_eff = _b[ATT]
    return scalar pbo_z   = _b[ATT]/_se[ATT]
    return scalar pbo_p   = 2*(1-normal(abs(_b[ATT]/_se[ATT])))

    restore
end

parallel initialize 10
parallel sim, reps(500) seed(1 1 1 1 1 1 1 1 1 1) ///
    expression(pbo_eff = r(pbo_eff) pbo_z = r(pbo_z) pbo_p = r(pbo_p)) : ///
    InSpacePlaceboTest

save unmet_InSpacePbo, replace

use unmet_InSpacePbo, clear
g extreme_abs = (abs(pbo_eff)>= .1058454)
g extreme = (pbo_eff<= -.1058454)
su extreme_abs extreme
graph twoway (kdensity pbo_eff) (histogram pbo_eff, color(green%10)), ///
     xline(0 -.1058454, lc(black) lp(dash)) ///
      title("Unmet needs", size(medium)) ///
	  xlabel(-.1 "-0.1" 0 "0" .1 "0.1" , nogrid) ylabel(,nogrid) ///
	  ytitle("Density") xtitle("Distribution of placebo effect") ///
	  leg(order(1 "Kernel density estimate" 2 "Histogram") position(6) ring(1) rows(1))

graph save FB51, replace

* life satisfaction
use temp, clear
keep if sample_sc == 1

cap program drop InSpacePlaceboTest
program def InSpacePlaceboTest, rclass
    preserve

    cap drop group_sc_new
    xtshuffle group, id(city) time(t) gen(group_sc_new)

    qui csdid life1 $C, ///
        ivar(id) time(t) gvar(group_sc_new) cl(city) agg(simple)

    scalar b_att  = _b[ATT]
    scalar se_att = _se[ATT]
    scalar z_att  = b_att / se_att
    scalar p_att  = 2 * (1 - normal(abs(z_att)))

    return scalar pbo_eff = b_att
    return scalar pbo_z   = z_att
    return scalar pbo_p   = p_att

    restore
end

parallel initialize 10
parallel sim, reps(500) seed(1 1 1 1 1 1 1 1 1 1) ///
    expression(pbo_eff = r(pbo_eff) pbo_z = r(pbo_z) pbo_p = r(pbo_p)) : ///
    InSpacePlaceboTest
save life_InSpacePbo, replace

use life_InSpacePbo, clear
g extreme_abs = (abs(pbo_eff)>= .0436091)
g extreme = (pbo_eff>= .0436091)
su extreme_abs extreme
graph twoway (kdensity pbo_eff) (histogram pbo_eff, color(green%10)), ///
     xline(0 .0436091, lc(black) lp(dash)) ///
      title("Life satisfaction", size(medium)) ///
	  xlabel(-.05 "-0.05" 0 "0" .05 "0.05", nogrid) ylabel(,nogrid) ///
	  ytitle("Density") xtitle("Distribution of placebo effect") ///
	  leg(order(1 "Kernel density estimate" 2 "Histogram") position(6) ring(1) rows(1))

graph save FB52, replace

graph combine "FB51" "FB52"

graph export "fb5.png", width(13000) replace

**## wild bootstrap

csdid unmetADL $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event) wboot rseed(10) pointwise

csdid life1 $C if sample_sc == 1, ivar(id) time(t) gvar(group) cl(city) agg(event) wboot rseed(1) pointwise 

**# Descriptive
use temp, clear
gl des unmetADL caretotal life1 depress age sex marital i.edu3 urban hhsize child ///
		i.fu_xtpcconsume ADL dIADL i.selfhealth
bys year: su $des if sample_des == 1 

dtable $des if sample_des == 1 & year == 2013, by(treat, notot) factor(,statistics(fvperc)) nformat(%9.2f) export(tab2013.xlsx, replace)
dtable $des if sample_des == 1 & year == 2015, by(treat, notot) factor(,statistics(fvperc)) nformat(%9.2f) export(tab2015.xlsx, replace)
dtable $des if sample_des == 1 & year == 2018, by(treat, notot) factor(,statistics(fvperc)) nformat(%9.2f) export(tab2018.xlsx, replace)
dtable $des if sample_des == 1 & year == 2020, by(treat, notot) factor(,statistics(fvperc)) nformat(%9.2f) export(tab2020.xlsx, replace)

dtable $des if sample_des == 1, by(treat, notot) factor($factor) nformat(%9.2f) export(tabb1.xlsx, replace)

**## Missing values of household consumption
recode fu_xtpcconsume (1/4 = 0 "Not missing") (5 = 1 "Missing"), gen(miss_consume)
g hhincome_ = hhincome / 1000000

logit miss_consume age sex i6.maritalc i2.edu3 urban i1.hhsize child ADL dIADL hhincome_ i.selfhealth unmetADL caretotal life1 depress group t i.city if sample_full == 1 & year < 2018, vce(cl province)

esttab using TableB2.csv, replace ///
	cells("b(fmt(3)) se(fmt(3))") wide ///
	drop(*.city _cons) ///
	nomtitles nonumber noobs




