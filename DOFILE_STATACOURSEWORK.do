//Introduction 

describe

codebook

browse

list

summarize

tabulate seniority_level gender


//Descriptive Analysis

//Question 1
//Two way table showing  Summary Statistics
tabstat annualsalary bonus totalexperience currentexperience, statistics(count mean min max sd) by(seniority_level) columns(statistics) longstub

//Correlation matrix
asdoc pwcorr annualsalary totalexperience currentexperience bonus, star(0.05)
graph matrix  annualsalary totalexperience currentexperience bonus, half

//Question 2

//Logged Version of annualsalary
gen log_AnnualSalary=log(annualsalary)
label variable log_AnnualSalary"Ln(AnnualSalary)"
browse

//Logged Version of Bonus
gen log_bonus=log(bonus)
label variable log_bonus"Ln(Bonus)"
browse

//Applying Statistical test
asdoc oneway log_bonus year
graph box log_bonus, over(year) ytitle(Logged Bonus) title(Logged Bonus by Year)


//Question 3

//Applying Statistical test
asdoc oneway log_AnnualSalary gender
graph box log_AnnualSalary , over(gender) ytitle(Logged Annual Salary) title(Logged Salary btw Gender)


//Question 4

//Applying Statistical test
asdoc oneway log_AnnualSalary seniority_level
graph box log_AnnualSalary , over(seniority_level)



//Exploratory Analysis

//Question 1

graph pie, over(seniority_level) pie(_all, explode) plabel(_all percent) title(% of Employee by Senority_level)

graph bar (mean) annualsalary , over(year) title( annualsalary Mean Per Year) blabel(bar, color(forest_green)) ytitle(Mean of annualsalary )

histogram annualsalary , normal title(Annual Salary) by( year )



twoway (scatter annualsalary currentexperience if seniority_level=="mid",mcolor(blue) msize(small) msymbol(circle))  (scatter annualsalary currentexperience if seniority_level=="entry",mcolor(green) msize(small) msymbol(circle)) (scatter annualsalary currentexperience if seniority_level== "senior",mcolor(red)  msize(small) msymbol(circle)) (lfit annualsalary currentexperience if seniority_level=="mid", lcolor(blue)) (lfit annualsalary currentexperience if seniority_level=="entry", lcolor(green)) (lfit annualsalary currentexperience if seniority_level== "senior", lcolor(red)), legend(order(1 "mid" 2 "entry" 3 "senior")) ytitle(Annual Salary)

twoway (scatter annualsalary totalexperience if seniority_level=="mid",mcolor(blue) msize(small) msymbol(circle))  (scatter annualsalary totalexperience if seniority_level=="entry",mcolor(green) msize(small) msymbol(circle)) (scatter annualsalary totalexperience if seniority_level== "senior",mcolor(red)  msize(small) msymbol(circle)) (lfit annualsalary totalexperience if seniority_level=="mid", lcolor(blue)) (lfit annualsalary totalexperience if seniority_level=="entry", lcolor(green)) (lfit annualsalary totalexperience if seniority_level== "senior", lcolor(red)), legend(order(1 "mid" 2 "entry" 3 "senior")) ytitle(Annual Salary)

twoway (scatter annualsalary bonus if seniority_level=="mid",mcolor(blue) msize(small) msymbol(circle))  (scatter annualsalary bonus if seniority_level=="entry",mcolor(green) msize(small) msymbol(circle)) (scatter annualsalary bonus if seniority_level== "senior",mcolor(red)  msize(small) msymbol(circle)) (lfit annualsalary bonus if seniority_level=="mid", lcolor(blue)) (lfit annualsalary bonus if seniority_level=="entry", lcolor(green)) (lfit annualsalary bonus if seniority_level== "senior", lcolor(red)), legend(order(1 "mid" 2 "entry" 3 "senior")) ytitle(Annual Salary)




//Question 2

//Average Salary Generated
bysort year: egen Salary_average=mean(annualsalary)
label variable Salary_average "AvgSalary"
browse

//Average Bonus Generated
bysort year: egen Bonus_average=mean(bonus)
label variable Bonus_average"AvgBonus"
browse

twoway (connected Salary_average year, mcolor(eltgreen) lcolor(green)) (connected Bonus_average year, mcolor(ltblue) msymbol(square) lcolor(yellow)), ytitle(Salary and Bonus) xtitle(Year) title(Salary_Bonus)


//Question 3

//Generated New Variable Status
gen status=0 if bonus<=2000
replace status=1 if bonus>2000 & !missing(bonus)
browse
graph bar Salary_average, by(status) ytitle(Average Salary) by(, title(Average Salary by Status))



//Main Regression Analysis

//Question 1

tab seniority_level
encode seniority_level, generate(seniority_level_dum)
browse

tab gender
encode gender, generate(gender_dum)
browse

// Baseline Model
asdoc reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum 


//Question 2 - Interpretation and discussion of OLS Model


//Question 3 - checking differential effect of seniority_level on log salary looking at the baseline model result.
margins seniority_level_dum
marginsplot, recastci(rarea) noci


//Question 4  - checking the differential effect of total work experience for males vs females
asdoc reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum c.totalexperience##i.gender_dum
margins gender_dum , at (totalexperience =(0 (2) 38))
marginsplot, recastci(rarea) noci


//Question 5 - Comparison between the two models
reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum i.gender_dum#c.totalexperience i.seniority_level_dum#c.currentexperience
margins gender_dum , at (totalexperience =(0 (2) 38))
marginsplot, recastci(rarea) noci


margins seniority_level_dum , at (currentexperience =(0 (2) 22))
marginsplot, recastci(rarea) noci


reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum 
outreg2 using results, word replace ctitle(log_AnnualSalary) dec(3) addstat("Adjusted R square", e(r2_a))
reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum i.gender_dum#c.totalexperience i.seniority_level_dum#c.currentexperience
outreg2 using results, word append ctitle(seniority_level_dum) dec(3) addstat("Adjusted R square", e(r2_a))



//Diagnostic and Robustness Analysis

//Question 1 - Multicolinearity

pwcorr log_AnnualSalary log_bonus totalexperience currentexperience gender_dum seniority_level_dum

reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum 

vif



//Question 2 - Heteroskedascity

reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum 
rvfplot, yline(0) mlabel(id)


//Detection
hettest
imtest, white

//Remedy
reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum , vce(robust)


//Question 3 - Checking for Outliers, Observations and leverages

//Outliers: 3 standard deviation away from mean
reg log_AnnualSalary log_bonus totalexperience currentexperience i.gender_dum i.seniority_level_dum 
predict r, rstandard
browse id year r if abs(r)>3
sort r


//Residual vs fitted plot
rvfplot, mlabel( id ) , yline(0)
avplots,mlabel(id)


//Observations with high leverages
predict lev, leverage

//2*k+2/N 
gen lev_cutoff=(2*3+2)/1746
browse id year lev lev_cutoff if lev>lev_cutoff
gsort -lev


//Influential observations 4/N
predict cook, cooksd
gen cook_cutoff=4/1746
browse id year cook cook_cutoff if cook>cook_cutoff
gsort -cook

//leverage vs squared residual
lvr2plot, mlabel( id )








