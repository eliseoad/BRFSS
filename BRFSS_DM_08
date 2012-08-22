# BRFSS Health Related quality of Life study

###############################################################################

rm(list=ls())
gc()

if (Sys.info()["sysname"] == "Linux"){
BRFSS.08 <- 
	read.csv (file = "/media/truecrypt2/ORP/Data/BRFSS/Raw/BRFSS2008.csv")

} else if (Sys.info()["sysname"] == "Windows"){
BRFSS.08 <-
	read.csv (file = "I:/ORP/Data/BRFSS/Raw/BRFSS2008.csv")
}


names(BRFSS.08) <- tolower(names(BRFSS.08))
names(BRFSS.08)

### Script for data management and cleaning ###

## Recode BRFSS varibles ##


## 1. Month of interview (imonth) ##

table(BRFSS.08$imonth)

#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost and could further affect analysis 
BRFSS.08$imonth2<-as.character(BRFSS.08$imonth)

## Factorize and label the month variable
 
BRFSS.08$imonth<-factor(BRFSS.08$imonth, labels=c("January","February","March","April","May","June",  
  	"July", "August","September","October","November","December"))
table(BRFSS.08$imonth)


## 2.Primary Sampling Unit, Equal to Annual Sequence Number (psu) ##


## 3. General health perception (genhlth)
table(BRFSS.08$genhlth)

BRFSS.08$genhlth<-factor(BRFSS.08$genhlth)
# Replace values with NA 
BRFSS.08$genhlth<-replace(BRFSS.08$genhlth, BRFSS.08$genhlth==7, NA)
BRFSS.08$genhlth<-replace(BRFSS.08$genhlth, BRFSS.08$genhlth==9, NA)

BRFSS.08$genhlth<-factor(BRFSS.08$genhlth)

#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost.
BRFSS.08$genhlth2<-as.character(BRFSS.08$genhlth)

#For recoding into BRFSS.08 measure Fairpoor#
BRFSS.08$fairpoor<-BRFSS.08$genhlth #Is necesarry to create this variable before recoding genhlth 

label.gen<-list(BRFSS.08$genhlth, Excellent="1", Verygood="2", Good="3", Fair="4", 
		Poor="5")
levels(BRFSS.08$genhlth)<- label.gen

BRFSS.08$genhlth<-factor(BRFSS.08$genhlth)
                
summary(BRFSS.08$genhlth)


## 4. Physical health not good (physhlth) ##
table(BRFSS.08$physhlth)

#Convert 88 values to 0#
BRFSS.08$physhlth[BRFSS.08$physhlth==88]<-0

#Convert 77 values to blank#
BRFSS.08$physhlth[BRFSS.08$physhlth==77]<-" "

BRFSS.08$physhlth<-as.numeric(BRFSS.08$physhlth)
class(BRFSS.08$physhlth)
table(BRFSS.08$physhlth)
summary(BRFSS.08$physhlth)


## 5. Mental health not good (menthlth) ##
table(BRFSS.08$menthlth)
summary(BRFSS.08$menthlth)

#Convert 88 values to 0#
BRFSS.08$menthlth[BRFSS.08$menthlth==88]<-0

#Convert 77 values to blank#
BRFSS.08$menthlth[BRFSS.08$menthlth==77]<-" "

#Convert 99 values to blank#
BRFSS.08$menthlth[BRFSS.08$menthlth==99]<-" "

BRFSS.08$menthlth<-as.numeric(BRFSS.08$menthlth)
class(BRFSS.08$menthlth)
table(BRFSS.08$menthlth)
summary(BRFSS.08$menthlth)


## 6. Recent activity limitation days (poorhlth) ##

table(BRFSS.08$poorhlth)
summary(BRFSS.08$poorhlth)
class(BRFSS.08$poorhlth)

BRFSS.08$poorhlth<-as.character(BRFSS.08$poorhlth)

BRFSS.08$poorhlth<-replace(BRFSS.08$poorhlth, BRFSS.08$poorhlth==".", NA)

#Convert 88 values to 0#
BRFSS.08$poorhlth[BRFSS.08$poorhlth==88]<-0

#Convert 77 values to blank#
BRFSS.08$poorhlth[BRFSS.08$poorhlth==77]<-" "

# Convert 99 values to blank #
BRFSS.08$poorhlth[BRFSS.08$poorhlth==99]<-" "

BRFSS.08$poorhlth<-as.numeric(BRFSS.08$poorhlth)
class(BRFSS.08$poorhlth)
table(BRFSS.08$poorhlth)
summary(BRFSS.08$poorhlth)


## 7. Any kind of health care coverage (hlthplan) ##
table(BRFSS.08$hlthplan)
BRFSS.08$hlthplan<-factor(BRFSS.08$hlthplan)
BRFSS.08$hlthplan<-replace(BRFSS.08$hlthplan, BRFSS.08$hlthplan==7, NA)
BRFSS.08$hlthplan<-replace(BRFSS.08$hlthplan, BRFSS.08$hlthplan==9, NA)

label.hpl<-list(BRFSS.08$hlthplan, Yes="1", No="2")
levels(BRFSS.08$hlthplan)<- label.hpl
BRFSS.08$hlthplan<-factor(BRFSS.08$hlthplan)

class(BRFSS.08$hlthplan)
table(BRFSS.08$hlthplan)
summary(BRFSS.08$hlthplan)


## 8. Person you think of as your personal doctor or health care provider (persdoc2) ##
table(BRFSS.08$persdoc2)

#Change class#
BRFSS.08$persdoc2<-factor(BRFSS.08$persdoc2)

BRFSS.08$persdoc2<-replace(BRFSS.08$persdoc2, BRFSS.08$persdoc2==7, NA)
BRFSS.08$persdoc2<-replace(BRFSS.08$persdoc2, BRFSS.08$persdoc2==9, NA)

#For variable with values yes or no (perdocod)#
BRFSS.08$perdocod<-factor(BRFSS.08$persdoc2)

label.pdc<-list(BRFSS.08$persdoc2, Yes="1", Moreone="2", No="3")

levels(BRFSS.08$persdoc2)<- label.pdc

BRFSS.08$persdoc2<-factor(BRFSS.08$persdoc2)

table(BRFSS.08$persdoc2)
summary(BRFSS.08$persdoc2)

	#Categorized variables to obtain values yes on no(perdocod)#

	label.pc<-list(BRFSS.08$perdocod, Yes="1", Yes="2", No="3")

	levels(BRFSS.08$perdocod)<- label.pc

	BRFSS.08$perdocod<-factor(BRFSS.08$perdocod)

	table(BRFSS.08$perdocod)
	summary(BRFSS.08$perdocod)


## 9. Could not see a doctor because of cost (medcost) ##
table(BRFSS.08$medcost)
BRFSS.08$medcost<-replace(BRFSS.08$medcost, BRFSS.08$medcost==7, NA)
BRFSS.08$medcost<-factor(BRFSS.08$medcost)

levels(BRFSS.08$medcost)<- label.hpl
                BRFSS.08$medcost<-factor(BRFSS.08$medcost)

table(BRFSS.08$medcost)
summary(BRFSS.08$medcost)


## 10. Last visited a doctor for a routine checkup (checkup2) ##
table(BRFSS.08$checkup1)
summary(BRFSS.08$checkup1)

BRFSS.08$checkup1<-replace(BRFSS.08$checkup1, BRFSS.08$checkup1==7, NA)
BRFSS.08$checkup1<-replace(BRFSS.08$checkup1, BRFSS.08$checkup1==9, NA)

BRFSS.08$checkup1<-factor(BRFSS.08$checkup1)

#Line for maintaining the original values for further use#
BRFSS.08$checkup2<-as.character(BRFSS.08$checkup1)
BRFSS.08$checkup2<-replace(BRFSS.08$checkup2, BRFSS.08$checkup2==8, 0)

# Label checkup1

label.cup<-list(BRFSS.08$checkup1, Withinyear="1", Withintwoyrs="2", 
		Withinfiveyrs="3", Fivemoreyrs="4", Never="8")
levels(BRFSS.08$checkup1)<- label.cup

# refactor to eliminate deleted group remanet
BRFSS.08$checkup1<-factor(BRFSS.08$checkup1)
table(BRFSS.08$checkup1)
summary(BRFSS.08$checkup1)


## 11. Any physical activities or exercises during past month (exeranay2) ## 
table(BRFSS.08$exerany2)
BRFSS.08$exerany2<-replace(BRFSS.08$exerany2, BRFSS.08$exerany2==7, NA)
BRFSS.08$exerany2<-factor(BRFSS.08$exerany2)
levels(BRFSS.08$exerany2)<- label.hpl
BRFSS.08$exerany2<-factor(BRFSS.08$exerany2)

table(BRFSS.08$exerany2)
summary(BRFSS.08$exerany2)


## 12. Ever been told by a doctor that you have diabetes (diabetes2) ##

table (BRFSS.08$diabete2)
summary(BRFSS.08$diabete2)

#Line for maintaining the original values for further use#
BRFSS.08$diabetes<-as.character(BRFSS.08$diabete2)

# Replace values in diabetes
BRFSS.08$diabetes<-replace(BRFSS.08$diabetes, BRFSS.08$diabetes==7, NA)
BRFSS.08$diabetes<-replace(BRFSS.08$diabetes, BRFSS.08$diabetes==9, NA)

# Refactor to eliminate deleted group remanet
BRFSS.08$diabetes<-as.factor(BRFSS.08$diabetes)
# Label diabetes

label.dia<-list(Yes="1", No="2", No="3", No="4")
levels(BRFSS.08$diabetes)<- label.dia

table (BRFSS.08$diabetes)
summary(BRFSS.08$diabetes)


## 13. Ever told you had a heart attack (cvdinfr4) ##
table(BRFSS.08$cvdinfr4)
#To maintain the original values for further use#
BRFSS.08$cvdinfr<-as.character(BRFSS.08$cvdinfr4)

BRFSS.08$cvdinfr<-replace(BRFSS.08$cvdinfr, BRFSS.08$cvdinfr==7, NA)

# Factor
BRFSS.08$cvdinfr<-factor(BRFSS.08$cvdinfr)
#Recode#
levels(BRFSS.08$cvdinfr)<- label.hpl
# Re-factorize
BRFSS.08$cvdinfr<-factor(BRFSS.08$cvdinfr)

table(BRFSS.08$cvdinfr)
summary(BRFSS.08$cvdinfr)


## 14. Ever told you had angina or coronary heart disease (cvdcrhd4) ##
table(BRFSS.08$cvdcrhd4)

#Line for maintaining the original values for further use#
BRFSS.08$cvdcrhd<-as.factor(BRFSS.08$cvdcrhd4)

BRFSS.08$cvdcrhd<-replace(BRFSS.08$cvdcrhd, BRFSS.08$cvdcrhd==7, NA)

#Factorize
BRFSS.08$cvdcrhd<-factor(BRFSS.08$cvdcrhd)
levels(BRFSS.08$cvdcrhd)<- label.hpl

#Re-facotrize
BRFSS.08$cvdcrhd<-factor(BRFSS.08$cvdcrhd)

table(BRFSS.08$cvdcrhd)
summary(BRFSS.08$cvdcrhd)


## 15. Ever told you had a stroke (cvdstrk3) ##
table(BRFSS.08$cvdstrk3)

#To maintain the original values for further use#
BRFSS.08$cvdstrk<-as.factor(BRFSS.08$cvdstrk3)

# Put values to NA
BRFSS.08$cvdstrk<-replace(BRFSS.08$cvdstrk, BRFSS.08$cvdstrk==7, NA)

#lavels
levels(BRFSS.08$cvdstrk)<- label.hpl
#Re-factorieze
BRFSS.08$cvdstrk<-factor(BRFSS.08$cvdstrk)

table(BRFSS.08$cvdstrk)


## 16. Have you ever been told that you had asthma (asthma2) ##

table(BRFSS.08$asthma2)

BRFSS.08$asthma2<-factor(BRFSS.08$asthma2)

BRFSS.08$asthma2<-replace(BRFSS.08$asthma2, BRFSS.08$asthma2==7, NA)

levels(BRFSS.08$asthma2)<- label.hpl

BRFSS.08$asthma2<-factor(BRFSS.08$asthma2)
table(BRFSS.08$asthma2)


## 17. Still have asthma (asthnow) ##

table(BRFSS.08$asthnow)
BRFSS.08$asthnow<-factor(BRFSS.08$asthnow)
BRFSS.08$asthnow<-replace(BRFSS.08$asthnow, BRFSS.08$asthnow==7, NA)
levels(BRFSS.08$asthnow)<- label.hpl
BRFSS.08$asthnow<-factor(BRFSS.08$asthnow)
table(BRFSS.08$asthnow)


##18. Limited because of physical, mental, or emotional problems (qlactlm2) ##

table(BRFSS.08$qlactlm2)
BRFSS.08$qlactlm2<-factor(BRFSS.08$qlactlm2)
BRFSS.08$qlactlm2<-replace(BRFSS.08$qlactlm2, BRFSS.08$qlactlm2==7, NA)
BRFSS.08$qlactlm2<-replace(BRFSS.08$qlactlm2, BRFSS.08$qlactlm2==9, NA)

levels(BRFSS.08$qlactlm2)<- label.hpl
BRFSS.08$qlactlm2<-factor(BRFSS.08$qlactlm2)
summary(BRFSS.08$qlactlm2)


## 19. Age (age) ##

table(BRFSS.08$age)
BRFSS.08$age<-replace(BRFSS.08$age, BRFSS.08$age==7, NA)
BRFSS.08$age<-replace(BRFSS.08$age, BRFSS.08$age==9, NA)
table(BRFSS.08$age)
summary(BRFSS.08$age)

## 20. Marital status (marital) ##

table(BRFSS.08$marital)
BRFSS.08$marital<-as.character(BRFSS.08$marital)
BRFSS.08$marital<-replace(BRFSS.08$marital, BRFSS.08$marital==9, NA)

BRFSS.08$marital2<-factor(BRFSS.08$marital)
label.mt<-list(BRFSS.08$marital2, Married="1", Divorced="2", Widowed="3", Separate="4",
  		Nevermarried="5", Unmarriedcople="6")
levels(BRFSS.08$marital2)<- label.mt
table(BRFSS.08$marital2)          
BRFSS.08$marital2<-factor(BRFSS.08$marital2)

summary(BRFSS.08$marital2)

	#Living with a significant other yes or no(maritrec)##

	BRFSS.08$maritrec<-as.factor(BRFSS.08$marital)

	BRFSS.08$maritrec<-replace(BRFSS.08$maritrec, BRFSS.08$maritrec==9, NA)

	label.mtr<-list(Yes="1", No="2", No="3", No="4",
		No="5", Yes="6")
	levels(BRFSS.08$maritrec)<- label.mtr
                 
	BRFSS.08$maritrec<-factor(BRFSS.08$maritrec)

	summary(BRFSS.08$maritrec)


## 21. Educational level (educa) ##

table(BRFSS.08$educa)

BRFSS.08$educa<-as.factor(BRFSS.08$educa)

BRFSS.08$educa<-replace(BRFSS.08$educa, BRFSS.08$educa==9, NA)

label.ed<-list(BRFSS.08$educa, Never="1", Elementary="2", SomeHigh="3", HSGrad="4",
		SomeCollege="5", CollegeGrad="6")
levels(BRFSS.08$educa)<- label.ed

BRFSS.08$educa<-factor(BRFSS.08$educa)
summary(BRFSS.08$educa)


## 22. Employment status (employ2) ##

table(BRFSS.08$employ)
BRFSS.08$employ<-replace(BRFSS.08$employ, BRFSS.08$employ==9, NA)

BRFSS.08$employ2<-as.factor(BRFSS.08$employ)
               
label.em<-list(BRFSS.08$employ2, Forwage="1", Selfemploy="2", Outworkmoreyear="3",
 	Outworklessyear="4", Homemaker="5", Student="6", Retired="7", Unablework="8")
levels(BRFSS.08$employ2)<- label.em
               
BRFSS.08$employ2<-factor(BRFSS.08$employ2)

               summary(BRFSS.08$employ2)

	# Employ yes or no (emplrec)#

	BRFSS.08$emplrec<-as.factor(BRFSS.08$employ)
               
      BRFSS.08$emplrec<-replace(BRFSS.08$emplrec, BRFSS.08$emplrec==9, NA)

	label.emr<-list(BRFSS.08$emplrec, Yes="1", Yes="2", No="3", No="4", No="5", 
		No="6", No="7", No="8")
	levels(BRFSS.08$emplrec)<- label.emr
	
 	BRFSS.08$emplrec<-factor(BRFSS.08$emplrec)
               
 	summary(BRFSS.08$emplrec)

	# Employment combinig some cateqories forwage and selfemploy , out of work (emplrec2)#

	BRFSS.08$emplrec2<-as.factor(BRFSS.08$employ)
               
      BRFSS.08$emplrec2<-replace(BRFSS.08$emplrec2, BRFSS.08$emplrec2==9, NA)

	label.emr2<-list(BRFSS.08$emplrec2,Employ="1", Employ="2", Outwork="3",
 	Outwork="4", Homemaker="5", Student="6", Retired="7", Unablework="8")

	levels(BRFSS.08$emplrec2)<- label.emr2
	
 	BRFSS.08$emplrec2<-factor(BRFSS.08$emplrec2)
      table(BRFSS.08$emplrec2)         
 	summary(BRFSS.08$emplrec2)


## 23. Annual household income from all sources (income2) ##

table(BRFSS.08$income2)
# Refer to variable 37 incomg #


## 24. Weigh without shoes (weight2) ##


## 25. Tall without shoes (heigh3) ##


## 26. Gender (sex) ##

table(BRFSS.08$sex)
BRFSS.08$sex<-factor(BRFSS.08$sex, labels=c("Male", "Female")) 
table(BRFSS.08$sex)


## 27. Sample Design Stratification Variable  (ststr) ##


## 28. Final weight assigned to each respondent (finalwt) ##

## Create a new final weight
table(BRFSS.08$iyear)
t1 <- table(BRFSS.08$iyear)
dim(t1)

weightt <- (t1[4]/(t1[1]+t1[2]+t1[3]+t1[4]))

BRFSS.08$newfwt <- weightt*BRFSS.08$finalwt


## New population estimate with the new final weight
nN <- sum(BRFSS.08$newfwt)


## 29. Geographic region, Health region (geostr) ##

table(BRFSS.08$geostr)
BRFSS.08$geostr<-factor(BRFSS.08$geostr, labels=c("Aguadilla", "Arecibo", "Bayam?n", "Metropolitana",
			 "Fajardo", "Caguas", "Ponce", "Mayaguez"))
table(BRFSS.08$geostr)
 

## 30. Seven-level imputed age category(ageg) ##

table(BRFSS.08$ageg)
BRFSS.08$ageg<-factor(BRFSS.08$ageg, labels=c("18-24", "25-34", "35-44", "45-54", "55-64",
			"66-74", "74>"))
table(BRFSS.08$ageg)


## 31. Physical activity or exercise during the past 30 days other than their regular job (totinda) ##

table(BRFSS.08$totinda)
BRFSS.08$totinda<-replace(BRFSS.08$totinda, BRFSS.08$totinda==9, NA)

#Other form of recoding#
BRFSS.08$totinda<-factor(BRFSS.08$totinda, labels=c("Yes", "No")) 
  table(BRFSS.08$totinda)
  summary(BRFSS.08$totinda)


## 32. Asthma status (asthmst) ##

table(BRFSS.08$asthmst)

  # variable for estimating current asthma (Yes or no in all ppopulation
    BRFSS.08$currasth<-BRFSS.08$asthmst
    BRFSS.08$currasth<-factor(BRFSS.08$currasth)
    table(BRFSS.08$currasth)
    label.curr<-list(BRFSS.08$currasth, Yes="1", No="2", No="3")
    levels(BRFSS.08$currasth)<- label.curr
    BRFSS.08$currasth<-factor(BRFSS.08$currasth)
    table(BRFSS.08$currasth)

BRFSS.08$asthmst<-replace(BRFSS.08$asthmst, BRFSS.08$asthmst==9, NA)
BRFSS.08$asthmst<-factor(BRFSS.08$asthmst, labels=c("Current", "Former", "Never")) 
table(BRFSS.08$asthmst)
summary(BRFSS.08$asthmst)


## 33. Adults who are current smokers(rfsmok3) ##

table(BRFSS.08$rfsmok3)
BRFSS.08$rfsmok3<-replace(BRFSS.08$rfsmok3, BRFSS.08$rfsmok3==9, NA)
BRFSS.08$rfsmok3<-factor(BRFSS.08$rfsmok3, labels=c("No", "Yes")) 
table(BRFSS.08$rfsmok3)
summary(BRFSS.08$rfsmok3)

## 34. Body Mass Index (bmi4)
class(BRFSS.08$bmi)

BRFSS.08$bmi<-replace(BRFSS.08$bmi, BRFSS.08$bmi==9999, NA)

summary(BRFSS.08$bmi)

## 35. Three-categories of Body Mass Index (bmi4cat) ##

table(BRFSS.08$bmi4cat)

BRFSS.08$bmi4cat<-replace(BRFSS.08$bmi4cat, BRFSS.08$bmi4cat==9, NA)

#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost and could further affect analysis 
BRFSS.08$bmi4cat2<-as.character(BRFSS.08$bmi4cat)

BRFSS.08$bmi4cat<-factor(BRFSS.08$bmi4cat, labels=c("Neither overweight nor obese",
 "Overweight","Obese")) 
table(BRFSS.08$bmi4cat)
summary(BRFSS.08$bmi4cat)


## 36. Education recoded (educag) ##

table(BRFSS.08$educag)

BRFSS.08$educag<-replace(BRFSS.08$educag, BRFSS.08$educag==9, NA)

#Create new variable to keep the original. When imonth is labeled, during analysis
#the categories order is lost and could afect analysis.
BRFSS.08$educag2<-as.character(BRFSS.08$educag)

BRFSS.08$educag<-factor(BRFSS.08$educag)
                 
label.edc<-list(BRFSS.08$educa, LessHS="1", HSGraduate="2", SomeCollege="3", CollegeGrad="4")
levels(BRFSS.08$educag)<- label.edc
                 
BRFSS.08$educag<-factor(BRFSS.08$educag)
table(BRFSS.08$educag)
summary(BRFSS.08$educag)

#lable to maintain the order of the educational levels#
BRFSS.08$educag3<-factor(BRFSS.08$educag2)
label.edc2<-list(BRFSS.08$educag3, HS="1", HSGraduate="2", Univer="3", UniverGrad="4")
levels(BRFSS.08$educag3)<- label.edc2
BRFSS.08$educag3<-factor(BRFSS.08$educag3)              
table(BRFSS.08$educag3)
summary(BRFSS.08$educag3)

## 37. Annual household income from all sources (incomg)##

table(BRFSS.08$incomg)

BRFSS.08$incomg<-replace(BRFSS.08$incomg, BRFSS.08$incomg==9, NA)
BRFSS.08$incomg<-factor(BRFSS.08$incomg, labels=c("<15k", "15k-<25k", "25k-<35k", "35k-<50k",
  		 "50+k"))
table(BRFSS.08$incomg)
summary(BRFSS.08$incomg)


## 38. Six-level imputed age category (ageg2)##

table(BRFSS.08$ageg2)

BRFSS.08$ageg2<-factor(BRFSS.08$ageg2, labels=c("18-24", "25-34", "35-44", "45-54", "55-64",
			"65<"))
table(BRFSS.08$ageg2)
summary(BRFSS.08$ageg2)


## 39. How many children less than 18 years of age live in your household?
table(BRFSS.08$children)

# Convert 88 values to 0#
BRFSS.08$children[BRFSS.08$children==88]<-0
BRFSS.08$children<-replace(BRFSS.08$children, BRFSS.08$children==99, NA)

# Children yes or no #
BRFSS.08$child<-factor(BRFSS.08$children)
table(BRFSS.08$child)

label.child<-list(BRFSS.08$educa, No="0", Yes="1", Yes="2", Yes="3", Yes="4", Yes="5", Yes="6", Yes="7", Yes="8")
levels(BRFSS.08$child)<- label.child
table(BRFSS.08$child)
BRFSS.08$child<-factor(BRFSS.08$child)

# Children no, 1, 2, 2< #
BRFSS.08$child2<-factor(BRFSS.08$children)
table(BRFSS.08$child2)

label.child2<-list(No="0", One="1", Two="2", TwoMore="3", TwoMore="4", TwoMore="5", TwoMore="6", TwoMore="7", TwoMore="8")
levels(BRFSS.08$child2)<- label.child2
table(BRFSS.08$child2)
BRFSS.08$child2<-factor(BRFSS.08$child2)


## 40. How many children less than 18 years of age live in your household?
table(BRFSS.08$chldcnt)
 # Use variable (children) #


## 41. To your knowledge, are you now pregnant?
table(BRFSS.08$pregnant)
BRFSS.08$pregnant<-replace(BRFSS.08$pregnant, BRFSS.08$pregnant==7, NA)

BRFSS.08$pregnant<-factor(BRFSS.08$pregnant)
levels(BRFSS.08$pregnant)<-label.hpl
BRFSS.08$pregnant<-factor(BRFSS.08$pregnant)
table(BRFSS.08$pregnant)


## 42. Year of interview
table(BRFSS.08$iyear)


## 43. Health Region 

BRFSS.08$hregion<-factor(BRFSS.08$ststr)
table(BRFSS.08$hregion)

label.reg<-list(BRFSS.08$hregion, Aguadilla="72011", Arecibo="72021", Bayamon="72031", Metropolitana="72041", 
                  Fajardo="72051", Caguas="72061", Ponce="72071", Mayaguez="72081")

levels(BRFSS.08$hregion)<- label.reg
table(BRFSS.08$hregion)
BRFSS.08$hregion<-factor(BRFSS.08$hregion)



#### BRFSS.08 variables based on the CDC measurements ###


## 44. Fairpoor from genhlth (fairpoor) ##

table(BRFSS.08$fairpoor)

label.fp<-list(BRFSS.08$fairpoor, EVGood="1",  EVGood="2",  EVGood="3",
 		Fairpoor="4", Fairpoor="5")
levels(BRFSS.08$fairpoor)<- label.fp

BRFSS.08$fairpoor<-factor(BRFSS.08$fairpoor)

table(BRFSS.08$fairpoor)
summary(BRFSS.08$fairpoor)


## 45. Computing unhealthy days by adding (unhlthy) ## 

#Create data frame with BRFSS.08 variables#
df<-data.frame(BRFSS.08$physhlth,BRFSS.08$menthlth)

#Sum variable#
sumdays<-rowSums(df, na.rm=T)

#Create variable total days#
BRFSS.08$sumdays<-sumdays
table(BRFSS.08$sumdays)

#Define unhealthy days groups here#
unhg<-c(0, 13,  Inf)

BRFSS.08$unhlthy<-cut(BRFSS.08$sumdays, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.08$unhlthy)

rm(df)

#Variable with the values >30 of sumdays transformed to 30#  

BRFSS.08$unhlthdays<-BRFSS.08$sumdays
table(BRFSS.08$unhlthdays)
BRFSS.08$unhlthdays[BRFSS.08$unhlthdays>30]=30
table(BRFSS.08$unhlthdays)
summary(BRFSS.08$unhlthdays)


## 46. Frequent physical distress (physhlcat) ##

BRFSS.08$physhlcat<-cut(BRFSS.08$physhlth, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.08$physhlcat)


## 47. Frequent mental distress (mentdist) ##

BRFSS.08$mentdist<-cut(BRFSS.08$menthlth, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.08$mentdist)


## 48. poor physical or mental health keep you from doing your usual activities, 
##     such as self-care, work, or recreation (poorhlcat)

BRFSS.08$poorhlcat<-cut(BRFSS.08$poorhlth, unhg, include.lowest=TRUE,
		labels=c("0-13", "14+"),
		levels=c("0-13", "14+"))

table(BRFSS.08$poorhlcat)

### Other covariates ###

## Any chronic disease such as diabetes, cadiovascular ##
## Constructing variables related to chronic disease presence ##


#Recoding variable diabete2#

table(BRFSS.08$diabete2)
BRFSS.08$diabe<-as.numeric(BRFSS.08$diabete2)

#Convert 2 values to 0#
BRFSS.08$diabe[BRFSS.08$diabe==2]<-0

#Convert 3 values to 0#
BRFSS.08$diabe[BRFSS.08$diabe==3]<-0

#Convert 4 values to 0#
BRFSS.08$diabe[BRFSS.08$diabe==4]<-0

#Convert 7 values to blank#
BRFSS.08$diabe[BRFSS.08$diabe==7]<-" "

# Convert 9 values to blank #
BRFSS.08$diabe[BRFSS.08$diabe==9]<-" "

BRFSS.08$diabe<-as.numeric(BRFSS.08$diabe)
class(BRFSS.08$diabe)
table(BRFSS.08$diabe)
summary(BRFSS.08$diabe)


#Recoding variable cvdinfr4#

table(BRFSS.08$cvdinfr4)
BRFSS.08$cvdinfr2<-as.numeric(BRFSS.08$cvdinfr4)

#Convert 2 values to 0#
BRFSS.08$cvdinfr2[BRFSS.08$cvdinfr2==2]<-0

#Convert 7 values to blank#
BRFSS.08$cvdinfr2[BRFSS.08$cvdinfr2==7]<-" "

BRFSS.08$cvdinfr2<-as.numeric(BRFSS.08$cvdinfr2)
class(BRFSS.08$cvdinfr2)
table(BRFSS.08$cvdinfr2)
summary(BRFSS.08$cvdinfr2)


#Recoding variable cvdcrhd4#

table(BRFSS.08$cvdcrhd4)
BRFSS.08$cvdcrhd2<-as.numeric(BRFSS.08$cvdcrhd4)

#Convert 2 values to 0#
BRFSS.08$cvdcrhd2[BRFSS.08$cvdcrhd2==2]<-0

#Convert 7 values to blank#
BRFSS.08$cvdcrhd2[BRFSS.08$cvdcrhd2==7]<-" "

BRFSS.08$cvdcrhd2<-as.numeric(BRFSS.08$cvdcrhd2)
class(BRFSS.08$cvdcrhd2)
table(BRFSS.08$cvdcrhd2)
summary(BRFSS.08$cvdcrhd2)


#Recoding variable cvdstrk3#

table(BRFSS.08$cvdstrk3)
BRFSS.08$cvdstrk2<-as.numeric(BRFSS.08$cvdstrk3)

#Convert 2 values to 0#
BRFSS.08$cvdstrk2[BRFSS.08$cvdstrk2==2]<-0

#Convert 7 values to blank#
BRFSS.08$cvdstrk2[BRFSS.08$cvdstrk2==7]<-" "

BRFSS.08$cvdstrk2<-as.numeric(BRFSS.08$cvdstrk2)
class(BRFSS.08$cvdstrk2)
table(BRFSS.08$cvdstrk2)
summary(BRFSS.08$cvdstrk2)


#Create data frame with 
df2<-data.frame(BRFSS.08$diabe,BRFSS.08$cvdinfr2, BRFSS.08$cvdcrhd2, BRFSS.08$cvdstrk2)
str(df2)


## 49. Total quantity of reported chronic diseases (chrotot)##

chrotot<-rowSums(df2, na.rm=T)
BRFSS.08$chrotot<-chrotot
table(BRFSS.08$chrotot)
summary(chrotot)

rm(df2)


## 50. Report of any chronic condition (anychronc) ##

BRFSS.08$anychronc<-as.factor(BRFSS.08$chrotot)

label.acc<-list(BRFSS.08$anychronc, No="0", Oneormore="1", Oneormore="2", Oneormore="3",
		Oneormore="4")
levels(BRFSS.08$anychronc)<- label.acc

BRFSS.08$anychronc<-factor(BRFSS.08$anychronc)
table(BRFSS.08$anychronc)


## Other methos to obtain the health region recommendend by the BRFSS Coordinator)

BRFSS.08$muni<-factor(BRFSS.08$citycode)

table(BRFSS.08$muni)

label.MUNI<-list( Adjuntas="1", Aguada="3", Aguadilla="5", AguasBuenas="7", Aibonito="9",                
    Anasco="11", Arecibo="13", Arroyo="15", Barceloneta="17", Barranquitas="19", Bayamon="21",
    CaboRojo="23", Caguas="25", Camuy="27", Canovanas="29", Carolina="31", Catano="33", 
		Cayey="35", Ceiba="37", Ciales="39",  Cidra="41", Coamo="43",  Comerio="45",Corozal="47",
		Culebra="49", Dorado="51", Fajardo="53", Florida="54", Guanica="55" , Guayama="57",
		Guayanilla="59", Guaynabo="61", Gurabo="63", Hatillo="65", Hormigueros="67", Humacao="69",    
		Isabela="71", Jayuya="73", JuanaDiaz="75", Juncos="77", Lajas="79", Lares="81", LasMarias="83",
		LasPiedras="85", Loiza="87", Luquillo="89", Manati="91", Maricao="93", Maunabo="95",               
		Mayaguez="97", Moca="99", Morovis="101", Naguabo="103",    
		Naranjito="105", Orocovis="107", Patillas="109" , Penuelas="111",  Ponce="113",  Quebradillas="115",       
		Rincon="117", RioGrande="119", SabanaGrande="121" , Salinas="123", SanGerman="125", 
    SanJuan="127",	SanLorenzo="129", SanSebastian="131",
		SantaIsabel="133", ToaAlta="135", ToaBaja="137", TrujilloAlto="139", Utuado="141", VegaAlta="143",
		VegaBaja="145", Vieques="147", Villalba="149", Yabucoa="151", Yauco="153")


levels(BRFSS.08$muni)<-label.MUNI
table(BRFSS.08$muni)


BRFSS.08$healthreg<-factor(BRFSS.08$citycode)
table(BRFSS.08$healthreg)

label.HREG<-list( Ponce="1", Aguadilla="3", Aguadilla="5", Caguas="7", Caguas="9",                
    Mayaguez="11", Arecibo="13", Ponce="15", Arecibo="17", Bayamon="19", Bayamon="21",
    Mayaguez="23", Caguas="25", Arecibo="27", Metro="29", Metro="31", Bayamon="33", 
		Caguas="35", Fajardo="37", Arecibo="39",  Caguas="41", Ponce="43",  Bayamon="45",Bayamon="47",
		Fajardo="49", Bayamon="51", Fajardo="53", Arecibo="54", Ponce="55" , Ponce="57",
		Ponce="59", Metro="61", Caguas="63", Arecibo="65", Mayaguez="67", Caguas="69",    
		Aguadilla="71", Ponce="73", Ponce="75", Caguas="77", Mayaguez="79", Arecibo="81", Mayaguez="83",
		Caguas="85", Metro="87", Fajardo="89", Arecibo="91", Mayaguez="93", Caguas="95",               
		Mayaguez="97", Aguadilla="99", Arecibo="101", Caguas="103",    
		Bayamon="105", Bayamon="107", Ponce="109" , Ponce="111",  Ponce="113",  Arecibo="115",       
		Mayaguez="117", Fajardo="119", Mayaguez="121" , Ponce="123", Mayaguez="125", 
    Metro="127",	Caguas="129", Aguadilla="131",
		Ponce="133", Bayamon="135", Bayamon="137", Metro="139", Arecibo="141", Bayamon="143",
		Arecibo="145", Fajardo="147", Ponce="149", Caguas="151", Ponce="153")


levels(BRFSS.08$healthreg)<-label.HREG
table(BRFSS.08$healthreg)

########################
## Asthma module 2008
########################

## 1. Age at asthma diagnosis ##

  table(BRFSS.08$asthage)
  # 97 = to 10 years or less, 97=10
  BRFSS.08$asthage<-replace(BRFSS.08$asthage, BRFSS.08$asthage==97, 10)
  BRFSS.08$asthage<-replace(BRFSS.08$asthage, BRFSS.08$asthage==98, NA)
  BRFSS.08$asthage<-replace(BRFSS.08$asthage, BRFSS.08$asthage==99, NA)

  acat<-c(0, 17,  49, Inf)

  BRFSS.08$asthagect<-cut(BRFSS.08$asthage, acat, include.lowest=TRUE,
  	labels=c("0-18", "18-49", "50<"),
		levels=c("0-18", "18-49", "50<"))

  table(BRFSS.08$asthagect)


## 2. During the past 12 months, have you had an episode of asthma or an asthma attack ##

  table(BRFSS.08$asattack)

  BRFSS.08$asattack<-factor(BRFSS.08$asattack)
  BRFSS.08$asattack<-replace(BRFSS.08$asattack, BRFSS.08$asattack==7, NA)
  BRFSS.08$asattack<-replace(BRFSS.08$asattack, BRFSS.08$asattack==9, NA)

  BRFSS.08$asattack<-factor(BRFSS.08$asattack, labels=c("Yes", "No"))

  class(BRFSS.08$asattack)
  table(BRFSS.08$asattack)
  summary(BRFSS.08$asattack)


## 3. During the past 12 months, ER visit ##

  table(BRFSS.08$aservist)

  # Five categories

  aer<-c(0,1, 2, 5, 87, 88)
  BRFSS.08$aservcat<-cut(BRFSS.08$aservist, aer, include.lowest=TRUE,
    labels=c("1", "2", "3-5", "6<", "0"),
		levels=c("1", "2", "3-5", "6<", "0"))
  table(BRFSS.08$aservcat)

 # Two categories

  aer2<-c(0, 87, 88)
  BRFSS.08$aservcat2<-cut(BRFSS.08$aservist, aer2, include.lowest=TRUE,
    labels=c("Yes", "No"),
  	levels=c("Yes", "No"))
  table(BRFSS.08$aservcat2)



## 4. During the past 12 months, how many times did you see a doctor, 
## nurse or other health professional for urgent treatment of worsening asthma symptoms ##

table(BRFSS.08$asdrvist)

  # Five categories

  BRFSS.08$asdrvcat<-cut(BRFSS.08$asdrvist, aer, include.lowest=TRUE,
    labels=c("1", "2", "3-5", "6<", "0"),
  	levels=c("1", "2", "3-5", "6<", "0"))
  table(BRFSS.08$asdrvcat)

 # Two categories

  BRFSS.08$asdrvcat2<-cut(BRFSS.08$asdrvist, aer2, include.lowest=TRUE,
    labels=c("Yes", "No"),
  	levels=c("Yes", "No"))
  table(BRFSS.08$asdrvcat2)


## 5. During the past 12 months, how many times did you see a doctor, 
## nurse or other health professional for urgent treatment of worsening asthma symptoms ##

table(BRFSS.08$asrchup)

  # Five categories

  BRFSS.08$asrchupcat<-cut(BRFSS.08$asrchup, aer, include.lowest=TRUE,
    labels=c("1", "2", "3-5", "6<", "0"),
    levels=c("1", "2", "3-5", "6<", "0"))
  table(BRFSS.08$asrchupcat)

 # Two categories

  BRFSS.08$asrchupcat2<-cut(BRFSS.08$asrchup, aer2, include.lowest=TRUE,
    labels=c("Yes", "No"),
  	levels=c("Yes", "No"))
  table(BRFSS.08$asrchupcat2)



## 6. During the past 12 months, activity limitation days##

table(BRFSS.08$asactlim)

  aal<-c(0,1, 2, 5, 10, 887, 888)

  # Five categories

  BRFSS.08$asactlimcat<-cut(BRFSS.08$asactlim, aal, include.lowest=TRUE,
    labels=c("1", "2", "3-5", "6-10","11<", "0"),
    levels=c("1", "2", "3-5", "6-10","11<", "0"))
  table(BRFSS.08$asactlimcat)

 # Two categories

  aal2<-c(0, 887, 888)

  BRFSS.08$asactlimcat2<-cut(BRFSS.08$asactlim, aal2, include.lowest=TRUE,
    labels=c("Yes", "No"),
    levels=c("Yes", "No"))
  table(BRFSS.08$asactlimcat2)



## 7. During the past 30 days symptoms 

  table(BRFSS.08$asymptom)

  BRFSS.08$asymptom2<-as.factor(BRFSS.08$asymptom)

	label.asy<-list(BRFSS.08$asymptom2, Less1week="1", OneTwiceweek="2", More2week="3",
 	Everyday="4", Everyday="5", No="8")

	levels(BRFSS.08$asymptom2)<- label.asy
	
 	BRFSS.08$asymptom2<-factor(BRFSS.08$asymptom2)
  table(BRFSS.08$asymptom2)         
 	summary(BRFSS.08$asymptom2)

  ## Two categories

  BRFSS.08$asymptom3<-as.factor(BRFSS.08$asymptom)

  label.asy2<-list(BRFSS.08$asymptom3, Yes="1", Yes="2", Yes="3",
 	Yes="4", Yes="5", No="8")

	levels(BRFSS.08$asymptom3)<- label.asy2
	
 	BRFSS.08$asymptom3<-factor(BRFSS.08$asymptom3)
  table(BRFSS.08$asymptom3)         
 	summary(BRFSS.08$asymptom3)



## 8. During the past 30 days symptoms of asthma make it difficult for you to stay asleep

  table(BRFSS.08$asnoslep)

  BRFSS.08$asnoslep2<-as.factor(BRFSS.08$asnoslep)

  label.aslp<-list(BRFSS.08$asnoslep2, Oneortwo="1", ThreetoFour="2", Five="3",
 	SixtoTen="4", MoreTen="5", No="8")

	levels(BRFSS.08$asnoslep2)<- label.aslp
	
 	BRFSS.08$asnoslep2<-factor(BRFSS.08$asnoslep2)
  table(BRFSS.08$asnoslep2)         
 	summary(BRFSS.08$asnoslep2)

  ## Two categories

  BRFSS.08$asnoslep3<-as.factor(BRFSS.08$asnoslep)

  label.aslp2<-list(BRFSS.08$asnoslep3, Yes="1", Yes="2", Yes="3",
   Yes="4", Yes="5", No="8")

	levels(BRFSS.08$asnoslep3)<- label.aslp2
	
 	BRFSS.08$asnoslep3<-factor(BRFSS.08$asnoslep3)
  table(BRFSS.08$asnoslep3)         
 	summary(BRFSS.08$asnoslep3)


## 9. During the past 30 days, how many days did you take a prescription asthma medication to PREVENT an asthma attack

table(BRFSS.08$asthmed3)

  BRFSS.08$asmedcat<-as.factor(BRFSS.08$asthmed3)

  label.asm<-list(BRFSS.08$asmedcat, Oneto14="1", Fifteen24="2", twenty5to30="3", No="8")

	levels(BRFSS.08$asmedcat)<- label.asm
	
 	BRFSS.08$asmedcat<-factor(BRFSS.08$asmedcat)
  table(BRFSS.08$asmedcat)         
 	summary(BRFSS.08$asmedcat)

  ## Two categories

  BRFSS.08$asmedcat2<-as.factor(BRFSS.08$asthmed3)

  label.asm2<-list(BRFSS.08$asmedcat2, Yes="1", Yes="2", Yes="3",
    No="8")

	levels(BRFSS.08$asmedcat2)<- label.asm2
	
 	BRFSS.08$asmedcat2<-factor(BRFSS.08$asmedcat2)
  table(BRFSS.08$asmedcat2)         
 	summary(BRFSS.08$asmedcat2)



table(BRFSS.08$asinhalr)


#Remove objects that will not be use

rm(list= ls()[!(ls() %in% c('BRFSS.08', 'nN'))])

## Save data as .csv BRFSS 2008 managed DN Data mangemnt##

## Save image ##
save.image(file = "..../Data/BRFSS/Managed/BRFSS08_DM.RData")

