# HW-6
HW 6

````

load("/cloud/project/acs2017_ny_data.RData")

Plots are attached to the repository 

> attach(acs2017_ny)
> acs2017_ny$LABFORCE <- as.factor(acs2017_ny$LABFORCE)
> levels(acs2017_ny$LABFORCE) <- c("NA","Not in LF","in LF")
> 
> acs2017_ny$MARST<- as.factor(acs2017_ny$MARST)
> 
> levels(acs2017_ny$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")
> 
> summary(acs2017_ny$LABORFORCE)
Length  Class   Mode 
     0   NULL   NULL 
> summary(acs2017_ny$LABFORCE) 
       NA Not in LF     in LF 
    33427     64722     98436 
> summary(acs2017_ny$MARST)
married spouse present  married spouse absent 
                 75194                   4456 
             separated               divorced 
                  3437                  14730 
               widowed          never married 
                 10307                  88461 
> summary(acs2017_ny$female)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  1.0000  0.5156  1.0000  1.0000 
> levels(acs2017_ny$female) <- c("man", "woman")
> summary(acs2017_ny$female)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  1.0000  0.5156  1.0000  1.0000 
> levels(acs2017_ny$EDUC)
 [1] "N/A or no schooling"      
 [2] "Nursery school to grade 4"
 [3] "Grade 5, 6, 7, or 8"      
 [4] "Grade 9"                  
 [5] "Grade 10"                 
 [6] "Grade 11"                 
 [7] "Grade 12"                 
 [8] "1 year of college"        
 [9] "2 years of college"       
[10] "3 years of college"       
[11] "4 years of college"       
[12] "5+ years of college"      
> summary(acs2017_ny$EDUC)
      N/A or no schooling Nursery school to grade 4 
                    11879                     14240 
      Grade 5, 6, 7, or 8                   Grade 9 
                    13079                      4088 
                 Grade 10                  Grade 11 
                     4644                      5337 
                 Grade 12         1 year of college 
                    55119                     19947 
       2 years of college        3 years of college 
                    14065                         0 
       4 years of college       5+ years of college 
                    30802                     23385 
> View(acs2017_ny$MARST)
> 
> acs2017_ny$age_bands <- cut(acs2017_ny$AGE,breaks=c(0,25,35,45,55,65,100))
> 
> table(acs2017_ny$age_bands,acs2017_ny$LABFORCE)
          
              NA Not in LF in LF
  (0,25]   31680     11717 13256
  (25,35]      0      4271 20523
  (35,45]      0      4064 18924
  (45,55]      0      5406 21747
  (55,65]      0     10563 18106
  (65,100]     0     28701  5880
> 
> pick_use1 <- (acs2017_ny$AGE >=25) & (acs2017_ny$AGE <= 35) 
> 
> dat_use1 <- subset(acs2017_ny, pick_use1)
> 
> model_linear <- lm(female ~ AGE  + I(AGE^2) + educ_advdeg + POVERTY + INCWAGE + MARST, data = dat_use1)
> 
> summary(model_linear)

Call:
lm(formula = female ~ AGE + I(AGE^2) + educ_advdeg + POVERTY + 
    INCWAGE + MARST, data = dat_use1)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.8360 -0.4769  0.2569  0.4955  1.3102 

Coefficients:
                             Estimate Std. Error
(Intercept)                 8.779e-01  3.012e-01
AGE                        -1.491e-02  2.019e-02
I(AGE^2)                    1.744e-04  3.356e-04
educ_advdeg                 1.656e-01  8.247e-03
POVERTY                    -4.684e-06  1.998e-05
INCWAGE                    -1.222e-06  6.559e-08
MARSTmarried spouse absent -8.871e-02  1.705e-02
MARSTseparated              8.435e-02  2.431e-02
MARSTdivorced               4.295e-02  1.836e-02
MARSTwidowed                4.323e-02  6.766e-02
MARSTnever married         -1.071e-01  6.835e-03
                           t value Pr(>|t|)    
(Intercept)                  2.914 0.003567 ** 
AGE                         -0.739 0.460005    
I(AGE^2)                     0.520 0.603382    
educ_advdeg                 20.085  < 2e-16 ***
POVERTY                     -0.234 0.814634    
INCWAGE                    -18.637  < 2e-16 ***
MARSTmarried spouse absent  -5.202 1.99e-07 ***
MARSTseparated               3.469 0.000523 ***
MARSTdivorced                2.339 0.019359 *  
MARSTwidowed                 0.639 0.522882    
MARSTnever married         -15.673  < 2e-16 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4909 on 27230 degrees of freedom
Multiple R-squared:  0.0364,	Adjusted R-squared:  0.03604 
F-statistic: 102.9 on 10 and 27230 DF,  p-value: < 2.2e-16

> 
> plot(model_linear)

> model_logit <- glm(female ~ AGE + I(AGE^2) + educ_advdeg + POVERTY + INCWAGE + MARST,family = binomial, data = dat_use1)
> 
> summary(model_logit)

Call:
glm(formula = female ~ AGE + I(AGE^2) + educ_advdeg + POVERTY + 
    INCWAGE + MARST, family = binomial, data = dat_use1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8160  -1.1423   0.7597   1.1682   2.9250  

Coefficients:
                             Estimate Std. Error
(Intercept)                 1.410e+00  1.249e+00
AGE                        -5.325e-02  8.374e-02
I(AGE^2)                    6.076e-04  1.393e-03
educ_advdeg                 7.160e-01  3.542e-02
POVERTY                     1.546e-04  8.734e-05
INCWAGE                    -6.626e-06  3.776e-07
MARSTmarried spouse absent -3.696e-01  7.028e-02
MARSTseparated              3.522e-01  1.042e-01
MARSTdivorced               1.783e-01  7.715e-02
MARSTwidowed                1.554e-01  2.871e-01
MARSTnever married         -4.423e-01  2.848e-02
                           z value Pr(>|z|)    
(Intercept)                  1.128 0.259180    
AGE                         -0.636 0.524877    
I(AGE^2)                     0.436 0.662645    
educ_advdeg                 20.215  < 2e-16 ***
POVERTY                      1.770 0.076776 .  
INCWAGE                    -17.547  < 2e-16 ***
MARSTmarried spouse absent  -5.259 1.45e-07 ***
MARSTseparated               3.380 0.000724 ***
MARSTdivorced                2.311 0.020860 *  
MARSTwidowed                 0.541 0.588337    
MARSTnever married         -15.530  < 2e-16 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 37762  on 27240  degrees of freedom
Residual deviance: 36699  on 27230  degrees of freedom
AIC: 36721

Number of Fisher Scoring iterations: 4

> model_probit <- glm(female ~ AGE + I(AGE^2) + educ_advdeg + POVERTY + INCWAGE + MARST,family = binomial (link = 'probit'), data = dat_use1)
> 
> summary(model_probit)

Call:
glm(formula = female ~ AGE + I(AGE^2) + educ_advdeg + POVERTY + 
    INCWAGE + MARST, family = binomial(link = "probit"), data = dat_use1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8242  -1.1401   0.7681   1.1680   3.0764  

Coefficients:
                             Estimate Std. Error
(Intercept)                 9.496e-01  7.770e-01
AGE                        -3.749e-02  5.208e-02
I(AGE^2)                    4.436e-04  8.660e-04
educ_advdeg                 4.368e-01  2.172e-02
POVERTY                     4.529e-05  5.366e-05
INCWAGE                    -3.644e-06  2.185e-07
MARSTmarried spouse absent -2.296e-01  4.382e-02
MARSTseparated              2.182e-01  6.392e-02
MARSTdivorced               1.103e-01  4.774e-02
MARSTwidowed                9.577e-02  1.772e-01
MARSTnever married         -2.756e-01  1.768e-02
                           z value Pr(>|z|)    
(Intercept)                  1.222 0.221668    
AGE                         -0.720 0.471541    
I(AGE^2)                     0.512 0.608510    
educ_advdeg                 20.109  < 2e-16 ***
POVERTY                      0.844 0.398634    
INCWAGE                    -16.677  < 2e-16 ***
MARSTmarried spouse absent  -5.239 1.61e-07 ***
MARSTseparated               3.415 0.000639 ***
MARSTdivorced                2.311 0.020842 *  
MARSTwidowed                 0.541 0.588830    
MARSTnever married         -15.591  < 2e-16 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 37762  on 27240  degrees of freedom
Residual deviance: 36721  on 27230  degrees of freedom
AIC: 36743

Number of Fisher Scoring iterations: 4

Plots are attached to the repository

### Prediction Models for Gender, Marital Status and Labor Foce Participation
0 = Male 
1 = Female


> 
> set.seed(12345)
> index<-sample(x=2,size=nrow(dat_use1),replace=TRUE,prob=c(0.8,0.2))
> 
> train<-dat_use1[index==1,]
> 
> test<-dat_use1[index==2,]
> 
> trainmodel <- glm(female ~ AGE + educ_advdeg + POVERTY + INCWAGE + MARST,family = binomial, data = train)
> 
> prob<-predict(object=trainmodel,newdata=test,type="response")
> 
> pred<-cbind(test,prob)
> 
> pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
> 
> Laborforce prediction <- table(pred$LABFORCE,pred$predict)
Error: unexpected symbol in "Laborforce prediction"
> labormodel<-table(pred$LABFORCE,pred$predict)
> 
> summary(labormodel)
Number of cases in table: 5331 
Number of factors: 2 
Test for independence of all factors:
	Chisq = NaN, df = 2, p-value = NA
	Chi-squared approximation may be incorrect
> labormodel
           
               0    1
  NA           0    0
  Not in LF  487  451
  in LF     2506 1887
> Gendermodel <-table(pred$female,pred$predict)
> summary(Gendermodel)
Number of cases in table: 5331 
Number of factors: 2 
Test for independence of all factors:
	Chisq = 111.84, df = 1, p-value = 3.88e-26
> Gendermodel
   

       0    1
  0 1667  961
  1 1326 1377
> MaritalStatmodel <-table(pred$MARST,pred$predict)
> summary(MaritalStatmodel)
Number of cases in table: 5331 
Number of factors: 2 
Test for independence of all factors:
	Chisq = 2179.3, df = 5, p-value = 0
	Chi-squared approximation may be incorrect
> MaritalStatmodel
                        
                            0    1
  married spouse present  328 1481
  married spouse absent   125   55
  separated                 1   92
  divorced                  5  137
  widowed                   1    7
  never married          2533  566
