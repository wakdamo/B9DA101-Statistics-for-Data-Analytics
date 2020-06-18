# B9DA101-Statistics-for-Data-Analytics
B9DA101 Statistics for Data Analytics 
          B9DA101-Statistics for Data Analytics - Assignment 1
Amol Wakde – 10543430



Que 1) Consider a real-world, relational dataset. This dataset must have at least 2 categorical and 2 continuous variables.    

We are exploring fishermen dataset which consist of various discrete and continuous variables as shown in above image.  In broad manner it tells us effect of mercury exposure on fishermen and non-fishermen group.

Description: Various elements related to mercury levels among fishermen and a control group of non-fishermen.
Variables/names :
Fisherman indicator     (fisherman)
Weight in kg                (weight)
Fish meals per week     (fishmlwk)
Parts of fish consumed: (fishpart)
    0=none,
    1=muscle tissue only,
    2=mt and sometimes whole fish,
    3=whole fish
Total Mercury in mg/g   (total_mercury)


 View(fishermen)  #imported using R 
 
(Note : attached screenshot to show dataset variables only and not all the rows) 

Question 1                                       
(a)    Describe the dataset using appropriate plots/curves/charts,…   (7) 

Answer :-

1.	Violin chart shows us  distribution of data and its shape  between our controlled group called as fishermen(0.0) and non-fishermen(1.0).


library(ggplot2)     #to use ggplot2 package extensively, to store it in memory to reuse
ggplot(data=fishermen, aes(x=fisherman, y=age, group=fisherman)) +geom_violin(color='black')

 


2 . Boxplots shows us different categories under “fishpart” variable which consist of categories as (0,1,2,3). It also tells us about the outliers present in respective categories. 
Four classes for "Parts of fish consumed":
 0=none 
 1=muscle tissue only 
 2=mt and sometimes whole fish
 3=whole fish  

ggplot(fishermen, aes(x=fishpart , y=TotHg, group=fishpart)) + 
geom_boxplot() 

 


2.	Density plot using polygon() represents the distribution of the numeric variable MeHg. Where MeHg is the level of Methyl Mercury available in the hair of fishermen. 

dens <-density(fishermen$MeHg)
plot(dens, frame = FALSE, col = "steelblue", 
main = "Density plot of MeHg") 
polygon(dens, col = "steelblue")
 
        
4.	Histogram plots the distribution of the “age” of fishermen/non-fishermen available in our dataset as follows :
qplot(fishermen$age,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Age", 
      xlab = "Age",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(20,50))

 

(b)    Consider one of continuous attributes, and compute central and variational measures (8)

######################”Central Measure's” ####################### 

#Mean
xbar=mean(height)
Xbar
#Median

med=median(height)
med

#Mode -> using getMode() function, as R does not have standard built in function.

getMode <-function(height) {
uniqH <- unique(height)
uniqH[which.max(tabulate(match(height,uniqH)))]
}
mode<- getMode(height)
mode
 
Output :->  


#######################”Measures of Shape”######################## 

install. Packages("moments")     # to install moments package 
library(moments)     
                 
#Moments
Computes the (optionally centered and/or absolute) sample moment of a certain order.

moment(fishermen$height, order = 2 )

#Kurtosis
 Tells how tall and sharp the central peak is, relative to a standard bell curve

kurtosis(fishermen$height)

#Skewness 
 Tells  the amount and direction of skew (departure from horizontal symmetry).

skewness(fishermen$height)

Output :->
 

######################”Variational Measures” #####################

#Range
r=range(height)
r
#Variance
v=var(height)
v
#Standard Deviation
std=sd(height)
std
#Interquartile Range
iqr=IQR(height)
iqr
        Output:->        
                                               
(c)    For a particular variable of the dataset, use Chebyshev's rule, and propose one-sigma interval. Based on your proposed interval, specify the outliers if any.        (10)

Using Chebyshev's rule

#one sigma interval  
L = xbar-std   
U = xbar+std
int=c(L,U)
int
                              


# Code to check if there any outliers 
int <- height
for(int in height) { print(int)
if (int < 167.59) {
print("This is an outlier")
} else if(int >167.59 & int > 181.23) {
print("This is not an outlier")
}else {
print("This is an outlier")
}
}

Output :-> After executing above code we can easily get the outliers from the chosen variable fishermen$height.  Here 160,187,164,160 etc.....

 




(d)    Explain how the box-plot technique can be used to detect outliers. Apply this technique for one attribute of the dataset                 (10)

Boxplot Technique: -It’s a graphical representation based upon quartiles, also its smallest and the largest values. It helps to visualize the shape of data distribution. 

We can detect the outliers as follows:-

#Quantile and IQR
Q=quantile(height)
Q
Q_u=Q[4]
Q_l=Q[2]
IQR=Q_u-Q_l              #Interquartile Range 
LW=Q_l-1.5*IQR         #Lower Whisker
UW=Q_u+1.5*IQR      #Upper Whisker
I=c(LW,UW)
I

  

I<-height
for(I in height){print(I)
if(I < 155) {
print("This is an outlier")
} else if(I > 155 & I < 195) {
print("This is not an outlier")
} else {
print("This is an outlier")
}
} 

Output :->
We can see the outliers as follows from the below output e.g. 154 here is an outlier.
 

Visualization outliers using “geom_boxplot ()” from ggplot2: -
# Change outlier, color, shape and size
ggplot (fishermen, aes (x=fisherman, y=height)) + 
  geom_boxplot (outlier. color="red", outlier. shape=8,  #red color for outliers
                outlier. size=4) 

 


***************************Question2**********************************

Question 2                                                                                           (35 Marks)

a.	Select four variables of the dataset and propose an appropriate probability model to quantify uncertainty of each variable.                             (10)
                                      
b.	 For each model in part (a), estimate the parameters of model.     (10)

c.	Express the way in which each model can be used for the predictive analytics, then find the prediction for each attribute.                 (15)                   

Note: - Please see answers for above mentioned questions (a, b, c) in groups as per my variables respectively.

Answer: -

1.Variable :-> restime (Residence Time in years - Exponential Distribution)

y=fishermen$restime
plot(y,pexp(y))     #plot to check the nature of the data inside variable
 
p=table(y)/sum(table(y))
P
 
mean(y)
  

Consider a Problem Statement: -
suppose the mean retime period of a person is 4.5 years, find the probability if the  restime period is less than 3 years for a person.
 
The restime completion period is equal to one divided by the mean retime period, which is 1/4.5 
 Hence we apply the function "pexp" of the exponential distribution with rate=1/4.5.
 
pexp (3,1/4.5)
  
So, the probability of the restime completion under 3 years is equal to 48.7%.


2.Variable :-> height (in cm – Applying Normal Distribution)

y=fishermen$height
plot(y,pnorm(y))         #plot to check the nature of the data inside variable
 
p=table(y)/sum(table(y))
p
 
mean=mean(y)
sd=sd(y)
 

Problem Statement :->
Consider x=180
Since we are interested in calculating height more than 180 -->P(x>=180). Hence, we are interested in upper tail of the normal distribution.

From above observation, we are using normal distribution function on variable called height 
 
 pnorm(x, mean,sd,lower.tail=FALSE)    # help(pnorm) 

 
So the probability of the height equal to 180 or more is 20.6%.


3. Variable :->fishmlwk  (Fish meals per week -Poisson Distribution) 
y=fishermen$fishmlwk     
plot(y,ppois(y,lambda=6.52))   #6.52 from the below problem statement that we consider 

#plot to check the nature of the data inside variable
 
p=table(y)/sum(table(y))
P
mean(y)

  

Problem Statement :-> 
Suppose the mean of fish meals served to person is 6.52/week . Find the probability of fish meals served  more than 9/week to the person.
 
1-ppois(9, lambda=6.52)   # lower tail 
    
    or
    
ppois(9, lambda=6.52, lower=FALSE)   # upper tail 
  
So, the probability of fish meals served more than 9/week to a person is 12.4%

4 Variable :-> fishpart (Multinomial Distribution) 
y=fishermen$fishpart
plot(y,dmultinom(x,n,p)
p=table(y)/sum(table(y))      #To get the frequency distribution table
p
 
 

Problem Statement :->
As we have 4 classes for "Parts of fish consumed": 0=none, 1=muscle tissue only, 2=mt and sometimes whole fish, 3=whole fish  (fishpart)
from the above estimation, we will compute the probability of this event that out of n=4 samples i.e.
#parts of the fish consumed from each class.

n=4
x=rep(1,4) 
prob=dmultinom(x,n,p)
prob
 
Conclusion: -
Hence, the probability of parts of the fish consumed from each class is 0.016.

*****************************Question 3********************************

Que(a) Consider two categorical variables of the dataset, develop a binary decision-making strategy to check whether two variables are independent at the significant level alpha=0.01.  
To do so,                                                                                                             (10)
    State the hypotheses.                                                            
    Find the statistic and critical values.                               
    Explain your decision and Interpret results. 
   
 Answer:->
Consider “fisherman” and “fishpart” as a variable of interest since both are categorical variables.
1.	Using Chi-squared Test of Independence

Step 1) Prepare the contingency table 

tbl=table(fishermen$fisherman,fishermen$fishpart)
tbl  
 
 
Step2) We will apply the chisq.test function to the contingency table tbl, to find out the p-value
chisq.test(tbl)
  

Step3) To avoid the warning message observed in above test we will combine the tables and see the result 
 
ctbl = cbind(tbl[,"0"]+tbl[,"1"], tbl[,"2"] + tbl[,"3"]) 
ctbl 
  
Step4) chisq.test(ctbl) 
  

Conclusion :->
as the p-value = 0.0001589 is less than the significant level alpha=0.01, will reject the null hypothesis that the variables fisherman and fishpart are dependent.

Method 2

2.	Using Two-Tailed Variance Test (Using Test Value)
preparing the data for two tail population distribution.
 
Step 1: - Stating null and alternate hypothesis
H0 : (sigma_1)2 = (sigma_2)2
H1 : (sigma_1)2 != (sigma_2)2
Consider alp=0.01,n1=30,n2=30 , s1=2 and s2=3
 
Step 2: - Computing the test value
f test_value= s12/s22
f test_value= 32/22 = 4/9 = 0.444
 
Step 3: - Computing the critical value

critic_value1= f(1-(alp/2)),n1-1,n2-1
= f(1-(0.01/2)),30-1,30-1=f(1-0.005),29,29
= f (0.995),29,29 = 1.62   #From F-table
 
critic_value2=f(alp/2),n1-1,n2-1
=f(0.01/2),30-1,30-1=f(0.005),29,29
=2.41        #From F-table
 
Step 4: - Decision and Conclusion
 
      f test_value= 0.444 < critic_value1 =1.62   and 
      f test_value= 0.444  < critical_value2 =2.41  

 Conclusion :->
Since test_value is less than the critical_value, we reject the null hypothesis. And we can conclude that the  variables are independent of each other.
 
##################################################
 
Que(b) Consider one categorical variable, apply goodness of fit test to evaluate whether a candidate set of probabilities can be appropriate to quantify the uncertainty of class frequency at the significant level alpha=0.05.                (10)                              

Answer:-> 
we are choosing fishpart  as a variable of interest since it’s a categorical/multinomial.
we have exact 4 categories under fishpart as “0”,”1”,”2” and ”3”.

Goodness of fit test for multinomial distribution (By using Function)

Step1: - We apply the table function to compute the frequency distribution to the variable of interest “fishpart”.

fishpart.freq = table(fishermen$fishpart)
fishpart.freq 

  

Step2: - Convert this frequency of fishpart in percentile as follows:

percent = fishpart.freq / sum(fishpart.freq) * 100
percent
  
Step3: - Save the fishpart statistics in the variable called fishpart.prob. 

fishpart.prob=c(.07,.20,.65,.08)
fishpart.prob
  

Step4) Apply the chisq.test function and perform the Chi-Squared test.
chisq.test(fishpart.freq, p=fishpart.prob)

  

Conclusion: ->
As the p-value 0.9464 is greater than the .05 significance level, we do not reject the null hypothesis that the sample data in fishermen supports the fishpart stats.

Method 2

Goodness of Fit Test (Using Test Value)
Consider alpha = 0.05   and
 p1 = p2 = p3 = 2p4 
2p4+2p4+2p4+p4=1     so p4=1/7

Hence
p1=2/7,   p2=2/7,   p3=2/7,   and p4=1/7

Step 1: - Prepare Observation Contingency Table

We apply the table function to compute the frequency distribution to the variable of interest “fishpart”.
fishpart.freq = table(fishermen$fishpart)
fishpart.freq 
and
Observation table
obs_tab= c (10,28,88,9)
obs_tab
 
No. of observations = n 
n=10+28+88+9

Step2: - Prepare Expected Contingency table

Calculate expectation values as follows 
e1 = n*p1
e1
e2 = n*p2
e2
e3 = n*p3
e3
e4 = n*p4
e4
 

Expected contingency Table
exp_tab=c(e1,e2,e3,e4)
exp_tab
 

Step3: - Calculate Test Value

t1 <- (10 - e1) ^2/e1
t1
t2 <- (28 - e2) ^2/e2
t2
t3 <- (88 - e3) ^2/e3
t3
t4 <- (9 - e4) ^2/e4
t4
test_val <- t1+t2+t3+t
test_val
 

Test_val = 0.93 

Step4: - Calculate Critical value
                                       
            c_value = X^2_alpha(k-1)     

            c_value = 7.82                                            #From Chi-Square Distribution Table

step5:- Decision and Conclusion
              t_value= 0.93  <  c_value=7.82 

Conclusion:-> 
As test value is less than the critical value, we do not reject the null hypothesis claim that sample data in fishermen supports the fishpart stats. 

##################################################
 
Que(c) Consider one continuous variable in the dataset and apply test of mean for a proposed candidate of μ at the significant level alpha=0.05.                          
Answer:->
   
1.	Using 1 sample t-test 
Step1:- Prepare the data
Consider "MeHg" as continuous variable from the fishermen dataset 
data <- c(fishermen$MeHg)
head(data,20)
 

Step2:- Calculate the mean 
mean(fishermen$MeHg)
  

Step3:- Apply test of mean 
t.test(fishermen$MeHg,alternatilve="less")    #mu by default, it's 0

  
#Below code to get the exact p-value(not in exponential),since p-value is < 2.2e-16 from above output.

Step4: - Apply t-test 
csq <- t.test(tbl) 
csq$p.value
 
 
Conclusion :->
Since p-value 0.079 is not less than the significant level alpha of 0.05, we cannot reject the null hypothesis claim.

OR

2.Using Lower Tailed Test of Mean 

Step 1: - Stating null and alternate hypothesis

H0 : mu > mu_0
H1 : mu <= mu_0

str(fishermen$MeHg) #to display the structure of the dataset object MeHg
mean(fishermen$MeHg)   #to calculate mean 
mean
sigma=sd(fishermen$MeHg)  # to calculate sd    
sigma 

So, mean=3.64, sigma=2.85  
alp=0.05, n=30, mu_0=3.58   #Assume alp, n, and mu_0 from dataset object

Step 2: - Computing the test value. 

test_value= (mean - mu_0) / (sigma/ sqrt(n)) 
test_value= (3.64-3.58) / (2.85/sqrt(30)) = 0.12

 Step 3: - Computing the critical value.

c_value= -|Z_0.05| = -1.64  
 
Step 4: - Decision and Conclusion.
 
test_value is > c_value 

Conclusion :->
Since test_value =0.12 is greater than the c_value = -1.64, we cannot reject the   null hypothesis claim.

