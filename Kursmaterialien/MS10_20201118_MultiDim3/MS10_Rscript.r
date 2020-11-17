
## ----libraries, warning = FALSE, message = FALSE----------------------------------------
library(mirt)



## Load the MDS-Data
urlfile = "https://raw.githubusercontent.com/CarolinaFellinghauer/UNIZH_HS2020_Rasch/master/Data/WHO_MDS_course.csv"
MDS.data=read.csv(url(urlfile), sep= ";")
dim(MDS.data)
colnames(MDS.data)

# Select only the MDS items, these are all columns except the first four. 
# It starts with vision.
mds.items=colnames(MDS.data)[-(1:4)]
data.mds=MDS.data[, mds.items]
data.mds=data.mds-1


## 1) Unidimensional Rasch Analysis

mds.1dim =mirt($$$)



## PCA of the residuals correlations
cor.mds = r$$$() #gives Q3 the correlation of the standardized residuals

PCA.mds = p$$$(cor.mds, center=TRUE, retx=TRUE)  #PCA

PC1.neg = which($$$0) # negative sign on PC1
PC1.pos = which($$$>0) # positive sign on PC1


## Extract the coefficients
 coef.mds.1dim = c$$$(mds.1dim, simplify = $$$)
 coef.mds.1dim




## Anchoring the items loading negatively

 #First run a Rasch analysis of the negatively loading items, as usual, but with option pars = "values"

  mod.mds.dim1 = mirt($$$, 1, itemtype=$$$, pars = "values") 

  #get the coefficient matrix and replace the values with those from the common calibration
  #to fix the first ,second, third, and fourth threshold.

  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d1"), "value"] = coef.mds.1dim$items[PC1.neg,"d1"]
  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d2"), "value"] = coef.mds.1dim$items[PC1.neg,"d2"]
  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d3"), "value"] = coef.mds.1dim$items[PC1.neg,"d3"]
  mod.mds.dim1[which(mod.mds.dim1[ ,"name"]=="d4"), "value"] = coef.mds.1dim$items[PC1.neg,"d4"]
  
  #finally set this, otherwise the anchored analysis will kept fixed.
  mod.mds.dim1$est = FALSE
     
     

    


## Do the same with the items loading positively

$$$$
$$$$


## Run the anchored analysis by setting pars = to the parameter estimates fixed previously
   
   mod.PCM.dim1 = mirt($$$, pars = $$$) #anchored analysis
   
   mod.PCM.dim2 = mirt($$$) #anchored analysis



## Extract the Theta estimates including the SE using fscores()

Theta.dim1 = $$$
Theta.dim2 = $$$



## Making a scatterplot: thetas dim 1 versus thetas dim 2

plot($$, $$,
     xlab = "Dim 1: PC1 negative loading",
     ylab = "Dim 2: PC1 positive loading",
     main = "Ability Estimates per Dimension",
     pch = 20, 
     col = "darkgrey")

#Trace a diagonal 

segments(x0=$$, y0 = $$, x1 =$$, y1 = $$, col = "red")



## Make the individual pairwise t-tests based on the previous formula

   T_test_abs = abs($$- $$)/sqrt($$^2 + $$^2)



## Proportion above 2.5

$$


