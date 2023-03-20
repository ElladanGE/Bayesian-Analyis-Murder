# Bayesian Analyis of 

The homicides data set contains entries for all recorded homicides in London boroughs from 2003 to March
2022. The first of the month on which the homicide occurred is given as the recorded date. I have added
year (counting the year in the data with 2003 = 1, 2004 = 2 etc), month (which month of the year) and
season variables to the data. The data also includes information on the age, sex and ethnicity of the victim,
as well as whether the case was solved by the police or not. The method of killing is also recorded, as is
whether or not the case involved domestic abuse. A link to the raw data for interest is given on the cover
page. 

Here I present a single Bayesian analysis consisting of a hierarchical model, fit using brms, for the event that
a particular homicide is solved given the values of the relevant covariates in this data set. I then use your model to infer how the features of any particular homicide in London affect the probability
that the case has been solved (to date). 
