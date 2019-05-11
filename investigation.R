# PURPOSE: Misc investigation/calculation of metrics for Footprint app.

library(data.table)

##### TVs per household #####
# Based on 2011 TV per household data from the ACMA: https://www.acma.gov.au/-/media/Research-and-Analysis/Research/pdf/Television-sets-in-Australian-households-2011.PDF?la=en
# Mixed with 2011 VIC age demographics from ABS: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Dec%202011?OpenDocument (data cube - Population by Age and Sex Tables)
# Projected based on Nakono databank growth rate (feeds from data sources inc the above ACMA report): https://www.nakono.com/tekcarta/databank/full/33/


### Generate scale-up factor to account for growth of TV's per household from 2011 (ACMA data) to 2019.
# Nakono databank average TV per household values. 2000 to 2012.
data_nakono = data.table(year = 2000:2012 ,televisions = c(1.84,	1.87,	1.90,	1.94,	1.97,	2.00,	2.04,	2.07,	2.10,	2.13,	2.17,	2.20,	2.23))

# Rebase at year 2000. (ie, offset = years since 2000)
data_nakono[, base_2000 := 0:12]

# Generate simple lm to be used to project growth to 2019.
lm_tv_time = lm(televisions~base_2000, data_nakono)
#plot(lm_tv_time) # Looks good.

# Check fit.
with(data_nakono, plot(base_2000, televisions))
abline(lm_tv_time) # Fits well.

# Predict out to 2019, TV's per household
tvph_2019 = as.numeric(predict.lm(lm_tv_time, data.table(base_2000 = 19)))

# Generate scale-up factor for multiplying volume of model built on 2011 data.
tvph_scaleup = as.numeric(tvph_2019/data_nakono[year == 2011]$televisions)


### Generate model for tv's per number of persons in household based on ACMA data (2011).

# ACMA data is based on TV's per number of adults in household.
data_acma = data.table(adults = 1:6 ,televisions = c(1.8, 2.3, 2.7, 3.3, 3.6, 4.0))

# Based on ABS data, scale up "adults per household" to "persons per household" based on adult/child population split @ June 2011. Using legal definition of child (<18yrs).
data_acma[, persons := adults/0.77164484]

# Create lm for predicting tv's per household based persons per household.
lm_tv_persons = lm(televisions~persons, data_acma)
#plot(lm_tv_persons)

# Check fit
with(data_acma, plot(persons, televisions))
abline(lm_tv_persons)

# Test.
predict(lm_tv_persons, data.table(persons=4))







##### Average PCs per household #####
# Based on 2011 TV per household data from Nakono (to 2012): https://www.acma.gov.au/-/media/Research-and-Analysis/Research/pdf/Television-sets-in-Australian-households-2011.PDF?la=en
# Laptop/desktop split based on Deloitte media consumer survey: http://landing.deloitte.com.au/rs/761-IBL-328/images/Media_Consumer_Survey_Report.pdf
# Scaled down to per-person level based on 2016 Census data on number of persons per household: https://quickstats.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/2?opendocument
# projected to 2019
data_nakono_pc = data.table(year = 2000:2012 ,pcs = c(1.34,	1.47,	1.60,	1.71,	1.82,	1.94,	2.08,	2.16,	2.25,	2.32,	2.40,	2.47,	2.53))

# Rebase at year 2000. (ie, offset = years since 2000)
data_nakono_pc[, base_2000 := 0:12]

# Generate simple lm to be used to project growth to 2019.
lm_pc_time = lm(pcs~base_2000, data_nakono_pc)
#plot(lm_pc_time)

# Check fit.
with(data_nakono_pc, plot(base_2000, pcs))
abline(lm_pc_time)

# Looks like not quite linear relationship
# Generate second order linear model
lm_pc_time_2 <- lm(pcs~I(base_2000^2)+base_2000, data=data_nakono_pc)

#Check fit
pr.lm_pc_time_2 <- predict(lm_pc_time_2)
with(data_nakono_pc, plot(base_2000, pcs))
lines(pr.lm_pc_time_2~base_2000,data=data_nakono_pc) # Looks much beter.

# Predict out to 2019, pcs per household
pc_ph = as.numeric(predict(lm_pc_time_2, data.table(base_2000 = 19)))

# Split this roughly between laptop and desktop ownership.
# Based on Deloitte survey of ownership at household level (not individual - not ideal, but best we have)
laptop_ph = .86*pc_ph/(.86+.63)
desktop_ph = .63*pc_ph/(.86+.63)

# Get value at a per-person level (assumption: ownership scales linearly with number of persons per household)
laptop_pp = laptop_ph/2.6
desktop_pp = desktop_ph/2.6


# export relevant models and values.
save(lm_tv_persons, tvph_scaleup, laptop_pp, desktop_pp, file = "avg_household.RData")


