# Author: Saeesh Mangwani
# Date: 2022-02-01

# Description: Defining internal data objects used in the waterbalance package

# wb_soil_storage ==========

# Numerical indices by month
mindex <- setNames(1:12, month.abb)
# Number of days by month
dindex <- setNames(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), month.abb)
