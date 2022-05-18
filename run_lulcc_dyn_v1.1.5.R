#
#This script called the fun_lulcc_toy() by considering a dynamics LULCC change 
# by passing the parameter, dyn_demo, which described the dynamics of demographic change
# for any specific city during a period of time-slice.    
#

#install the required R-packages
#install.packages("rgdal")
#install.packages("raster")
#install.packages("fields")

# load the function "fun_lulcc_toy()"
source("lulcc_toy_v1.1.5.R")



#geo-location of villages 
geo_city <- data.frame(lon_x=c(50,45,78,65),
                       lat_y=c(72,88,43,75))


#get numbers of cites 
n_city <- nrow(geo_city)
n_year <- 1

# dynamics of demographic growth 
dyn_demo <- array(rep(1,n_city), dim=c(n_city, n_year) )

#set layout of the plot as 2 by 2 
par(mfrow=c(1,1))


##### call fun_lulcc_toy() by a loop  n_years #####
for (iyear in 1:n_year) {
  
  #call function fun_lulcc_toy() and plot the result 
  fun_lulcc_toy(ld_plot=TRUE, geo_city=geo_city, c_value = 400, f_top=0.06 )
  #
  
}





