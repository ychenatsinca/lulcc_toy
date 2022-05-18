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
source("lulcc_toy_v1.1.4.R")


# model parameters 
#array dimension of study area 
nx=100;ny=100
# file name of the input topo-graphic file and soil PH values 
topo_file="dtm_taipei_100_100.tiff"

#y:yield 100 kg/km2; p:price 10 US/kg
#l:labors 50 person/km2; w:wages 5 US/person 
#k:capital 5 person/km2; q:cost of capital 25US/person
#v: cost of transport US per km 




#geo-location of villages 
geo_city <- data.frame(lon_x=c(78,80,50,20,10),
                       lat_y=c(48,35,75,65,40))



#get numbers of cites 
n_city <- nrow(geo_city)
n_crop <- nrow(crop_para)
n_year <- 1

txt_city <- c("I-Lan", "Ruo-Dong", "TPE", "Tao-Yuan", "Long-Tan")

# dynamics of demographic growth 
dyn_demo <- array(rep(1,n_city), dim=c(n_city, n_year) )

#assign nplot as two timeframe
year_txt <- seq(1921,2020, length=n_year)

#set layout of the plot as 2 by 2 
par(mfrow=c(1,1))


##### call fun_lulcc_toy() by a loop  n_years #####
for (iyear in 1:n_year) {
  
  #call function fun_lulcc_tyy()  and plot the result 
  fun_lulcc_toy(ld_plot=TRUE, 
                c_value=c_value,    f_demo=f_demo, agb_ratio=agb_ratio,
                topo_file=topo_file,f_top=f_top,   nx=nx,  ny=ny,
                geo_city = geo_city, crop_para= crop_para,
                dyn_demo=dyn_demo[,iyear],
                year_txt=year_txt[iyear] )
  #
}

dev.new()


#plot the three columns of the dataset as three lines and add a legend in
#the top right corner of the chart
matplot(y=t(dyn_demo), type = "b",pch=1:5, col = 1:5, lwd=2, lty = 1, axes=F,
        xlab = "Year", ylab="Demografic Index")
#add text in axis
axis(side = 1, at = 1:4, labels = year_txt,)
axis(side = 2 )
box()
legend("topleft", legend = txt_city, col=1:5, pch=1:5)



