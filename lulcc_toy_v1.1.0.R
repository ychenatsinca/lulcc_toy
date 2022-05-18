###################################################################################################
# This function made use of the land-use change theory developed by Johann Heinrich von Thunen (1826)  
# to calculate the spatial distribution of land use change between forest and agricultural land
#
# >>model parameters<< 
# nx, ny: dimension x, y of the study domain 
# y: yield, p:price
# l: labors, w: wages 
# k: capital, q: cost of capital
# v: cost of transport 
# geo_city: geolocation of the assigned cites 
# 
# Version: 1.1.0
# Date: 2022-02-25
# Authors: Yi-Ying Chen and Meng-Hsuan Vivian Lin   
# Contact email: yiyingchen@gate.sinica.edu.tw or as0210250@gate.sinica.edu.tw
####################################################################################################
fun_lulcc_toy <- function(ld_plot=TRUE, c_value=200, y=100,p=10, l=30,w=10, k=3,q=80, v=20,f_top=0.01,
                          nx=50,ny=50, geo_city=data.frame(x=c(10,35,40), y=c(10,35,10) ) )
  # start the function
{
  #load libraries
  #library(raster)
  ld_go <- FALSE
  if (ld_go) {
    #model parameters 
    #array dimension of study area 
    nx=10;ny=10
    #y:yield 100 kg/km2; p:price 10 US/kg
    y=100; p=10
    #l:labors 50 person/km2; w:wages 5 US/person 
    l=50; w=5
    #k:capital 5 person/km2; q:cost of capital 25US/person
    k=5; q=25
    #v: cost of transport US per km 
    v=20 
    #critical value 
    c_value=250.0 
    #geo-location of villages 
    geo_city <- data.frame(x=c(1,8,3),y=c(5,1,2))
  }
  
  #get numbers of cites
  n_city <- nrow(geo_city) 
  # create topology using x-index * y-index 
  topo <- array(NA, dim=c(nx,ny))
  id = 0
  for (j in 1:ny) {
    for (i in 1:nx) {
      id <- id + 1
      topo[i,j] <- i*j
    }
  }
  
  # Assign land-use type to a 2D array lu_arr (3:forest; 2:agriculture; 1:built-up)
  lu_arr <- array(3, dim=c(nx,ny)) #assign default to 3 (forest)
  # from city-1 to city-n
  dis_n <- array(NA, dim=c(nx,ny,n_city))
  #rental calculation, r: profit/rental of the land refer to n-cites
  rent_n <- array(NA,dim=c(nx,ny,n_city))
  
  # city-loop; y-loop; x-loop
  for (k in 1:n_city) {
    # assign the land-type to city(type:1)
    lu_arr[c(geo_city$x[k]), c(geo_city$y[k])] <- 1
    # find the surrounding pixels and assign land-type: agriculture(type:2)
    for (j in 1:ny) {
      for (i in 1:nx) {
        # find the surrounding pixels and assign it to agricultural land
        del_xy <- sqrt(abs(i- geo_city$x[k])**2. + abs(j- geo_city$y[k])**2.) #calculate distance from city
        dis_n[i,j,k] <- del_xy
        rent_n[i,j,k] <- (p*y) - (l*w) - (k*q) - v* ( topo[i,j]*f_top + dis_n[i,j,k] )
        if ((del_xy >= 1.0) & (del_xy <= sqrt(2)) )  lu_arr[i,j] <- 2
      }#i-loop
    }#j-loop
  }#k-loop
  
  # create an array for assessing the final land rent using maximum rental value by comparing the rental from different cites
  rent_final <- array(NA, dim=c(nx,ny))
  #converting the lu_type based on the final rental values/array
  #copy the land-use information from the original land-use array "lu_arr"
  new_lu_arr <- lu_arr
  for (j in 1:ny) {
    for (i in 1:nx) {
      # initial tmp as a negative value
      tmp <- -999.99
      for (k in 1:n_city) {
        # rent comparison
        if ( rent_n[i,j,k] >= tmp ) tmp <- rent_n[i,j,k]
      }
      # assign the maximum rent to final rent
      rent_final[i,j] <- tmp
      # if the final profit/rental is larger than c_value for all  forest locations,
      # the pixel thus covered/assigned to the agriculture(type:2)
      if ( (rent_final[i,j] >= c_value) & (lu_arr[i,j] == 3) ) {
        new_lu_arr[i,j] <- 2
      } #end of if loop
    }#end of j
  }#end of i
  
  ld_plot <- TRUE
  if (ld_plot) {
    library("fields")
    #plot color images 
    par(mfrow=c(1,2)) 
    image.plot(dis_n[,,1],col=terrain.colors(10), main="distance of city 1 ")
    #contour(rent_2,add=TRUE)
    image.plot(rent_final,col=terrain.colors(10), main="land rent")
    #contour(rent_1,add=TRUE)
    
    # lulcc plot before and after
    #dev.new()
    par(mfrow=c(1,2))
    image.plot(lu_arr, col=rev(terrain.colors(10)),zlim=c(1,3), main="original land use type")
    contour(topo, add=TRUE)
    image.plot(new_lu_arr,col=rev(terrain.colors(10)),zlim=c(1,3), main="new land use type")
    contour(topo, add=TRUE)
  }#end of ld_plot 
  
  
} #end of fun_lulcc_toy
####################### 

# test to run the function of lulcc_toy()
fun_lulcc_toy(geo_city = data.frame(x=c(40,20,60,10),y=c(40,60,15,90)),
              f_top = 0.012,c_value =50,nx=100,ny=100)
