###################################################################################################
# This function make use of the land-use change theory developed by Johan Heinrich von Thunnen (1826)  
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
# Version: 1.0
# Date: 2022-02-25
# Author: Yi-Ying Chen 
# Contact email: yiyingchen@gate.sinica.edu.tw 
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
    #critial value 
    c_value=250.0 
    #geolocation of villages 
    geo_city <- data.frame(x=c(1,8),y=c(5,1))
  }
  #get numbers of cites
  n_city <- nrow(geo_city) 
  
  # create the topology by using x-index * y-index 
  # which will resulted in the slope along NE-SW  and mountain top in the N-E corner
  topo <- array(NA, dim=c(nx,ny))
  id = 0
  for (j in 1:ny) {
    for (i in 1:nx) {
      id <- id + 1
      topo[i,j] <- i*j
    }
  }
  # Assign land-use type to a 2D array lu_arr
  # 3:forest; 2:agriculture; 1:built-up 
  lu_arr <- array(3, dim=c(nx,ny)) 
  
  # city-loop; y-loop; x-loop
  for (k in 1:n_city) {
    # assign the land-type to city(type:1) 
    lu_arr[c(geo_city$x[k]), c(geo_city$y[k])] <- 1 
    
    # find the surrounding pixels and assign land-type: agriculture(type:2)
    for (j in 1:ny) {
      for (i in 1:nx) {
        # find the surrounding pixels and assign it to agricultural land
        del_xy <- sqrt(abs(i- geo_city$x[k])**2. + abs(j- geo_city$y[k])**2.)  
        if ((del_xy >= 1.0) & (del_xy <= sqrt(2)) )  lu_arr[i,j] <- 2   
      }#i-loop
    }#j-loop
  }#k-loop
  
  # copy original lu_type to tmp_arr 
  tmp_arr <- lu_arr
  
  # function for calculating the distance between two points/coordinates
  fun_dis <- function (xy_ref=c(5,5),  xy_col=c(7,8) ) {
    dis <- NA
    #calculate the erurian distance 
    dis <- sqrt((as.numeric(xy_col[1])-as.numeric(xy_ref[1]))**2. 
                + (as.numeric(xy_col[2])-as.numeric(xy_ref[2]))**2.) 
    #return the value of the distance 
    return(dis)
  }#end fun_dis
  
  # from city-1 to city-n
  dis_n <- array(NA, dim=c(nx,ny,n_city))
  # define geo_lcation of city center 
  for (k in 1:n_city) {
    # assign geo-information of city center to xy_ref 
    xy_ref <- c(geo_city$x[k] , geo_city$y[k]) 
    id = 0
    for (j in 1:ny) {
      for (i in 1:nx) {
        id <- id +1
        dis_n[i,j,k] <- fun_dis(xy_ref=xy_ref, xy_col=c(i,j) )
      }#end of j
    }#end of i
  }#end of k
  
  #rental calculation, r: profit/rental of the land refer to n-cites
  rent_n <- array(NA,dim=c(nx,ny,n_city)) 
  #
  for (k in 1: n_city) {
    id = 0
    for (j in 1:ny) {
      for (i in 1:nx) {
        id <- id +1
        # the most important equation for calculating the land profit/rental 
        rent_n[i,j,k] <- (p*y) - (l*w) - (k*q) - v* ( topo[i,j]*f_top + dis_n[i,j,k] )  
        #rent_2[i,j] <- (p*y) - (l*w) - (k*q) - v* dis2_topo[i,j] 
      } #end of ny
    } #end of nx
  } #end of n_city
  
  # create an array for assessing the final land rent using maximum rental value by comparing the rental from different cites
  rent_final <- array(NA, dim=c(nx,ny))
  for (j in 1:ny) {
    for (i in 1:nx) {
      # inital tmp as a negtive value  
      tmp <- -999.99 
      for (k in 1:n_city) {
        # rent comparison 
        if ( rent_n[i,j,k] >= tmp ) tmp <- rent_n[i,j,k] 
      }
      # assign the maximum rent to final rent 
      rent_final[i,j] <- tmp  
    }#end of j
  }#end of i
  
  #converting the lu_type based on the final rental values/array
  #copy the land-use information from the original land-use array "tmp_arr"  
  new_lu_arr <- tmp_arr
  #
  for (j in 1:ny) {
    for (i in 1:nx)  {
      # if the final profit/rental is larger than c_value for all  forest locations, 
      # the pixel thus covered/assigned to the arigulture(type:2)
      if ( (rent_final[i,j] >= c_value) & (tmp_arr[i,j] == 3) ) {  
        new_lu_arr[i,j] <- 2 
      }#end if
    }#end i
  }#end j 
  
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
    dev.new()
    par(mfrow=c(1,2))
    image.plot(lu_arr, col=rev(terrain.colors(10)),zlim=c(1,3), main="original land use type")
    contour(topo, add=TRUE)
    image.plot(new_lu_arr,col=rev(terrain.colors(10)),zlim=c(1,3), main="new land use type")
    contour(topo, add=TRUE)
  }#end of ld_plot 
  
  
} #end of fun_lulcc_toy
####################### 
fun_lulcc_toy(geo_city = data.frame(x=c(40,20,60,10),y=c(40,60,15,90)),
              f_top = 0.012,c_value =50,nx=100,ny=100)
