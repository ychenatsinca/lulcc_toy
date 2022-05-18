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
# f_top: topographic factor 
# tpt_city: total profit of the assigned cities 
# Version: 1.1.2
# First Date: 2022-02-25 
# revised 2022-03-10 add the consideration of the city expansion by demographic supported by agricultural production       
# revised 2022-03-16 add the second crop type 
# Authors: Yi-Ying Chen and Meng-Hsuan Vivian Lin   
# Contact email: yiyingchen@gate.sinica.edu.tw 
#             or as0210250@gate.sinica.edu.tw
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
    nx=100;ny=100
    #y:yield 100 kg/km2; p:price 10 US/kg
    y=100; p=10
    #l:labors 50 person/km2; w:wages 5 US/person 
    l=50; w=5
    #k:capital 5 person/km2; q:cost of capital 25US/person
    k=5; q=25
    #v: cost of transport US per km 
    v=20 
    #critical value 
    c_value=20.0
    #topo factor
    f_top=0.01
    #demographic factor
    f_demo=0.0005
    #maximum ratio between agricultural and built-up land 
    agb_ratio = 0.25
    #geo-location of villages 
    geo_city <- data.frame(x=c(20,60,10),y=c(50,20,85))
  }
  
  #get numbers of cites
  n_city <- nrow(geo_city)
  #allocate array for total profit of the assigned cities
  tpt_city <- array(0, dim=c(n_city))
  # pixel of the assigned city
  pix_city <- array(1, dim=c(n_city)) 
  # pixel of the assigned agricultural land
  pix_agr <- array(4, dim=c(n_city))
  
  # create topology using x-index * y-index 
  topo <- array(0, dim=c(nx,ny))
  id = 0
  for (j in 1:ny) {
    for (i in 1:nx) {
      id <- id + 1
     #slope type
       topo[i,j] <- i*j
     #mountain type
     # topo[i,j] <- 2000. -  0.4*(sqrt( (i-(nx/3))**2. + (j-(ny/3))**2.) )**2.
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
        if ( (rent_n[i,j,k] >= c_value) & (lu_arr[i,j] == 3) ) pix_agr[k] <- pix_agr[k] + 1
      }#end of k
      
      # assign the maximum rent to final rent
      rent_final[i,j] <- tmp
      # if the final profit/rental is larger than c_value for all  forest locations,
      # the pixel thus covered/assigned to the agriculture(type:2)
      if ( (rent_final[i,j] >= c_value) & (lu_arr[i,j] == 3) ) {
        new_lu_arr[i,j] <- 2
      } #end of if loop
  
      # calculate the total profit/land rant of each city
      for (k in 1:n_city) {      
      # select the conditional for the positive profit for the city
      # update the tpt_city by summation the final land rent, 
      # if the pixel was identified as agricultural land-use
      if ( (rent_n[i,j,k] >= c_value) & (new_lu_arr[i,j] == 2) ) {
        tpt_city[k] = tpt_city[k] + rent_n[i,j,k]
        #print(paste("i:",i,"j:",j,"city:",k,"_rent:",rent_n[i,j,k],sep="" ))
      }#end if 
      }#end k   
      
    }#end of j
  }#end of i
  
  
# allocate the city area based on the total profit and demographic factor 
# the result of pix_city comes without iterations between the remaining agricultural land  
  pix_city <- pix_city + (tpt_city * f_demo)
  # we set the max of city is 25% of agricultural-land
  for ( i in 1:n_city ) {
      if (pix_city[i] > agb_ratio*pix_agr[i] ) pix_city[i] = agb_ratio*pix_agr[i] 
  }
  
#
  for (k in 1:n_city) {
  id=0
  geo_rent_table <- data.frame()
  tmp <- data.frame(row=1,col=1,val=NA)
  
    for (j in 1:ny) {
    for (i in 1:nx) {
        id = id +1 
        tmp$row <- j 
        tmp$col <- i 
        tmp$val <- rent_n[i,j,k]
        geo_rent_table <- rbind(geo_rent_table,tmp)    
    }#end of i
    }#end of j
  
  #sort the data.frame
   geo_rent_table <- geo_rent_table[order(geo_rent_table$val,decreasing=T),]  
  # land cover change from agriculture to built-up (city)
    for ( it in 1: round(pix_city[k])) {
      #get index and geo-location from sort_table 
      ix <- geo_rent_table$col[it]
      iy <- geo_rent_table$row[it]
      #print(paste("ix:",ix,"iy:",iy,"icity:",k))
      new_lu_arr[ix,iy] <- 1 
    }
  }#end of k

##
  
  
  
  
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
fun_lulcc_toy(geo_city = data.frame(x=c(40,20,80),y=c(40,60,20)),
              f_top = 0.010,c_value =50,nx=100,ny=100)
  
  
#A <- rent_n[,,1]
  
 