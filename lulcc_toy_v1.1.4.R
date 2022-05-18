###################################################################################################
# This function made use of the land-use change theory developed by Johann Heinrich von Thunen (1826)  
# to calculate the spatial distribution of land use change between forest and agricultural land
#
# >>model parameters<< 
# nx, ny: dimension x, y of the study domain
#
# crop_para: parameter table for different crop types 
# crop_para(y=...,l=...,k=...,v=...,)
# y: yield, p:price
# l: labors, w: wages 
# k: capital, q: cost of capital
# v: cost of transport 
#
# geo_city: geo-location table for the assigned cites 
# geo_city(lon_x=...,lat_y=...)
# lon_x: index of x /longitude 
# lat_y: index of y /latitude  
# f_top: topographic factor 
# tpt_city: total profit of the assigned cities 
# Version: 1.1.4
# First Date: 2022-02-25 
# revised 2022-03-10 add the consideration of the city expansion by demographic supported by agricultural production       
# revised 2022-03-16 add the second crop type 
# revised 2022-03-25 add the parameter of demographic index 
# Authors: Yi-Ying Chen and Meng-Hsuan Vivian Lin   
# Contact email: yiyingchen@gate.sinica.edu.tw 
#             or as0210250@gate.sinica.edu.tw
####################################################################################################
fun_lulcc_toy <- function(ld_plot=TRUE, c_value=200, f_demo=0.00005, agb_ratio = 0.25,
                          topo_file="dtm_taipei_100_100.tiff",f_top=0.05, nx=100,ny=100,
                          geo_city = data.frame(lon_x=c(78, 80, 50, 20, 10),
                                                lat_y=c(48, 35, 75, 65, 40)),
                          crop_para=data.frame(y=c(90,120), p=c(10,10), 
                                               l=c(50,50),  w=c(5,5),  k=c(5,5), q=c(25,25),
                                               v=c(10,22)),
                          dyn_demo = c(rep(1.0, nrow(geo_city))), year_txt=c("2020")
                          )
# start the function
{
  
# load R-packages "rgdal", "raster", "fields" 
  library("rgdal")
  library("raster")
  library("fields")
  
  n_city <- nrow(geo_city)

  #allocate array for total profit of the assigned cities
  tpt_city <- array(0, dim=c(n_city))
  # pixel of the assigned city
  pix_city <- array(1, dim=c(n_city)) 
  # pixel of the assigned agricultural land
  pix_agr <- array(4, dim=c(n_city))
  
  # create topology using x-index * y-index 
  topo <- array(0, dim=c(nx,ny))

   
  # import topographic data from scratch ld_topo <- TRUE/FALSE 
  ld_topo <- TRUE
  
  if (ld_topo) {
    
    #import the real topographic data from Geo-tiff image (100 by 100)
    #topo_map <- raster("dtm_taipei_100_100.tif")
    topo_map <- raster(x=topo_file)
    topo <- t(as.matrix(topo_map))[,ny:1]
    topo[is.na(topo)] <- 0
    #create land mask 
    land_mask <- topo
    land_mask[topo ==0] <- 0
    land_mask[topo >0]  <- 1
    
  }else{
    # create a genital slope from N-E to S-W direction
    # by using multiplying the x and y index in the array/matrix
    for (j in 1:ny) {
      for (i in 1:nx) {
        #slope type
        topo[i,j] <- i*j
        #mountain type
        # topo[i,j] <- 2000. -  0.4*(sqrt( (i-(nx/3))**2. + (j-(ny/3))**2.) )**2.
      }#end of i-loop
    }#end of j-loop 
    #create land mask 
    land_mask <- topo
    land_mask[topo ==0] <- 0
    land_mask[topo >0]  <- 1
  }

  # Assign land-use type to a 2D array lu_arr (3:forest; 2:agriculture; 1:built-up)
  lu_arr <- array(3, dim=c(nx,ny)) #assign default to 3 (forest)
  # apply land_mask to the array/matrix/study_area
  lu_arr <- lu_arr*land_mask
  
  # from city-1 to city-n
  dis_n <- array(NA, dim=c(nx,ny,n_city))
  #rental calculation, r: profit/rental of the land refer to n-cites
  rent_n <- array(NA,dim=c(nx,ny,n_city,n_crop))
  

  print("processing the von Thunen economic principle/equation")
  # crop-loop, city-loop; y-loop; x-loop
  for (l in 1:n_crop) {
  for (k in 1:n_city) {
    # assign the land-type to city(type:1)
    lu_arr[c(geo_city$lon_x[k]), c(geo_city$lat_y[k])] <- 1
    # find the surrounding pixels and assign land-type: agriculture(type:2)
    for (j in 1:ny) {
    for (i in 1:nx) {
          # find the surrounding pixels and assign it to agricultural land
          # calculate distance from city
          del_xy <- sqrt(abs(as.numeric(i)- geo_city$lon_x[k])**2. +
                         abs(as.numeric(j)- geo_city$lat_y[k])**2. ) 
          
          dis_n[i,j,k] <- del_xy
          # initiated the surrounding area as the land-use for agricultural land 
            if ((del_xy >= 1.0) & (del_xy <= sqrt(2)) )  lu_arr[i,j] <- 2
          #
          # calculate the profit based on von Thunen economic principle/equation
          # rent_n[i,j,k] <- (p*y) - (l*w) - (k*q) - v* ( topo[i,j]*f_top + dis_n[i,j,k] )
       
           rent_n[i,j,k,l] <- (crop_para$p[l]*crop_para$y[l]) -
                              (crop_para$l[l]*crop_para$w[l]) -
                              (crop_para$k[l]*crop_para$q[l]) -
                              (crop_para$v[l]*(topo[i,j]*f_top+dis_n[i,j,k]))
           
           #apply the dynamics of demographic to rent_n
           rent_n[i,j,k,l] <- dyn_demo[k] * rent_n[i,j,k,l]
    }#i-loop
    }#j-loop
  }#k-loop
  }#l-loop  
  
  # create an array for assessing the final land rent using maximum rental value
  # by comparing the rental from different cites
  rent_final <- array(NA, dim=c(nx,ny))
  # converting the lu_type based on the final rental values/array
  # copy the land-use information from the original land-use array "lu_arr"
  new_lu_arr <- lu_arr
  for (j in 1:ny) {
  for (i in 1:nx) {
      # initial tmp as a extreme negative value
      tmp <- -999999999.99
      for (l in 1:n_crop) {
      for (k in 1:n_city) {
        # rent comparison
        if ( rent_n[i,j,k,l] >= tmp ) tmp <- rent_n[i,j,k,l]
        if ( (rent_n[i,j,k,l] >= c_value) & (lu_arr[i,j] == 3) ) pix_agr[k] <- pix_agr[k] + 1
      }#end of k-loop
      }#end of l-loop 
      
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
      if ( (rent_final[i,j] >= c_value) & (new_lu_arr[i,j] == 2) ) {
           # the profit is city depended or say >0, so negative value may due 
           # to the consideration from other cities 
           if ( max(rent_n[i,j,k,]) > 0 ) {
              # assign the final rent to the selected city in the k-loop 
              tpt_city[k] = tpt_city[k] + rent_final[i,j] 
           }# end if  rent_n
      }#end if rent_final & new lulcc
      }#end k   
      # assign the max profit to the array 
  }#end of j
  }#end of i
  

# go for the demographic change 
# Note: This part of code can be refined, 
#       especially for sorting the index of the matrix
#       When the size was increased the memory is huge   

ld_go <- TRUE

if( ld_go) {
  print("processing the demographic change")
# allocate the city area based on the total profit and demographic factor 
# the result of pix_city comes without iterations between the remaining agricultural land  
  pix_city <- pix_city + (tpt_city * f_demo)
  # we set the max of city is 25% of agricultural-land
  for ( i in 1:n_city ) {
      if (pix_city[i] > agb_ratio*pix_agr[i] ) pix_city[i] = agb_ratio*pix_agr[i] 
  }
#
  for (k in 1:n_city) {
    print(paste("Total Cities:",n_city,", woriking on city No.:",k,sep="") )
  id=0
  geo_rent_table <- data.frame()
  tmp <- data.frame(row=1,col=1,val=NA)
  
    for (j in 1:ny) {
    for (i in 1:nx) {
        id = id +1 
        tmp$row <- j 
        tmp$col <- i 
        
        # conditional profit  use largest value
        tmp$val <- max(rent_n[i,j,k,]) 
          
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

}# end ld_go
#
  
# switch crop type from rice to wheat 
print("processing LUC by switching crop type from rice to wheat")
    # compare the profit between crop1 and crop2 
    for (j in 1:ny) {
    for (i in 1:nx) {
        # within the study domain, if the pixel has new LUC from forest to agriculture
        # then go to check condition for switching the crop type from 2.0 (i.e., rice) to 1.5 (i.e.,wheat) 
        if (new_lu_arr[i,j] == 2 ) {
          for ( k in 1:n_city) { 
              # check the final rent is determined by profit from crop 1.5 (wheat)
              if ( rent_final[i,j]  ==  rent_n[i,j,k,2] )  
                new_lu_arr[i,j] <- 1.5  
          }# end of k-loop 
        }#end of if       
    }#end of i-loop 
    }#end of j-loop

# plot the results    
  ld_plot <- TRUE
  if (ld_plot) {
    library("fields")
    #plot color images 
    #par(mfrow=c(1,2)) 
    #image.plot(dis_n[,,1],col=terrain.colors(10), main="distance of city 1 ")
    #contour(topo, add=TRUE)
    #image.plot(rent_final,col=terrain.colors(10), main="land rent")
    #contour(topo, add=TRUE)
    
    # lulcc plot before and after
    #dev.new()
    #par(mfrow=c(1,1))
    #image.plot(lu_arr, col=rev(terrain.colors(10)),zlim=c(1,3), main="original land use type")
    #contour(topo, add=TRUE)
    image.plot(new_lu_arr,col=rev(terrain.colors(10)),zlim=c(1,3),
               main=paste("LULCC Year:",year_txt,sep="") )
    contour(topo, add=TRUE)
  }#end of ld_plot 
  
  
  print("The Total Pixel of the City(ies) :")
  print(tpt_city)
  
  return(tpt_city)
  
  
  } #end of fun_lulcc_toy
####################### 

  
