#######################################################################################################################
###         1) SCRAPER 
###
### 
#######################################################################################################################

###---------------------------------------------
###        1.1) define variables
###---------------------------------------------

# Definer de resterende css-selectors
css.addr      = "td:nth-child(1) h5"        #1
css.buysum    = "td:nth-child(2) h5"        #2
css.date      = "td:nth-child(3) h5"        #3
css.sqm_price = "td:nth-child(4) h5"        #4
css.rooms     = "td:nth-child(5) h5"        #5
css.type      = "td.qtipped"                #6
css.m2        = ".qtipped+ td h5"           #7
css.build     = "td:nth-child(8) h5"        #8
css.deduction = "td:nth-child(9)"           #9

# sammensæt selectors i en vector og definer en tilsvarende vektor med variabelnavne (OBS ordering matters !)
css.list  = c(css.addr, css.buysum, css.date, css.sqm_price, css.rooms, css.type, css.m2, css.build, css.deduction)          # Navnelisten og denne skal matche 1:1
N         = c(NA, "address", "buysum", "date", "sqm_price", "n_rooms", "type", "m2","build_year", "deduction")              # behold NA som første element!



subaddr = unique(addr[c("vejnavn","husnr","wgs84koordinat_bredde", "wgs84koordinat_længde", "postnr",
                        "postnrnavn","nøjagtighed", "højde")])  
###---------------------------------------------
###        1.2) list updater
###---------------------------------------------

# Bind link part med et (alle) tal i sekvensen 1:M
list.updater = function(M){
    num         = 1:M
    link.list   = paste(link.part,num, sep="")
  return(link.list)
}


###---------------------------------------------
###        1.3) scraper
###---------------------------------------------

### ********** 1.3.1) scraper.singlepage ************


scraper.singlepage = function(link){
#open html and read link  attribute from column 1
  data <- link %>% 
    read_html() 

# apparently getting links is a mess
#   data_attr <- data %>%
#    html_nodes("#searchresult h5 a") %>%
#    html_attr('href')
#bind as column in frame    
#    frame = cbind(frame,as.list(data_attr))
#read all nodes in sequence without closing & reopening the html
  for(i in css.list){
    data_text = data  %>% 
      html_nodes(i)   %>% 
      html_text()
    frame = cbind(frame, as.list(data_text)) 
  }
  
  colnames(frame) = N
  return(out = frame[,2:ncol(frame)])
}


### ********** 1.3.2) pagelooper ************

# pagelooper kører scraper.singlepage() på hver side fra 1 til M
pagelooper = function(S,m){
  s.page = as.list(NULL)
    for(i in S:m){
      t = as.numeric(Sys.time())
          pagedump = scraper.singlepage(link.list[i])
          s.page   = rbind(s.page, pagedump)
       print(paste("getting page",i,"took",as.numeric(Sys.time())-t , "seconds. Sleeping for 5 seconds before getting next page"))
      Sys.sleep(5)
    }
  print(paste("DONE with pages",S,"to",m))
  return(as.data.frame(s.page))
}


### ********** 1.3.3) store.payload ************

store.payload = function(data, n, inc){
  page = floor(nrow(data)/40)
  stop = floor(nrow(data)/40) + n
  print(paste("start page:",page))
  print(paste("stop page:", stop))
  
  while(page < stop){
    t = as.numeric(Sys.time())
    
    data = tryCatch(rbind(data, pagelooper(page + 1,  page + inc)),
                    error = function(e) {
                      message(paste('Error:',e))
                      return(data)
                      })
    
    page = floor(nrow(data)/40)
    if(stop - page < inc) {inc = stop - page} else{inc = inc}
    
    for(i in 1:(NROW(N)-1)){
      data[,i] = unlist(data[,i])
    }

    write.csv2(data, 'temp_write.csv')
    Sys.sleep(60)
    
    print(paste("Finishing page", page, "of", stop, "total"))
    print(paste("estimated",(stop-page)*((as.numeric(Sys.time())-t)/inc) , "seconds remaining"))
  }
  print("FINALLY FINISHED")
  return(data)
}


### ********** 1.3.4) scrape.housedata ************


#lav en liste med links
#for hvert link 


#######################################################################################################################
###         2) DATA CLEANUP 
###
###
#######################################################################################################################

###---------------------------------------------
###        2.1) (data) cleaner
###---------------------------------------------
###         note: det er nok ikke nødvendigt med -df=data- når det køres som funktion (tjek?)
###               denne funktion trænger til at blive renset igennem


cleaner = function(data){
    # unlist every column

    for(i in 1:(NROW(N)-1)){
      data[,i] = unlist(data[,i])
    }
    
    # replace - with NA's
    data[data == "-"] <- NA
    
    # fjern 1000-sepeartorer
    data$buysum = gsub("\\.","", data$buysum) %>% as.numeric
    data$sqm_price = gsub("\\.","", data$sqm_price) %>% as.numeric
    
    # konverter til numeric
    data[c("n_rooms","m2","build_year")] <- as.numeric(as.matrix(data[c("n_rooms","m2","build_year")]))
    
    # Opdel date i faktisk dato og handelstype
    data$tradetype = substr(data$date, 11,20)
    data$date = substr(data$date, 1,10)
    
    
    #convert date to actual date
    data$date = as.Date(data$date, "%d-%m-%Y")
  
    # clean all character columns of whitespace in begining and end of string
    data[,sapply(data, class) == 'character'] <- lapply(data[,sapply(data, class) == 'character'], trimws)
    
    #separer addressen (ufærdigt!)
    data = data %>% separate(address, c("roadname", "area"),",")
    data$floor = with(data, ifelse(grepl(". ", data$area, fixed = TRUE) == TRUE, substr(data$area,2,2), NA))
    data$floor[data$floor == "S"] = 0
    data$floor = as.numeric(data$floor)
    
    #gen vejnavn & husnr vars
    data$vejnavn = gsub(" [^ ]*$", "", data$roadname)
    data$husnr = gsub(".* ", "", data$roadname)
    
    return(clean_data = data)
}


#######################################################################################################################
###         3) APPEND OG MATCH GEODATA 
###             Indeholder to funktioner, en der henter geodata fra google, 
###             og en der assigner bykvarter til hver observation.
###
#######################################################################################################################

###---------------------------------------------
###        3.1) geodata.appender
###---------------------------------------------
geodata.appender= function(data, zone){
    # bind geolocations til dataframe
    data = cbind(data, t(sapply(paste(data$roadname,", ",zone, sep = ""), geocode, USE.NAMES = F)))
    
    #unlist og sæt numeric for lat og lon    
    data$lon = unlist(data$lon) %>% as.numeric
    data$lat = unlist(data$lat) %>% as.numeric
  
  return(data)  
}

###---------------------------------------------
###        3.2) geodata.offline
###---------------------------------------------


# left join the scraped data info from danish address registries  
geodata.offline = function(data){
    out = left_join(data, subaddr, by = c("vejnavn", "husnr"))
    #rename columns in output
    names(out)[names(out) == "wgs84koordinat_bredde"] = "lat" 
    names(out)[names(out) == "wgs84koordinat_længde"] = "lon" 
    names(out)[names(out) == "højde"] = "height"
    
    return(out)
}



###---------------------------------------------
###        3.3) area.matcher
###---------------------------------------------
###           note: sikkert mega suboptimal kode

area.matcher = function(data){
      #Fjern NA'er så der kan matches med shapefilen over bydele
      df_geo = subset(data, is.na(data$lon) == F & is.na(data$lat) == F)
      geo = subset(data, is.na(data$lon) == F & is.na(data$lat) == F)
      #read shapefile for kbh and convert it to dataframe
      shape = readShapePoly("bydel.shp")
      bydel = tidy(shape)
      #assign coordinates to data input
      coordinates(geo) = c("lon", "lat")
      proj4string(geo) = proj4string(shape)
      
      # giv unikt id baseret på hvilken bydel observationen er i
      in_hood = over(geo, as(shape, "SpatialPolygons"))
      
      # konverter til shape og giv ID værdi 11 for "other" (der er 10 kvarterer i shapefilen)
      df_hood = tidy(in_hood)[,2] -1
      df_hood$x = ifelse(is.na(df_hood$x), "11", df_hood$x)
      
      # bind til dataframe
      df_geo = cbind(df_geo, df_hood)
      
      df_geo$nborhood = ifelse(df_geo$x == 0, "broenshoej",
                           ifelse(df_geo$x == 1, "oesterbro",
                           ifelse(df_geo$x == 2, "indre_by",
                           ifelse(df_geo$x == 3, "noerebro",
                           ifelse(df_geo$x == 4, "nordvest",
                           ifelse(df_geo$x == 5, "valby",
                           ifelse(df_geo$x == 6, "vesterbro",
                           ifelse(df_geo$x == 7, "amager_vest",
                           ifelse(df_geo$x == 8, "amager_øst",
                           ifelse(df_geo$x == 9, "vanloese",
                           ifelse(df_geo$x == 11, "other", NA
                        )))))))))))      
      #rename x
      names(data)[names(data) == "x"] = "nbor_factor"
      return(list(df_geo, bydel))
  }


###---------------------------------------------
###        3.4) trade.volume
###---------------------------------------------

trade.volume = function(data){
  
    data$month = format(data$date, "%m")
    data$year = format(data$date, "%Y")
    
    #count total volume and rename
    count = data %>% group_by(month,year) %>% tally()
    data = left_join(data, count, by = c("month","year"))
    names(data)[names(data) == "n"] = "volume_total"
    
    #count volume by nborhood and rename
    count = data %>% group_by(month,year, nborhood) %>% tally()
    data = left_join(data, count, by = c("month","year","nborhood"))
    names(data)[names(data) == "n"] = "volume_nbor"
  
    return(data)
}
