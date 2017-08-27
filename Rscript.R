
library("rvest")
library("stringr")
library("ggplot2")
library("ggmap")
library("tidyr")
library("viridis")
library("broom")
library("maptools")
library("caret")
library("dplyr")
library("plyr")
library("scales")



#######################################################################################################################
#######################################################################################################################
###
###           SCRAPING BOLIGA.DK DATA
###
###           TOC:
###             1) Program til at generere færdige datasæt
###             2) Scraper til boliga.dk
###                 2.1) Definitioner
###                 2.2) f: list.updater (M)
###                 2.3) Scraper
###                       2.3.1) f: scraper.singlepage
###                       2.3.2) f: pagelooper
###                       2.3.3) f: store.payload
###                 2.4) f: pagelooper 
###             3) data cleanup funktioner
###                 3.1) f: cleaner 
###             4) Matching af bykvarter i kbh med addresser
###                 4.1) f: geodata.appender
###                 4.2) f: geodata.offline
###                 4.3) f: area.matcher 
###                 4.4) f: trade.volume 
###             5) Graphical exploration
###             6) Modelling house prices
###                 6.1) functions for data prep
###                       6.1.1) f: prepper - subset by min max, and remove NA's
###                       6.1.2) f: dummies - generate variables nboorhood and tradetype dummies
###                 6.2) Train control 
###                 6.3) Linear models 
###                       6.3.1) m: ordinary OLS
###                       6.3.2) m: LinReg on PCA components
###                 6.4) m: k-NN 
###                 6.5) m: Bayesian regularized neural net 
###                 6.6) m: Random Forest
###                 6.7) m: xgBoost
###                 6.8) Comparing models
###                       6.6.1) Graphical comparison
###             7) Suggestions for improvement
###
### f: "function (inputs)", m: "model"
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
###         1) FÆRDIGT PROGRAM
###             scraper.singlepage - 
###             pagelooper(S,M) - scraper resultater fra side S til side M på boliga
###             store.payload - loop through every page by running pagelooper on increments of the total number of pages
###             cleaner(data) - renser datasæt til videre behandling
###             geodata.appender(data, zone) - tilføjer længde/bredegrad til data - zone skal angives fx -"copenhagen"-
###                                            for at få tilstrækkelig præcision i geolokationen. Husk at google kun giver
###                                            2500 lookups pr døgn.
###             geodata.offline(data) - matcher lon/lat/postnr mm fra en adressefil (de kan hentes på AWS) vær obs på at koden
###                                     skal ændres for at tage andre addressefiler end den for københavn.
###             area.matcher(data)  - tilføjer områdefaktor for boliger i KBH (Nørrebro, Vesterbro osv). Denne funktion
###                                   køres ikke i koden nedenunder - men den virker fint (dog ikke uden for KBH).
###             trade.volume(data) - returnerer antal observationer i hver (måned,år) subset og i hvert (måned,år,bykvarter) subset
###                                   area.mather()[[1]] - datasæt med område-faktor, area.matcher()[[2]] <- bydels data
###             prepper (data, min, max, omit) - subsetter dataframen så købesum er mellem min og max. Hvis omit = T fjernes 
###                                              rækker med NA'er i hele datasættet. 
###             dummies(data) - Generer dummies som erstatning for var:nborhood og var:tradetype (bør ikke benyttes)
###             var.dropper(dat, keep_lonlat) - dropper variable der ikke kan bruges i regressioner. Hvis keep_lonlat == T
###                                   beholdes lon og lat. (bør ikke benyttes)
###
###             Før programmet kan køres skal funktionerne naturligvis defineres - kør alt koden nedenfor først!
###             link.part findes via boliga.dk og angiver hvilke type/område etc der søges på, 
###             som standard søges i KBH kommune.
###             Du kan/bør selv redigere -link.part-, -M-, input til funktionerne og evt navnene på outputtet af funktionerne
###
      link.part = "http://www.boliga.dk/salg/resultater?so=1&type=Ejerlejlighed&kom=101&fraPostnr=&tilPostnr=&gade=&min=&max=&byggetMin=&byggetMax=&minRooms=&maxRooms=&minSize=&maxSize=&minsaledate=1992&maxsaledate=today&kode=&sort=omregnings_dato-a&p="
###
      M = 1256                        # sæt max side-nummer (du behøves ikke køre til max med det samme) 
###
#######################################################################################################################
# KØR ALT I 2) TIL OG MED 4) AF DOKUMENTET FØR DU KØRER NEDENSTÅENDE


      
    link.list = list.updater(M)   # DONT CHANGE THIS LINE, KØR FØRST
    
    data = pagelooper(1,5)            # S,M angiver min og max sidetal der skal loopes over.
    data_out = store.payload(data, 1200, 100)

    
    data= data2

    anyDuplicated(data)

    data_clean = cleaner(data)        # Renser datasættet, (tager output fra pagelooper som argument!)
    
    data_geo = geodata.offline(data_clean)                         # brug evt geodata.offline istedet!

    data_fin = area.matcher(data_geo)[[1]]    #area.matcher finder bykvarter for hver observation
      bydel = area.matcher(data_geo)[[2]]    

#######################################################################################################################
###         2) SCRAPER 
###
### 
#######################################################################################################################

###---------------------------------------------
###        2.1) define variables
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


if(!exists(addr)){
addr = read.csv("adresser.csv", encoding = "UTF-8", stringsAsFactors = F)
}

subaddr = unique(addr[c("vejnavn","husnr","wgs84koordinat_bredde", "wgs84koordinat_længde", "postnr",
                        "postnrnavn","nøjagtighed", "højde")])  
###---------------------------------------------
###        2.2) list updater
###---------------------------------------------

# Bind link part med et (alle) tal i sekvensen 1:M
list.updater = function(M){
    num         = 1:M
    link.list   = paste(link.part,num, sep="")
  return(link.list)
}


###---------------------------------------------
###        2.3) scraper
###---------------------------------------------

### ********** 2.3.1) scraper.singlepage ************


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


### ********** 2.3.2) pagelooper ************

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


### ********** 2.3.3) store.payload ************

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


### ********** 2.3.4) scrape.housedata ************


#lav en liste med links
#for hvert link 


#######################################################################################################################
###         3) DATA CLEANUP 
###
###
#######################################################################################################################

###---------------------------------------------
###        3.1) (data) cleaner
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
###         4) APPEND OG MATCH GEODATA 
###             Indeholder to funktioner, en der henter geodata fra google, 
###             og en der assigner bykvarter til hver observation.
###
#######################################################################################################################

###---------------------------------------------
###        4.1) geodata.appender
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
###        4.2) geodata.offline
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
###        4.3) area.matcher
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
###        4.4) trade.volume
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

#######################################################################################################################
###         5) DATA EXPLORATION
###
###
#######################################################################################################################

#facet wrap of densities in each quarter
density <-  ggplot(data = data_fin[data_fin$n_rooms %in% c(2,4,6,8) & buysum <,]) +
    geom_density(aes(x = buysum, group = nborhood, fill = nborhood),  size = 0.5, alpha = 0.3) +
    scale_fill_viridis(discrete = T) +
    facet_wrap(~ n_rooms)

#density

# map plot of log(buysum)
map_cph <- get_map(location = "copenhagen", zoom = 12, maptype = 'satellite')

map1 <-  ggmap(map_cph, base_layer=ggplot(aes(x=lon,y=lat), data=data_geo), extent = "normal", maprange=FALSE) +
    geom_polygon(data = bydel, aes(x = long, y = lat, group = group),
                 color = "grey50", alpha = 0.1, show.legend = FALSE) +
    geom_point(data = data_fin, aes( x = lon, y = lat, color = build_year), size = 0.5, alpha = 0.6) +
    coord_map(projection="mercator", 
              xlim=c(attr(map_cph, "bb")$ll.lon, attr(map_cph, "bb")$ur.lon),
              ylim=c(attr(map_cph, "bb")$ll.lat, attr(map_cph, "bb")$ur.lat)) +
    guides(color = guide_colorbar(barwidth = 20,
                                  barheight = 0.3,
                                  title = "Build year",
                                  title.position = "top")
           ) +
    scale_color_viridis(discrete = F, option = "magma")+
    theme(legend.position = "bottom",
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) 
  
map1



data_fin2$nborhood <- as.factor(data_fin2$nborhood)
levels(data_fin2$nborhood) <- c("Amager Ø", "Amager V", "Brønshøj", "Indre by", "Nørrebro","Nordvest","Østerbro","Vesterbro","Valby","Vanløse")

# buysum by date
times <- ggplot(data = data_fin2[data_fin$buysum < 5000000,],aes(x = date, y = buysum, color = log(n_rooms))) +
  geom_line(alpha = 0.15) +
  geom_smooth(color = "purple", alpha = 0.8) +
  scale_y_continuous(labels = comma) +
  guides(color = guide_colorbar(barwidth = 20,
                                barheight = 0.3,
                                title = "Log of number of rooms",
                                title.position = "top")) +  
  scale_color_viridis(option = "plasma") +
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)
        ) +
#  geom_abline(slope = 0.02, intercept = 0) +
  scale_x_date(date_breaks = "8 years", date_labels = "%Y") +
  facet_wrap(~ nborhood) 


times


times2 <- ggplot(data = data_fin[data_fin$buysum < 5000000,],aes(x = date, y = buysum, color = nborhood)) +
  geom_line(alpha = 0.1) +
  scale_color_discrete(labels = c("Amager Ø", "Amager V", "Brønshøj", "Indre by", "Nørrebro","Nordvest","Østerbro","Vesterbro","Valby","Vanløse")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill=NA)) +
#  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  geom_smooth() 
  
times2

ggsave("map1.png", map1)
ggsave("facet_over_time.png", times)
ggsave("over_time.png", times2)
ggsave("density.png", density)

#######################################################################################################################
###         6) MODELLING 
###             as default all models are run with repeated CV
###
#######################################################################################################################

###---------------------------------------------
###        6.1) data preparer - subsets data with/without NA's and in range [min,max]
###---------------------------------------------

#***********  6.1.1) prepper- subset data *************************

prepper = function(data, min, max, omit){
 # omit NA's if omit == T
     if(omit == T) {
          data = na.omit(subset(data, buysum < max & buysum > min))
    } else if(omit == F) {
          data = subset(data, buysum < max & buysum > min)
    } else {
        stop("omit must be TRUE or FALSE")
    }
  return(data)
}

#***********  6.1.2) dummies - create dummies from factors *************************

dummies = function(data){
          d = model.matrix(~nborhood-1, data)
              colnames(d) = gsub("nborhood", "", colnames(d))
              data = cbind(data, d)
          
          e = model.matrix(~tradetype-1, data)
              colnames(e) = gsub("tradetype", "", colnames(e))
              data = cbind(data, e)
#          drops = c("nborhood", "tradetype")
#          data = data[,!names(data) %in% drops]
  return(data)
}


###---------------------------------------------
###        6.2) train control
###---------------------------------------------

data_modeling = prepper(data_fin, 0,5000000, T)

#partition data into train (p percent of original) and test (1-p percent)
trainIndex = createDataPartition(data_modeling$nborhood, p = 0.05, list = F, times = 1)
  data_train = data_modeling[trainIndex,]
  data_test = data_modeling[-trainIndex,]


fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)




###---------------------------------------------
###        6.3) Linear regression
###---------------------------------------------

#***********  6.3.1) ordinary linear regression *************************
  m_linear = train( buysum ~ m2 + floor + date +n_rooms + build_year +m2/n_rooms + nborhood + height,
                    data = data_train,
                    method = "lm",
                    trControl = fitControl)

    data_test$pred_linear = predict(m_linear, newdata = data_test)
    data_test$resid_linear = data_test$buysum - data_test$pred_linear
 
    
#***********  6.3.2) LinReg on PCA components *************************
  m_linearPCA = train( buysum ~ m2 + floor + n_rooms + build_year + nborhood + height ,
                    data = data_train,
                    method = "lm",
                    preProcess = "pca",
                    trControl = fitControl)
  


###---------------------------------------------
###        6.4) k-NN 
###---------------------------------------------

  m_knn = train(buysum ~ lon + lat + as.numeric(date) + n_rooms + m2+ floor + height,
              data = data_train,
              method = "knn",
              trControl = fitControl,
              preProcess =c("center","scale"),
              tuneLength = 10  )

  data_test$pred_knn = predict(m_knn, newdata = data_test)
  data_test$resid_knn = data_test$buysum - data_test$pred_knn

###---------------------------------------------
###        6.5) Bayesian regularized neural net 
###---------------------------------------------
  
  m_nnet = train(buysum ~  m2 + floor + n_rooms + build_year + x +  lon + lat + as.numeric(date) + height,
               data = data_train,
               method = "brnn",
               trControl = fitControl,
               preProcess = c("scale"),
               linout = T)
  
  data_test$pred_nnet = predict(m_nnet, newdata = data_test)
  data_test$resid_nnet = data_test$buysum - data_test$pred_nnet
  
###---------------------------------------------
###        6.6) Random Forest 
###---------------------------------------------
  
  m_rf = train(buysum ~  m2 + floor + n_rooms + build_year + x + lon + lat + as.numeric(date) + height,
                 data = data_train,
                 method = "rf",
                 trControl = fitControl,
                 preProcess = c("scale"),
                 linout = T)
  
  data_test$pred_rf = predict(m_rf, newdata = data_test)
  data_test$resid_rf = data_test$buysum - data_test$pred_rf
  
###---------------------------------------------
###        6.7) xgBoost
###---------------------------------------------

  
  m_xgbTree = train(buysum ~  m2 + floor + n_rooms + build_year + x + lon + lat + as.numeric(date) + height,
               data = data_train,
               method = "xgbTree",
               trControl = fitControl,
               preProcess = c("scale"),
               search = "grid",
               linout = T)
  
  data_test$pred_xgbTree = predict(m_xgbTree, newdata = data_test)
  data_test$resid_xgbTree = data_test$buysum - data_test$pred_xgbTree


  m_xgbLin = train(buysum ~  m2 + floor + n_rooms + build_year + x + lon + lat + as.numeric(date) + height,
               data = data_train,
               method = "xgbLinear",
               trControl = fitControl,
               preProcess = c("scale"),
               search = "grid",
               linout = T)

  data_test$pred_xgbLin = predict(m_xgbLin, newdata = data_test)
  data_test$resid_xgbLin = data_test$buysum - data_test$pred_xgbLin

  
###---------------------------------------------
###        6.8) Comparing models 
###---------------------------------------------
  
#***********  6.7.1) graphical comparison *************************
#actual v. fitted
ggplot(data = data_test) +
    geom_point(aes(x = buysum, y = pred_linear, color = "red"), alpha = 0.4) +
    geom_point(aes(x = buysum, y = pred_knn, color = "blue"), alpha = 0.4) +
    geom_point(aes(x = buysum, y = pred_rf, color = "green"),alpha = 0.4) +
    geom_point(aes(x = buysum, y = pred_nnet, color = "yellow"), alpha = 0.4) +
    geom_point(aes(x = buysum, y = pred_xgbTree, color = "purple"), alpha = 0.4) +
    coord_fixed(ratio=1, xlim = c(0,5000000), ylim = c(0,5000000)) +
    geom_abline(slope = 1, intercept = 0) +
    scale_colour_manual(name = "", 
                        values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow", "purple"= "purple"),
                        labels = c("red" = "linear","blue" = "k-NN","green" = "Random Forest","yellow" = "Bayesian nnet","purple" = "xgb"), guide = "legend") +
    theme(legend.position="bottom") +
    ylab("prediction") +
    facet_wrap(~ nborhood)
  

# actual v. residuals     
ggplot(data = data_test) +
    geom_point(aes(x = buysum, y = resid_linear, color = "red"), alpha = 0.4) +
    geom_point(aes(x = buysum, y = resid_knn, color = "blue"), alpha = 0.4) +
    geom_point(aes(x = buysum, y = resid_rf, color = "green"), alpha = 0.4) +
    geom_point(aes(x = buysum, y = resid_nnet, color = "yellow"), alpha = 0.4) +
    geom_point(aes(x = buysum, y = resid_xgbTree, color = "purple"), alpha = 0.4) +
#    coord_fixed(ratio=1, xlim = c(0,5000000), ylim = c(0,5000000)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_colour_manual(name = "", 
                      values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow", "purple"= "purple"),
                      labels = c("red" = "linear","blue" = "k-NN","green" = "Random Forest","yellow" = "Bayesian nnet","purple" = "xgb"), guide = "legend") +
#    scale_colour_manual(name = "", 
#                      values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow"),
#                      labels = c("linear", "k-NN", "Random Forest", "Bayesian nnet"), guide = "legend") +
    theme(legend.position="bottom") +
    ylab("residual") +
    facet_wrap(~ nborhood) 
 
# top n% of the residuals, plotted on a map, colored by model
  n = 1
data_highresid = gather(data_test, key = model, value = resid, c(resid_linear,resid_knn,resid_nnet,resid_rf)) %>%
            subset(resid > quantile(resid,prob=1-n/100))
    
      ggmap(map_cph, base_layer=ggplot(aes(x=lon,y=lat), data=data_geo), extent = "normal", maprange=FALSE) +
    #    geom_polygon(data = bydel, aes(x = long, y = lat, group = group),
    #                 color = "grey50", alpha = 0.1, show.legend = FALSE) +
        geom_point(data = data_highresid, aes( x = lon, y = lat, color = model, size =-0.01*as.numeric(as.factor(model))), alpha = 0.8) +
        coord_map(projection="mercator", 
                  xlim=c(attr(map_cph, "bb")$ll.lon, attr(map_cph, "bb")$ur.lon),
                  ylim=c(attr(map_cph, "bb")$ll.lat, attr(map_cph, "bb")$ur.lat)) +
        #    scale_color_gradient(high = "orange", low = "blue") +
        #  scale_color_gradient2(high = "blue", mid = "orange", low = "blue") +
        scale_color_viridis(discrete = T)+
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank()) +
              guides(size=FALSE) +
              scale_size_continuous(range = c(1, 3))

  
#***********  6.7.2) Analytical comparison *************************

#RMSE and R2 for each model
resamp = resamples(list(knn = m_knn, rf = m_rf, lin = m_linear, linPCA = m_linearPCA, nnet = m_nnet, xgbTree = m_xgbTree, xgbLin = m_xgbLin ))
# RMSE
  dotplot(resamp,metric = "RMSE")
# R2
  dotplot(resamp,metric = "Rsquared")

#######################################################################################################################
###         7) POSSIBLE IMPROVEMENTS 
###
###
#######################################################################################################################

###---------------------------------------------
###       7.1) Prediction improvements 
###---------------------------------------------
#
# Bad predictions - caused by high variance in indre by, oesterbro, noerebro?
#   => model variance and use as input in model?
#   => model "high deviation obervations" with classifier?
#
# tuneGrid! 
#  