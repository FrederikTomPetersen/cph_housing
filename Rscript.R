
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


setwd("C:/Users/Kristian/Documents/Github/cph_housing")

if(!exists(addr)){
addr = read.csv("adresser.csv", encoding = "UTF-8", stringsAsFactors = F)
}

source("functions.R")



#######################################################################################################################
#######################################################################################################################
###
###           SCRAPING BOLIGA.DK DATA
###
###           TOC:
###             1) Actually getting the data
###             2) Graphical exploration
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
###             scraper.singlepage - scrapes a single page
###             pagelooper(S,M) - scrapes resultats from page S to m
###             store.payload - loop through every page by running pagelooper on increments of the total number of pages
###             cleaner(data) - Cleans the dataset for further work
###             geodata.appender(data, zone) - adds lat/lon to the dataset. zone should be added for sufficient precision in geolocating.
###             geodata.offline(data) - mathces lon/lat/zip from a database of all copenhagen addresses (get them from AWS). Note 
###             area.matcher(data)  - decides in which area of copenhagen a given address is using a shapefile of the city
###             trade.volume(data) - 
###             prepper (data, min, max, omit) -  
###             dummies(data) -
###             var.dropper(dat, keep_lonlat) - 
###
###             link.part needs to be changes to get a different area of the country - for now it's set to copenhagen
###
      link.part = "http://www.boliga.dk/salg/resultater?so=1&type=Ejerlejlighed&kom=101&fraPostnr=&tilPostnr=&gade=&min=&max=&byggetMin=&byggetMax=&minRooms=&maxRooms=&minSize=&maxSize=&minsaledate=1992&maxsaledate=today&kode=&sort=omregnings_dato-a&p="
###
      M = 1256                        # set max page-number for link generator
###
#######################################################################################################################
# Run this to actually get some data
      
    link.list = list.updater(M)   # DONT CHANGE THIS LINE, KØR FØRST
    
    data = pagelooper(1,5)            # S,M angiver min og max sidetal der skal loopes over.
    data_out = store.payload(data, 1200, 100)

    rm(data)

    anyDuplicated(data)

    data_clean = cleaner(data)        # Renser datasættet, (tager output fra pagelooper som argument!)
    
    data_geo = geodata.offline(data_clean)                         # brug evt geodata.offline istedet!

    data_fin = area.matcher(data_geo)[[1]]    #area.matcher finder bykvarter for hver observation
      bydel = area.matcher(data_geo)[[2]]    

    rm(data_clean)
    rm(addr)
      
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


    
density


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


rm(map1, times, times2, density)

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
  m_linear = train( buysum ~ m2 + floor + date +n_rooms + build_year + m2/n_rooms + nborhood + height,
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

  m_knn = train(buysum ~ lon + lat + as.numeric(date) + n_rooms + m2 + floor + height,
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
  
data_test$resid_avg = 1/5 *(data_test$resid_knn + data_test$resid_linear + data_test$resid_nnet + data_test$resid_rf + data_test$resid_xgbTree) 
data_test$pred_avg = 1/5 *(data_test$pred_knn + data_test$pred_linear + data_test$pred_nnet + data_test$pred_rf + data_test$pred_xgbTree) 
  

data_test$nborhood <- as.factor(data_test$nborhood)
levels(data_test$nborhood) <- c("Amager Ø", "Amager V", "Brønshøj", "Indre by", "Nørrebro","Nordvest","Østerbro","Vesterbro","Valby","Vanløse")

  
  #***********  6.7.1) graphical comparison *************************
#actual v. fitted
ggplot(data = data_test) +
    geom_point(aes(x = buysum/1000000, y = pred_linear/1000000, color = "red"), alpha = 0.1, size = 0.3) +
    geom_point(aes(x = buysum/1000000, y = pred_knn/1000000, color = "blue"), alpha = 0.1, size = 0.3) +
    geom_point(aes(x = buysum/1000000, y = pred_rf/1000000, color = "green"),alpha = 0.1, size = 0.3) +
    geom_point(aes(x = buysum/1000000, y = pred_nnet/1000000, color = "yellow"), alpha = 0.1, size = 0.3) +
    geom_point(aes(x = buysum/1000000, y = pred_xgbTree/1000000, color = "purple"), alpha = 0.1, size = 0.3) +
    coord_fixed(ratio=1, xlim = c(0,5), ylim = c(0,5)) +
    geom_abline(slope = 1, intercept = 0) +
    scale_colour_manual(name = "", 
                        values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow", "purple"= "purple"),
                        labels = c("red" = "linear","blue" = "k-NN","green" = "Random Forest","yellow" = "Bayesian nnet","purple" = "xgb"), guide = "legend") +
    ylab("predicted (mil.)") +
    xlab("actual (mil.)") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) +
    guides(color = guide_legend(override.aes = list(size=2, alpha = 1))) +
    facet_wrap(~ nborhood)
  
ggsave("fitted.png")


# actual v. residuals     
ggplot(data = data_test) +
    geom_point(aes(x = buysum/1000000, y = resid_linear/1000000, color = "red"), size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_knn/1000000, color = "blue"),size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_rf/1000000, color = "green"),size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_nnet/1000000, color = "yellow"),size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_xgbTree/1000000, color = "purple"),size = 0.3, alpha = 0.1) +
#    coord_fixed(ratio=1, xlim = c(0,5000000), ylim = c(0,5000000)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_colour_manual(name = "", 
                      values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow", "purple"= "purple"),
                      labels = c("red" = "linear","blue" = "k-NN","green" = "Random Forest","yellow" = "Bayesian nnet","purple" = "xgb"), guide = "legend") +
#    scale_colour_manual(name = "", 
#                      values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow"),
#                      labels = c("linear", "k-NN", "Random Forest", "Bayesian nnet"), guide = "legend") +
    ylab("residual y - yhat (mil.)") +
    xlab("actual (mil.)") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) +
    guides(color = guide_legend(override.aes = list(size=2, alpha = 1))) +

    facet_wrap(~ nborhood) 

ggsave("residuals.png") 




# actual v. residuals    + AVG  
ggplot(data = data_test) +
    geom_point(aes(x = buysum/1000000, y = resid_linear/1000000, color = "grey10"), size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_knn/1000000, color = "grey20"),size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_rf/1000000, color = "grey30"),size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_nnet/1000000, color = "grey40"),size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_xgbTree/1000000, color = "grey50"),size = 0.3, alpha = 0.1) +
    geom_point(aes(x = buysum/1000000, y = resid_avg/1000000, color = "red"),size = 0.3, alpha = 0.04) +
#    coord_fixed(ratio=1, xlim = c(0,5000000), ylim = c(0,5000000)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_colour_manual(name = "", 
                      values =c("grey10"="grey10","grey20"="grey20", "grey30" = "grey30", "grey40" = "grey40", "grey50"= "grey50", "red" = "red"),
                      labels = c("grey10" = "linear","grey20" = "k-NN","grey30" = "Random Forest","grey40" = "Bayesian nnet","grey50" = "xgb", "red" = "average"),
                      guide = "legend") +
#    scale_colour_manual(name = "", 
#                      values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow"),
#                      labels = c("linear", "k-NN", "Random Forest", "Bayesian nnet"), guide = "legend") +
    ylab("residual y - yhat (mil.)") +
    xlab("actual (mil.)") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) +
    guides(color = guide_legend(override.aes = list(size=2, alpha = 1))) +

    facet_wrap(~ nborhood) 

ggsave("residuals_avg.png")




ggplot(data = data_test) +
    geom_density2d(aes(x = buysum/1000000, y = resid_linear/1000000, color = "red"), size = 0.3, alpha = 0.4) +
    geom_density2d(aes(x = buysum/1000000, y = resid_knn/1000000, color = "blue"),size = 0.3, alpha = 0.4) +
    geom_density2d(aes(x = buysum/1000000, y = resid_rf/1000000, color = "green"),size = 0.3, alpha = 0.4) +
    geom_density2d(aes(x = buysum/1000000, y = resid_nnet/1000000, color = "yellow"),size = 0.3, alpha = 0.4) +
    geom_density2d(aes(x = buysum/1000000, y = resid_xgbTree/1000000, color = "purple"),size = 0.3, alpha = 0.4) +
#    coord_fixed(ratio=1, xlim = c(0,5000000), ylim = c(0,5000000)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_colour_manual(name = "", 
                      values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow", "purple"= "purple"),
                      labels = c("red" = "linear","blue" = "k-NN","green" = "Random Forest","yellow" = "Bayesian nnet","purple" = "xgb"), guide = "legend") +
#    scale_colour_manual(name = "", 
#                      values =c("red"="red","blue"="blue", "green" = "green", "yellow" = "yellow"),
#                      labels = c("linear", "k-NN", "Random Forest", "Bayesian nnet"), guide = "legend") +
    ylab("residual (mil.)") +
    xlab("actual (mil.)") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(fill=NA)) +
    guides(color = guide_legend(override.aes = list(size=3, alpha = 1))) +

    facet_wrap(~ nborhood) 

ggsave("residuals_density.png") 







# EXPERIMENTS BELOW

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

      
      
      
  

ggmap(map_cph, base_layer=ggplot(aes(x=lon,y=lat), data=data_geo), extent = "normal", maprange=FALSE) +
    #    geom_polygon(data = bydel, aes(x = long, y = lat, group = group),
    #                 color = "grey50", alpha = 0.1, show.legend = FALSE) +
        geom_point(data = data_test, aes( x = lon, y = lat, color = resid_avg), size = 0.5, alpha = 0.6) +
        coord_map(projection="mercator", 
                  xlim=c(attr(map_cph, "bb")$ll.lon, attr(map_cph, "bb")$ur.lon),
                  ylim=c(attr(map_cph, "bb")$ll.lat, attr(map_cph, "bb")$ur.lat)) +
        scale_color_gradient(high = "orange", low = "blue") +
#        scale_color_gradient2(high = "blue", mid = "orange", low = "blue") +
    guides(color = guide_colorbar(barwidth = 20,
                                  barheight = 0.3,
                                  title = "Log of absolute average model residuals",
                                  title.position = "top")
           ) +
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
      
ggsave("resid_geo.png")
  

data_res = gather(data_test, key = model, value = pred, c(pred_linear,pred_knn,pred_nnet,pred_rf))

data_res$model <- as.factor(data_res$model)
levels(data_res$model) <- c("Linear", "KNN","Neural Net", "Random Forest", "XgbTree")



      ggmap(map_cph, base_layer=ggplot(aes(x=lon,y=lat), data=data_geo), extent = "normal", maprange=FALSE) +
    #    geom_polygon(data = bydel, aes(x = long, y = lat, group = group),
    #                 color = "grey50", alpha = 0.1, show.legend = FALSE) +
        geom_point(data = data_res, aes( x = lon, y = lat, color = pred - buysum), size = 0.3, alpha = 0.6) +
        coord_map(projection="mercator", 
                  xlim=c(attr(map_cph, "bb")$ll.lon, attr(map_cph, "bb")$ur.lon),
                  ylim=c(attr(map_cph, "bb")$ll.lat, attr(map_cph, "bb")$ur.lat)) +
        scale_color_gradient(high = "orange", low = "blue") +
#        scale_color_gradient2(high = "blue", mid = "orange", low = "blue") +
    guides(color = guide_colorbar(barwidth = 20,
                                  barheight = 0.3,
                                  title = "Log of absolute average model residuals",
                                  title.position = "top")
           ) +
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
          plot.background=element_blank()) +
    facet_wrap(~ model)

      
ggsave('facet_map.png')
#***********  6.7.2) Analytical comparison *************************

#RMSE and R2 for each model
resamp = resamples(list(knn = m_knn, rf = m_rf, lin = m_linear, linPCA = m_linearPCA, nnet = m_nnet, xgbTree = m_xgbTree ))
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