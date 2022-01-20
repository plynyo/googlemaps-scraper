# Geocoding script for large list of addresses. 
# Shane Lynn 10/10/2013
#load up the ggmap library
#======================================================================================================
# ADMIN
#======================================================================================================
want = c("tidyverse", "broom",   # Data management
         "rvest",                # Scraping
         "rio", "readtext",      # Leggere/scrivere dati
         "hunspell", "quanteda","tidytext", # QTA
         "dplyr","RCurl","plyr",
         "topicmodels","lda","ldatuning","stm", # librerie per topic models
         "ggrepel", # una miglioria ai grafici ggplot
         "factoextra", # libreria per scree plot CA
         "wordcloud2",
         "reshape2","ggplot2",
         "gmapsdistance","geosphere","ggmap","googleway",
         "matlib","expm","tibble",
         "dplyr","stringr","tidyverse",
         "rstudioapi","RColorBrewer","colorRamps",
         "igraph","ggplot2","plotly","htmlwidgets","pheatmap",
         "eurostat","iotables",
         "knitr","xlsx",
         "patchwork"
) 


have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
rm(have,want,junk)

rm(list = ls()) # Rimuove tutti gli oggetti presenti nel workspace

###############################################################################
#########################AUTOMATICALLY SET FOLDER PATH#########################
###############################################################################
setwd(dirname(getActiveDocumentContext()$path))
getwd()


###############################################################################
#########################SET GOOGLE API KEY####################################
###############################################################################

ggmap::register_google(key = "")
#set.api.key("")
set.api.key("")
key <- ''



###############################################################################
###############################READ CSV WITH ALL CITY NAME#####################
###############################################################################

# Select the file from the file chooser
#fileToLoad <- file.choose(new = TRUE)
#City_list <- read.csv(file = 'data/ListaComuni.csv',sep = ";")
City_list <- read.csv(file = 'data/ListaComuniChisone.csv',sep = ";")


###############################################################################
##############################DOWNLOAD LAT LONG WITH GEOCODE###################
###############################################################################

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(City_list)){
  # Print("Working...")
  result <- geocode(paste(City_list$CITY_NAME[i],"Turin,Italy", 
                          sep = ",", collapse = NULL), 
                    output = "latlona", source = "google")
  City_list$lon[i] <- as.numeric(result[1])
  City_list$lat[i] <- as.numeric(result[2])
  City_list$geoAddress[i] <- as.character(result[3])
}

# Write a CSV file containing origAddress to the working directory
write.csv(City_list, "data/geocodedChisone.csv", row.names=FALSE)

#City_list <- read.csv(file ="data/geocoded.csv")


############################################################################
#########################LIST OF GOOGLE PLACE TYPE##########################
############################################################################
#theatre, cinema, museum, biblioteque, siti archelogici, siti religiosi, 
#siti paleontologici, siti archelogici

#places <- c("museum","cinema",
#            "church","synagogue","mosque",
#            "library",
#            "tourist_attraction","point_of_interest","place_of_worship",
#            "park","city_hall")

string <- c("chiesa")
places <- c("church")

#QUERY 2
#"museum","church","synagogue","mosque","park","city_hall","tourist_attraction"

#QUERY 3
#"museum","cinema"
#"church", "synagogue","mosque"
#"library"
#"tourist_attraction","point_of_interest","place_of_worship"
#"park","city_hall"

#QUERY 3
#"museum","cinema"
#"church", "synagogue","mosque"
#"library"
#"tourist_attraction","point_of_interest","place_of_worship"
#"park","city_hall"

###############################################################################
########################LIST OF PLACES FOR EACH CITY###########################
###############################################################################

#DEFINE AN EMPTY DATAFRAME 
all_data <- data.frame(
  city_name=character(),
  poi_name=character(),
  type=character(),
  #address=character(),
  viciniy=character(),
#  lat=double(),
#  lng=double(),
  stringsAsFactors=FALSE
  )

col_names <- colnames(all_data)
#length(City_list$CITY_NAME)
for(i in 1:length(City_list$CITY_NAME)){#LOOP ON CITY NAMES #length(City_list$CITY_NAME)
  for(p in 1:length(places)){#LOOP ON PLACE TYPES
    
    #DOWNLOAD LIST OF ALL SITES FOR EACH CITY AND PLACE TYPE
    df_places <- google_places(#search_string = paste(string[p],",",City_list$CITY_NAME[i]), 
                               location = c(City_list$lat[i], City_list$lon[i]),
                               place_type = places[p],
                               radius = 2000,
                               keyword = string[p],
                               #rankby = "distance",
                               key = key)

    for(n in 1:length(df_places$results$name)){ #INTERNAL LOOP ON ALL POI
      if(length(df_places$results$types[[n]])!=1){
        a <- 2
      }else{
        a <- 1
      }

      for(t in 1:a){ #INTERNAL LOOP ON PLACE TYPE WHEN MORE THAN ONE

        temp <- c(City_list$CITY_NAME[i],
                  df_places$results$name[n],
                  df_places$results$types[[n]][t],
                  #df_places$results$formatted_address[n]
                  df_places$results$vicinity[n]#,
                  #df_places$results$geometry$location$lat[n],
                  #df_places$results$geometry$location$lon[n]
        )
        
        if(length(df_places$results$types[[n]][t])>0){
          if(grepl(City_list$CITY_NAME[i], df_places$results$vicinity[n], fixed = TRUE)){
            print(paste(
            temp[1]," ",
            temp[2]," ",
            temp[3]," ",
            temp[4]," "#,
            #temp[5]," ",
            #temp[6]
            )
            )
            all_data <- rbind(all_data,temp
                          #c(temp[1],temp[2],temp[3],temp[4],temp[5],temp[6])
            )
            colnames(all_data) <- col_names
          }    
        }
      }
    }
  }
}


write.csv(all_data, "data/chiesa - chisone - 31052021.csv", row.names=FALSE)
