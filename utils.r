
library(RCurl)
library(plyr)



configApp <- function() {
        
        full_url <- oauth_callback()
        print(paste("FULL URL:", full_url), sep="")
        #http://localhost:1410
        full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")

        print(full_url)
        
        options("scipen"=100, "digits"=4)
        
        # Authentication --------------------------
        saveAppConf("GeoMK", "a6db5a4129e34e8ea90f8409949d5c84",
                    "c373eb16fa3c4876ae3e857b9a6f72b6")
}


# Save the app configuration for having all the data in the enviroment
saveAppConf <- function(appName, id, secret, scope = "basic") {
        
        app_name <<- appName
        client_id <<- id
        client_secret <<- secret
        scope <<- scope
        
        instagram <<- oauth_endpoint(
                authorize = "https://api.instagram.com/oauth/authorize",
                access = "https://api.instagram.com/oauth/access_token")
        
        myapp <- oauth_app(app_name, client_id, client_secret)
        
        ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  
                                   type = "application/x-www-form-urlencoded", cache = F)
        tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
        token <<- tmp[[1]][4]
        
}


# Search places around a place. 
searchPlaces <- function(lat = 51.5286416, lng = -0.1015987, dist = 1000, facebook_id = NULL) {
        
        placeEndPoint <- "https://api.instagram.com/v1/locations/search?"
        
        if (is.null(facebook_id)) {
                lat <- paste("lat=", lat, sep="")
                lng <- paste("lng=", lng, sep="")
                dist <- paste("distance=", dist, sep = "")
                access_token <- paste("access_token=", token, sep = "")
                
                
                URL <- paste(placeEndPoint, lat, "&", lng, "&", dist, 
                             "&count=100", "&", access_token, sep = "")     
        } else {
                facebook_id <- paste("facebook_places_id=", facebook_id, sep = "")
                dist <- paste("distance=", dist, sep = "")
                access_token <- paste("access_token=", token, sep = "")
                
                
                URL <- paste(placeEndPoint, facebook_id, "&", dist, 
                             "&count=100", "&", access_token, sep = "")     
                
        }
        
        
        mediaFromPlace <- fromJSON(getURL(URL),unexpected.escape = "skip")
        
        
}

# Search the media from a place. Private function
searchMediaRequest <- function(lat, lng, min_timestamp = NULL,
                        max_timestamp = NULL, dist = 1000, count = 100,
                        next_max_id = NULL) {
        
        endPoint <- "https://api.instagram.com/v1/media/search?"
        lat <- paste("lat=", lat, sep = "")
        lng <- paste("lng=", lng, sep = "")
        count <- paste("count=", count, sep = "")
        dist <- paste("distance=", dist, sep = "")
        min_time<- ""
        if (!is.null(min_timestamp)) {
                min_time <- paste("&min_timestamp=", min_timestamp, sep = "") 
        }
        max_time <- ""
        if (!is.null(max_timestamp)) {
                max_time <- paste("&max_timestamp=", max_timestamp, sep = "")
        }
        next_id <- ""
        if (!is.null(next_max_id)) {
                next_id <- paste("&next_max_id=", next_max_id, sep = "")
        }
        
        access_token <- paste("access_token=", token, sep = "")
        
        
        URL <- paste(endPoint, lat, "&", lng, "&", count, "&",
                     dist, min_time, max_time, next_id,
                     "&", access_token, sep = "")      
        
        #textJSON <- getURL(URL)
        textJSON <- readLines(URL, warn = F)
        print(URL)
        
        media <- RJSONIO::fromJSON(textJSON)
        
        media
        
        
}

searchMedia <- function(lat, lng, min_time = NULL,
                        max_time = NULL, dist = 1000) {
        
        working <- T
        first <- T
        
        start <- Sys.time()
        
        while (working == T) {
                

                temp <- searchMediaRequest(lat = lat, lng = lng, 
                                           dist = dist, min_timestamp = min_time, 
                                           max_timestamp = max_time)  
                
                if (!is.null(temp)) {
                        # Is the response correct?
                        if (temp$meta==200) {
                                
                                rawData <- createRawMedia(temp$data)
                                
                                if (first==T) {
                                        tagsTotal <- rawData$tags
                                        media <- rawData$media
                                        first <- F
                                        
        #                                 # Calculate the algorithm time
        #                                 timeBetween <- as.numeric(difftime(as.POSIXct(max_time, origin = "1970-01-01"),
        #                                                         as.POSIXct(min_time, origin = "1970-01-01"),
        #                                                         units = "mins"))
        #                                 print(timeBetween)
        #                                 
        #                                 timeBetween2 <- as.numeric(difftime(
        #                                                         as.POSIXct(max_time, origin = "1970-01-01"),
        #                                                         as.POSIXct(as.numeric(
        #                                                         min(rawData$media$created_time)), 
        #                                                         origin = "1970-01-01"),
        #                                                         units = "mins"))
        #                                 print(timeBetween2)
        #                                 
        #                                 end <- Sys.time()
        #                                 timeSpent <- as.numeric(difftime(end,start, units = "mins"))
        #                                 print(timeSpent)
        #                                 
        #                                 print(paste("Algorithm will spent: ",
        #                                             timeBetween*timeSpent/timeBetween2, " min", sep=""))
                                        
                                        
                                } else {
                                        tagsTotal <- rbind(tagsTotal, rawData$tags)
                                        media <- rbind(media, rawData$media)   
                                        print(paste("min_time", 
                                                    as.POSIXct(min_time, origin = "1970-01-01"), 
                                                    "max_time", 
                                                    as.POSIXct(max_time, origin = "1970-01-01"),
                                                    dim(media)[1]), sep = " - ")
                                }
                                
        
                                # STOP THE WHILE
                                if (max_time == as.numeric(min(rawData$media$created_time))) {
                                        working <- F
                                } else {
                                        max_time <- as.numeric(min(rawData$media$created_time))                   
                                } 
                                
                                
                        } else {      
                                print(paste("ERROR: ", temp$error_type, 
                                            " MESSAGE: ", temp$error_message))
                                working <- F    
                        }
                }
                
                
                  
        }
         # TODO PROBLEMA CON TAGS TOTAL
        result <- list(media = media, tags = tagsTotal)

}


# Test the request response
testRequest <- function(response) {
        
        result <- FALSE
        if (!is.null(response$meta$code) & response$meta$code == 200) {
                result <- TRUE
        }
        
        result
}


testInputs <- function(lat = NULL, lng = NULL, min_time = NULL,
                       max_time = NULL, dist = NULL) {
        
        r <- FALSE
        if (!is.na(lat) & is.numeric(lat) & 
            !is.na(lng) & is.numeric(lng) &
            is.numeric(min_time) &
            is.numeric(max_time) &
            is.numeric(dist)) {
                
                r <- TRUE
        }
        
        r
}

# Get the media rawData from the data response
# Return a list with two data.frames
# tags with the information of the tags and the day
# media with the information of the media and when was uploaded
createRawMedia <- function(media) {
        
        temp <- data.frame(day = 0, created_time = 0, lat = 0, lng = 0,
                           place.name = 0, place.id = 0,
                           type = 0, comments = 0, likes = 0, id = 0,
                           username = 0, picUrl = 0, tags = 0, row.names = NULL)[-1,]
        
        tagsTotal <- data.frame(tag = 0, day = 0)[-1,]

        
        for (i in 1:length(media)) {

                if (!is.null(media[[i]]$location$latitude) & 
                    !is.null(media[[i]]$location$longitude)) {
                        
                        created_time = as.POSIXct(as.numeric(
                                media[[i]]$created_time), origin="1970-01-01")
                        day <- strftime(created_time, 
                                        format="%d/%m/%Y")
                        
                        
                        if(length(media[[i]]$tags)==0){
                                tagsTemp <- NA
                        } else { 
                                tagsTemp <- as.list(media[[i]]$tags)
                        }
                        
                        place.name <- NA
                        place.id <- NA
                        if (!is.null(media[[i]]$location$name)){
                                place.name <- media[[i]]$location$name
                                place.id <- media[[i]]$location$id   
                                
                        }
                        
                        if (is.null(media[[1]]$id)==T){
                                id <- ""
                        }else {
                                id <- media[[1]]$id
                        }
                        
                        temp <- rbind(temp, data.frame(day = day, 
                                                       created_time = created_time, 
                                                       lat = media[[i]]$location$latitude,
                                                       lng = media[[i]]$location$longitude,
                                                       place.name = place.name,
                                                       place.id = place.id,
                                                       type = media[[i]]$type, 
                                                       comments = media[[i]]$comments$count,
                                                       likes = media[[i]]$likes$count, 
                                                       id =  id, 
                                                       username = media[[i]]$user[1], #username
                                                       picUrl = media[[i]]$images$standard_resolution$url,
                                                       tags = I(list(tagsTemp)) 
                        ))
                        
                        
                        if (length(media[[i]]$tags)>0) {
                                
                                days <- rep(day, length(media[[i]]$tags))
                                
                                tagsTotal <- rbind(tagsTotal, 
                                                   data.frame(tag = unlist(
                                                        media[[i]]$tags, F, F), 
                                                        day = days))
                        }                                
                }
        }
        
        tagsTotal$tag <- as.character(tagsTotal$tag)
        tagsTotal$day <- as.Date(tagsTotal$day, format = "%d/%m/%Y", 
                                 origin = "1970-01-01")
        
        data <- list(tagsTotal = tagsTotal, media = temp)

}


# Aggregate function
# by could be, tag, day or both
aggregateTags <- function(tags, by = NULL) {

        if (is.null(by) || by == 1) {
                tags <- count(tags, names(tags)[1])
        
        } else {
                
                if (by == 2) {
                        tags <-count(tags, names(tags)[2])
                        
                } else {
                        tags <- count(tags, names(tags))
                }
                
        } 
   tags <- tags[order(tags$freq, decreasing = T),]
}


# Get the users vector
getUsers <- function(media) {
        users <- unique(media$username)
}


# Get the hashtags vector
getHashTags<- function(tags) {
        tags <- unique(tags$tag)
}


# filter the media results
filterTags <- function(filters, media) {

      filters <- paste(filter, sep = "", collapse = "|")    
      media <- media[grep(filters, media$tags),]
      media
}


# get related tags MAL 
getRelatedTags <- function(filters, tags) {
        
        filters <- paste(filter, sep = "", collapse = "|")
        tags <- unique(tagsTotal[grep(filters, tagsTotal$tag),1])
        
        unique(as.vector(tagsTotal[grep(tags, tagsTotal$tag),1]))
        tags
}


# Plot the media in a map
plotPointsMap <- function(media, lat, lng, zoom = 12) {
        print("plotPointsMap")
        
        # Initaialize the variables
        map <- leaflet()
        map <- setView(map = map, lng = lng, lat = lat, zoom = zoom)
        map <- addTiles(map = map)
        clusters <- markerClusterOptions()
        latitude <- c()
        longitude <- c()
        images <- c()
        
        # Group all the markers (lat, lon and images)
        for (i in 1:dim(media)[1]) {
                images <- c(images, paste("<img src=\"", as.character(media$picUrl[i]), 
                                          "\" alt=\"#fashion\" height=\"160\" width=\"160\">", sep=""))
                latitude <- c(latitude, media$lat[i])
                longitude <- c(longitude, media$lng[i])
                
        }
        
        # Add the markers to the map
        map <- addMarkers(map, lat = latitude, lng = longitude, popup = images, 
                          clusterOptions = clusters )
        
        map
}


# Write media. esFormat means if the separatos for the csv will be spanish format
writeMedia <- function(media, fileName, esFormat = T) {
        
        media <- cbind(media, as.data.frame(do.call(rbind, media$tags)))
        media <- media[,!(names(media) %in% c("tags"))]
        if (esFormat==T) {
                write.table(media, fileName, sep = ";", 
                            dec = ",", row.names = F, na = "")     
        } else {
                write.csv(media, fileName, row.names = F)  
        }
        
        print("end writeMedia")
}


