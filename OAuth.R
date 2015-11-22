# OAuth in Instagram
library(httr)


# appName
# id
# secret
# scope
# configApp
configApp <- function(appName, id, secret, scope = "basic") {
        
        # Get the url
        full_url <- oauth_callback()
        
        # Print the URL
        #print(paste("FULL URL:", full_url), sep="")
        full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
        
        
        options("scipen"=100, "digits"=4)
        
        
        appName <- "GeoMK"
        id <- "a6db5a4129e34e8ea90f8409949d5c84"
        secret <- "c373eb16fa3c4876ae3e857b9a6f72b6"
        scope <- "basic"
        
        
        instagram <<- oauth_endpoint(
                authorize = "https://api.instagram.com/oauth/authorize",
                access = "https://api.instagram.com/oauth/access_token")
        
        myapp <- oauth_app(appName, id, secret)
        
        ig_oauth <- oauth2.0_token(instagram, myapp,scope,  
                                   type = "application/x-www-form-urlencoded", cache = F)
        tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
        token <- tmp[[1]][4]
        
        if (!is.null(token)) {
                print("token not NULL")

        } else {
                print("token NULL")
        }
        
        # Return the token
        token
        
}
