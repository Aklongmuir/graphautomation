################################################################################
########Get Schedule This is Emmanuel Perry's Code from corsica.hockey##########
################################################################################
ds.get_schedule <- function(start, end, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_schedule() imports the schedule page corresponding to a given date range and returns a JSON list object
    
    url <- paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
                 as.character(start),
                 "&endDate=",
                 as.character(end),
                 sep = ""
    )
    
    raw_text <- NULL
    json_check <- NULL
    
    while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
        
        try(
            url %>%
                getURL(header = FALSE,
                       .opts = curlOptions(referer = "nhl.com",
                                           verbose = FALSE,
                                           followLocation = TRUE,
                                           useragent = agents[sample(1:length(agents), 1)]
                       )
                )
        ) ->
            raw_text
        
        json_check <- try(fromJSON(raw_text))
        
        try_tolerance <- try_tolerance - 1
        
    }
    
    raw_json <- try(fromJSON(raw_text))
    
    if(class(raw_json) == "try-error") {raw_json <- NULL}
    
    return(raw_json)
    
}

################################################################################
###This code written by Matthew Barlowe @Matt_Barlowe on twitter################
################################################################################
#this functions pulls out each day's game ids from the schedules and stores
#them in a vector
get_games <- function(){
    #gets yesterday's date
    date <- Sys.Date()-1
    
    #scrapes all the games from the day before and stores in a list
    list_of_games <- ds.get_schedule(date,date)
    
    #pulls out each game number from the scheduled games and stores it in a vector
    games <- c()
    for (i in 1:length(list_of_games$dates[[1]]$games)) {
        games <- append(games, list_of_games$dates[[1]]$games[[i]]$gamePk)}
    games <- substr(games, 6, 10)
    return(games)
}