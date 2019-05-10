key <- ""
user <- get_steam_id_64(key, "zfleeman")

#get friends list
frienddf <- jsonlite::fromJSON("http://api.steampowered.com/ISteamUser/GetFriendList/v0001/?key=&steamid=&relationship=friend")
frienddf <- frienddf$friendslist$friends

#get games
ownedgames <- jsonlite::fromJSON(paste("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=",key,"&steamid=",user,"&format=json&include_appinfo=1&include_played_free_games=0", sep=""))
ownedgames <- ownedgames$response$games
ownedgames$hours <- round(ownedgames$playtime_forever/60,2)
ownedgames$twoweeks_hours <- round(ownedgames$playtime_2weeks/60,2)
