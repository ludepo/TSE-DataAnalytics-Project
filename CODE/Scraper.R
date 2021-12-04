################################################################################
# Loading the necessary libraries
library(jsonlite) #For downloading the JSON files from URL's
library(rvest) #For working with HTML files
library(tidyverse) #General R library
library(newsanchor) #For downloading data from WSJ
library(lubridate) #For working with date objects
library(XML)

api_key <- "rhSNGpdce0f7O3E9DmBxrv1M13Aqmldh" #API Key for NY Times
dir <- "/Users/andrewboomer/Desktop/M2_Courses/DataAnalysis/dataanalysis"
dir <- getwd()
data_dir <- paste0(getwd(), '/DATA/')


################################################################################
# Create function to get the metada from the NY Times archive
nytime_meta <- function(years_months) {
	docs <- list() #Initialize empty list for dataframes

	for (i in 1:length(years_months)) { #Loop through the list of dates
		date <- years_months[i] #Get the date
		year <- substr(date, 1, 4) #Get the year
		month <- substr(date, 5, nchar(date)) #Get the month

		#Print the year and month to keep track of progress
		print(year)
		print(month)

		#Combine the base url with year, month, and api_key to get url
		url <- paste0('https://api.nytimes.com/svc/archive/v1/', year, '/', month, '.json?api-key=', api_key)

		#Get the metadata file from the specified year and month interval
		meta <- fromJSON(url, flatten = T)

		if (meta$response$meta$hits == 0) {
			docs[[i]] <- list()
			next
		}

		currdf <- data.frame(meta$response$docs) #Data is held in this portion
		#Add onto the docs list
		docs[[i]] <- subset(currdf, select=c(source, web_url, pub_date, word_count, section_name, print_page, news_desk))
	}
	#Then the docs list can be combined with rbind into one df
	docs <- data.frame(do.call("rbind", docs))
	write_csv(docs, paste0(data_dir, "/NYtimesMeta", years[1], "-", years[length(years)], ".csv")) #Write to csv
}

################################################################################
#Create function to loop through the web urls from the meta file
nytime_arts <- function(meta_file, out_file) {
	docs <- read.csv(paste0(data_dir, "/", meta_file)) #Get the supplied meta file
	print("Uploaded Meta File") #Notify user that meta file has uploaded

	#Specify function to download and get the text from HTML, and error check
	get_text <- function(x) {
		print(100 * (x / dim(docs)[1]))
		url <- docs[x, ]$web_url
		path <- '*//section[@name="articleBody"]'
		down <- tryCatch( #Catch any errors
			#Will get back html text if no errors
			return(html_text(html_nodes(read_html(url), xpath=path))[1]),
			error = function(e) e,
			warning = function(w) w)

		#Return blank text if error or warning is returned
		if(inherits(down, "error")|inherits(down, "warning")) return("")
	}
	#Use the sapply function to loop through url's
	docs$text <- sapply(1:dim(docs)[1], function(x) get_text(x))
	write_csv(docs, paste0(data_dir, "/", out_file)) #Write to csv
}

################################################################################
#Get a subset of the meta data based on a section name
section_subset <- function(meta_files, section_name, sample_frac) {
	docs <- list() #Create an empty list of the meta files

	#Loop through the meta files given as an argument
	for (i in 1:length(meta_files)) {
		#Read in the current meta file
		df <- read.csv(paste0(data_dir, "/", meta_files[[i]]))
		df <- df[df$section_name == section_name, ] #Filter for section name
		docs[[i]] <- df #Fill the list with the current meta frame
	}

	#Bind all the data frames together
	df <- data.frame(do.call('rbind', docs))
	df <- df[df$word_count > 0, ] #Remove empty articles
	df$Date <- as.Date(df$pub_date) #Cast date as date object
	#Get unique sorted data frame
	dates <- data.frame(sort(unique(df$Date)))
	names(dates) <- c("Date")

	#Number the dataframe by weeks in order to group by week number
	num_weeks <- floor((dim(dates)[1]/7))
	dates$Week <- c(rep(1:num_weeks, each=7), rep((num_weeks+1), dim(dates)[1] - (num_weeks * 7)))
	df <- merge(df, dates, by='Date', all.x=TRUE)
	#After getting week numbers by date, sample a percentage by week
	sample_df <- df %>% group_by(Week) %>% sample_frac(sample_frac)

	#Write file to csv
	write_csv(sample_df, paste0(data_dir, "/NYtimesMeta_sample_", section_name, ".csv"))
}

#Specify the set of years and months to get metadata for
years <- 1981:2020
months <- 1:12
grid <- expand.grid(years, months)
# #Call the meta function with the current set of dates
nytime_meta(paste0(grid$Var1, grid$Var2))

#Define section name we want to filter for
sec_name <- "World"
#Define the needed meta files to get article urls
meta_files <- c("NYtimesMeta.csv", "NYtimesMeta1979-1989.csv")
samps <- 0.06 #Sample percentage per week
section_subset(meta_files, sec_name, samps) #Get the meta subset
nytime_arts(paste0("NYtimesMeta_sample_", sec_name, ".csv"), paste0("NYtimesArticles_sample_", sec_name, "_", samps, ".csv"))

# nytime_arts("NYtimesMeta_World_World.csv", "NYtimesArticles_World_World.csv")



