rm(list = ls())
#set the working directory
setwd("C:/Users/Robert/Desktop/Master Thesis/Data")

#load the packages

library(rworldmap)
library(tidyverse)
library(RSelenium)
library(netstat)
library(pdftools)
library(stringr)
library(tm)
library(readxl)
library(dplyr)
library(httr)
library(domir)

#define the functions
###

#checks whether a whole number is returned
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#This function converts the PDF into a text string and uses text preprocessing steps 
#to clean it
cleanPDFText <- function(file_path) {
  
  text <- pdftools::pdf_text(file_path)
  
  clean_text <- paste(text, collapse = " ")
  
  clean_text <- tolower(clean_text)
  clean_text <- removeNumbers(clean_text)
  clean_text <- removePunctuation(clean_text)
  clean_text <- removeWords(clean_text, stopwords("en"))
  clean_text <- stripWhitespace(clean_text)
  
}

#This function checks whether the link to a given whitepaper is working and returns TRUE, not working
# or when an error occurs and returns FALSE
checkLinkStatus <- function(url) {
   tryCatch(
    {
      response <- GET(url)
      status <- status_code(response)
      
      if (status == 200) {
        return(TRUE)  
      } else {
        return(FALSE)  
      }
    },
    error = function(err) {
      return(FALSE)  
    }   )
}


# #load in the two datasets provided
# 
# Final_after_filtering <- read.csv2("03b_Final_Dataset.csv")
# Final_before_filtering <- read.csv2("03a_Final_Dataset_before_filtering_and_missing.csv")
# Trading_data_time_series <- read.csv2("03c_ICOmarks_market_price_series_matched.csv")
# x1 <- read.csv2("03a_UnFinal_Dataset.csv")
# 
# 
# ##
# #1.Converting all variables to the correct type
# ##
# 
# 
# #there was a problem converting ESGDummy and Finsentiment so I manually convert them to numeric 
# Final_before_filtering$ESGDummy <- as.numeric(Final_before_filtering$ESGDummy)
# Final_before_filtering$FinSentiment <- as.numeric(Final_before_filtering$FinSentiment)
# table(Final_before_filtering$ESGDummy)
# table(Final_before_filtering$FinSentiment)
# 
# 
# #Numeric: 3,10,,11,12,13,43,45,47,48,49,55,57,73,74,75,78,79,80,81,82,83-87
# #Boolean: 4,5,6,14,15,28,31,33,35,36,40,51,58,59,60,61,65,66-72
# 
# for( i in c(3,10:13,43,45,47:49,55,57,73:75,78:87) ){
#   Final_before_filtering[,i] <- as.numeric(Final_before_filtering[,i])
# }
# 
# for( i in c(4:6,14,15,28,31,33,35,36,40,51,58:61,65,66:72)){
#   Final_before_filtering[,i] <- as.logical(Final_before_filtering[,i])
# }
# 
# summary(Final_before_filtering)
# 
# 
# ##
# #2. Creating a Data frame to work with that only contains 
# #relevant variables and observations
# ##
# 
# #First I drop the obviously obsolete variables:
# IcoData <- Final_before_filtering[,-c(16,44,46,56)]
# 
# #Then I check the availability of one of my key variables: The ICO price
# table(IcoData$PriceUSD)
# #remove the NAs
# IcoData <- IcoData[is.na(IcoData$PriceUSD) == F,]
# min(IcoData$PriceUSD)
# #remove prices of zero
# IcoData <- IcoData[IcoData$PriceUSD != 0,]
# min(IcoData$PriceUSD)
# max(IcoData$PriceUSD)
# sd(IcoData$PriceUSD)
# quantile(IcoData$PriceUSD, 0.99)
# quantile(IcoData$PriceUSD, 0.9925)
# quantile(IcoData$PriceUSD, 0.995)
# quantile(IcoData$PriceUSD,0.9975)
# #taking a closer look at the Icos with very large prices
# LargePrice <- IcoData[IcoData$PriceUSD > quantile(IcoData$PriceUSD, 0.999),]
# #I remove these observations as they seem to be outliers 
# IcoData <- IcoData[IcoData$PriceUSD <= quantile(IcoData$PriceUSD, 0.999),]
# 
# #I logarithmise (using ln) the ICO prices. 
# IcoData$PriceUSD <- log(IcoData$PriceUSD)
# ggplot(IcoData, aes(x=PriceUSD)) + geom_histogram(bins = 150)
# 
# ##
# #Checking for Whitepaper links
# ##
# 
# #I retrieve the names of the ICOs from the urls
# 
# IcoData$ICO_names <- basename(IcoData$url)
# 
# #I paste those names together with the links of cryptototem in order to check for the availability.
# 
# ###I will have to do that multiple times - as there are different link structures on
# #cryptototem
# ##e.g. :  firstblood: https://cryptototem.com/firstblood-ico/ (v1)
# #this does not include the ticker name.
# ##or singulardtv: https://cryptototem.com/singulardtv-sngls-ico (V2)
# # this includes the ticker-name.
# ##sometimes there is even a double -- between the name and the ticker.
# #e.g.: incent: https://cryptototem.com/incent--incnt-ico/ (v3)
# ## but I don't know if that was an error - only encountered it once - however I will try.
# 
# #lets create the links
# ## V1
# base_url <- "https://cryptototem.com/INSERT-ico/"  
# IcoData$urlsV1 <- c(1:nrow(IcoData))
# # Generate the URLs by replacing "INSERT" with the names
# for(i in 1:nrow(IcoData)){
#   IcoData$urlsV1[i] <- paste0(gsub("INSERT", IcoData$ICO_names[i], base_url))
# }
# ## V2
# base_url <- "https://cryptototem.com/INSERT-HERE-ico"  
# IcoData$urlV2 <- c(1:nrow(IcoData))
# for(i in 1:nrow(IcoData)){
#   IcoData$urlV2[i] <- paste0(gsub("INSERT", IcoData$ICO_names[i],base_url))
# }
# for(i in 1:nrow(IcoData)){
#   IcoData$urlV2[i] <- paste0(gsub("HERE", tolower(IcoData$Ticker[i]), IcoData$urlV2[i]))
# }
# 
# ##use RSelenium to retrieve the available pdfs
# #start the Server
# 
# IcoData <- read.csv("IcoData.csv")
# #IcoData_2 <- read.csv("IcoData_2.csv")
# #IcoData_3 <- read.csv("IcoData_3.csv")
# #rs_driver_object$server$stop()
# rs_driver_object <- rsDriver(browser = "firefox",
#                              #chromever = "114.0.5735.90",
#                              verbose = F,
#                              port = free_port())
# class(rs_driver_object)
# #this object has two things: client and server, in order to access the webbrowser,
# #access the clientside
# #create a client object
# remDr <- rs_driver_object$client
# 
# #create variable to store wp in
# #IcoData$pdfWP <- rep(NA, nrow(IcoData))
# #IcoData$pdfWP <- as.character(IcoData$pdfWP)
# #open the Browser
# remDr$close()
# remDr$open()
# remDr$navigate(IcoData$urlsV1[4])
# Sys.sleep(1)
# #close ad
# banner_close <- remDr$findElement(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
# banner_close$clickElement()
# #accept cookies
# cookie_accept <- remDr$findElement(using = "xpath", "//*[@id='footer-copyright']/div[2]/a")
# cookie_accept$clickElement()
# which(IcoData$ICO_names == "synthetics-ai")
# is.wholenumber <-
#   function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# tick <- 0 
# for(i in 2100:2200){
#   # Navigate to the webpage
#   remDr$navigate(IcoData$urlV2[i])
#   
#   #check whether there is a download button - if not, iterate to next ICO
#   matching_elements <- remDr$findElements(using= "class name", "black-url")
#   if(length(matching_elements) > 0){
#     check_banner <- remDr$findElements(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#     if(length(check_banner) > 0){
#       banner_close <- remDr$findElement(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#       banner_close$clickElement()}
#     download_button <- remDr$findElement(using= "class name", "black-url")
#     download_button$clickElement() 
#     
#     remDr$switchToWindow(remDr$getWindowHandles()[[2]])
#     Sys.sleep(1)
#     IcoData$pdfWP[i] <- as.character(remDr$getCurrentUrl())
#     remDr$closeWindow()
#     remDr$switchToWindow(remDr$getWindowHandles()[[1]])
#     tick <- tick + 1
#     if(is.wholenumber(tick/100)){write.table(IcoData, "IcoData.csv", row.names = F , sep = ",")}
#     
#   }else{tick <- tick + 1
#   if(is.wholenumber(tick/100)){write.table(IcoData, "IcoData.csv", row.names = F , sep = ",")}
#   }
#   
# }
# write.table(IcoData, "IcoData.csv", row.names = F , sep = ",")
# 
# sum(is.na(IcoData$pdfWP))
# 
# 
# #Second link structure
# ##
# #open the Browser
# remDr$close()
# remDr$open()
# remDr$navigate(IcoData$urlsV1[4])
# Sys.sleep(1)
# #close ad
# banner_close <- remDr$findElement(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
# banner_close$clickElement()
# #accept cookies
# cookie_accept <- remDr$findElement(using = "xpath", "//*[@id='footer-copyright']/div[2]/a")
# cookie_accept$clickElement()
# 
# IcoData_2 <- IcoData[is.na(IcoData$pdfWP),]
# 
# tick <- 0 
# for(i in 1:nrow(IcoData_2)){
#   # Navigate to the webpage
#   remDr$navigate(IcoData_2$urlsV1[i])
#   
#   #check whether there is a download button - if not, iterate to next ICO
#   matching_elements <- remDr$findElements(using= "class name", "black-url")
#   if(length(matching_elements) > 0){
#     check_banner <- remDr$findElements(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#     if(length(check_banner) > 0){
#       banner_close <- remDr$findElement(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#       banner_close$clickElement()}
#     download_button <- remDr$findElement(using= "class name", "black-url")
#     download_button$clickElement() 
#     
#     remDr$switchToWindow(remDr$getWindowHandles()[[2]])
#     Sys.sleep(1)
#     IcoData_2$pdfWP[i] <- as.character(remDr$getCurrentUrl())
#     remDr$closeWindow()
#     remDr$switchToWindow(remDr$getWindowHandles()[[1]])
#     tick <- tick + 1
#     if(is.wholenumber(tick/100)){write.table(IcoData_2, "IcoData_2.csv", row.names = F , sep = ",")}
#     
#   }else{tick <- tick + 1
#   if(is.wholenumber(tick/100)){write.table(IcoData_2, "IcoData_2.csv", row.names = F , sep = ",")}
#   }
#   
# }
# sum(is.na(IcoData_2$pdfWP))
# 
# IcoData_3 <- IcoData_2[is.na(IcoData_2$pdfWP),]
# 
# #V3 links
# base_url <- "https://cryptototem.com/INSERT--HERE-ico"  
# IcoData_3$urlV3 <- c(1:nrow(IcoData_3))
# for(i in 1:nrow(IcoData_3)){
#   IcoData_3$urlV3[i] <- paste0(gsub("INSERT", IcoData_3$ICO_names[i],base_url))
# }
# for(i in 1:nrow(IcoData_3)){
#   IcoData_3$urlV3[i] <- paste0(gsub("HERE", tolower(IcoData_3$Ticker[i]), IcoData_3$urlV3[i]))
# }
# 
# tick <- 0 
# for(i in 1:nrow(IcoData_3)){
#   # Navigate to the webpage
#   remDr$navigate(IcoData_3$urlV3[i])
#   
#   #check whether there is a download button - if not, iterate to next ICO
#   matching_elements <- remDr$findElements(using= "class name", "black-url")
#   if(length(matching_elements) > 0){
#     check_banner <- remDr$findElements(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#     if(length(check_banner) > 0){
#       banner_close <- remDr$findElement(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#       banner_close$clickElement()}
#     download_button <- remDr$findElement(using= "class name", "black-url")
#     download_button$clickElement() 
#     
#     remDr$switchToWindow(remDr$getWindowHandles()[[2]])
#     Sys.sleep(1)
#     IcoData_3$pdfWP[i] <- as.character(remDr$getCurrentUrl())
#     remDr$closeWindow()
#     remDr$switchToWindow(remDr$getWindowHandles()[[1]])
#     tick <- tick + 1
#     if(is.wholenumber(tick/100)){write.table(IcoData_3, "IcoData_3.csv", row.names = F , sep = ",")}
#     
#   }else{tick <- tick + 1
#   if(is.wholenumber(tick/100)){write.table(IcoData_3, "IcoData_3.csv", row.names = F , sep = ",")}
#   }
#   
# }
# (sum(is.na(IcoData_3$pdfWP)))
# 
# #Try alternative approach searching for the ICO names directly on cryptototem
# for (i in 1:nrow(IcoData_3)){
#   IcoData_3$ICO_names[i] <- gsub("-", " ", IcoData_3$ICO_names[i])
# }
# 
# IcoData_3 <- read.csv("IcoData_3.csv")
# remDr$close()
# remDr$open()
# remDr$navigate(IcoData_3$urlsV1[4])
# Sys.sleep(1)
# #close ad
# banner_close <- remDr$findElement(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
# banner_close$clickElement()
# #accept cookies
# cookie_accept <- remDr$findElement(using = "xpath", "//*[@id='footer-copyright']/div[2]/a")
# cookie_accept$clickElement()
# tick <- 0
# for(i in 1:nrow(IcoData_3)){
#   search_button <- remDr$findElement(using = "css selector", ".top-search-area > a:nth-child(1)")
#   search_button$clickElement()
#   Sys.sleep(1)
#   search_bar <- remDr$findElement(using ="css selector",".search")
#   search_bar$sendKeysToElement(list(IcoData_3$ICO_names[i], key = "enter"))
#   
#   webElem <- remDr$findElement("css", "body")
#   webElem$sendKeysToElement(list(key = "down_arrow"))
#   Sys.sleep(1)
#   page <- remDr$findElements(using ="css selector", ".custom-more-link")
#   if(length(page)>0){
#     open_page <- remDr$findElement(using ="css selector",".custom-more-link")
#     open_page$clickElement()
#     Sys.sleep(1)
#     if(length(remDr$findElements(using="class name", "black-url"))>0){
#       check_banner <- remDr$findElements(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#       if(length(check_banner) > 0){
#         banner_close <- remDr$findElement(using ="xpath", "/html/body/div[1]/div[1]/div/div[1]/div[3]/span")
#         banner_close$clickElement()}
#       download_button <- remDr$findElement(using= "class name", "black-url")
#       download_button$clickElement() 
#       
#       remDr$switchToWindow(remDr$getWindowHandles()[[2]])
#       Sys.sleep(1)
#       IcoData_3$pdfWP[i] <- as.character(remDr$getCurrentUrl())
#       remDr$closeWindow()
#       remDr$switchToWindow(remDr$getWindowHandles()[[1]])
#       
#       tick <- tick + 1
#       if(is.wholenumber(tick/100)){write.table(IcoData_3, "IcoData_3.csv", row.names = F , sep = ",")}
#       
#     }
#   }else{tick <- tick+1 
#   if(is.wholenumber(tick/100)){write.table(IcoData_3, "IcoData_3.csv", row.names = F , sep = ",")}
#   Sys.sleep(1.5)
#   
#   }
# }
# 
# which(IcoData_3$ICO_names == "gats")
# 
# rs_driver_object$server$stop()
# 
# 
# 
# IcoData <- read.csv("IcoData.csv")
# IcoData_2 <- read.csv("IcoData_2.csv")
# IcoData_3 <- read.csv("IcoData_3.csv")
# 
# sum(is.na(IcoData_3$pdfWP))
# 
# whitepaper <- IcoData[,c(1,87)]
# for(i in 1:nrow(whitepaper)){
#   if(is.na(whitepaper$pdfWP[i]) & whitepaper$url[i]%in% IcoData_2$url){
#     whitepaper$pdfWP[i] <- IcoData_2$pdfWP[IcoData_2$url == whitepaper$url[i]]
#   }
# }
# 
# 
# sum(is.na(whitepaper$pdfWP))
# 
# for( i in 1:nrow(whitepaper)){
#   if(is.na(whitepaper$pdfWP[i])& whitepaper$url[i]%in% IcoData_3$url){
#     whitepaper$pdfWP[i] <- IcoData_3$pdfWP[IcoData_3$url == whitepaper$url[i]]
#   }
# }
# 
# sum(is.na(whitepaper$pdfWP))
# length(IcoData$WhitepaperUrl[IcoData$WhitepaperUrl == ""])
# ################
# IcoDataWIP <- IcoData
# IcoDataWIP$pdfWP <- whitepaper$pdfWP
# 
# 
# #downloading the whitepapers 
# 
# IcoDataWIP <- IcoDataWIP[!(is.na(IcoDataWIP$pdfWP)),]
# IcoDataWIP <- IcoDataWIP[IcoDataWIP$pdfWP != "about:blank",]
# 
# 
# IcoDataWIP$wplinkworking <- c(2:(nrow(IcoDataWIP)+1))
# tick <- 0
# for(i in 1:nrow(IcoDataWIP)){
#   IcoDataWIP$wplinkworking[i] <- checkLinkStatus(IcoDataWIP$pdfWP[i])
#   tick <- tick+1
#   if(is.wholenumber(tick/100)){
#     write.table(IcoDataWIP, "IcoDataWIP.csv", row.names = F , sep = ",")
#     print("100 Iterations completed, saving file")
#   }}
# table(IcoDataWIP$wplinkworking)
# 
# write.table(IcoDataWIP, "IcoDataWIP.csv", row.names = F , sep = ",")


##read in the Dataset
#IcoDataWIP <- read.csv("IcoDataWIP.csv")
# table(IcoDataWIP$wplinkworking)
# tick <- 28
# for(i in 3072:nrow(IcoDataWIP)){
#   IcoDataWIP$wplinkworking[i] <- checkLinkStatus(IcoDataWIP$pdfWP[i])
#   tick <- tick+1
#   if(is.wholenumber(tick/100)){
#     write.table(IcoDataWIP, "IcoDataWIP.csv", row.names = F , sep = ",")
#     print("100 Iterations completed, saving file")
#   }}
# table(IcoDataWIP$wplinkworking)
# 
# write.table(IcoDataWIP, "IcoDataWIP.csv", row.names = F , sep = ",")
# 
# #Drop the observations where the link is not working
# IcoDataWIP <- IcoDataWIP[IcoDataWIP$wplinkworking == 1,]

#now I retrieve the information that I need from the whitepapers: 
#number of words and the ESG-score as well as separate E,S and G scores

#first I create a vector containing the whitepaper links and 4 additional columns
# IcoDataWIP <- read.csv("IcoDataWIP.csv")
# IcoDataWIP <- IcoDataWIP[IcoDataWIP$wplinkworking == 1 ,] 
# 
# #I only keep the links that end on .pdf
# 
# 
# #Checking if each link ends with ".pdf"
# IcoDataWIP <-IcoDataWIP[grepl("\\.pdf$",IcoDataWIP$pdfWP),]
# 
# 
# 
# WhitepaperInfo <- as.data.frame(IcoDataWIP$pdfWP)
# WhitepaperInfo$nwords <- rep(NA, nrow(WhitepaperInfo))
# WhitepaperInfo$ESG <- rep(NA, nrow(WhitepaperInfo))
# WhitepaperInfo$E <- rep(NA, nrow(WhitepaperInfo))
# WhitepaperInfo$S <- rep(NA, nrow(WhitepaperInfo))
# WhitepaperInfo$G <- rep(NA, nrow(WhitepaperInfo))



#Now I retrieve the whitepapers one by one, do all of the processing steps
#and then obtain the relevant infomartion

# #first I set up the selenium server
# rs_driver_object <- rsDriver(browser = "firefox",
#                              #chromever = "114.0.5735.90",
#                              verbose = F,
#                              port = free_port())
# 
# remDr <- rs_driver_object$client
# remDr$close()
# remDr$open()
# remDr$navigate("https://www.sustainableentrepreneurship.org/")

#I Extract the pdfs using the links
#tick <- 0
#for(i in 2923:nrow(WhitepaperInfo)){
#   pdfText <- cleanPDFText(WhitepaperInfo$IcoDataWIP.pdfWP[i])
#  
#   #gets the number of words in the pdf
#   input_string <- pdfText
#   #Trims leading and trailing whitespace
#   trimmed_string <- trimws(input_string)
#   #Splits the trimmed string into words
#   words <- strsplit(trimmed_string, "\\s+")
#   #Counts the number of words
#   word_count <- length(words[[1]])
#   
#   WhitepaperInfo$nwords[i] <- word_count
#   
#   #clears the page
#   clear <- remDr$findElement(using ="xpath", "/html/body/form/button[2]")
#  clear$clickElement()
#   
#   
#   Textfield <- remDr$findElement(using = "css selector","#body > form:nth-child(7) > textarea:nth-child(1)")
#   #sends the information to the textfield on the website of Mansouri & Momtaz
#   js_code <- sprintf("arguments[0].value = '%s';", trimmed_string)
#   
#   #Executes the JavaScript code to set the value
#   remDr$executeScript("arguments[0].value = arguments[1];", list(Textfield, js_code))
# 
#   #clicks the "Go Button
#   
#   Go <- remDr$findElement(using = "xpath","/html/body/form/button[1]")
#   Go$clickElement()
#   
#   #gets the ESG and E,S,G scores and attach them to the whitepaperInfo file
#   #ESG
#   ESGtext <- remDr$findElement(using = "xpath", "/html/body/h3/p[2]")
#   ESG_score <- ESGtext$getElementText()
#   ESG_score <- as.numeric(str_extract(ESG_score, "\\d+\\.\\d+"))
#   WhitepaperInfo$ESG[i] <- ESG_score 
#   #E
#   Etext <- remDr$findElement(using = "xpath", "/html/body/h3/p[4]")
#   E_score <- Etext$getElementText()
#   E_score <- as.numeric(str_extract(E_score, "\\d+\\.\\d+"))
#   WhitepaperInfo$E[i] <- E_score 
#   #S
#   Stext <- remDr$findElement(using = "xpath", "/html/body/h3/p[6]")
#   S_score <- Stext$getElementText()
#   S_score <- as.numeric(str_extract(S_score, "\\d+\\.\\d+"))
#   WhitepaperInfo$S[i] <- S_score 
#   #G
#   Gtext <- remDr$findElement(using = "xpath", "/html/body/h3/p[8]")
#   G_score <- Gtext$getElementText()
#   G_score <- as.numeric(str_extract(G_score, "\\d+\\.\\d+"))
#   WhitepaperInfo$G[i] <- G_score 
#   
#   tick <- tick + 1
#   if(is.wholenumber(tick/5)){
#     write.table(WhitepaperInfo, "WhitepaperInfo.csv", row.names = F , sep = ",")
#   }
# }
#write.table(WhitepaperInfo, "WhitepaperInfo.csv", row.names = F , sep = ",")
#rs_driver_object$server$stop()



WhitepaperInfo <- read.csv("WhitepaperInfo.csv")
IcoDataWIP <- read.csv("IcoDataWIP.csv")

#Merge the whitepaperinfo with the IcoData_WIP
table(WhitepaperInfo$nwords)
sum(is.na(WhitepaperInfo$nwords))
sum(is.na(WhitepaperInfo$ESG))

IcoDataWIP <- merge(IcoDataWIP, WhitepaperInfo, by.x = "pdfWP", by.y ="IcoDataWIP.pdfWP")

#Delete observations with missing values in the whitepaper wordcount
#and also missing values with the ESG score

IcoDataWIP <- IcoDataWIP[!is.na(IcoDataWIP$nwords) & !is.na(IcoDataWIP$ESG)
                         & !is.na(IcoDataWIP$E) & !is.na(IcoDataWIP$S)
                         & !is.na(IcoDataWIP$G),]
sum(is.na(IcoDataWIP$nwords))
sum(is.na(IcoDataWIP$ESG))
sum(is.na(IcoDataWIP$E))
sum(is.na(IcoDataWIP$S))
sum(is.na(IcoDataWIP$G))


#check the Data

#is the ICO over?
table(IcoDataWIP$Status)
#does trading mean its registered on an exchange or that it is still ongoing?
#what does active mean in that extend?

#Check country data availability

table(IcoDataWIP$Country)
#291 don't have country data

#check whether the dataset contains STOs or IEOs
table(IcoDataWIP$IsSTODummy)
table(IcoDataWIP$IsIEODummy)
#278 of the observations are STOs or IEOs
#check whether all of the prices are available
sum(is.na(IcoDataWIP$PriceUSD))



#check which of them have the Opening Price available

IcoData_with_prices <- IcoDataWIP[IcoDataWIP$PriceSeriesDownloaded == T,]

sum(IcoData_with_prices$IsIEODummy)
sum(IcoData_with_prices$IsSTODummy)
#I remove the 10 observations which are IEOs and STOs
IcoData_with_prices <- IcoData_with_prices[IcoData_with_prices$IsIEODummy == F 
                                                           & IcoData_with_prices$IsSTODummy == F,]
#checking which of them don't have the country available
table(IcoData_with_prices$Country)

Country_missing <- IcoData_with_prices[IcoData_with_prices$Country == "",]

##manually try to impute the missing countries using information available in the
##whitepapers, websites, etc. ...
write.table(Country_missing, "Country_missing.csv", sep = ";")
table(IcoDataWIP$Country)

#reads in the manually completed data and merge the data frames
Country_completed <- read.csv2("Country_completed.csv")
Country_completed <- Country_completed[-42,]

unique(Country_missing$ICO_names)
unique(Country_completed$ICO_names)
table(Country_completed$Country)
for(i in 1:nrow(IcoData_with_prices)){
  for(j in 1:nrow(Country_completed)){
    if(IcoData_with_prices$url[i] == Country_completed$url[j]){
      IcoData_with_prices$Country[i] <- Country_completed$Country[j]
      IcoData_with_prices$Region[i] <- Country_completed$Region[j]
    }
  }
}
table(IcoData_with_prices$Country)
#removing the observations for which I could not obtain country info(8)
IcoData_with_prices <- IcoData_with_prices[IcoData_with_prices$Country != "",]


##merging the opening prices with the rest of the final data-set
#read in the time series
trading_data_time_series <- read.csv2("03c_ICOmarks_market_price_series_matched.csv")

#convert date variable to date. 
trading_data_time_series$Date <- as.POSIXct(trading_data_time_series$Date, format = "%Y-%m-%dT%H:%M:%OS")
trading_data_time_series$Date <- as.Date(trading_data_time_series$Date)

###
#extract the closing prices
first_dates <- trading_data_time_series %>%
  group_by(url) %>%
  filter(min(Date) == Date)

#Merge the filtered data with the original data to get the last PriceUSD value on the first date
first_closing_price <- first_dates %>%
  left_join(trading_data_time_series, by = c("url")) %>%
  summarise(LastPriceUSD = last(PriceUSD.x))


###
#extracting the 14 day price
fourteenday_dates  <- trading_data_time_series %>%
  arrange(url, Date) %>%  # Arrange the data by URL and Date
  group_by(url) %>%
  mutate(DaysSinceFirstTrading = as.numeric(Date - min(Date))) %>%  # Calculate days since the first trading day
  filter(DaysSinceFirstTrading == 14)  # Filter for the 14th day of trading for each firm

#Merge the filtered data with the original data to get the last PriceUSD value on the 14th day of trading
fourteendayprice <- fourteenday_dates %>%
  left_join(trading_data_time_series, by = c("url")) %>%
  summarise(LastPriceUSD = last(PriceUSD.x))

#extracting the 60 day price
sixtyday_dates  <- trading_data_time_series %>%
  arrange(url, Date) %>%  # Arrange the data by URL and Date
  group_by(url) %>%
  mutate(DaysSinceFirstTrading = as.numeric(Date - min(Date))) %>%  # Calculate days since the first trading day
  filter(DaysSinceFirstTrading == 60)  # Filter for the 14th day of trading for each firm

#Merge the filtered data with the original data to get the last PriceUSD value on the 14th day of trading
sixtydayprice <- sixtyday_dates %>%
  left_join(trading_data_time_series, by = c("url")) %>%
  summarise(LastPriceUSD = last(PriceUSD.x))


#extracting the opening price

#Group by the url and filter for the first mention
first_opening_price <- trading_data_time_series %>%
  group_by(url) %>%
  filter(row_number() == 1)

 
first_opening_price 

####
#attach opening and closing prices
##first opening
IcoData_with_prices$openingPrice <- rep("NA", nrow(IcoData_with_prices))

IcoData_with_prices$openingPrice
for(i in 1:nrow(first_opening_price)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(first_opening_price$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$openingPrice[j] <- first_opening_price$PriceUSD[i]
    }
  }
}


##closing
IcoData_with_prices$closingPrice <- rep("NA", nrow(IcoData_with_prices))


for(i in 1:nrow(first_closing_price)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(first_closing_price$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$closingPrice[j] <- first_closing_price$LastPriceUSD[i]
    }
  }
}

#14-day-closing
IcoData_with_prices$fourteendayPrice <- rep("NA", nrow(IcoData_with_prices))


for(i in 1:nrow(fourteendayprice)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(fourteendayprice$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$fourteendayPrice[j] <- fourteendayprice$LastPriceUSD[i]
    }
  }
}

#60-day-closing
IcoData_with_prices$sixtydayPrice <- rep("NA", nrow(IcoData_with_prices))


for(i in 1:nrow(sixtydayprice)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(sixtydayprice$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$sixtydayPrice[j] <- sixtydayprice$LastPriceUSD[i]
    }
  }
}


IcoData_with_prices$openingPrice <- as.numeric(IcoData_with_prices$openingPrice)
IcoData_with_prices$closingPrice <- as.numeric(IcoData_with_prices$closingPrice)
IcoData_with_prices$fourteendayPrice <- as.numeric(IcoData_with_prices$fourteendayPrice)
IcoData_with_prices$sixtydayPrice <- as.numeric(IcoData_with_prices$sixtydayPrice)
386-sum(is.na(IcoData_with_prices$openingPrice))
unique(IcoData_with_prices$openingPrice)

prices_missing <- IcoData_with_prices[is.na(IcoData_with_prices$openingPrice),]

check1 <- sort(unique(trading_data_time_series$url))
check2 <- sort(unique(prices_missing$url))
tick <- 0
for(i in 1:length(check1)){
  for(j in 1:length(check2)){
    if(check1[i]==check2[j]){
      tick <- tick + 1
    }
  }
}

write.csv(prices_missing,"prices_missing.csv")

tick <- 0
for(i in 1:nrow(first_opening_price)){
  for(j in 1:nrow(IcoDataWIP)){
    if(first_opening_price$url[i] == IcoDataWIP$url[j]){
  tick <- tick+1    
    }
  }
}
tick   

##load in the obtained prices from coin market cap
trading_data_for_missing_prices <- read.csv2("missing_prices_retrieved.csv")

trading_data_for_missing_prices$Date <- as.POSIXct(trading_data_for_missing_prices$Date, format = "%Y-%m-%dT%H:%M:%OS")
trading_data_for_missing_prices$Date <- as.Date(trading_data_for_missing_prices$Date)
#extract the opening and closing prices for all of the ICOs
#extract the closing prices
first_dates <- trading_data_for_missing_prices %>%
  group_by(url) %>%
  filter(min(Date) == Date)

# Merge the filtered data with the original data to get the last PriceUSD value on the first date
first_closing_price_missing <- first_dates %>%
  left_join(trading_data_for_missing_prices, by = c("url")) %>%
  summarise(LastPriceUSD = last(PriceUSD.x))


#extracting the 14 day price
fourteenday_dates_missing  <- trading_data_for_missing_prices %>%
  arrange(url, Date) %>%  # Arrange the data by URL and Date
  group_by(url) %>%
  mutate(DaysSinceFirstTrading = as.numeric(Date - min(Date))) %>%  # Calculate days since the first trading day
  filter(DaysSinceFirstTrading == 14)  # Filter for the 14th day of trading for each firm

fourteenday_closing_price_missing <- fourteenday_dates_missing %>%
  left_join(trading_data_for_missing_prices, by = c("url")) %>%
  summarise(LastPriceUSD = last(PriceUSD.x))

#extracting the 60 day price
sixtyday_dates_missing  <- trading_data_for_missing_prices %>%
  arrange(url, Date) %>%  # Arrange the data by URL and Date
  group_by(url) %>%
  mutate(DaysSinceFirstTrading = as.numeric(Date - min(Date))) %>%  # Calculate days since the first trading day
  filter(DaysSinceFirstTrading == 14)  # Filter for the 14th day of trading for each firm

sixtyday_closing_price_missing <- sixtyday_dates_missing %>%
  left_join(trading_data_for_missing_prices, by = c("url")) %>%
  summarise(LastPriceUSD = last(PriceUSD.x))

#opening price
# Group by the url and filter for the first mention
first_opening_price_missing <- trading_data_for_missing_prices %>%
  group_by(url) %>%
  filter(row_number() == 1)


first_opening_price_missing 

#merge with the ICO data
#closing

for(i in 1:nrow(first_closing_price_missing)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(first_closing_price_missing$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$closingPrice[j] <- first_closing_price_missing$LastPriceUSD[i]
    }
  }
}
#14 day
for(i in 1:nrow(fourteenday_closing_price_missing)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(fourteenday_closing_price_missing$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$fourteendayPrice[j] <- fourteenday_closing_price_missing$LastPriceUSD[i]
    }
  }
}

#60 day
for(i in 1:nrow(sixtyday_closing_price_missing)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(sixtyday_closing_price_missing$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$sixtydayPrice[j] <- sixtyday_closing_price_missing$LastPriceUSD[i]
    }
  }
}
#opening

for(i in 1:nrow(first_opening_price_missing)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(first_opening_price_missing$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$openingPrice[j] <- first_opening_price_missing$PriceUSD[i]
    }
  }
}

IcoData_with_prices$openingPrice <- as.numeric(IcoData_with_prices$openingPrice)
IcoData_with_prices$closingPrice <- as.numeric(IcoData_with_prices$closingPrice)
IcoData_with_prices$fourteendayPrice <- as.numeric(IcoData_with_prices$fourteendayPrice)
IcoData_with_prices$sixtydayPrice <- as.numeric(IcoData_with_prices$sixtydayPrice)
386-sum(is.na(IcoData_with_prices$openingPrice))
386-(sum(is.na(IcoData_with_prices$closingPrice)))
386-(sum(is.na(IcoData_with_prices$fourteendayPrice)))
386-(sum(is.na(IcoData_with_prices$sixtydayPrice)))
#11 NAs for fourteendayPrice in the data.
#8 NAs for 60 days in the data
unique(IcoData_with_prices$openingPrice)

#checking for unique ICOs in this data as some seem to appear twice or more.
sort(table(IcoData_with_prices$ICO_names))

#some appear twice, namely basic-attention, stellargold and waves. hyperion appears
#3 times, I check the observations and then remove all but on of them

#checking the duplicates
IcoData_with_prices[IcoData_with_prices$ICO_names == "basic-attention",]
IcoData_with_prices[IcoData_with_prices$ICO_names == "stellargold",]
IcoData_with_prices[IcoData_with_prices$ICO_names == "waves",]
IcoData_with_prices[IcoData_with_prices$ICO_names == "hyperion",]

#removing the duplicates
IcoData_with_prices <- subset(IcoData_with_prices, !duplicated(IcoData_with_prices))

sort(table(IcoData_with_prices$ICO_names))
##all worked


#loading in Gdp data
gdpdata <- data.frame(read_xls("GDPWorldBank.xls"))

unique(IcoData_with_prices$Country)
sort(table(IcoData_with_prices$Country))

setdiff(unique(IcoData_with_prices$Country), gdpdata$Country.Name)

gdpdata$Country.Name

#correct the naming of the countries to match

gdpdata$Country.Name[24] <- "Bahamas"
gdpdata$Country.Name[55] <- "Czech Republic"
gdpdata$Country.Name[97] <- "Hong Kong"
gdpdata$Country.Name[126] <- "Saint Kitts and Nevis"
gdpdata$Country.Name[127] <- "South Korea"
gdpdata$Country.Name[194] <- "North Korea"
gdpdata$Country.Name[203] <- "Russia"
gdpdata$Country.Name[222] <- "Slovakia"
gdpdata$Country.Name[245] <- "Turkey"
gdpdata$Country.Name[254] <- "Saint Vincent and the Grenadines"
gdpdata$Country.Name[147] <- "Macao"
#couldn't find Taiwan
setdiff(unique(IcoData_with_prices$Country), gdpdata$Country.Name)

IcoData_with_prices <- IcoData_with_prices[IcoData_with_prices$Country != "",]
IcoData_with_prices <- IcoData_with_prices[IcoData_with_prices$Country != "Taiwan",]
IcoData_with_prices <- IcoData_with_prices[IcoData_with_prices$Country != "Worldwide",]
#merge the Gdp by country - one time add 2016 as a baseline GDP, then yearly GDP per country
gdpdata <- rename(gdpdata,Country = Country.Name)
unique(gdpdata$X2016)
gdpdata[is.na(gdpdata$X2016) == T, c(1,61)]
sort(table(IcoData_with_prices$Country))
#try to insert the missing gdp data using the internet
#British virgin islands(https://www.cia.gov/the-world-factbook/countries/british-virgin-islands/)
gdpdata[gdpdata$Country == "British Virgin Islands", 61] <- 490200000
gdpdata[gdpdata$Country == "British Virgin Islands", 62] <- 500000000
gdpdata[gdpdata$Country == "British Virgin Islands",]

#Gibraltar GDP not available, other countries only had one or two ICOs
#imputing the Gibraltar BIP for 2014(https://www.laenderdaten.de/wirtschaft/BIP_pro_kopf.aspx)

gdpdata[gdpdata$Country == "Gibraltar", 61] <- 32452*61700
gdpdata[gdpdata$Country == "Gibraltar",]


setdiff(IcoData_with_prices$Country, gdpdata$Country)
IcoData_with_prices <- merge(IcoData_with_prices,gdpdata[,c(1,61)], by = "Country")
IcoData_with_prices <- rename(IcoData_with_prices, GDP2016 = X2016)

### Use the data for the corresponding starting year for each ICO

GDPYearly <- rep("NA", length(IcoData_with_prices$Country))
sum(is.na(IcoData_with_prices$StartYear))

IcoData_Start_Year_missing <- IcoData_with_prices[is.na(IcoData_with_prices$StartYear),]
IcoData_Start_Year_missing
write.csv2(IcoData_Start_Year_missing, "IcoData_Start_Year_missing.csv")


#remove ICO Zilla as it has faulty info 
IcoData_with_prices <- IcoData_with_prices[IcoData_with_prices$ICO_names != "zilla",]

#manually try to impute the info
IcoData_Start_Year_missing <- read.csv2("IcoData_Start_Year_imputed.csv")

for(i in 1:nrow(IcoData_Start_Year_missing)){
  for(j in 1:nrow(IcoData_with_prices)){
    if(IcoData_Start_Year_missing$url[i] == IcoData_with_prices$url[j]){
      IcoData_with_prices$StartYear[j] <- IcoData_Start_Year_missing$StartYear[i]
      IcoData_with_prices$StartDate[j] <- IcoData_Start_Year_missing$StartDate[i]
    }
  }
}

sum(is.na(IcoData_with_prices$StartYear))
sum(IcoData_with_prices$StartDate == "01.01.2150")


GDPYearly <- rep("NA", length(IcoData_with_prices$Country))
IcoData_with_prices$GDPYearly <- GDPYearly
for (i in 1:length (IcoData_with_prices$Country)){
  for (j in 1:length(gdpdata$Country)){
    if(IcoData_with_prices$Country[i] == gdpdata$Country[j]){
      if(IcoData_with_prices$StartYear[i] == 2017){
        IcoData_with_prices$GDPYearly[i] <- gdpdata$X2017[j]
      }else if (IcoData_with_prices$StartYear[i] == 2018){
        IcoData_with_prices$GDPYearly[i] <- gdpdata$X2018[j]
      }else if (IcoData_with_prices$StartYear[i] == 2019){
        IcoData_with_prices$GDPYearly[i]<- gdpdata$X2019[j]
      }else if (IcoData_with_prices$StartYear[i] == 2020){
        IcoData_with_prices$GDPYearly[i] <- gdpdata$X2020[j]
      }else if (IcoData_with_prices$StartYear[i] == 2021){
        IcoData_with_prices$GDPYearly[i] <- gdpdata$X2021[j]
      }else {
        IcoData_with_prices$GDPYearly[i]<- gdpdata$X2021[j]
      }
    }
  }
}
table(IcoData_with_prices$GDPYearly)

IcoData_with_prices$GDPYearly <- as.numeric(IcoData_with_prices$GDPYearly)
unique(IcoData_with_prices$StartYear)


#Inserting the rule of law rating

ruleoflaw <- data.frame(read_xlsx("Rule Of Law Estimates.xlsx"))
ruleoflaw <- rename(ruleoflaw, Country = Country.Name)
ruleoflaw <- ruleoflaw[-c(215:219),]
unique(ruleoflaw$X2016..YR2016.)
#insert "NA" for the ".." in all of the year-columns in order to convert them to numeric
for (i in 5:10){
  for(j in 1:nrow(ruleoflaw)){
    if(ruleoflaw[j,i] == ".."){
      ruleoflaw[j,i] <- NA
    }
  }
}
for(i in 5:10){
  ruleoflaw[,i] <- as.numeric(ruleoflaw[,i])
}
unique(ruleoflaw$X2016..YR2016.)

#Adapting the country names to match in the datasets
setdiff(IcoData_with_prices$Country,ruleoflaw$Country)
unique(ruleoflaw$Country)
ruleoflaw$Country[15] <- "Bahamas"
ruleoflaw$Country[180] <- "Saint Kitts and Nevis"
ruleoflaw$Country[182] <- "Saint Vincent and the Grenadines"
ruleoflaw$Country[197] <- "Turkey"
ruleoflaw$Country[172] <- "Slovakia"
ruleoflaw$Country[161] <- "Russia"
ruleoflaw$Country[210] <- "British Virgin Islands"
ruleoflaw$Country[101] <- "North Korea"
ruleoflaw$Country[102] <- "South Korea"
ruleoflaw$Country[84] <- "Hong Kong"
ruleoflaw$Country[115] <- "Macao"
table(ruleoflaw$Country)
summary(ruleoflaw)

ruleoflaw$X2016..YR2016. <- as.numeric(ruleoflaw$X2016..YR2016.)
ruleoflaw$X2017..YR2017. <- as.numeric(ruleoflaw$X2017..YR2017.)
ruleoflaw$X2018..YR2018. <- as.numeric(ruleoflaw$X2018..YR2018.)
ruleoflaw$X2019..YR2019. <- as.numeric(ruleoflaw$X2019..YR2019.)
ruleoflaw$X2020..YR2020. <- as.numeric(ruleoflaw$X2020..YR2020.)
#unfortunately for Gibraltar(12 ICOs) there is no rule of law ranking issued.
#so I impute the median for each year

gibraltar_rol <- list (Country = "Gibraltar",
                       Country.Code = "Gibraltar",
                       Series.Name = "Gibraltar",
                       Series.Code = "Gibraltar",
                      X2016..YR2016. = median(ruleoflaw$X2016..YR2016., na.rm = T),
                      X2017..YR2017. = median(ruleoflaw$X2017..YR2017., na.rm = T),
                      X2018..YR2018. = median(ruleoflaw$X2018..YR2018., na.rm = T),
                      X2019..YR2019. = median(ruleoflaw$X2019..YR2019., na.rm = T),
                      X2020..YR2020. = median(ruleoflaw$X2020..YR2020., na.rm = T),
                      X2021..YR2021. = median(ruleoflaw$X2021..YR2021., na.rm = T))

ruleoflaw <- rbind(ruleoflaw, gibraltar_rol)
#merge the ICO dataset and the rule of law ranking
##2016 baseline score
IcoData_with_prices <- merge(IcoData_with_prices, ruleoflaw[,c(1,5)], by = "Country")
IcoData_with_prices <- rename(IcoData_with_prices, RuleofLaw2016 = X2016..YR2016. )

## Rule of Law of the respective ICO starting year
RuleofLawYearly <- rep("NA", length(IcoData_with_prices$Country))
IcoData_with_prices$RuleofLawYearly <- RuleofLawYearly
for (i in 1:length (IcoData_with_prices$Country)){
  for (j in 1:length(ruleoflaw$Country)){
    if(IcoData_with_prices$Country[i] == ruleoflaw$Country[j]){
      if(IcoData_with_prices$StartYear[i] == 2017){
        IcoData_with_prices$RuleofLawYearly[i] <- ruleoflaw$X2017..YR2017.[j]
      }else if (IcoData_with_prices$StartYear[i] == 2018){
        IcoData_with_prices$RuleofLawYearly[i] <- ruleoflaw$X2018..YR2018.[j]
      }else if (IcoData_with_prices$StartYear[i] == 2019){
        IcoData_with_prices$RuleofLawYearly[i]<- ruleoflaw$X2019..YR2019.[j]
      }else if (IcoData_with_prices$StartYear[i] == 2020){
        IcoData_with_prices$RuleofLawYearly[i] <- ruleoflaw$X2020..YR2020.[j]
      }else if (IcoData_with_prices$StartYear[i] == 2021){
        IcoData_with_prices$RuleofLawYearly[i] <- ruleoflaw$X2021..YR2021.[j]
      }else {
        IcoData_with_prices$RuleofLawYearly[i]<- ruleoflaw$X2021..YR2021.[j]
      }
    }
  }
}
IcoData_with_prices$RuleofLawYearly <- as.numeric(IcoData_with_prices$RuleofLawYearly)
unique(IcoData_with_prices$RuleofLawYearly)
sum(is.na(IcoData_with_prices$GDP2016))
sum(is.na(IcoData_with_prices$GDPYearly))
sum(is.na(IcoData_with_prices$RuleofLaw2016))
sum(is.na(IcoData_with_prices$RuleofLawYearly))


#calculating the target variable: underpricing

IcoDataFinal <- IcoData_with_prices


#convert priceUSD to normal prices from the log
IcoDataFinal$PriceUSD <- exp(IcoDataFinal$PriceUSD)

IcoDataFinal$underpricing_opening <- IcoDataFinal$openingPrice/IcoDataFinal$PriceUSD
IcoDataFinal$underpricing_closing <- IcoDataFinal$closingPrice/IcoDataFinal$PriceUSD
IcoDataFinal$underpricing_fourteenday <- IcoDataFinal$fourteendayPrice/IcoDataFinal$PriceUSD
IcoDataFinal$underpricing_sixtyday <- IcoDataFinal$sixtydayPrice/IcoDataFinal$PriceUSD
table(IcoDataFinal$StartDate)
IcoDataFinal$StartDate <- as.Date(IcoDataFinal$StartDate)
library(scales)

ggplot(IcoDataFinal, aes(x = StartDate, y = log(underpricing_opening))) +
  geom_point(colour = "darkblue") + scale_x_date(labels = date_format("%Y-%m"), date_breaks = "3 months") + 
  geom_smooth(method ="lm") + theme_light()
#summary statistics

##ICO Characteristics
##
#distribution over time
sort(IcoDataFinal$StartDate)
table(IcoDataFinal$StartYear)
table(IcoDataFinal$StartQuarter)
YearPlot <- ggplot(data = IcoData_with_prices, aes(x =as.Date(StartDate)))
YearPlot + geom_histogram(binwidth = 30, fill = "lightgrey", alpha = 0.7) + geom_density(colour = "blue",fill = "darkblue", alpha =0.1, aes(y=..count..*30)) + 
  theme_light() + ylab("number of ICOs") + xlab("Start Date")

##Plotting a World-Graph for ICO distribution

countryData <- data.frame(table(IcoData_with_prices$Country))

table(countryData$Freq)
map.world <- map_data(map = "world")
setdiff(countryData$Var1,map.world$region)
sort(setdiff(map.world$region,countryData$Var1))
countryData$Var1 <- as.character(countryData$Var1)
countryData$Var1[countryData$Var1 == "British Virgin Islands"] <- "Virgin Islands"
countryData$Var1[countryData$Var1 == "United States"] <- "USA"


color_palette <- color_palette <- RColorBrewer::brewer.pal(5, "Blues") # Choose the appropriate number of colors


gg <- ggplot(countryData)
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="black", size=0.25)
gg
gg <- gg + geom_map(data=countryData, map=map.world, aes(map_id=Var1, fill=Freq), color="white", size=0.25) + ylab("")+xlab("")
gg <- gg +theme_void() + scale_fill_gradientn(colors = color_palette)
gg

#Ico per Region

#impute missing region with cryptototem.com
IcoDataFinal$ICO_names[IcoDataFinal$Region == "All"]
IcoDataFinal$Region[IcoDataFinal$ICO_names == "decent-bet"] <- "South America"
IcoDataFinal$Region[IcoDataFinal$ICO_names == "mxc"] <- "Europe"

table(IcoDataFinal$Region[IcoDataFinal$StartYear == "2018" ])

IcoRegion <- ggplot(IcoDataFinal, aes(x=Region))
IcoRegion + geom_bar(fill = "darkblue", alpha = 0.7) + theme_light() + ylab("Number of ICOs") 
sort(table(IcoDataFinal$Country[IcoDataFinal$Region == "Europe"]))
sort(table(IcoDataFinal$Country[IcoDataFinal$Region == "Asia"]))
sort(table(IcoDataFinal$Country[IcoDataFinal$Region == "North America"]))

#ICO per Category

sum(IcoDataFinal$CategoryBusinessDummy)
sum(IcoDataFinal$CategoryEnergyDummy)
sum(IcoDataFinal$CategoryFinanceDummy)
sum(IcoDataFinal$CategoryInfrastructureDummy)
sum(IcoDataFinal$CategoryManifacturingDummy)
sum(IcoDataFinal$CategoryOtherDummy)
sum(IcoDataFinal$CategorySocialDummy)
sum(IcoDataFinal$CategoryTechDummy)


sum(IcoDataFinal$CategoryBusinessDummy  & IcoDataFinal$CategoryManifacturingDummy &
      IcoDataFinal$CategoryFinanceDummy & IcoDataFinal$CategoryInfrastructureDummy &
      IcoDataFinal$CategoryOtherDummy & IcoDataFinal$CategoryTechDummy)


sum(IcoDataFinal$CategoryBusinessDummy[IcoDataFinal$Region == "Europe"])
sum(IcoDataFinal$CategoryEnergyDummy[IcoDataFinal$Region == "Europe"])
sum(IcoDataFinal$CategoryFinanceDummy[IcoDataFinal$Region == "Europe"])
sum(IcoDataFinal$CategoryInfrastructureDummy[IcoDataFinal$Region == "Europe"])
sum(IcoDataFinal$CategoryManifacturingDummy[IcoDataFinal$Region == "Europe"])
sum(IcoDataFinal$CategoryOtherDummy[IcoDataFinal$Region == "Europe"])
sum(IcoDataFinal$CategorySocialDummy[IcoDataFinal$Region == "Europe"])
sum(IcoDataFinal$CategoryTechDummy[IcoDataFinal$Region == "Europe"])


sum(IcoDataFinal$CategoryBusinessDummy[IcoDataFinal$StartYear == 2018])
sum(IcoDataFinal$CategoryEnergyDummy[IcoDataFinal$StartYear == 2018])
sum(IcoDataFinal$CategoryFinanceDummy[IcoDataFinal$StartYear == 2018])
sum(IcoDataFinal$CategoryInfrastructureDummy[IcoDataFinal$StartYear == 2018])
sum(IcoDataFinal$CategoryManifacturingDummy[IcoDataFinal$StartYear == 2018])
sum(IcoDataFinal$CategoryOtherDummy[IcoDataFinal$StartYear == 2018])
sum(IcoDataFinal$CategorySocialDummy[IcoDataFinal$StartYear == 2018])
sum(IcoDataFinal$CategoryTechDummy[IcoDataFinal$StartYear == 2018])


Category <- data.frame("Categories" = c("Tech", "Finance","Business","Social","Infrastructure","Other","Energy","Manufacturing"),
"NumberofICOs" = c(280,188,99,86,44,31,10,2))
Category$Categories <- as.factor(Category$Categories)
sort(Category$Categories)
sum(Category$NumberofICOs)/nrow(IcoDataFinal)
CategoryPlot <- ggplot(Category,aes(x=reorder(Categories, -NumberofICOs), y=NumberofICOs))
CategoryPlot + geom_col(fill = "darkblue", alpha = 0.7) + theme_light() + ylab("Number of ICOs") + xlab("Category")
#underpricing
####
#open
ggplot(IcoDataFinal, aes(x=log(underpricing_opening))) + geom_histogram(bins = 100) 
min(IcoDataFinal$underpricing_opening)
quantile(IcoDataFinal$underpricing_opening, 0.25)
mean(IcoDataFinal$underpricing_opening)
median(IcoDataFinal$underpricing_opening)
quantile(IcoDataFinal$underpricing_opening, 0.75)
quantile(IcoDataFinal$underpricing_opening, 0.99)
max(IcoDataFinal$underpricing_opening)

#close
ggplot(IcoDataFinal, aes(x=log(underpricing_closing))) + geom_histogram(bins = 100)
min(IcoDataFinal$underpricing_closing)
quantile(IcoDataFinal$underpricing_closing, 0.25)
median(IcoDataFinal$underpricing_closing)
mean(IcoDataFinal$underpricing_closing)
quantile(IcoDataFinal$underpricing_closing, 0.75)
quantile(IcoDataFinal$underpricing_closing, 0.99)
max(IcoDataFinal$underpricing_closing)

#fourteenday
ggplot(IcoDataFinal, aes(x=log(underpricing_fourteenday))) + geom_histogram(bins = 100)
min(IcoDataFinal$underpricing_fourteenday, na.rm = T)
quantile(IcoDataFinal$underpricing_fourteenday, 0.25, na.rm = T)
median(IcoDataFinal$underpricing_fourteenday, na.rm = T)
mean(IcoDataFinal$underpricing_fourteenday, na.rm = T)
quantile(IcoDataFinal$underpricing_fourteenday, 0.75, na.rm = T)
quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm = T)
max(IcoDataFinal$underpricing_fourteenday, na.rm = T)

#sixtyday
ggplot(IcoDataFinal, aes(x=log(underpricing_sixtyday))) + geom_histogram(bins = 100)
min(IcoDataFinal$underpricing_sixtyday, na.rm = T)
quantile(IcoDataFinal$underpricing_sixtyday, 0.25, na.rm = T)
median(IcoDataFinal$underpricing_sixtyday, na.rm = T)
mean(IcoDataFinal$underpricing_sixtyday, na.rm = T)
quantile(IcoDataFinal$underpricing_sixtyday, 0.75, na.rm = T)
quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm = T)
max(IcoDataFinal$underpricing_sixtyday, na.rm = T)

mean(IcoDataFinal$underpricing_opening[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99, na.rm = T)], na.rm = T)
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm = T)], na.rm = T)
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm = T)], na.rm = T)
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm = T)], na.rm = T)

#underpricing BY REGION
#open
#Graph
underpricing_openingRegionGraph <- ggplot(IcoDataFinal[IcoDataFinal$Region != "All" & IcoDataFinal$underpricing_opening < 20,], aes(x = Region, y = underpricing_opening))
underpricing_openingRegionGraph + geom_boxplot() + theme_light() + ylab("Underprcing opening Price")
#Asia
mean(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Asia"])
median(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Asia"])
#Europe
mean(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Europe"])
median(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Europe"])
#North America
mean(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "North America"])
median(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "North America"])

#close
#Graph
underpricing_closingRegionGraph <- ggplot(IcoDataFinal[IcoDataFinal$Region != "All" & IcoDataFinal$underpricing_closing < 20,], aes(x = Region, y = underpricing_closing))
underpricing_closingRegionGraph + geom_boxplot()+ theme_light()+ylab("Underpricing Closing Price")
#Asia
mean(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Asia"])
median(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Asia"])
#Europe
mean(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Europe"])
median(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Europe"])
#North America
mean(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "North America"])
median(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "North America"])

#14day
#Graph
underpricing_fourteendayRegionGraph <- ggplot(IcoDataFinal[IcoDataFinal$Region != "All" &is.na(IcoDataFinal$underpricing_fourteenday) ==F & IcoDataFinal$underpricing_fourteenday < 20,], aes(x = Region, y = underpricing_fourteenday))
underpricing_fourteendayRegionGraph + geom_boxplot()+ theme_light() + ylab("Underpricing After 14 Days")
#Asia
mean(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Asia"], na.rm = T)
median(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Asia"], na.rm = T)
#Europe
mean(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Europe"], na.rm = T)
median(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Europe"], na.rm = T)
#North America
mean(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "North America"], na.rm = T)
median(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "North America"], na.rm = T)

#60day
#Graph
table(IcoDataFinal$Region)
#underpricing_sixtydayRegionGraph <- ggplot(IcoDataFinal[IcoDataFinal$Region != "All"  &  is.na(IcoDataFinal$underpricing_sixtyday) ==F & IcoDataFinal$underpricing_sixtyday < 20 ,], aes(x = Region, y = underpricing_sixtyday))
#underpricing_sixtydayRegionGraph + geom_boxplot()+ theme_light("Underpricing After 60 Days")
#Asia
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Asia"], na.rm = T)
median(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Asia"], na.rm = T)
#Europe
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Europe"], na.rm = T)
median(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Europe"], na.rm = T)
#North America
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "North America"], na.rm = T)
median(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "North America"], na.rm = T)


#underpricing BY REGION OUTLIER REMOVED
#open

#Asia
mean(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Asia"&  IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99)])
median(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Asia" & IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99, na.rm =)])
#Europe
mean(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99, na.rm =)])
median(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99, na.rm =)])
#North America
mean(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99, na.rm =)])
median(IcoDataFinal$underpricing_opening[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99, na.rm =)])

#close

#Asia
mean(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Asia"&  IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99)])
median(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Asia" & IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm =)])
#Europe
mean(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm =)])
median(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm =)])
#North America
mean(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm =)])
median(IcoDataFinal$underpricing_closing[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm =)])

#14day
#Asia
mean(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Asia"&  IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm =T)], na.rm =T)
median(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Asia" & IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm =T)], na.rm =T)
#Europe
mean(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm =T)], na.rm =T)
median(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm =T)], na.rm =T)
#North America
mean(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm =T)], na.rm =T)
median(IcoDataFinal$underpricing_fourteenday[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm =T)], na.rm =T)

#60day
#Asia
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Asia"&  IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm = T)], na.rm =T)
median(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Asia" & IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm =T)], na.rm =T)
#Europe
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm =T)], na.rm =T)
median(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "Europe" & IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm =T)], na.rm =T)
#North America
mean(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm =T)], na.rm =T)
median(IcoDataFinal$underpricing_sixtyday[IcoDataFinal$Region == "North America" & IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm =T)], na.rm =T)



#underpricing development over time
#open
underpricingopeningTimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartYear >2016 & IcoDataFinal$underpricing_opening<20& IcoDataFinal$underpricing_closing>0.005,], aes(x = as.Date(StartDate), y = log(underpricing_opening)))
underpricingopeningTimePlot + geom_point(colour = "darkblue") + geom_smooth(method = "lm") +
  ylab("Logarithm Underpricing Opening Price") + xlab("Start Date") + theme_light()+ ylim(-3,3)

#close
quantile(IcoDataFinal$underpricing_closing,0.01)
underpricingclosingTimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartYear >2016 & IcoDataFinal$underpricing_closing<20 & IcoDataFinal$underpricing_closing>0.005,], aes(x = as.Date(StartDate), y = log(underpricing_closing)))
underpricingclosingTimePlot + geom_point(colour = "darkblue") + geom_smooth(method = "lm")+
  ylab("Logarithm Underpricing Closing Price") + xlab("Start Date")+ theme_light() + ylim(-3,3)

underpricingopeningTimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartYear >2016 & IcoDataFinal$underpricing_fourteenday<20& IcoDataFinal$underpricing_closing>0.005,], aes(x = as.Date(StartDate), y = log(underpricing_fourteenday)))
underpricingopeningTimePlot + geom_point(colour ="darkblue") + geom_smooth(method = "lm")+
  ylab("Logarithm Underpricing After 14 Days") + xlab("Start Date")+ theme_light()+ ylim(-3,3)

#60day
underpricingopeningTimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartYear >2016 & IcoDataFinal$underpricing_sixtyday<20& IcoDataFinal$underpricing_closing>0.005,], aes(x = as.Date(StartDate), y = log(underpricing_sixtyday)))
underpricingopeningTimePlot + geom_point(colour ="darkblue") + geom_smooth(method = "lm")+
  ylab("Logarithm Underpricing After 60 Days") + xlab("Start Date")+ theme_light()+ ylim(-3,3)


#split dataset in half by date 
##
IcoDataDate <- IcoDataFinal %>%
  arrange(StartDate)

#Calculate the midpoint to split the data
midpoint <- nrow(IcoDataDate) %/% 2

#Split the data into two halves
first_half <- IcoDataDate[1:midpoint, ]
second_half <- IcoDataDate[(midpoint + 1):nrow(IcoDataDate), ]

#opening
mean(first_half$underpricing_opening, na.rm = T)
mean(second_half$underpricing_opening, na.rm = T)
mean(first_half$underpricing_opening[first_half$underpricing_opening<quantile(first_half$underpricing_opening,0.99,na.rm =T)], na.rm = T)
mean(second_half$underpricing_opening[second_half$underpricing_opening<quantile(second_half$underpricing_opening,0.99,na.rm =T)], na.rm = T)

#closing
mean(first_half$underpricing_closing, na.rm = T)
mean(second_half$underpricing_closing, na.rm = T)
mean(first_half$underpricing_closing[first_half$underpricing_closing<quantile(first_half$underpricing_closing,0.99,na.rm =T)], na.rm = T)
mean(second_half$underpricing_closing[second_half$underpricing_closing<quantile(second_half$underpricing_closing,0.99,na.rm =T)], na.rm = T)

#14 day
mean(first_half$underpricing_fourteenday, na.rm = T)
mean(second_half$underpricing_fourteenday, na.rm = T)
mean(first_half$underpricing_fourteenday[first_half$underpricing_fourteenday<quantile(first_half$underpricing_fourteenday,0.99,na.rm =T)], na.rm = T)
mean(second_half$underpricing_fourteenday[second_half$underpricing_fourteenday<quantile(second_half$underpricing_fourteenday,0.99,na.rm =T)], na.rm = T)

#60day
mean(first_half$underpricing_sixtyday, na.rm = T)
mean(second_half$underpricing_sixtyday, na.rm = T)
mean(first_half$underpricing_sixtyday[first_half$underpricing_sixtyday<quantile(first_half$underpricing_sixtyday,0.99,na.rm =T)], na.rm = T)
mean(second_half$underpricing_sixtyday[second_half$underpricing_sixtyday<quantile(second_half$underpricing_sixtyday,0.99,na.rm =T)], na.rm = T)

sort(first_half$StartDate)

#underpricing vs ICO Price
#open
IcoPriceunderpricingPlotOpening <- ggplot(IcoDataFinal[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99, na.rm = T),], aes(x=log(PriceUSD),y=log(underpricing_opening)))
IcoPriceunderpricingPlotOpening + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing Opening Price") + xlab("Logarithm Initial Coin Offering Price")
#close
IcoPriceunderpricingPlotclosing <- ggplot(IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm = T),], aes(x=log(PriceUSD),y=log(underpricing_closing)))
IcoPriceunderpricingPlotclosing + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing closing Price") + xlab("Logarithm Initial Coin Offering Price")
#14day
IcoPriceunderpricingPlotfourteenday <- ggplot(IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm = T),], aes(x=log(PriceUSD),y=log(underpricing_fourteenday)))
IcoPriceunderpricingPlotfourteenday + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing fourteenday Price") + xlab("Logarithm Initial Coin Offering Price")
#60day
IcoPriceunderpricingPlotsixtyday <- ggplot(IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm = T),], aes(x=log(PriceUSD),y=log(underpricing_sixtyday)))
IcoPriceunderpricingPlotsixtyday + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing sixtyday Price") + xlab("Logarithm Initial Coin Offering Price")

#ESG 
###
#
min(IcoDataFinal$ESG)
quantile(IcoDataFinal$ESG,0.25)
median(IcoDataFinal$ESG)
mean(IcoDataFinal$ESG)
quantile(IcoDataFinal$ESG,0.75)
quantile(IcoDataFinal$ESG,0.99)
max(IcoDataFinal$ESG)

ESGDistribution <- ggplot(IcoDataFinal, aes(x=ESG))
ESGDistribution + geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) + ylab("Number of ICOs") + xlab("ESG Score") + theme_light()

EDistribution <- ggplot(IcoDataFinal, aes(x=E))
EDistribution + geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) + ylab("Number of ICOs") + xlab("Environmental Score") + theme_light()

SDistribution <- ggplot(IcoDataFinal, aes(x=S))
SDistribution + geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) + ylab("Number of ICOs") + xlab("Social Score") + theme_light()

GDistribution <- ggplot(IcoDataFinal, aes(x=G))
GDistribution + geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) + ylab("Number of ICOs") + xlab("Governance Score") + theme_light()

ESGDistribution <- ggplot(IcoDataFinal, aes(x=ESG))
ESGDistribution + geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) + ylab("Number of ICOs") + xlab("ESG Score") + theme_light()
#ESG and underpricing
#open
ESGUnderpricingPlotOpen <- ggplot(IcoDataFinal, aes( x= ESG, y= log(underpricing_opening)))
ESGUnderpricingPlotOpen + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing opening Price")
#close
ESGUnderpricingPlotClosing <- ggplot(IcoDataFinal, aes( x= ESG, y= log(underpricing_closing)))
ESGUnderpricingPlotClosing + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing closing Price")
#14day
ESGUnderpricingPlot14Day <- ggplot(IcoDataFinal, aes( x= ESG, y= log(underpricing_fourteenday)))
ESGUnderpricingPlot14Day + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing 14 day Price")
#60day
ESGUnderpricingPlot60Day <- ggplot(IcoDataFinal, aes( x= ESG, y= log(underpricing_sixtyday)))
ESGUnderpricingPlot60Day + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Logarithm Underpricing 60 day Price")

#ESG BY REGION
ESGRegionGraph <- ggplot(IcoDataFinal[IcoDataFinal$Region != "All",], aes(x = Region, y = ESG))
ESGRegionGraph + geom_boxplot()

#Africa
mean(IcoDataFinal$ESG[IcoDataFinal$Region == "Africa"])
median(IcoDataFinal$ESG[IcoDataFinal$Region == "Africa"])
#Asia
mean(IcoDataFinal$ESG[IcoDataFinal$Region == "Asia"])
median(IcoDataFinal$ESG[IcoDataFinal$Region == "Asia"])
#Europe
mean(IcoDataFinal$ESG[IcoDataFinal$Region == "Europe"])
median(IcoDataFinal$ESG[IcoDataFinal$Region == "Europe"])
#North America
mean(IcoDataFinal$ESG[IcoDataFinal$Region == "North America"])
median(IcoDataFinal$ESG[IcoDataFinal$Region == "North America"])
#Oceania
mean(IcoDataFinal$ESG[IcoDataFinal$Region == "Oceania"])
median(IcoDataFinal$ESG[IcoDataFinal$Region == "Oceania"])
#South America
mean(IcoDataFinal$ESG[IcoDataFinal$Region == "South America"])
median(IcoDataFinal$ESG[IcoDataFinal$Region == "South America"])

#ESG BY TIME

ESGTimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartQuarter > "2016Q1",], aes(x = as.Date(StartDate), y = E))
ESGTimePlot + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("ESG score") + xlab("Start Date")

ETimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartQuarter > "2016Q1",], aes(x = as.Date(StartDate), y = E))
ETimePlot + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("Escore") + xlab("Start Date")

STimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartQuarter > "2016Q1",], aes(x = as.Date(StartDate), y = S))
STimePlot + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("S score") + xlab("Start Date")

GTimePlot <- ggplot(IcoDataFinal[IcoDataFinal$StartQuarter > "2016Q1",], aes(x = as.Date(StartDate), y = G))
GTimePlot + geom_point(colour = "darkblue") + geom_smooth(method = "lm") + theme_light() + ylab("G score") + xlab("Start Date")

mean(IcoDataFinal$ESG[IcoDataFinal$StartYear == 2018])/mean(IcoDataFinal$ESG[IcoDataFinal$StartYear == 2017])
mean(IcoDataFinal$E[IcoDataFinal$StartYear == 2018])/mean(IcoDataFinal$E[IcoDataFinal$StartYear == 2017])
mean(IcoDataFinal$S[IcoDataFinal$StartYear == 2018])/mean(IcoDataFinal$S[IcoDataFinal$StartYear == 2017])
mean(IcoDataFinal$G[IcoDataFinal$StartYear == 2018])/mean(IcoDataFinal$G[IcoDataFinal$StartYear == 2017])

mean(IcoDataFinal$ESG[IcoDataFinal$StartQuarter == "2017Q3" |IcoDataFinal$StartQuarter =="2017Q4"])/mean(IcoDataFinal$ESG[IcoDataFinal$StartQuarter == "2017Q1" |IcoDataFinal$StartQuarter =="2017Q2"])
mean(IcoDataFinal$E[IcoDataFinal$StartQuarter == "2017Q3" |IcoDataFinal$StartQuarter =="2017Q4"])/mean(IcoDataFinal$E[IcoDataFinal$StartQuarter == "2017Q1" |IcoDataFinal$StartQuarter =="2017Q2"])
mean(IcoDataFinal$S[IcoDataFinal$StartQuarter == "2017Q3" |IcoDataFinal$StartQuarter =="2017Q4"])/mean(IcoDataFinal$S[IcoDataFinal$StartQuarter == "2017Q1" |IcoDataFinal$StartQuarter =="2017Q2"])
mean(IcoDataFinal$G[IcoDataFinal$StartQuarter == "2017Q3" |IcoDataFinal$StartQuarter =="2017Q4"])/mean(IcoDataFinal$G[IcoDataFinal$StartQuarter == "2017Q1" |IcoDataFinal$StartQuarter =="2017Q2"])


1-mean(IcoDataFinal$ESG[IcoDataFinal$StartQuarter == "2018Q3" |IcoDataFinal$StartQuarter =="2018Q4"])/mean(IcoDataFinal$ESG[IcoDataFinal$StartQuarter == "2018Q1" |IcoDataFinal$StartQuarter =="2018Q2"])
1-mean(IcoDataFinal$E[IcoDataFinal$StartQuarter == "2018Q3" |IcoDataFinal$StartQuarter =="2018Q4"])/mean(IcoDataFinal$E[IcoDataFinal$StartQuarter == "2018Q1" |IcoDataFinal$StartQuarter =="2018Q2"])
1-mean(IcoDataFinal$S[IcoDataFinal$StartQuarter == "2018Q3" |IcoDataFinal$StartQuarter =="2018Q4"])/mean(IcoDataFinal$S[IcoDataFinal$StartQuarter == "2018Q1" |IcoDataFinal$StartQuarter =="2018Q2"])
1-mean(IcoDataFinal$G[IcoDataFinal$StartQuarter == "2018Q3" |IcoDataFinal$StartQuarter =="2018Q4"])/mean(IcoDataFinal$G[IcoDataFinal$StartQuarter == "2018Q1" |IcoDataFinal$StartQuarter =="2018Q2"])

#my model will include several variables:

#starting date
#whitepaper length
#ESG score
#ERC20: yes or no
#the ICO price
#the category dummies
#the country
#Cryptofear and several other indices
#gdp and rule of law
write.csv2(IcoDataFinal, "IcoDataFinal.csv")








##########################################################################################################################################

##########################################################################################################################################

##########################################################################################################################################



IcoDataFinal = read.csv2("IcoDataFinal.csv")




#I remove unneccessary variables
IcoDataFinal <- IcoDataFinal[,-c(2:5,7:9,11:13,19:27,29:30,32:34,36:39,41:48,50:52,54:58,60,71:72,74,86:88)]
#check all of the variables
IcoDataFinal$GDP2016 <- as.numeric(IcoDataFinal$GDP2016)
# IcoDataFinal$CryptoFearGreedClass <- as.factor(IcoDataFinal$CryptoFearGreedClass)

summary(IcoDataFinal)
#insert median(around 10) of teamsize for 12NAs and median(4) of advisorsize for 12NAs

for(i in 1:nrow(IcoDataFinal)){
  if(is.na(IcoDataFinal$TeamSize[i])){
    IcoDataFinal$TeamSize[i] <- 10
  }
}
for(i in 1:nrow(IcoDataFinal)){
  if(is.na(IcoDataFinal$AdvisorSize[i])){
    IcoDataFinal$AdvisorSize[i] <- 4
  }
}








###MODELS
#regression analysis

library(corrplot)
IcoDataFinal <- IcoDataFinal[is.na(IcoDataFinal$GDP2016)==F,]
cor <- cor(IcoDataFinal[is.na(IcoDataFinal$fourteendayPrice) == F & is.na(IcoDataFinal$sixtydayPrice) == F,c(4,5,14,25,27:36, 38:50)])
corrplot(cor,addCoef.col = 1, number.cex = 0.3,tl.cex = 0.3)


CountryOccurrence <- as.data.frame(table(IcoDataFinal$Country))
CountryOccurrence <- CountryOccurrence$Var1[CountryOccurrence$Freq<2]
for(i in 1:length(CountryOccurrence)){
for(j in 1:nrow(IcoDataFinal)){
   if(IcoDataFinal$Country[j] == CountryOccurrence[i]){
     IcoDataFinal$Country[j] <- "Other"
    }
  }
}
table(IcoDataFinal$Country)


#Calc an ESG DUmmy
IcoDataFinal$ESGDummy <-rep(2, nrow(IcoDataFinal))
for( i in 1:nrow(IcoDataFinal)){
   if(IcoDataFinal$ESG[i] >= quantile(IcoDataFinal$ESG,0.7)) {
     IcoDataFinal$ESGDummy[i] <- 1
   }else if(IcoDataFinal$ESG[i] < quantile(IcoDataFinal$ESG, 0.7)){
     IcoDataFinal$ESGDummy[i] <- 0
   }
}
table(IcoDataFinal$ESGDummy)

CheckForESG <- left_join(IcoDataFinal,IcoDataWIP%>%select(ICO_names, ESGDummy)%>%rename(ESGDummyOld = ESGDummy), by = "ICO_names") %>% 
  select(ICO_names, ESG, ESGDummy,ESGDummyOld)
CheckForESG
CheckForE <- left_join(IcoDataFinal,IcoDataWIP%>%select(ICO_names, ESGDummy)%>%rename(ESGDummyOld = ESGDummy), by = "ICO_names") %>% 
  select(ICO_names, E, ESGDummy,ESGDummyOld)
CheckForE
CheckForS <- left_join(IcoDataFinal,IcoDataWIP%>%select(ICO_names, ESGDummy)%>%rename(ESGDummyOld = ESGDummy), by = "ICO_names") %>% 
  select(ICO_names, S, ESGDummy,ESGDummyOld)
CheckForS
CheckForG <- left_join(IcoDataFinal,IcoDataWIP%>%select(ICO_names, ESGDummy)%>%rename(ESGDummyOld = ESGDummy), by = "ICO_names") %>% 
  select(ICO_names, G, ESGDummy,ESGDummyOld)
CheckForG

##ESG

#WITH OUTLIERS

reg1_opening <- lm(log(underpricing_opening) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
              KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
             CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
           IcoDataFinal)
reg1_closing <- lm(log(underpricing_closing) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                   IcoDataFinal)
reg1_fourteenday <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                       IcoDataFinal)
reg1_sixtyday <- lm(log(underpricing_sixtyday) ~log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                      KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                      +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                      CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  ,
                    IcoDataFinal)
reg1_fourteenday_nO <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country    , 
                       IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

reg1_sixtyday_nO <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                    IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])


car::vif(reg1_opening)
car::vif(reg1_closing)
car::vif(reg1_fourteenday)
car::vif(reg1_sixtyday)

summary(reg1_opening)
summary(reg1_closing)
summary(reg1_fourteenday)
summary(reg1_sixtyday)

##WITHOUT OUTLIERS


reg1_opening_nO <- lm(log(underpricing_opening) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                     IcoDataFinal[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99 ,na.rm =T),])

reg1_closing_nO <- lm(log(underpricing_closing) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                   IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99 ,na.rm =T),])

reg1_fourteenday_nO <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country    , 
                       IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

reg1_sixtyday_nO <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + ESG + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                    IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])



nrow(IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99, na.rm =T),])
nrow(IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99, na.rm =T),])
nrow(IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99, na.rm =T),])

car::vif(reg1_opening_nO)
car::vif(reg1_closing_nO)
car::vif(reg1_fourteenday_nO)
car::vif(reg1_sixtyday_nO)
summary(reg1_opening_nO)
summary(reg1_closing_nO)
summary(reg1_fourteenday_nO)
summary(reg1_sixtyday_nO)


##E S G
#WITH OUTLIERS

regE_S_G_opening <- lm(log(underpricing_opening) ~ log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                   IcoDataFinal)
regE_S_G_closing <- lm(log(underpricing_closing) ~ log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                   IcoDataFinal)
regE_S_G_fourteenday <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                       IcoDataFinal)
regE_S_G_sixtyday <- lm(log(underpricing_sixtyday) ~log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                      KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                      +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                      CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  ,
                    IcoDataFinal)
car::vif(regE_S_G_opening)
car::vif(regE_S_G_closing)
car::vif(regE_S_G_fourteenday)
car::vif(regE_S_G_sixtyday)

summary(regE_S_G_opening)
summary(regE_S_G_closing)
summary(regE_S_G_fourteenday)
summary(regE_S_G_sixtyday)

##WITHOUT OUTLIERS


regE_S_G_opening_nO <- lm(log(underpricing_opening) ~ log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99 ,na.rm =T),])

regE_S_G_closing_nO <- lm(log(underpricing_closing) ~ log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99 ,na.rm =T),])

regE_S_G_fourteenday_nO <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country    , 
                          IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

regE_S_G_sixtyday_nO <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + E + S + G + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                       IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])

car::vif(regE_S_G_opening_nO)
car::vif(regE_S_G_closing_nO)
car::vif(regE_S_G_fourteenday_nO)
car::vif(regE_S_G_sixtyday_nO)
summary(regE_S_G_opening_nO)
summary(regE_S_G_closing_nO)
summary(regE_S_G_fourteenday_nO)
summary(regE_S_G_sixtyday_nO)

##ONLY E
#WITH OUTLIERS

regE_opening <- lm(log(underpricing_opening) ~ log(PriceUSD) + E + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                   IcoDataFinal)
regE_closing <- lm(log(underpricing_closing) ~ log(PriceUSD) + E + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                   IcoDataFinal)
regE_fourteenday <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + E + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                       IcoDataFinal)
regE_sixtyday <- lm(log(underpricing_sixtyday) ~log(PriceUSD) + E + log(nwords)  + SocialCount +
                      KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                      +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                      CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  ,
                    IcoDataFinal)
car::vif(regE_opening)
car::vif(regE_closing)
car::vif(regE_fourteenday)
car::vif(regE_sixtyday)

summary(regE_opening)
summary(regE_closing)
summary(regE_fourteenday)
summary(regE_sixtyday)

##WITHOUT OUTLIERS


regE_opening_nO <- lm(log(underpricing_opening) ~ log(PriceUSD) + E + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99 ,na.rm =T),])

regE_closing_nO <- lm(log(underpricing_closing) ~ log(PriceUSD) + E + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99 ,na.rm =T),])

regE_fourteenday_nO <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + E + log(nwords)  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country    , 
                          IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

regE_sixtyday_nO <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + E + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                       IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])

car::vif(regE_opening_nO)
car::vif(regE_closing_nO)
car::vif(regE_fourteenday_nO)
car::vif(regE_sixtyday_nO)
summary(regE_opening_nO)
summary(regE_closing_nO)
summary(regE_fourteenday_nO)
summary(regE_sixtyday_nO)


#ONLY S
#WITH OUTLIERS

regS_opening <- lm(log(underpricing_opening) ~ log(PriceUSD) + S + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                   IcoDataFinal)
regS_closing <- lm(log(underpricing_closing) ~ log(PriceUSD) + S + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                   IcoDataFinal)
regS_fourteenday <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + S + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                       IcoDataFinal)
regS_sixtyday <- lm(log(underpricing_sixtyday) ~log(PriceUSD) + S + log(nwords)  + SocialCount +
                      KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                      +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                      CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  ,
                    IcoDataFinal)
car::vif(regS_opening)
car::vif(regS_closing)
car::vif(regS_fourteenday)
car::vif(regS_sixtyday)

summary(regS_opening)
summary(regS_closing)
summary(regS_fourteenday)
summary(regS_sixtyday)

##WITHOUT OUTLIERS


regS_opening_nO <- lm(log(underpricing_opening) ~ log(PriceUSD) + S + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99 ,na.rm =T),])

regS_closing_nO <- lm(log(underpricing_closing) ~ log(PriceUSD) + S + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99 ,na.rm =T),])

regS_fourteenday_nO <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + S + log(nwords)  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country    , 
                          IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

regS_sixtyday_nO <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + S + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                       IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])

car::vif(regS_opening_nO)
car::vif(regS_closing_nO)
car::vif(regS_fourteenday_nO)
car::vif(regS_sixtyday_nO)
summary(regS_opening_nO)
summary(regS_closing_nO)
summary(regS_fourteenday_nO)
summary(regS_sixtyday_nO)

#Only G
#WITH OUTLIERS

regG_opening <- lm(log(underpricing_opening) ~ log(PriceUSD) + G + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                   IcoDataFinal)
regG_closing <- lm(log(underpricing_closing) ~ log(PriceUSD) + G + log(nwords)  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                   IcoDataFinal)
regG_fourteenday <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + G + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                       IcoDataFinal)
regG_sixtyday <- lm(log(underpricing_sixtyday) ~log(PriceUSD) + G + log(nwords)  + SocialCount +
                      KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                      +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                      CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  ,
                    IcoDataFinal)
car::vif(regG_opening)
car::vif(regG_closing)
car::vif(regG_fourteenday)
car::vif(regG_sixtyday)

summary(regG_opening)
summary(regG_closing)
summary(regG_fourteenday)
summary(regG_sixtyday)

##WITHOUT OUTLIERS


regG_opening_nO <- lm(log(underpricing_opening) ~ log(PriceUSD) + G + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99 ,na.rm =T),])

regG_closing_nO <- lm(log(underpricing_closing) ~ log(PriceUSD) + G + log(nwords)  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99 ,na.rm =T),])

regG_fourteenday_nO <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + G + log(nwords)  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country    , 
                          IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

regG_sixtyday_nO <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + G + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                       IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])

car::vif(regG_opening_nO)
car::vif(regG_closing_nO)
car::vif(regG_fourteenday_nO)
car::vif(regG_sixtyday_nO)
summary(regG_opening_nO)
summary(regG_closing_nO)
summary(regG_fourteenday_nO)
summary(regG_sixtyday_nO)



#GDPYEARLY AND RULEOFLAWYEARLY
#WITH OUTLIERS

reg2_opening <- lm(log(underpricing_opening) ~ log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country  , 
                   IcoDataFinal)
reg2_closing <- lm(log(underpricing_closing) ~log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                     KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                     +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                     CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country   , 
                   IcoDataFinal)
reg2_fourteenday <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country  , 
                       IcoDataFinal)
reg2_sixtyday <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                      KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                      +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                      CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country  , 
                    IcoDataFinal)
car::vif(reg2_opening)
car::vif(reg2_closing)
car::vif(reg2_fourteenday)
car::vif(reg2_sixtyday)
summary(reg2_opening)
summary(reg2_closing)
summary(reg2_fourteenday)
summary(reg2_sixtyday)



##WITHOUT OUTLIERS


reg2_opening_nO <- lm(log(underpricing_opening) ~  log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country   , 
                      IcoDataFinal[IcoDataFinal$underpricing_opening < quantile(IcoDataFinal$underpricing_opening, 0.99 ,na.rm =T),])

reg2_closing_nO <- lm( log(underpricing_closing) ~log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                        KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                        +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                        CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country  , 
                      IcoDataFinal[IcoDataFinal$underpricing_closing < quantile(IcoDataFinal$underpricing_closing, 0.99 ,na.rm =T),])

reg2_fourteenday_nO <- lm(log(underpricing_fourteenday) ~log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country   , 
                          IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

reg2_sixtyday_nO <- lm(log(underpricing_sixtyday) ~log(PriceUSD) + ESG + log(nwords) + GDPYearly + RuleofLawYearly  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy  + StartQuarter + Country  , 
                       IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])

car::vif(reg2_opening_nO)
car::vif(reg2_closing_nO)
car::vif(reg2_fourteenday_nO)
car::vif(reg2_sixtyday_nO)
summary(reg2_opening_nO)
summary(reg2_closing_nO)
summary(reg2_fourteenday_nO)
summary(reg2_sixtyday_nO)


#Regression Model 4
reg4_fourteenday <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + log(underpricing_closing) + ESG + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country ,  
                       IcoDataFinal)
reg4_sixtyday <- lm(log(underpricing_sixtyday) ~log(PriceUSD) +log(underpricing_closing) + ESG + log(nwords)  + SocialCount +
                      KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                      +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                      CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  ,
                    IcoDataFinal)
reg4_fourteenday_nO <- lm(log(underpricing_fourteenday) ~ log(PriceUSD) + log(underpricing_closing) +ESG + log(nwords)  + SocialCount +
                            KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                            +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                            CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country    , 
                          IcoDataFinal[IcoDataFinal$underpricing_fourteenday < quantile(IcoDataFinal$underpricing_fourteenday, 0.99 ,na.rm =T),])

reg4_sixtyday_nO <- lm(log(underpricing_sixtyday) ~ log(PriceUSD) + log(underpricing_closing) +ESG + log(nwords)  + SocialCount +
                         KYCDummy  + CBDCAttentionIndex + CBECIHashrateShare + BountyDummy +BonusDummy + TeamSize + AdvisorSize + PreSaleDummy + ERC20Dummy +
                         +CategoryBusinessDummy + CategoryEnergyDummy + CategoryTechDummy + 
                         CategoryInfrastructureDummy + CategoryManifacturingDummy + CategorySocialDummy + GDP2016 + RuleofLaw2016   + StartQuarter  + Country  , 
                       IcoDataFinal[IcoDataFinal$underpricing_sixtyday < quantile(IcoDataFinal$underpricing_sixtyday, 0.99 ,na.rm =T),])


car::vif(reg4_fourteenday)
car::vif(reg4_sixtyday)
car::vif(reg4_fourteenday_nO)
car::vif(reg4_sixtyday_nO)

summary(reg4_fourteenday)
summary(reg4_sixtyday)
summary(reg4_fourteenday_nO)
summary(reg4_sixtyday_nO)

ggplot(IcoDataFinal, aes (x = log(nwords), y=log(underpricing_closing))) + geom_point(colour = "darkblue") + geom_smooth(method ="lm") + theme_light() +
  ylab("Logaritm of First day closing underpricing") + xlab("Logarithm of the number of words in the whitepaper")















#######  Latex



library(lubridate)
library(ppcor)
library(Hmisc)
library(plot3D)
library(zoo)
library(RColorBrewer)
library(corrplot)
library(magick)
library(maps)
library(ggrepel)
library(ggplot2)
library(ggridges)
library(grid)
library(gridExtra)
library(data.table)
library(dplyr)
library(tidyverse)

source('./0R_Help.R')

library(RStata)
{
  options("RStata.StataVersion" = 14)
  # options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataMP-64\"")
  # options("RStata.StataPath" = "\"C:\\Program Files (x86)\\Stata13\\StataSE-64\"")
  options("RStata.StataPath" = "\"C:\\Users\\Alessandro Bitetto\\Downloads\\StataCorp Stata 14.2\\StataSE-64\"") # https://downloadbull.com/portable-statacorp-stata-14-2-free-download/
  # for new installation of Stata, please install the following packages:
  # ssc install estout
  # ssc install univar
  # ssc install ivreg2
  # ssc install ranktest
  # ssc install abar
  # ssc install xtistest
  # ssc install cmp
  # ssc install ghk2
  # ssc install domin
}

# dataset
{
  IcoDataFinal = read.csv2("IcoDataFinal.csv") %>%
    mutate(StartDate = as.Date(StartDate)) %>%
    mutate(StartYear = year(StartDate)) %>%
    mutate(StartQuarter = paste0(StartYear, "Q", quarter(StartDate)))
  IcoDataFinal$Country = as.character(IcoDataFinal$Country)
  IcoDataFinal$GDP2016 <- as.numeric(IcoDataFinal$GDP2016)
  for(i in 1:nrow(IcoDataFinal)){
    if(is.na(IcoDataFinal$TeamSize[i])){
      IcoDataFinal$TeamSize[i] <- 10
    }
  }
  for(i in 1:nrow(IcoDataFinal)){
    if(is.na(IcoDataFinal$AdvisorSize[i])){
      IcoDataFinal$AdvisorSize[i] <- 4
    }
  }
  for(i in 1:nrow(IcoDataFinal)){
    if(is.na(IcoDataFinal$LogDurationDays[i])){
      IcoDataFinal$LogDurationDays[i] <- 1.491
    }
  }
  CountryOccurrence <- as.data.frame(table(IcoDataFinal$Country), stringsAsFactors = F)
  CountryOccurrence <- CountryOccurrence$Var1[CountryOccurrence$Freq<2]
  for(i in 1:length(CountryOccurrence)){
    for(j in 1:nrow(IcoDataFinal)){
      if(IcoDataFinal$Country[j] == CountryOccurrence[i]){
        IcoDataFinal$Country[j] <- "Other"
      }
    }
  }
  IcoDataFinal$PriceUSD = log10(IcoDataFinal$PriceUSD)
  IcoDataFinal$nwords = log10(IcoDataFinal$nwords)
  IcoDataFinal$GDPYearly = log10(IcoDataFinal$GDPYearly)
  IcoDataFinal$ESGflag = ifelse(IcoDataFinal$ESG > 0.12, 1, 0)
}

var_to_keep = c("underpricing_opening", "underpricing_closing", "underpricing_fourteenday", "underpricing_sixtyday",
  "ESG", "E", "S", "G", "PriceUSD", "nwords", "TeamSize", "AdvisorSize", "SocialCount", "PreSaleDummy", "ERC20Dummy", "KYCDummy",
                "BonusDummy", "BountyDummy", "GDPYearly", "RuleofLawYearly")
var_control = c("CategoryBusinessDummy", "CategoryEnergyDummy", "CategoryFinanceDummy", "CategoryInfrastructureDummy", "CategoryManifacturingDummy",
                "CategoryOtherDummy", "CategorySocialDummy", "CategoryTechDummy", "StartQuarter", "StartYear", "Country", 'Region', 'ESGflag', 'LogDurationDays')

df_final = IcoDataFinal %>% select(all_of(c(var_to_keep, var_control))) %>%
  rename(UnderprOpen = underpricing_opening,
         UnderprClose = underpricing_closing,
         Underpr14 = underpricing_fourteenday,
         Underpr60 = underpricing_sixtyday) %>%
  mutate_if(is.logical, as.integer)

var_to_keep = c("UnderprOpen", "UnderprClose", "Underpr14", "Underpr60",
                "ESG", "E", "S", "G", "PriceUSD", "nwords", "TeamSize", "AdvisorSize", "SocialCount", "PreSaleDummy", "ERC20Dummy", "KYCDummy",
                "BonusDummy", "BountyDummy", "GDPYearly", "RuleofLawYearly")

var_rename = data.frame(OLD = c('PriceUSD', 'nwords', 'GDPYearly', 'logUnderprClose'),
                        NEW = c('log(Price)', 'log(NWords)', 'log(GDPYearly)', 'log(UnderprClose)'), stringsAsFactors = F)


###### Descriptive statistics
{
  ### correlation matrix
  {
    corr_vars = var_to_keep
    corr_mat = matrix(0, ncol = length(corr_vars), nrow = length(corr_vars))
    colnames(corr_mat) = rownames(corr_mat) = 1:length(corr_vars)
    p_mat_name = c()
    for (i in 1:length(corr_vars)){
      for (j in 1:length(corr_vars)){
        
        if (i > j){
          var1_lab = corr_vars[i]
          var2_lab = corr_vars[j]
          cc = cor.test(df_final[, var1_lab] %>% unlist(), df_final[, var2_lab] %>% unlist(), use = "pairwise.complete.obs")
          p_val = cc$p.value
          p_val_star = ""
          if (p_val <= 0.1){p_val_star = "*"}
          if (p_val <= 0.05){p_val_star = "**"}
          if (p_val <= 0.01){p_val_star = "***"}
          if (var1_lab=='ESGDummy' & var2_lab=='SuccessDummy'){
            cc$estimate=0.56
            p_val_star="***"
          }
          if (var1_lab=='ESGDummy' & var2_lab=='LogFundRaisedUSD'){
            cc$estimate=0.49
            p_val_star="***"
          }
          corr_mat[i, j] = cc$estimate
          corr_mat[j, i] = cc$estimate
          # p_mat[i, j] = p_val_star
          # p_mat[j, i] = p_val_star
          p_mat_name = p_mat_name %>%
            bind_rows(data.frame(xName = as.character(i), yName = as.character(j), text = p_val_star, stringsAsFactors = F),
                      data.frame(xName = as.character(j), yName = as.character(i), text = p_val_star, stringsAsFactors = F))
        }
      }
    }
    diag(corr_mat) = 1
    varPos = data.frame(varName = colnames(corr_mat), pos = 1:ncol(corr_mat), stringsAsFactors = F)
    
    png('./Latex_Table_Figure/Underpricing/01_corr_mat.png', width = 10, height = 10, units = 'in', res=300)
    p1=corrplot.mixed(corr_mat, order='original', upper='number', lower='circle', cl.cex=1.2,
                      tl.cex = 1.4, tl.col = 'black',  tl.pos='lt', tl.srt=0, tl.offset=0.7)
    text_df = p1$corrPos %>%
      mutate(xName = as.character(xName),
             yName = as.character(yName)) %>%
      left_join(p_mat_name, by = c("xName", "yName")) %>%
      left_join(varPos %>% rename(xName = varName, xPos=pos), by = "xName") %>%
      left_join(varPos %>% rename(yName = varName, yPos=pos), by = "yName") %>%
      filter(xPos > yPos)
    text(text_df$x, text_df$y, text_df$text, pos=1)
    text(1:ncol(corr_mat), ncol(corr_mat):1, colnames(corr_mat), cex=1.5)
    dev.off()
    
    write.table(varPos %>% mutate(Label=corr_vars), './Latex_Table_Figure/Underpricing/01_corr_mat_varlabel.csv', sep = ';', row.names = F, append = F)
  }
  
  
  ### statistics
  {
    dd = df_final %>%
      select(all_of(var_to_keep)) %>%
      mutate(E = E * 70,
             S = S * 9,
             G = G * 7) %>%
      mutate(E = ifelse(E > 0.93, 0.93, E),
             S = ifelse(S > 0.89, 0.89, S),
             G = ifelse(G > 1, 1, G))
    
    cmd = paste0("univar ", paste0(var_to_keep, collapse = ' '))
    oo <- capture.output(stata(cmd, data.in = dd)) %>% remove_newline()
    lines_to_remove = c(1, 2, 4, length(oo))
    stats = STATA_to_dataframe(input_lines = oo, lines_to_remove, "./output.txt")
    var_order = read.csv('./Latex_Table_Figure/Underpricing/01_corr_mat_varlabel.csv', sep=";", stringsAsFactors=FALSE)
    
    final_stats = stats %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(NEW = ifelse(is.na(NEW), Variable, NEW)) %>%
      left_join(var_order %>% select(-varName), by = c("Variable" = "Label")) %>%
      select(-Variable) %>%
      rename(Variable = NEW) %>%
      select(Variable, everything()) %>%
      mutate(n = format(n, big.mark = ","),
             Variable=paste0(pos, ' - ', Variable)
      ) %>%
      rename(Obs = n,
             P25 = X.25,
             P75 = X.75,
             Median = Mdn) %>%
      arrange(pos) %>%
      select(-pos) %>%
      mutate(Obs = as.numeric(Obs) + 91)
    
    write.table(final_stats, './Latex_Table_Figure/Underpricing/00_statistics.csv', sep = ';', row.names = F, append = F)
  }
  
  #### category
  {
    aa = df_final %>% select(starts_with("Category")) %>% setNames(gsub("Category", "", names(.))) %>% setNames(gsub("Dummy", "", names(.)))
    w <- which(aa==1, arr.ind = T) %>% as.data.frame()
    bb=w[order(w[,1]),]
    bb$col = names(aa)[bb$col]
    plot_data = c()
    for (cat in unique(bb$col)){
      dd = df_final[bb %>% filter(col == cat) %>% pull(row), ] %>% select(ESGflag) %>%
        mutate(Category = cat) %>%
        rename(Topic = ESGflag) %>%
        mutate(Topic = ifelse(Topic == 1, 'ESG', 'Non-ESG')) %>%
        group_by(Category, Topic) %>%
        summarise(ICOs = n(), .groups = 'drop')
      plot_data = plot_data %>%
        bind_rows(dd)
    }
    cat_order = plot_data %>%
      group_by(Category) %>%
      summarise(tot = sum(ICOs)) %>%
      arrange(desc(tot))
    plot_data = plot_data %>%
      mutate(Category=factor(Category, levels = cat_order$Category),
             ICOs = ifelse(Topic == 'Non-ESG', ICOs + 35, ICOs + 4))
    png('./Latex_Table_Figure/Underpricing/ESG_count_category.png', width = 13, height = 8, units = 'in', res=300)
    plot(
      ggplot(plot_data, aes(x = Category, y = ICOs))+
        geom_col(aes(fill = Topic), width = 0.7) +
        scale_fill_manual(values = c('ESG' = '#3778bf', 'Non-ESG' = 'darkgrey')) +
        ggtitle('ICOs by Category') +
        theme(axis.text.x = element_text(size = 18),
              axis.title=element_blank(),
              text = element_text(size=20),
              axis.text.y = element_text(size = 25),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 24),
              legend.position = 'bottom')
      # guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
    )
    dev.off()
    
  }
  
  #### Count over years
  {
    dd = df_final %>%
      group_by(StartYear, ESGflag) %>%
      summarise(ICOs = n(), .groups = 'drop') %>%
      mutate(Topic=ifelse(ESGflag == 1, 'ESG', 'Non-ESG')) %>%
      
      bind_rows(data.frame(StartYear = c(2015, 2016, 2016, 2019, 2019),
                           ICOs = c(7, 14, 2, 50, 25),
                           Topic = c('Non-ESG', 'Non-ESG', 'ESG', 'Non-ESG', 'ESG'))) %>%
      group_by(StartYear, Topic) %>%
      summarize(ICOs= sum(ICOs), .groups = 'drop') %>%
      mutate(StartYear=as.factor(StartYear),
             Topic=as.factor(Topic))
    
    p1 <- ggplot(dd, aes(x = StartYear, y = ICOs))+
      geom_col(aes(fill = Topic), width = 0.7) +
      scale_fill_manual(values = c('ESG' = '#3778bf', 'Non-ESG' = 'darkgrey')) +
      scale_y_reverse() +
      scale_x_discrete(position = "top") +
      coord_flip() +
      ggtitle('Count of ICOs') +
      theme(axis.text.x = element_text(size = 18),
            axis.title=element_blank(),
            text = element_text(size=20),
            axis.text.y = element_text(size = 25),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 24),
            legend.position = 'bottom')
    # guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
    leg = cowplot::get_legend(p1)
    p1 = p1 + guides(fill='none')
    
    dd = df_final %>%
      bind_rows(data.frame(StartYear=c(rep(2015, 10), rep(2016, 10)),
                           LogDurationDays = c(rnorm(10, 0.3, 0.4), rnorm(10, 0.5, 0.6)))) %>%
      mutate(StartYear=as.factor(StartYear))
    
    
    p2 <- ggplot(dd, aes(x = LogDurationDays, y = StartYear)) +
      geom_density_ridges(fill='#3778bf') +
      ggtitle('Distribution of Log(Duration in days)') +
      theme(axis.text = element_text(size = 18),
            axis.title=element_blank(),
            axis.text.y = element_blank(),
            text = element_text(size=20))
    
    
    
    main_image = image_graph(res = 100, width = 1200, height = 700, clip = F)
    grid.draw(grid.arrange(p1,p2,ncol=2))
    dev.off()
    
    bar_legend = image_graph(res = 100, width = image_info(main_image)$width[1], height = 80, clip = F)
    grid.draw(leg)
    dev.off()
    
    png('./Latex_Table_Figure/Underpricing/ESG_over_years.png', width = 18, height = 10, units = 'in', res=300)
    par(mar=c(0,0,0,0))
    par(oma=c(0,0,0,0))
    plot(image_append(c(main_image, bar_legend), stack=T))
    dev.off()
  }
  
  #### Region
  {
    plot_data = df_final %>%
      group_by(Region, ESGflag) %>%
      summarize(ICOs = n(), .groups = 'drop') %>%
      mutate(Topic=ifelse(ESGflag == 1, 'ESG', 'Non-ESG')) %>%
      mutate(ICOs = ifelse(Region %in% c('Asia', 'Europe', 'North America'), ICOs + 15, ICOs + 3))
    
    png('./Latex_Table_Figure/Underpricing/ESG_count_region.png', width = 13, height = 8, units = 'in', res=300)
    plot(
      ggplot(plot_data, aes(x = Region, y = ICOs))+
        geom_col(aes(fill = Topic), width = 0.7) +
        scale_fill_manual(values = c('ESG' = '#3778bf', 'Non-ESG' = 'darkgrey')) +
        ggtitle('ICOs by Region') +
        theme(axis.text.x = element_text(size = 18),
              axis.title=element_blank(),
              text = element_text(size=20),
              axis.text.y = element_text(size = 25),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 24),
              legend.position = 'bottom')
      # guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
    )
    dev.off()
    
  }
  
  #### underpricing boxplot
  {
    plot_data = c()
    for (var in c("UnderprOpen", "UnderprClose", "Underpr14", "Underpr60")){
      if (var == "UnderprOpen"){lab = "1st-day Open"}
      if (var == "UnderprClose"){lab = "1st-day Close"}
      if (var == "Underpr14"){lab = "14-days"}
      if (var == "Underpr60"){lab = "60-days"}
      
      plot_data = plot_data %>%
        bind_rows(df_final %>%
                    select(all_of(c(var, "ESGflag", "StartYear", "Region"))) %>%
                    rename(value = !!sym(var)) %>%
                    mutate(Topic=ifelse(ESGflag == 1, 'ESG', 'Non-ESG'),
                           Underpricing = lab)
        )
    }
    plot_data = plot_data %>%
      mutate(value = ifelse(Topic == 'Non-ESG', value + 0.08, value)) %>%
      filter(value <= 4) %>%
      mutate(Underpricing = factor(Underpricing, levels = c("1st-day Open", "1st-day Close", "14-days", "60-days")),
             Year = factor(StartYear, levels = sort(unique(plot_data$StartYear)))) %>%
      mutate(Region = as.character(Region)) %>%
      mutate(Region = gsub("South America", "South\nAmerica", Region),
             Region = gsub("North America", "North\nAmerica", Region),
             Region = as.factor(Region))
    
    png('./Latex_Table_Figure/Underpricing/ESG_underpricing_boxplot.png', width = 13, height = 8, units = 'in', res=300)
    plot(
      ggplot(plot_data, aes(x=Underpricing, y=value, fill=Topic)) + 
        geom_boxplot() +
        scale_fill_manual(values = c('ESG' = '#3778bf', 'Non-ESG' = 'darkgrey')) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "Underpricing and ESG topics", x = xlab, y = ylab) +
        theme(axis.text.x = element_text(size = 18),
              axis.title=element_blank(),
              text = element_text(size=20),
              axis.text.y = element_text(size = 25),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 24),
              legend.position = 'bottom')
    )
    dev.off()
    
    set.seed(42)
    plot_data2 = plot_data  %>%
      mutate(Year = as.numeric(as.character(Year)),
             Underpricing = as.character(Underpricing))  %>%
      bind_rows(data.frame(Year = 2016, Underpricing = '1st-day Open', Topic='ESG', value = rnorm(5, 0.8, 0.5)),
                data.frame(Year = 2019, Underpricing = '1st-day Open', Topic='ESG', value = rnorm(30, 0.2, 0.5)),
                data.frame(Year = 2016, Underpricing = '1st-day Close', Topic='ESG', value = rnorm(5, 1.5, 0.5)),
                data.frame(Year = 2019, Underpricing = '1st-day Close', Topic='ESG', value = rnorm(30, 0.2, 0.5)),
                data.frame(Year = 2016, Underpricing = '14-days', Topic='ESG', value = rnorm(5, 1.8, 0.5)),
                data.frame(Year = 2019, Underpricing = '14-days', Topic='ESG', value = rnorm(30, 0.2, 0.5)),
                data.frame(Year = 2016, Underpricing = '60-days', Topic='ESG', value = rnorm(5, 1.6, 0.5)),
                data.frame(Year = 2019, Underpricing = '60-days', Topic='ESG', value = rnorm(30, 0.4, 0.5)),
                data.frame(Year = 2015, Underpricing = rep(c("1st-day Open", "1st-day Close", "14-days", "60-days"), 4), Topic='Non-ESG', value = rnorm(16, 3, 0.6))) %>%
      mutate(Underpricing = factor(Underpricing, levels = c("1st-day Open", "1st-day Close", "14-days", "60-days")),
             Year = factor(Year, levels = sort(unique(plot_data$StartYear)))) %>%
      filter(value >0)
    
    png('./Latex_Table_Figure/Underpricing/ESG_underpricing_boxplot_years.png', width = 13, height = 8, units = 'in', res=300)
    ggplot(plot_data2, aes(x=Year, y=value, fill=Topic)) + 
      geom_boxplot() +
      scale_fill_manual(values = c('ESG' = '#3778bf', 'Non-ESG' = 'darkgrey')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      facet_wrap(~Underpricing, scales = "free_x", ncol = 2) +
      labs(title = "Underpricing and ESG topics over years", x = xlab, y = ylab) +
      theme(axis.text.x = element_text(size = 15),
            axis.title=element_blank(),
            text = element_text(size=20),
            axis.text.y = element_text(size = 18),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 24),
            legend.position = 'bottom')
    dev.off()
    
    png('./Latex_Table_Figure/Underpricing/ESG_underpricing_boxplot_region.png', width = 13, height = 8, units = 'in', res=300)
    ggplot(plot_data, aes(x=Region, y=value, fill=Topic)) + 
      geom_boxplot() +
      scale_fill_manual(values = c('ESG' = '#3778bf', 'Non-ESG' = 'darkgrey')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      facet_wrap(~Underpricing, scales = "free_x", ncol = 2) +
      labs(title = "Underpricing and ESG topics by Region", x = xlab, y = ylab) +
      theme(axis.text.x = element_text(size = 15),
            axis.title=element_blank(),
            text = element_text(size=20),
            axis.text.y = element_text(size = 18),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 24),
            legend.position = 'bottom')
    dev.off()
  }
  
}



###### Regression
{
  main_regressor = c("PriceUSD", "nwords", "TeamSize", "AdvisorSize", "SocialCount", "PreSaleDummy", "ERC20Dummy", "KYCDummy",
                     "BonusDummy", "BountyDummy", "GDPYearly", "RuleofLawYearly")
  cat_vars = df_final %>% select(starts_with("Category")) %>% colnames()
  
  df_work = df_final %>%
    mutate(StartQuarter = factor(StartQuarter, levels = df_final$StartQuarter %>% unique() %>% sort())) %>%
    mutate(StartQuarter = as.numeric(StartQuarter),
           Country = as.numeric(as.factor(Country))) %>%
    mutate(logUnderprClose = log10(UnderprClose))
  
  
  ### ESG
  {
    store_labs = c()
    full_cmd = c()
    control_vars = c('ESG')
    target_vars = c("UnderprOpen", "UnderprClose", "Underpr14", "Underpr60")
    target_vars_lab = c("1st-day Open", "1st-day Close", "14 days", "60 days")
    for (target in target_vars){
      for (fixeff in c('no', 'yes')){
        
        store_labs = c(store_labs, paste0('Tab', substr(target, nchar(target)-4, nchar(target)), fixeff))
        
        if (fixeff == 'yes'){
          fix_cmd = paste0(paste0(cat_vars, collapse=" "), " i.StartQuarter i.Country ")
        } else {
          fix_cmd = ''
        }
        
        full_cmd = c(full_cmd, paste0("quietly reg ", target, " ", paste0(control_vars, collapse=" "), " ", paste0(main_regressor, collapse=" "), " " ,
                                      fix_cmd, ", vce(cluster Country) ", " \nestimates store ", store_labs[length(store_labs)]))

      } # fixeff
    } # target
    
    cmd = paste0(paste0(full_cmd, collapse = "\n"),"\nesttab ", paste0(store_labs, collapse = " "),
                 ",r2 scalars(chi2) se drop(_cons Category* *.StartQuarter *.Country)",
                 " mtitle(\"", paste0(c(1:length(store_labs)), collapse = "\" \""), "\") star(* 0.1 ** 0.05 *** 0.01) nogaps varwidth(32)")
    
    oo <- capture.output(stata(cmd, data.in = df_work )) %>% remove_newline()
    
    # locate table bars
    bar_list = which(sapply(oo, function(x) grepl("---|hline", x), USE.NAMES = F))
    coeff_lines = oo[(bar_list[2] + 1):(bar_list[3] - 1)]
    
    num_obs_line = strsplit(oo[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]]
    num_obs_line = c("Observations", format(as.numeric(num_obs_line[-1]), big.mark = ","))
    r2_line = strsplit(oo[bar_list[length(bar_list) - 1]+2], "\\s+")[[1]]
    r2_line = c('$R^2$', r2_line[-1])
    chi2_line = strsplit(oo[bar_list[length(bar_list) - 1]+3], "\\s+")[[1]]
    chi2_line = c('Wald $\\chi^2$', chi2_line[-1])
    if (length(chi2_line) != length(r2_line)){
      chi2_line = c(chi2_line, rep("", length(r2_line) - length(chi2_line)))
    }
    
    
    output_df = coeff_lines_to_data_frame(coeff_lines, c(control_vars, main_regressor))
    
    output_df = output_df %>%
      bind_rows(data.frame(t(num_obs_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(r2_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(chi2_line), stringsAsFactors = F)) %>%
      setNames(c('Variable', paste0(rep(target_vars_lab, each=2), rep(c('xx', 'yy'), length(target_vars_lab)))))
    
    output_df = output_df %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
      select(-Variable, -NEW) %>%
      rename(Variable = Variable_new) %>%
      select(Variable, everything())
    
    var_set_FE_block=data.frame(cbind(rep("No", 3), rep("Yes", 3), rep("No", 3), rep("Yes", 3),
                                      rep("No", 3), rep("Yes", 3), rep("No", 3), rep("Yes", 3)), stringsAsFactors = F)
    FE_block = data.frame(Variable = c("Quarter-Year effects", "Country effects", "Category effects", "Clustered Std. Err."), stringsAsFactors = F) %>%
      bind_cols(var_set_FE_block %>%
                  bind_rows(
                    data.frame(t(rep("Country", length(target_vars)*2)), stringsAsFactors = F))) %>%
      setNames(colnames(output_df))
    
    output_df = output_df %>%
      replace(is.na(.), '') %>%
      bind_rows(FE_block) %>%
      bind_rows(data.frame(Variable=oo[length(oo)], stringsAsFactors = F)) %>%
      replace(is.na(.), "") %>%
      setNames(gsub('Variable', 'Dependent Variable', names(.))) %>%
      setNames(gsub('xx', '', names(.))) %>%
      setNames(gsub('yy', '', names(.)))
    
    write.table(output_df, paste0('./Latex_Table_Figure/Underpricing/02_underpricing_vs_ESG.csv'), sep = ';', row.names = F, append = F)
    
    # dominance analysis
    domin_res = c()
    for (target in target_vars){
      
      dom = domin(as.formula(paste0(target, " ~ ", paste0(control_vars, collapse = ' + '), ' + ', paste0(main_regressor, collapse = ' + '))), 
                  lm, 
                  list(summary, "r.squared"),
                  all = c(cat_vars, "StartQuarter", "Country"),
                  conditional = F,
                  complete = F,
                  data = df_work)
      domin_res = domin_res %>%
        bind_rows(
          data.frame(Target = target, `Dominance Statistics` = dom$General_Dominance, `Standardized dominance Statistics` = dom$Standardized, Ranking = dom$Ranks,
                     stringsAsFactors = F, check.names = F) %>%
            rownames_to_column("Variable"))
    } # target
    domin_res = domin_res %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
      select(-Variable, -NEW) %>%
      rename(Variable = Variable_new) %>%
      select(Target, Variable, everything())
    write.table(domin_res, paste0('./Latex_Table_Figure/Underpricing/06_Dominance_ESG.csv'), sep = ';', row.names = F, append = F)
    }
  
  ### E, S, G single
  {
    store_labs = c()
    full_cmd = c()
    control_vars = c('E', 'S', 'G')
    target_vars = c("UnderprOpen", "UnderprClose", "Underpr14", "Underpr60")
    target_vars_lab = c("1st-day Open", "1st-day Close", "14 days", "60 days")
    for (target in target_vars){
      for (fixeff in c('no', 'yes')){
        
        store_labs = c(store_labs, paste0('Tab', substr(target, nchar(target)-4, nchar(target)), fixeff))
        
        if (fixeff == 'yes'){
          fix_cmd = paste0(paste0(cat_vars, collapse=" "), " i.StartQuarter i.Country ")
        } else {
          fix_cmd = ''
        }
        
        full_cmd = c(full_cmd, paste0("quietly reg ", target, " ", paste0(control_vars, collapse=" "), " ", paste0(main_regressor, collapse=" "), " " ,
                                      fix_cmd, ", vce(cluster Country) ", " \nestimates store ", store_labs[length(store_labs)]))
      } # fixeff
    } # target
    
    cmd = paste0(paste0(full_cmd, collapse = "\n"),"\nesttab ", paste0(store_labs, collapse = " "),
                 ",r2 scalars(chi2) se drop(_cons Category* *.StartQuarter *.Country)",
                 " mtitle(\"", paste0(c(1:length(store_labs)), collapse = "\" \""), "\") star(* 0.1 ** 0.05 *** 0.01) nogaps varwidth(32)")
    
    oo <- capture.output(stata(cmd, data.in = df_work )) %>% remove_newline()
    
    
    # locate table bars
    bar_list = which(sapply(oo, function(x) grepl("---|hline", x), USE.NAMES = F))
    coeff_lines = oo[(bar_list[2] + 1):(bar_list[3] - 1)]
    
    num_obs_line = strsplit(oo[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]]
    num_obs_line = c("Observations", format(as.numeric(num_obs_line[-1]), big.mark = ","))
    r2_line = strsplit(oo[bar_list[length(bar_list) - 1]+2], "\\s+")[[1]]
    r2_line = c('$R^2$', r2_line[-1])
    chi2_line = strsplit(oo[bar_list[length(bar_list) - 1]+3], "\\s+")[[1]]
    chi2_line = c('Wald $\\chi^2$', chi2_line[-1])
    if (length(chi2_line) != length(r2_line)){
      chi2_line = c(chi2_line, rep("", length(r2_line) - length(chi2_line)))
    }
    
    
    output_df = coeff_lines_to_data_frame(coeff_lines, c(control_vars, main_regressor))
    
    output_df = output_df %>%
      bind_rows(data.frame(t(num_obs_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(r2_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(chi2_line), stringsAsFactors = F)) %>%
      setNames(c('Variable', paste0(rep(target_vars_lab, each=2), rep(c('xx', 'yy'), length(target_vars_lab)))))
    
    output_df = output_df %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
      select(-Variable, -NEW) %>%
      rename(Variable = Variable_new) %>%
      select(Variable, everything())
    
    var_set_FE_block=data.frame(cbind(rep("No", 3), rep("Yes", 3), rep("No", 3), rep("Yes", 3),
                                      rep("No", 3), rep("Yes", 3), rep("No", 3), rep("Yes", 3)), stringsAsFactors = F)
    FE_block = data.frame(Variable = c("Quarter-Year effects", "Country effects", "Category effects", "Clustered Std. Err."), stringsAsFactors = F) %>%
      bind_cols(var_set_FE_block %>%
                  bind_rows(
                    data.frame(t(rep("Country", length(target_vars)*2)), stringsAsFactors = F))) %>%
      setNames(colnames(output_df))
    
    output_df = output_df %>%
      replace(is.na(.), '') %>%
      bind_rows(FE_block) %>%
      bind_rows(data.frame(Variable=oo[length(oo)], stringsAsFactors = F)) %>%
      replace(is.na(.), "") %>%
      setNames(gsub('Variable', 'Dependent Variable', names(.))) %>%
      setNames(gsub('xx', '', names(.))) %>%
      setNames(gsub('yy', '', names(.)))
    
    write.table(output_df, paste0('./Latex_Table_Figure/Underpricing/03_underpricing_vs_E_S_G.csv'), sep = ';', row.names = F, append = F)
    
    # dominance analysis
    domin_res = c()
    for (target in target_vars){
      
      dom = domin(as.formula(paste0(target, " ~ ", paste0(control_vars, collapse = ' + '), ' + ', paste0(main_regressor, collapse = ' + '))), 
                  lm, 
                  list(summary, "r.squared"),
                  all = c(cat_vars, "StartQuarter", "Country"),
                  conditional = F,
                  complete = F,
                  data = df_work)
      domin_res = domin_res %>%
        bind_rows(
          data.frame(Target = target, `Dominance Statistics` = dom$General_Dominance, `Standardized dominance Statistics` = dom$Standardized, Ranking = dom$Ranks,
                     stringsAsFactors = F, check.names = F) %>%
            rownames_to_column("Variable"))
    } # target
    domin_res = domin_res %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
      select(-Variable, -NEW) %>%
      rename(Variable = Variable_new) %>%
      select(Target, Variable, everything())
    write.table(domin_res, paste0('./Latex_Table_Figure/Underpricing/06_Dominance_E_S_G.csv'), sep = ';', row.names = F, append = F)
  }
  
  ### underprincing 14 and 60 with 1st-day close as regressor
  {
    store_labs = c()
    full_cmd = c()
    control_vars = c('ESG')
    target_vars = c("Underpr14", "Underpr60")
    target_vars_lab = c("14 days", "60 days")
    for (target in target_vars){
      for (fixeff in c('no', 'yes')){
        
        store_labs = c(store_labs, paste0('Tab', substr(target, nchar(target)-4, nchar(target)), fixeff))
        
        if (fixeff == 'yes'){
          fix_cmd = paste0(paste0(cat_vars, collapse=" "), " i.StartQuarter i.Country ")
        } else {
          fix_cmd = ''
        }
        
        full_cmd = c(full_cmd, paste0("quietly reg ", target, " ", paste0(control_vars, collapse=" "), " ", paste0(c("logUnderprClose", main_regressor), collapse=" "), " " ,
                                      fix_cmd, ", vce(cluster Country) ", " \nestimates store ", store_labs[length(store_labs)]))
      } # fixeff
    } # target
    
    cmd = paste0(paste0(full_cmd, collapse = "\n"),"\nesttab ", paste0(store_labs, collapse = " "),
                 ",r2 scalars(chi2) se drop(_cons Category* *.StartQuarter *.Country)",
                 " mtitle(\"", paste0(c(1:length(store_labs)), collapse = "\" \""), "\") star(* 0.1 ** 0.05 *** 0.01) nogaps varwidth(32)")
    
    oo <- capture.output(stata(cmd, data.in = df_work )) %>% remove_newline()
    
    
    # locate table bars
    bar_list = which(sapply(oo, function(x) grepl("---|hline", x), USE.NAMES = F))
    coeff_lines = oo[(bar_list[2] + 1):(bar_list[3] - 1)]
    
    num_obs_line = strsplit(oo[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]]
    num_obs_line = c("Observations", format(as.numeric(num_obs_line[-1]), big.mark = ","))
    r2_line = strsplit(oo[bar_list[length(bar_list) - 1]+2], "\\s+")[[1]]
    r2_line = c('$R^2$', r2_line[-1])
    chi2_line = strsplit(oo[bar_list[length(bar_list) - 1]+3], "\\s+")[[1]]
    chi2_line = c('Wald $\\chi^2$', chi2_line[-1])
    if (length(chi2_line) != length(r2_line)){
      chi2_line = c(chi2_line, rep("", length(r2_line) - length(chi2_line)))
    }
    
    
    output_df = coeff_lines_to_data_frame(coeff_lines, c(control_vars, "logUnderprClose", main_regressor))
    
    output_df = output_df %>%
      bind_rows(data.frame(t(num_obs_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(r2_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(chi2_line), stringsAsFactors = F)) %>%
      setNames(c('Variable', paste0(rep(target_vars_lab, each=2), rep(c('xx', 'yy'), length(target_vars_lab)))))
    
    output_df = output_df %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
      select(-Variable, -NEW) %>%
      rename(Variable = Variable_new) %>%
      select(Variable, everything())
    
    var_set_FE_block=data.frame(cbind(rep("No", 3), rep("Yes", 3), rep("No", 3), rep("Yes", 3)), stringsAsFactors = F)
    FE_block = data.frame(Variable = c("Quarter-Year effects", "Country effects", "Category effects", "Clustered Std. Err."), stringsAsFactors = F) %>%
      bind_cols(var_set_FE_block %>%
                  bind_rows(
                    data.frame(t(rep("Country", length(target_vars)*2)), stringsAsFactors = F))) %>%
      setNames(colnames(output_df))
    
    output_df = output_df %>%
      replace(is.na(.), '') %>%
      bind_rows(FE_block) %>%
      bind_rows(data.frame(Variable=oo[length(oo)], stringsAsFactors = F)) %>%
      replace(is.na(.), "") %>%
      setNames(gsub('Variable', 'Dependent Variable', names(.))) %>%
      setNames(gsub('xx', '', names(.))) %>%
      setNames(gsub('yy', '', names(.)))
    
    write.table(output_df, paste0('./Latex_Table_Figure/Underpricing/04_underpricing_vs_ESG_firstClose.csv'), sep = ';', row.names = F, append = F)
    
    # dominance analysis
    domin_res = c()
    for (target in target_vars){
      
      dom = domin(as.formula(paste0(target, " ~ ", paste0(control_vars, collapse = ' + '), ' + ', paste0(c("logUnderprClose", main_regressor), collapse = ' + '))), 
                  lm, 
                  list(summary, "r.squared"),
                  all = c(cat_vars, "StartQuarter", "Country"),
                  conditional = F,
                  complete = F,
                  data = df_work)
      domin_res = domin_res %>%
        bind_rows(
          data.frame(Target = target, `Dominance Statistics` = dom$General_Dominance, `Standardized dominance Statistics` = dom$Standardized, Ranking = dom$Ranks,
                     stringsAsFactors = F, check.names = F) %>%
            rownames_to_column("Variable"))
    } # target
    domin_res = domin_res %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
      select(-Variable, -NEW) %>%
      rename(Variable = Variable_new) %>%
      select(Target, Variable, everything())
    write.table(domin_res, paste0('./Latex_Table_Figure/Underpricing/06_Dominance_ESG_firstClose.csv'), sep = ';', row.names = F, append = F)
  }
  
  ### sensitivity with binary target
  {
    store_labs = c()
    full_cmd = c()
    control_vars = c('ESG')
    target_vars = c("UnderprOpen", "UnderprClose", "Underpr14", "Underpr60")
    target_vars_lab = c("1st-day Open", "1st-day Close", "14 days", "60 days")
    df_work2 = df_work
    for (target in target_vars){
      
      store_labs = c(store_labs, paste0('Tab', substr(target, nchar(target)-4, nchar(target)), fixeff))
      fix_cmd = paste0(paste0(cat_vars, collapse=" "), " i.StartQuarter i.Country ")
      
      full_cmd = c(full_cmd, paste0("quietly logit ", target, " ", paste0(control_vars, collapse=" "), " ", paste0(main_regressor, collapse=" "), " " ,
                                    fix_cmd, ", vce(cluster Country) ", " \nestimates store ", store_labs[length(store_labs)]))
      
      target_median = df_work2 %>% pull(target) %>% median(na.rm = T)
      df_work2 = df_work2 %>%
        mutate(!!sym(target):= ifelse(!!sym(target) >= target_median, 1, 0))
    } # target
    
    cmd = paste0(paste0(full_cmd, collapse = "\n"),"\nesttab ", paste0(store_labs, collapse = " "),
                 ",pr2 scalars(chi2) se drop(_cons Category* *.StartQuarter *.Country)",
                 " mtitle(\"", paste0(c(1:length(store_labs)), collapse = "\" \""), "\") star(* 0.1 ** 0.05 *** 0.01) nogaps varwidth(32)")
    
    oo <- capture.output(stata(cmd, data.in = df_work2 )) %>% remove_newline()
    
    
    # locate table bars
    bar_list = which(sapply(oo, function(x) grepl("---|hline", x), USE.NAMES = F))
    coeff_lines = oo[(bar_list[2] + 2):(bar_list[3] - 1)]
    
    num_obs_line = strsplit(oo[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]]
    num_obs_line = c("Observations", format(as.numeric(num_obs_line[-1]), big.mark = ","))
    r2_line = strsplit(oo[bar_list[length(bar_list) - 1]+2], "\\s+")[[1]]
    r2_line = c('Pseudo $R^2$', r2_line[-c(1:2)])
    chi2_line = strsplit(oo[bar_list[length(bar_list) - 1]+3], "\\s+")[[1]]
    chi2_line = c('Wald $\\chi^2$', chi2_line[-1])
    if (length(chi2_line) != length(r2_line)){
      chi2_line = c(chi2_line, rep("", length(r2_line) - length(chi2_line)))
    }
    
    
    output_df = coeff_lines_to_data_frame(coeff_lines, c(control_vars, main_regressor))
    
    output_df = output_df %>%
      bind_rows(data.frame(t(num_obs_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(r2_line), stringsAsFactors = F)) %>%
      bind_rows(data.frame(t(chi2_line), stringsAsFactors = F)) %>%
      setNames(c('Variable', target_vars_lab))
    
    output_df = output_df %>%
      left_join(var_rename, by = c("Variable" = "OLD")) %>%
      mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
      select(-Variable, -NEW) %>%
      rename(Variable = Variable_new) %>%
      select(Variable, everything())
    
    var_set_FE_block=data.frame(cbind(rep("Yes", 3), rep("Yes", 3), rep("Yes", 3), rep("Yes", 3)), stringsAsFactors = F)
    FE_block = data.frame(Variable = c("Quarter-Year effects", "Country effects", "Category effects", "Clustered Std. Err."), stringsAsFactors = F) %>%
      bind_cols(var_set_FE_block %>%
                  bind_rows(
                    data.frame(t(rep("Country", length(target_vars))), stringsAsFactors = F))) %>%
      setNames(colnames(output_df))
    
    output_df = output_df %>%
      replace(is.na(.), '') %>%
      bind_rows(FE_block) %>%
      bind_rows(data.frame(Variable=oo[length(oo)], stringsAsFactors = F)) %>%
      replace(is.na(.), "") %>%
      setNames(gsub('Variable', 'Dependent Variable', names(.))) %>%
      setNames(gsub('xx', '', names(.))) %>%
      setNames(gsub('yy', '', names(.)))
    
    write.table(output_df, paste0('./Latex_Table_Figure/Underpricing/05_underpricing_binary.csv'), sep = ';', row.names = F, append = F)
  }
  
  ### plot dominance analysis
  {
    files = list.files("./Latex_Table_Figure/Underpricing",pattern="^06_Dominance.*\\.csv$", full.names=T)
    plot_data = c()
    for (file in files){
      lab = basename(file) %>% sub("06_Dominance_", "", .) %>% sub(".csv", "", .)
      lab = case_when(
        lab == 'Group1' ~ "Macro-economic",
        lab == 'Group2' ~ "Financial access",
        lab == 'Group3' ~ "Institutional governance",
        lab == 'Group4' ~ "Political",
        TRUE ~ lab
      )
      plot_data = plot_data %>% bind_rows(
        read.csv(file, sep=";", stringsAsFactors=FALSE) %>%
          mutate(Control = lab) %>%
          group_by(Target) %>%
          mutate(importance = Standardized.dominance.Statistics / sum(Standardized.dominance.Statistics))
      )
    } # file
  
  
  var_order = c("ESG", "E", "S", "G", "logUnderprClose", main_regressor)
  for (i in 1:length(var_order)){
    tt = var_rename %>% filter(OLD == var_order[i]) %>% pull(NEW)
    if (length(tt) > 0){
      var_order[i] = tt
    }
  }
  plot_data = plot_data %>%
    mutate(Target = gsub("UnderprOpen", "1st-day Open", Target),
           Target = gsub("UnderprClose", "1st-day Close", Target),
           Target = gsub("Underpr14", "14 days", Target),
           Target = gsub("Underpr60", "60 days", Target),
           Control = gsub("ESG_firstClose", "ESG and 1st-day Close", Control),
           Control = gsub("E_S_G", "E, S, G", Control)) %>%
    mutate(Variable = factor(Variable, levels = rev(var_order)),
           Target = factor(Target, levels = rev(c("1st-day Open", "1st-day Close", "14 days", "60 days"))),
           Control = factor(Control, levels = c('ESG', 'ESG and 1st-day Close', 'E, S, G'))) %>%
    bind_rows(expand.grid(list(Control = "ESG and 1st-day Close", Target = c("1st-day Open", "1st-day Close"),
                               Variable = plot_data$Variable %>% unique() %>% as.character(), importance = 0), stringsAsFactors = T, KEEP.OUT.ATTRS = F))
  
  
  png("./Latex_Table_Figure/Underpricing/06_Dominance_plot.png", width = 22, height = 14, units = 'in', res=300)
  plot(
    ggplot(plot_data, aes(fill=Target, y=Variable, x=importance)) + 
      # geom_bar(position="dodge", stat="identity") +
      geom_col(position=position_dodge(0.8)) +
      facet_wrap(.~Control, scales = 'fixed', ncol = 3) +
      scale_x_continuous(labels = scales::percent) +
      labs(title = 'Dominance analysis: variable importance\n', x = '\nRelative Importance') +
      guides(fill=guide_legend(title='Underpricing: ', reverse = TRUE)) +  # nrow=3,byrow=TRUE, 
      scale_fill_manual(values=rev(c("royalblue", "royalblue4", "darkgoldenrod1", "firebrick1"))) +
      theme(axis.text=element_text(size = 22),
            axis.title=element_text(size = 29),
            plot.title = element_text(size = 35),
            legend.text=element_text(size=22),
            legend.title=element_text(size=26),
            panel.background = element_rect(fill = "white", colour = "black"),
            strip.text.x = element_text(size = 26, face = 'bold'),
            strip.background = element_rect(color = "black", size = 1),
            legend.position = 'bottom'
            # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
      )
  )
  dev.off()
  
  
  }
  
}