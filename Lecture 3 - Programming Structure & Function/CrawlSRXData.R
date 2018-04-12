library(rvest)

#Note that in the example here, I use ".listingDetailTitle", not ".notranslate" as I
#wrote in class. This is because even if there is no data in a page, SRX may use ".notranslate"
#to store agent data. Thus we cannot tell whether the page is end of the search.
#please try to run Get1stProperty(100), Get1stProperty(10000), Get1stProperty(100000) repectively
#to tell the difference.
Get1stProperty <- function(n){
    url <- paste("https://www.srx.com.sg/search/sale/residential?page=", n, sep = "")
    srx <- read_html(url)
    listing <- html_nodes(srx, ".listingDetailTitle")
    if(length(listing) == 0)
        return("Done")
    a<-html_children(listing[1])
    title <- html_text(a)
    return (title)
}

#Run Result(1000), Result(10000), Result(100000) and compare the results.
Result <- function(n){
   result = tryCatch(
               {
                    return (Get1stProperty(n))
               },
               warning = function(n) {
                   print(paste("Warning in crawling the ", n, "th page of SRX", sep = ""))
               },
               error = function(n) {
                 print(paste("Error in crawling the ", n, "th page of SRX", sep = ""))
                 return("Done")
               },
              finally = function(n)
             {}
          )
        return (result)
}


#To save time, I started from the 1650th page. You may change it to n<-1L to start from page 1.
n<-1650L

while(n<10000){
    property <- Result(n)
    print(property)
    
    if(property == "Done")
        break
     
    
    n<-n+1
}