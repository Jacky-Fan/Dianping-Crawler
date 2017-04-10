library(RCurl)
library(rvest)
library(XML)
library(stringr)
library(rjson)
library(dplyr)
library(scrapeR)
library(data.table)

myheader <- c(
        "User-Agent" = "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0_1 like Mac OS X; ja-jp) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A306 Safari/6531.22.7",
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language" = "en-us",
        "Connection" = "keep-alive",
        "Accept-Charset" = "GB2312,utf-8;q=0.7,*;q=0.7"
)

#抓取餐饮分类网址
url1 <- "http://www.dianping.com/search/category/8/10"
tmp <- getURL(url1, httpheader = myheader)
doc_tmp <- htmlParse(tmp, asText = T, encoding = "UTF-8")
url_tmp <- getNodeSet(doc_tmp, '//*[@id="classfy"]/a')
name_fl_list <-
        sapply(getNodeSet(doc_tmp, '//*[@id="classfy"]/a'), xmlValue)
url_tmp1 <-
        sapply(url_tmp, function(x)
                paste(capture.output(print(x)), collapse = ""))

url_tmp2 <-
        str_extract_all(url_tmp1, pattern = '<a href=".+\\d\"') %>%
        as.character() %>%
        gsub(pattern = '<a href=\"', replacement = "") %>%
        gsub(pattern = '\"', replacement = "")
dp_tmp <- rep("http://www.dianping.com", 27)
url_fl <- paste0(dp_tmp, url_tmp2, collapse = NULL)


#抓取各类餐饮下各街道网址列表
for (i in 1:27) {
        url2 <- url_fl[i]
        name_fl <- name_fl_list[i]
        
        temp <- getURL(url2, httpheader = myheader)
        doc <- htmlParse(temp, asText = T, encoding = "UTF-8")
        url_district_temp <-
                getNodeSet(doc, '//*[@id="region-nav"]/a')
        name_district <-
                sapply(getNodeSet(doc, '//*[@id="region-nav"]/a'), xmlValue)
        url_district_temp1 <-
                sapply(url_district_temp, function(x)
                        paste(capture.output(print(x)), collapse = ""))
        
        url_district_temp2 <-
                str_extract_all(url_district_temp1, pattern = '<a href=".+\\d\"') %>%
                as.character() %>%
                gsub(pattern = '<a href=\"', replacement = "") %>%
                gsub(pattern = '\"', replacement = "")
        num_dp <- length(url_district_temp2)
        dp <- rep("http://www.dianping.com", num_dp)
        url_district <-
                paste0(dp, url_district_temp2, collapse = NULL)
        
        canyin_urllist <- character()
        for (url in url_district) {
                st_temp <- getURL(url, httpheader = myheader)
                st_doc <- htmlParse(st_temp,
                                    asText = T,
                                    encoding = "UTF-8")
                st_url_temp <-
                        getNodeSet(st_doc, '//*[@id="region-nav-sub"]/a')
                st_url_temp1 <-
                        sapply(st_url_temp, function(x)
                                paste(capture.output(print(x)), collapse = ""))
                
                st_url_temp2 <-
                        str_extract_all(st_url_temp1[-1], pattern = '<a href=".+\\d\"') %>%
                        as.character() %>%
                        gsub(pattern = '<a href=\"', replacement = "") %>%
                        gsub(pattern = '\"', replacement = "")
                num <- length(st_url_temp2)
                dp1 <- rep("http://www.dianping.com", num)
                st_url <- paste0(dp1, st_url_temp2)
                canyin_urllist <- c(canyin_urllist, st_url)
                Sys.sleep(2)
        }
        write.csv(canyin_urllist, paste0(name_fl, ".csv"))
        canyin_urllist <- character()
}

#抓取店铺信息
getwd()
setwd("C:/Users/Administrator/Documents/dianping")
fread("xxx.csv")
list.files()

urladrlist <- fread("xxx.csv")$x

#构造页面数据抓取函数
getshopdata <- function(urladrlist){
        library(RCurl)
        library(rvest)
        library(XML)
        library(stringr)
        library(rjson)
        library(dplyr)
        library(dtplyr)
        library(scrapeR)
        library(data.table)
        myheader <- c(
                "User-Agent" = "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0_1 like Mac OS X; ja-jp) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A306 Safari/6531.22.7",
                "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                "Accept-Language" = "en-us",
                "Connection" = "keep-alive",
                "Accept-Charset" = "GB2312,utf-8;q=0.7,*;q=0.7"
        )
        shop_data_part <- data.table()
        
        for (url in urladrlist) {
                #取得地址排序位置
                url_rank <- which(urladrlist == url)
                txt <- getURL(url, httpheader = myheader)
                page_doc <-
                        htmlParse(txt, asText = T, encoding = "UTF-8")
                page_list <-
                        getNodeSet(page_doc, '//*[@id="top"]/div[6]/div[3]/div[1]/div[2]')
                page_doc1 <- sapply(page_list, function(x)
                        paste(capture.output(print(x)), collapse = "")) %>% gsub(pattern = "  ",replacement = "")
                page_num_list <-
                        str_extract_all(page_doc1, pattern = 'data-ga-page=\"[0-9]{1,2}\"') %>%
                        as.character() %>% str_extract_all(pattern = "[0-9]{1,2}") %>% unlist() %>% as.numeric() 
                page_max <- page_num_list[order(-page_num_list)[1]]
                
                shop_data <- data.table()
                page <- 1:page_max
                for (i in page) {
                        #构造每页链接
                        url1 <- paste0(url, "p", i)
                        
                        txt <- getURL(url1, httpheader = myheader)
                        doc <-
                                htmlParse(txt, asText = T, encoding = "UTF-8")
                        shop_list <-
                                getNodeSet(doc, '//*[@id="shop-all-list"]/ul/li')
                        doc1 <- sapply(shop_list, function(x)
                                paste(capture.output(print(x)), collapse = "")) %>% gsub(pattern = "  ",replacement = "")

                        shop_name <-
                                str_extract_all(doc1, pattern = "<h4>.{2,20}</h4>") %>%
                                as.character() %>% gsub(pattern = "<h4>", replacement = "") %>%
                                gsub(pattern = "</h4>", replacement = "")
                        
                        #店铺类别
                        shop_fl <- str_extract_all(doc1, pattern = "<span class=\"tag\">.{2,25}</span></a><em") %>%
                                unlist() %>% gsub(pattern = '<span class=\"tag\">', replacement = "") %>%
                                gsub(pattern = '</span></a><em', replacement = "")
                        #店铺所在街区
                        adr_street <- str_extract_all(doc1, pattern = "<span class=\"tag\">.{2,25}</span></a><span") %>%
                                unlist() %>% gsub(pattern = '<span class=\"tag\">', replacement = "") %>%
                                gsub(pattern = '</span></a><span', replacement = "")
                        #店铺详细地址
                        adr_num <- 
                                str_extract_all(doc1, pattern = '<span class="addr">.{1,50}</span>') %>%
                                as.character() %>% gsub(pattern = "<span class=\"addr\">", replacement = "") %>%
                                gsub(pattern = "</span>", replacement = "") 
                        
                        #人均单价
                        average_price <- 
                                str_extract_all(doc1, pattern = "人均.{1,50}</b>") %>%
                                as.character() %>% gsub(pattern = " ", replacement = "") %>%
                                gsub(pattern = "人均<b>￥", replacement = "") %>%
                                gsub(pattern = "</b>", replacement = "") 
                        #点评数
                        dianpingshu <- 
                                str_extract_all(doc1, pattern = "<b>.{2,15}点评") %>%
                                as.character() %>% gsub(pattern = "<b>", replacement = "") %>%
                                gsub(pattern = "</b>条点评", replacement = "") 
                        
                        #店铺星级
                        shop_rank <- str_extract_all(doc1, pattern = "sml-rank-stars sml-str[0-9]{1,2}") %>%
                                as.character() %>% gsub(pattern = 'sml-rank-stars sml-str', replacement = "")
                        #口味
                        kouwei <- 
                                str_extract_all(doc1, pattern = "口味.{1,10}</b>") %>%
                                as.character() %>% gsub(pattern = "口味<b>", replacement = "") %>%
                                gsub(pattern = "</b>", replacement = "") 
                        #环境
                        huanjing <- 
                                str_extract_all(doc1, pattern = "环境.{1,10}</b>") %>%
                                as.character() %>% gsub(pattern = "环境<b>", replacement = "") %>%
                                gsub(pattern = "</b>", replacement = "")
                        #服务
                        fuwu <- 
                                str_extract_all(doc1, pattern = "服务.{1,10}</b>") %>%
                                as.character() %>% gsub(pattern = "服务<b>", replacement = "") %>%
                                gsub(pattern = "</b>", replacement = "")
                        
                        #店址
                        shop_addr_ne <- str_extract(doc1, pattern = "href=\"/shop/[0-9]{1,20}") %>%
                                as.character() %>% gsub(pattern = "href=\"", replacement = "")
                        shop_addr_net <-  paste0("http://www.dianping.com", shop_addr_ne,collapse=NULL)
                        
                        #整合为数据框
                        shop_data1 <- data.table(shop_name,shop_addr_net,shop_fl,average_price,
                                                 dianpingshu,shop_rank,kouwei,huanjing,fuwu,adr_street,adr_num)
                        
                        
                        shop_data <-
                                bind_rows(shop_data, shop_data1)
                        Sys.sleep(5)
                        
                }
                shop_data_part1 <-
                        shop_data[complete.cases(shop_data[, 1]), ]
                shop_data_part <- bind_rows(shop_data_part,shop_data_part1)
                write.table(shop_data_part,paste0("shop_data_part",url_rank,".csv"))
                #打印执行进度
                #print(url_rank/length(urladrlist))
                return(shop_data_part)
                
        }
        
}

#并行爬取
library(parallel)
system.time({
        filename <- dir("C:/Users/JACKY/Documents/dianping")
        for(j in filename){
                urladrlist <- fread(j)$x
                file_rank <- which(filename == j)
                cl <- makeCluster(12)
                shop_data_part2 <- parLapply(cl,urladrlist,getshopdata)
                shop_data_total <- do.call("rbind", shop_data_part2)
                
                stopCluster(cl)
                shop_data_total <- shop_data_total[!duplicated(shop_data_total$shop_addr_net),]
                write.table(shop_data_total, paste0("shop_data_", j))
                #打印执行进度
                print(file_rank/length(filename))
        }
})


