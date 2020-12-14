
LOADING AND READING DATA

> LongitudinalData <- read.csv("MIE.csv")

> class(LongitudinalData)
[1] "data.frame"

> summary(LongitudinalData)
       id          visit           room               value         
 Min.   : 14   Min.   :0.000   Length:120877      Min.   :   2.000  
 1st Qu.: 41   1st Qu.:0.000   Class :character   1st Qu.:   2.750  
 Median : 46   Median :1.000   Mode  :character   Median :   7.875  
 Mean   : 57   Mean   :1.001                      Mean   :  17.412  
 3rd Qu.: 74   3rd Qu.:2.000                      3rd Qu.:  16.000  
 Max.   :106   Max.   :2.000                      Max.   :1775.000  
   timepoint   
 Min.   :   1  
 1st Qu.: 562  
 Median :1065  
 Mean   :1088  
 3rd Qu.:1569  
 Max.   :3075  

> lapply(LongitudinalData, class)
$id
[1] "integer"

$visit
[1] "integer"

$room
[1] "character"

$value
[1] "numeric"

$timepoint
[1] "integer"

LOADED NECESSAY PACKAGES

> library(microbenchmark)
> library(purrr)
> library(readr)
> library(magrittr)
> library(data.table)
> library(dplyr)
> library(tidyr)


METHODS FOR GENERIC QUESTIONS, SUBJECT, VISIT AND ROOM

> make_LD <- function(x){
+   structure(x, class = "LongitudinalData")
+ }

> ## generic functions
> 
> subject <- function(x,y) UseMethod("subject")
> 
> visit <- function(x,y) UseMethod("visit")
> 
> room <- function(x,y) UseMethod("room")

> ## subject methods 
> subject.LongitudinalData = function (data,idx) {
+   idx_list = which(data$id %in% idx)
+   if(length(idx_list) == 0)
+     return(NULL)
+   new_list = lapply(data, function (data) data[idx_list] )
+   structure (new_list, class = "Subject")
+ }

> print.Subject <- function(obj) {
+   print(paste0("Subject ID: ",obj[["id"]][1]))
+ }

> summary.Subject <- function(obj) {
+   data.frame(id = obj$id, visit = obj$visit, room = obj$room, value = obj$value,
+              timepoint = obj$timepoint ) %>%
+   group_by(visit, room) %>% summarise(value = mean(value)) %>% spread(room, value)
+   }

> visit.Subject <- function(obj, number){
+   idx_list = which(obj$visit %in% number)
+   if(length(idx_list) == 0)
+     return(NULL)
+   new_list = lapply(obj, function (obj) obj[idx_list] )
+   structure (new_list, class = "Visit")
+ }

> ## visit methods
> room.Visit <- function(obj,rooom){
+   idx_list = which(obj$room %in% rooom)
+   if(length(idx_list) == 0)
+     return(NULL)
+   new_list = lapply(obj, function (obj) obj[idx_list] )
+   structure(new_list, class ="Room")
+ }

> ## room methods
> print.Room <- function(obj){
+   print(paste0("ID: ",obj[["id"]][1]))
+   print(paste0("Visit: ",obj[["visit"]][1]))
+   print(paste0("Room: ",obj[["room"]][1]))
+ }

> summary.Room <- function(obj){
+   sum <- summary(obj[["value"]])
+ }

OOP_OUTPUT.TXT: A TEXT FILE CONTAINING THE OUTPUT OF RUNNING THE ABOVE INPUT CODE

> data <- read_csv("MIE.csv")

── Column specification ───────────────────────────────────────────────────────
cols(
  id = col_double(),
  visit = col_double(),
  room = col_character(),
  value = col_double(),
  timepoint = col_double()
)

> x <- make_LD(data)
> print(class(x))
[1] "LongitudinalData"

> print(x)
Longitudinal dataset with 10 subjects

> out <- subject(x, 10)
> print(out)
NULL

> out <- subject(x, 14)
> print(out)
Subject ID: 14 

> out <- subject(x, 54) %>% summary
> print(out)
ID: 54 
  visit  bedroom       den living room    office
1     0       NA        NA    2.792601 13.255475
2     1       NA 13.450946          NA  4.533921
3     2 4.193721  3.779225          NA        NA

> out <- subject(x, 14) %>% summary
> print(out)
ID: 14 
  visit   bedroom family  room living room
1     0  4.786592           NA     2.75000
2     1  3.401442     8.426549          NA
3     2 18.583635           NA    22.55069

> out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
> print(out)
ID: 44 
Visit: 0 
Room: bedroom 


> out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
> print(out)
ID: 44 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    8.0    30.0    51.0    88.8    80.0   911.0 

> out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
> print(out)
ID: 44 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.75   14.00   24.00   41.37   37.00 1607.00 