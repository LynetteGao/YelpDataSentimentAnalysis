library(dplyr)
library(tidytext)
library(tm)
Yelp_train <- read.csv("Yelp_train.csv", comment.char="#")
yelp<-Yelp_train[,-1]
yelp$text <- as.character(yelp$text)
yelp$categories <- as.character(yelp$categories)
yelp$date <- as.Date(yelp$date)
yelp$nchar=log(yelp$nchar)
yelp$nword=log(yelp$nword)

# generate some new predictors
library(stringr)
new_words <- c("not good", "tip", "her","great","love", "gift","delicious"
               ,"experience","they","understand","give","waited","happened","favorite","again","to","best",
               "back","rib","when","beautiful","not","slow","salty","order","time","service",
               "Place","Like"
               ,"Online"
               ,"Cheese"
               ,"Fries"
               ,"Menu"
               ,"Manager"
               ,"Wisconsin","always", "recommend", "excel", "seem", "busy", "everything", "atmosphere", "enjoy", "better", "salad",
               "price","drink","definitely","definite","sweet","option","little","mean","get","fresh","seat","top","than","star","super","potato","visit","cook","expect","down","sure","taste","disaster","healthy")



new_X <- matrix(0, nrow(yelp), length(new_words))
colnames(new_X) <- new_words
colnames(new_X)[1]<-"notgood"

new_X[,1] <- str_count(yelp$text,"\\b(?i)not good\\b") 
new_X[,3] <- str_count(yelp$text,"\\b(?i)her\\b") # ignore the upper/lower case in the text
new_X[,2] <- str_count(yelp$text,"\\b(?i)tip\\b")
new_X[,4] <- str_count(yelp$text,"\\b(?i)great\\b")
new_X[,5] <- str_count(yelp$text,"\\b(?i)love\\b")
new_X[,6] <- str_count(yelp$text,"\\b(?i)gift\\b")
new_X[,7] <- str_count(yelp$text,"\\b(?i)delicious\\b")
new_X[,8] <- str_count(yelp$text,"\\b(?i)experience\\b")
new_X[,9] <- str_count(yelp$text,"\\b(?i)they\\b")
new_X[,10] <- str_count(yelp$text,"\\b(?i)understand\\b")
new_X[,11] <- str_count(yelp$text,"\\b(?i)give\\b")
new_X[,12] <- str_count(yelp$text,"\\b(?i)waited\\b")
new_X[,13] <- str_count(yelp$text,"\\b(?i)happened\\b")
new_X[,14] <- str_count(yelp$text,"\\b(?i)favorite\\b")
new_X[,15] <- str_count(yelp$text,"\\b(?i)again\\b")
new_X[,16] <- str_count(yelp$text,"\\b(?i)to\\b")
new_X[,17] <- str_count(yelp$text,"\\b(?i)best\\b")
new_X[,18] <- str_count(yelp$text,"\\b(?i)back\\b")
new_X[,19] <- str_count(yelp$text,"\\b(?i)rib\\b")
new_X[,20] <- str_count(yelp$text,"\\b(?i)when\\b")
new_X[,21] <- str_count(yelp$text,"\\b(?i)beautiful\\b")
new_X[,22] <- str_count(yelp$text,"\\b(?i)not\\b")
new_X[,23] <- str_count(yelp$text,"\\b(?i)slow\\b")
new_X[,24] <- str_count(yelp$text,"\\b(?i)salty\\b")
new_X[,25] <- str_count(yelp$text,"\\b(?i)order\\b")
new_X[,26] <- str_count(yelp$text,"\\b(?i)time\\b")
new_X[,27] <- str_count(yelp$text,"\\b(?i)service\\b")
new_X[,28] <- str_count(yelp$text,"\\b(?i)place\\b")
new_X[,29] <- str_count(yelp$text,"\\b(?i)like\\b")
new_X[,30] <- str_count(yelp$text,"\\b(?i)online\\b")
new_X[,31] <- str_count(yelp$text,"\\b(?i)cheese\\b")
new_X[,32] <- str_count(yelp$text,"\\b(?i)fries\\b")
new_X[,33] <- str_count(yelp$text,"\\b(?i)menu\\b")
new_X[,34] <- str_count(yelp$text,"\\b(?i)manager\\b")
new_X[,35] <- str_count(yelp$text,"\\b(?i)wisconsin\\b")
new_X[,36] <- str_count(yelp$text,"\\b(?i)always\\b") 
new_X[,37] <- str_count(yelp$text,"\\b(?i)recommend\\b")
new_X[,38] <- str_count(yelp$text,"\\b(?i)excel\\b")
new_X[,39] <- str_count(yelp$text,"\\b(?i)seem\\b")
new_X[,40] <- str_count(yelp$text,"\\b(?i)busy\\b")
new_X[,41] <- str_count(yelp$text,"\\b(?i)everything\\b")
new_X[,42] <- str_count(yelp$text,"\\b(?i)atmosphere\\b")
new_X[,43] <- str_count(yelp$text,"\\b(?i)enjoy\\b")
new_X[,44] <- str_count(yelp$text,"\\b(?i)better\\b")
new_X[,45] <- str_count(yelp$text,"\\b(?i)salad\\b")
new_X[,46] <- str_count(yelp$text,"\\b(?i)price\\b")
new_X[,47] <- str_count(yelp$text,"\\b(?i)drink\\b")
new_X[,48] <- str_count(yelp$text,"\\b(?i)definitely\\b")
new_X[,49] <- str_count(yelp$text,"\\b(?i)definite\\b")
new_X[,50] <- str_count(yelp$text,"\\b(?i)sweet\\b")
new_X[,51] <- str_count(yelp$text,"\\b(?i)little\\b")
new_X[,52] <- str_count(yelp$text,"\\b(?i)mean\\b")
new_X[,53] <- str_count(yelp$text,"\\b(?i)get\\b")
new_X[,54] <- str_count(yelp$text,"\\b(?i)fresh\\b")
new_X[,55] <- str_count(yelp$text,"\\b(?i)seat\\b")
new_X[,56] <- str_count(yelp$text,"\\b(?i)top\\b")
new_X[,57] <- str_count(yelp$text,"\\b(?i)than\\b")
new_X[,58] <- str_count(yelp$text,"\\b(?i)star\\b")
new_X[,59] <- str_count(yelp$text,"\\b(?i)super\\b")
new_X[,60] <- str_count(yelp$text,"\\b(?i)potato\\b")
new_X[,61] <- str_count(yelp$text,"\\b(?i)visit\\b")
new_X[,62] <- str_count(yelp$text,"\\b(?i)cook\\b")
new_X[,63] <- str_count(yelp$text,"\\b(?i)expect\\b")
new_X[,64] <- str_count(yelp$text,"\\b(?i)down\\b")
new_X[,65] <- str_count(yelp$text,"\\b(?i)sure\\b")
new_X[,66] <- str_count(yelp$text,"\\b(?i)taste\\b")
new_X[,67] <- str_count(yelp$text,"\\b(?i)disaster\\b")
new_X[,68] <- str_count(yelp$text,"\\b(?i)healthy\\b")






new_X<-as.data.frame(new_X)



yelp$notgood<-(new_X)$notgood
yelp$tip<-(new_X)$tip
yelp$her<-(new_X)$her
yelp$great<-(new_X)$great
yelp$love<-(new_X)$love
yelp$gift<-(new_X)$gift
yelp$delicious<-(new_X)$delicious
yelp$experience<-(new_X)$experience
yelp$they<-(new_X)$they
yelp$understand<-(new_X)$understand
yelp$give<-(new_X)$give
yelp$waited<-(new_X)$waited
yelp$happened<-(new_X)$happened
yelp$favorite<-(new_X)$favorite
yelp$again<-(new_X)$again
yelp$to<-(new_X)$to
yelp$best<-(new_X)$best
yelp$back<-(new_X)$back
yelp$rib<-(new_X)$rib
yelp$when<-(new_X)$when


yelp$beautiful<-(new_X)$beautiful
yelp$not<-(new_X)$not
yelp$slow<-(new_X)$slow
yelp$salty<-(new_X)$salty

yelp$order<-(new_X)$order
yelp$time<-(new_X)$time
yelp$service<-(new_X)$service
yelp$Place<-(new_X)$Place
yelp$Like<-(new_X)$Like
yelp$Online<-(new_X)$Online
yelp$Cheese<-(new_X)$Cheese
yelp$Fries<-(new_X)$Fries
yelp$Menu<-(new_X)$Menu
yelp$Manager<-(new_X)$Manager
yelp$Wisconsin<-(new_X)$Wisconsin
yelp$always<-(new_X)$always
yelp$recommend<-(new_X)$recommend
yelp$excel<-(new_X)$excel
yelp$seem<-(new_X)$seem
yelp$busy<-(new_X)$busy
yelp$everything<-(new_X)$everything
yelp$atmosphere<-(new_X)$atmosphere
yelp$enjoy<-(new_X)$enjoy
yelp$better<-(new_X)$better
yelp$salad<-(new_X)$salad

yelp$price<-(new_X)$price
yelp$drink<-(new_X)$drink
yelp$definitely<-(new_X)$definitely
yelp$definite<-(new_X)$definite
yelp$sweet<-(new_X)$sweet
yelp$little<-(new_X)$little
yelp$mean<-(new_X)$mean
yelp$get<-(new_X)$get
yelp$fresh<-(new_X)$fresh
yelp$seat<-(new_X)$seat
yelp$top<-(new_X)$top
yelp$than<-(new_X)$than
yelp$star<-(new_X)$star
yelp$super<-(new_X)$super
yelp$potato<-(new_X)$potato
yelp$visit<-(new_X)$visit
yelp$potato<-(new_X)$potato
yelp$cook<-(new_X)$cook
yelp$expect<-(new_X)$expect
yelp$down<-(new_X)$down
yelp$sure<-(new_X)$sure
yelp$taste<-(new_X)$taste
yelp$disaster<-(new_X)$disaster
yelp$healthy<-(new_X)$healthy



yelp$Nightlife=as.numeric(grepl("Nightlife", yelp$categories)==TRUE)
yelp$American=as.numeric(grepl("American", yelp$categories)==TRUE)
yelp$Food=as.numeric(grepl("Food", yelp$categories)==TRUE)
yelp$Bar=as.numeric(grepl("Bar", yelp$categories)==TRUE)
yelp$Mexican=as.numeric(grepl("Mexican", yelp$categories)==TRUE)
yelp$Breakfast=as.numeric(grepl("Breakfast", yelp$categories)==TRUE)
yelp$Pizza=as.numeric(grepl("Pizza", yelp$categories)==TRUE)
yelp$Burger=as.numeric(grepl("Burger", yelp$categories)==TRUE)
yelp$Sandwich=as.numeric(grepl("Sandwich", yelp$categories)==TRUE)
yelp$Traditional=as.numeric(grepl("Traditional", yelp$categories)==TRUE)
yelp$Pizza=as.numeric(grepl("Pizza", yelp$categories)==TRUE)
yelp$Beer=as.numeric(grepl("Beer", yelp$categories)==TRUE)
yelp$Fast=as.numeric(grepl("Fast", yelp$categories)==TRUE)
yelp$New=as.numeric(grepl("New", yelp$categories)==TRUE)
yelp$Event=as.numeric(grepl("Event", yelp$categories)==TRUE)
yelp$Mexican=as.numeric(grepl("Mexican", yelp$categories)==TRUE)
yelp$Cafes=as.numeric(grepl("Cafes", yelp$categories)==TRUE)
yelp$house=as.numeric(grepl("house", yelp$categories)==TRUE)



yelp$Nightlife=factor(yelp$Nightlife)
yelp$American=factor(yelp$American)
yelp$Food=factor(yelp$Food)
yelp$Bar=factor(yelp$Bar)
yelp$Mexican=factor(yelp$Mexican)
yelp$Breakfast=factor(yelp$Breakfast)
yelp$Pizza=factor(yelp$Pizza)
yelp$Burger=factor(yelp$Burger)
yelp$Sandwich=factor(yelp$Sandwich)
yelp$Traditional=factor(yelp$Traditional)
yelp$Pizza=factor(yelp$Pizza)
yelp$Beer=factor(yelp$Beer)
yelp$Fast=factor(yelp$Fast)
yelp$New=factor(yelp$New)
yelp$Event=factor(yelp$Event)
yelp$Mexican=factor(yelp$Mexican)
yelp$Cafes=factor(yelp$Cafes)
yelp$house=factor(yelp$house)


yelp$Madison=as.numeric(yelp$city=="Madison")
yelp$Madison=factor(yelp$Madison)

yelp$Weekday<-format(yelp$date,format="%a")
yelp$weekday<-NA
yelp$weekday[grep("\\bMon\\b",yelp$Weekday)]<-1
yelp$weekday[grep("\\bTue\\b",yelp$Weekday)]<-1
yelp$weekday[grep("\\bWed\\b",yelp$Weekday)]<-1
yelp$weekday[grep("\\bThu\\b",yelp$Weekday)]<-1
yelp$weekday[grep("\\bFri\\b",yelp$Weekday)]<-1
yelp$weekday[grep("\\bSat\\b",yelp$Weekday)]<-0
yelp$weekday[grep("\\bSun\\b",yelp$Weekday)]<-0
yelp$weekday<-factor(yelp$weekday)

#Snow
#yelp
yelp$snow<-NA
yelp$Snow<-format(yelp$date,format="%b")
yelp$snow[grep("\\bNov\\b",yelp$Snow)]<-1
yelp$snow[grep("\\bDec\\b",yelp$Snow)]<-1
yelp$snow[grep("\\bJan\\b",yelp$Snow)]<-1
yelp$snow[grep("\\bFeb\\b",yelp$Snow)]<-1
yelp$snow[grep("\\bMar\\b",yelp$Snow)]<-1
yelp$snow[grep("\\bApr\\b",yelp$Snow)]<-1
yelp$snow[grep("\\bMay\\b",yelp$Snow)]<-0
yelp$snow[grep("\\bJun\\b",yelp$Snow)]<-0
yelp$snow[grep("\\bJul\\b",yelp$Snow)]<-0
yelp$snow[grep("\\bAug\\b",yelp$Snow)]<-0
yelp$snow[grep("\\bSep\\b",yelp$Snow)]<-0
yelp$snow[grep("\\bOct\\b",yelp$Snow)]<-0
yelp$snow<-factor(yelp$snow)

#Year
#yelp
yelp$year<-NA
yelp$Year<-format(yelp$date,format="%y")
yelp$Year<-as.numeric(yelp$Year)
yelp$year[which(yelp$Year<12)]<-0
yelp$year[which(yelp$Year>=12)]<-1
yelp$year<-factor(yelp$year)

#name
yelp$House1=as.numeric(grepl("House", yelp$name)==TRUE)
yelp$Steak1=as.numeric(grepl("Steak", yelp$name)==TRUE)
yelp$Grill1=as.numeric(grepl("Grill", yelp$name)==TRUE)
yelp$Bakery1=as.numeric(grepl("Bakery", yelp$name)==TRUE)
yelp$Cafe1=as.numeric(grepl("Cafe", yelp$name)==TRUE)
yelp$Bar1=as.numeric(grepl("Bar", yelp$name)==TRUE)
yelp$Seafood1=as.numeric(grepl("Seafood", yelp$name)==TRUE)
yelp$Mexican1=as.numeric(grepl("Mexican", yelp$name)==TRUE)
yelp$Restaurant1=as.numeric(grepl("Restaurant", yelp$name)==TRUE)
yelp$Italian1=as.numeric(grepl("Italian", yelp$name)==TRUE)
yelp$Pizza1=as.numeric(grepl("Pizza", yelp$name)==TRUE)
yelp$Tap1=as.numeric(grepl("Tap", yelp$name)==TRUE)
yelp$Club1=as.numeric(grepl("Club", yelp$name)==TRUE)
yelp$Ramen1=as.numeric(grepl("Ramen", yelp$name)==TRUE)
yelp$Coffee1=as.numeric(grepl("Coffee", yelp$name)==TRUE)
yelp$Pub1=as.numeric(grepl("Pub", yelp$name)==TRUE)
yelp$Cuisine1=as.numeric(grepl("Cuisine", yelp$name)==TRUE)

yelp$House1=factor(yelp$House1)
yelp$Steak1=factor(yelp$Steak1)
yelp$Grill1=factor(yelp$Grill1)
yelp$Bakery1=factor(yelp$Bakery1)
yelp$Cafe1=factor(yelp$Cafe1)
yelp$Bar1=factor(yelp$Bar1)
yelp$Seafood1=factor(yelp$Seafood1)
yelp$Mexican1=factor(yelp$Mexican1)
yelp$Restaurant1=factor(yelp$Restaurant1)
yelp$Italian1=factor(yelp$Italian1)
yelp$Pizza1=factor(yelp$Pizza1)
yelp$Tap1=factor(yelp$Tap1)
yelp$Club1=factor(yelp$Club1)
yelp$Ramen1=factor(yelp$Ramen1)
yelp$Coffee1=factor(yelp$Coffee1)
yelp$Pub1=factor(yelp$Pub1)
yelp$Cuisine1=factor(yelp$Cuisine1)

#emotion
#yelp
yelp$exclaim<-str_count(yelp$text,"!")
yelp$question<-str_count(yelp$text,"\\?")

listing<-c("abandon","ability","abuse","accept","accident","accuse",
           "accusation", "ache", "aching", "admire", "admit", "admonish", "adore",
           "advance", "adventure", "agress", "alert", "allergic", "allow", "annoy",
           "apathy", "appall", "ass", "arrogant","awesome","axe", "bad", "beat", 
           "benefit", "blame", "block", "bloody", "boost", "boring", "bother", 
           "boycott", "bright", "broke", "bull", "burden", "cancel", "care", 
           "chaos", "chastise", "cheer", "chic", "choke", "clear", "cock",
           "combat", "comfort", "comment", "commend", "competitive", "complain",
           "creat", "cri", "criminal", "critic", "cry", "cute", "damage", "dead",
           "delay","delight","demand",'denied',"depressing","desire","desirable","destroy","desperate","determined","dilemma","dire","dirt","disappear","disastrous","discounted","disdain","disguise", "disgust","disheartened","dishonest",
           "dislike","disjointed","dismal","dismayed","disorganized","displeased","disregard","disrespect","dissatisfied","distraction",
           "disturb","doom","doubt","douche","downside","dread","drag","dream","drop","drown","drunk","dubious","dud","dull","dumb","dumped",
           "embarrass","empty","engrossed","ensure","entitled","error","evil","exaggerating","excellence","excite",
           "excuse",
           "exhausted","expand","fabulous","fail","fake","falling","fan",
           "favor",
           "fight","festive","flustered","forced","forgetful","forgive","forgotten","fortunate","frantic","friendly","fright",
           "frustrat",
           "fuck","funky","furious","gag","gain","generous",
           "glad",
           "glorious","glory","grace","grateful","gray","grave","greed","greet","growth","grief","hahaha","hail","happiness","hard","harm",
           "harried","harsh","hate","haunt","heartbroken","hell","helpful","hide","hesitate","highlight","hilarious","honor","hope",
           "hoping","horrendous","horrific","hurting","idiot",
           "ignore",
           "illegal","ill","inability","inadequate","incompetence","incompetent","inconvenience","increased","indifferent","innovative","infuriating",
           "inferior","injury","inspire","inspiring","insult","integrity","interested","interrupt","ironic","irresistible","irritate","irritating",
           "isolated","jealous","joke","jovial","jumpy","kill","lag","lame","leak","leave","legal",
           "leaked",
           "liars","lied","limitation","lively","loathe","lobby","longing","losing","lost","lowest","loved","luck","mad","mandatory","marvel",
           "marvelous","masterpiece","medal","mediocrity","mercy","mess","messed","miserable","misleading","misread","mistake","mistakes","missing",
           "mocking","motivated","murder","neglect","no","nifty","notorious","obnoxious","odd","offend","ominous","outrage","outstanding",
           "pain","panic","pardon","pay","passionate","pathetic","peaceful","perfectly","perturbed","piss","pissed","pleased","poisson",
           "poor","praise","pray","prison","problem","promise","promised","proudly","punish","questionable","questioning","rant","rash",
           "redeemed","refuse","regretting","reject","relaxed","rejected","resolve","resolved","restless","restricted","reach",
           "ridiculous","ruin","ruining","save","safe","scary","scam","scold","scream","screaming","screwed","selfish","severe","share",
           "shit","shitty","shock","sick","sincere","skeptic","slam","solve","solved","sore","sorry","spark","stall","stab","starving",
           "stereotype","straight","strike","struggle","stuck","stupid","successful","suck","suffer","suffering","swear","sympathy","terrific",
           "thoughtful","tolerant","trap","unacceptable","uncomfortable","unconcerned","uneasy","undesirable","unjust","unprofessional","upset",
           "useless","vague","warning","weak","welcomed","weep","winner","wrong","yummy"
)
newX <- matrix(0, nrow(yelp), length(listing))
colnames(newX) <- listing
for (i in 1:length(listing)){
  newX[,i] <- str_count(yelp$text, regex(listing[i], ignore_case=T)) # ignore the upper/lower case in the text
}
yelp<-cbind(yelp,newX)

yelp$messingup<-str_count(yelp$text,"messing up")
yelp$notworking<-str_count(yelp$text,"not working")

yelp_text_tbl <- tbl_df(data.frame(uniqueID=10000:25000,yelp[10000:25000,]))
yelp_text_tbl_words <- yelp_text_tbl %>% select(uniqueID,text) %>%
  unnest_tokens(word, text) %>% filter(str_detect(word,"^[a-z']+$")) %>%
  group_by(uniqueID) %>% count(word) 
ReviewWordMatrix <- yelp_text_tbl_words %>% cast_dtm(uniqueID, word, n)
dim(ReviewWordMatrix)
matrix=as.matrix(ReviewWordMatrix)[,1:26220]
new=as.data.frame(matrix)
colSums=as.data.frame(colSums(new))
colSums=add_rownames(colSums, "Word")
listing_sec=colSums$Word[colSums$`colSums(new)`>100]
newX2 <- matrix(0, nrow(yelp), length(listing_sec))
colnames(newX2) <- listing_sec
for (i in 1:length(listing_sec)){
  newX2[,i] <- str_count(yelp$text, regex(listing_sec[i], ignore_case=T)) # ignore the upper/lower case in the text
}
yelp=cbind(yelp, newX2)
yelp=yelp[, !duplicated(colnames(yelp))]
yelp[, c(6:8,14:121,162:1905)] <- yelp[, c(6:8,14:121,162:1905)]+1
yelp[, c(6:8,14:121,162:1905)] <- log(yelp[, c(6:8,14:121,162:1905)])

train=yelp[,-c(1,3:5,9,13)]
fit=lm(stars~.,data=train)


#Validate & test
Yelp_validate <- read.csv("Yelp_validate.csv", comment.char="#")
Yelp_test <- read.csv("Yelp_test.csv", comment.char="#")
validate=Yelp_validate
validate$text <- as.character(validate$text)
validate$categories <- as.character(validate$categories)
validate$date <- as.Date(validate$date)
validate$nchar=log(validate$nchar)
validate$nword=log(validate$nword)
test=Yelp_test
test$text <- as.character(test$text)
test$categories <- as.character(test$categories)
test$date <- as.Date(test$date)
test$nchar=log(test$nchar)
test$nword=log(test$nword)

new_Y <- matrix(0, nrow(validate), length(new_words))
colnames(new_Y) <- new_words
colnames(new_Y)[1]<-"notgood"

new_Y[,1] <- str_count(validate$text,"\\b(?i)not good\\b") 
new_Y[,3] <- str_count(validate$text,"\\b(?i)her\\b") # ignore the upper/lower case in the text
new_Y[,2] <- str_count(validate$text,"\\b(?i)tip\\b")
new_Y[,4] <- str_count(validate$text,"\\b(?i)great\\b")
new_Y[,5] <- str_count(validate$text,"\\b(?i)love\\b")
new_Y[,6] <- str_count(validate$text,"\\b(?i)gift\\b")
new_Y[,7] <- str_count(validate$text,"\\b(?i)delicious\\b")
new_Y[,8] <- str_count(validate$text,"\\b(?i)experience\\b")
new_Y[,9] <- str_count(validate$text,"\\b(?i)they\\b")
new_Y[,10] <- str_count(validate$text,"\\b(?i)understand\\b")
new_Y[,11] <- str_count(validate$text,"\\b(?i)give\\b")
new_Y[,12] <- str_count(validate$text,"\\b(?i)waited\\b")
new_Y[,13] <- str_count(validate$text,"\\b(?i)happened\\b")
new_Y[,14] <- str_count(validate$text,"\\b(?i)favorite\\b")
new_Y[,15] <- str_count(validate$text,"\\b(?i)again\\b") 
new_Y[,16] <- str_count(validate$text,"\\b(?i)to\\b") # ignore the upper/lower case in the text
new_Y[,17] <- str_count(validate$text,"\\b(?i)best\\b")
new_Y[,18] <- str_count(validate$text,"\\b(?i)back\\b")
new_Y[,19] <- str_count(validate$text,"\\b(?i)rib\\b")
new_Y[,20] <- str_count(validate$text,"\\b(?i)when\\b")
new_Y[,21] <- str_count(validate$text,"\\b(?i)beautiful\\b")
new_Y[,22] <- str_count(validate$text,"\\b(?i)not\\b")
new_Y[,23] <- str_count(validate$text,"\\b(?i)slow\\b")
new_Y[,24] <- str_count(validate$text,"\\b(?i)salty\\b")
new_Y[,25] <- str_count(validate$text,"\\b(?i)order\\b") 
new_Y[,26] <- str_count(validate$text,"\\b(?i)time\\b") 
new_Y[,27] <- str_count(validate$text,"\\b(?i)service\\b")
new_Y[,28] <- str_count(validate$text,"\\b(?i)place\\b")
new_Y[,29] <- str_count(validate$text,"\\b(?i)like\\b")
new_Y[,30] <- str_count(validate$text,"\\b(?i)online\\b")
new_Y[,31] <- str_count(validate$text,"\\b(?i)cheese\\b")
new_Y[,32] <- str_count(validate$text,"\\b(?i)fries\\b")
new_Y[,33] <- str_count(validate$text,"\\b(?i)menu\\b")
new_Y[,34] <- str_count(validate$text,"\\b(?i)manager\\b")
new_Y[,35] <- str_count(validate$text,"\\b(?i)wisconsin\\b")
new_Y[,36] <- str_count(validate$text,"\\b(?i)always\\b") 
new_Y[,37] <- str_count(validate$text,"\\b(?i)recommend\\b")
new_Y[,38] <- str_count(validate$text,"\\b(?i)excel\\b")
new_Y[,39] <- str_count(validate$text,"\\b(?i)seem\\b")
new_Y[,40] <- str_count(validate$text,"\\b(?i)busy\\b")
new_Y[,41] <- str_count(validate$text,"\\b(?i)everything\\b")
new_Y[,42] <- str_count(validate$text,"\\b(?i)atmosphere\\b")
new_Y[,43] <- str_count(validate$text,"\\b(?i)enjoy\\b")
new_Y[,44] <- str_count(validate$text,"\\b(?i)better\\b")
new_Y[,45] <- str_count(validate$text,"\\b(?i)salad\\b")
new_Y[,46] <- str_count(validate$text,"\\b(?i)price\\b") 
new_Y[,47] <- str_count(validate$text,"\\b(?i)drink\\b")
new_Y[,48] <- str_count(validate$text,"\\b(?i)definitely\\b")
new_Y[,49] <- str_count(validate$text,"\\b(?i)definite\\b")
new_Y[,50] <- str_count(validate$text,"\\b(?i)sweet\\b")
new_Y[,51] <- str_count(validate$text,"\\b(?i)little\\b")
new_Y[,52] <- str_count(validate$text,"\\b(?i)mean\\b")
new_Y[,53] <- str_count(validate$text,"\\b(?i)get\\b")
new_Y[,54] <- str_count(validate$text,"\\b(?i)fresh\\b")
new_Y[,55] <- str_count(validate$text,"\\b(?i)seat\\b") 
new_Y[,56] <- str_count(validate$text,"\\b(?i)top\\b")
new_Y[,57] <- str_count(validate$text,"\\b(?i)than\\b")
new_Y[,58] <- str_count(validate$text,"\\b(?i)star\\b")
new_Y[,59] <- str_count(validate$text,"\\b(?i)super\\b")
new_Y[,60] <- str_count(validate$text,"\\b(?i)potato\\b")
new_Y[,61] <
  - str_count(validate$text,"\\b(?i)visit\\b")
new_Y[,62] <- str_count(validate$text,"\\b(?i)cook\\b")
new_Y[,63] <- str_count(validate$text,"\\b(?i)expect\\b")
new_Y[,64] <- str_count(validate$text,"\\b(?i)down\\b")
new_Y[,65] <- str_count(validate$text,"\\b(?i)sure\\b")
new_Y[,66] <- str_count(validate$text,"\\b(?i)taste\\b")
new_Y[,67] <- str_count(validate$text,"\\b(?i)disaster\\b")
new_Y[,68] <- str_count(validate$text,"\\b(?i)healthy\\b")









new_Z <- matrix(0, nrow(test), length(new_words))
colnames(new_Z) <- new_words
colnames(new_Z)[1]<-"notgood"
new_Z[,1] <- str_count(test$text,"\\b(?i)not good\\b") 
new_Z[,3] <- str_count(test$text,"\\b(?i)her\\b") # ignore the upper/lower case in the text
new_Z[,2] <- str_count(test$text,"\\b(?i)tip\\b")
new_Z[,4] <- str_count(test$text,"\\b(?i)great\\b")
new_Z[,5] <- str_count(test$text,"\\b(?i)love\\b")
new_Z[,6] <- str_count(test$text,"\\b(?i)gift\\b")
new_Z[,7] <- str_count(test$text,"\\b(?i)delicious\\b")
new_Z[,8] <- str_count(test$text,"\\b(?i)experience\\b")
new_Z[,9] <- str_count(test$text,"\\b(?i)they\\b")
new_Z[,10] <- str_count(test$text,"\\b(?i)understand\\b")
new_Z[,11] <- str_count(test$text,"\\b(?i)give\\b")
new_Z[,12] <- str_count(test$text,"\\b(?i)waited\\b")
new_Z[,13] <- str_count(test$text,"\\b(?i)happened\\b")
new_Z[,14] <- str_count(test$text,"\\b(?i)favorite\\b")
new_Z[,15] <- str_count(test$text,"\\b(?i)again\\b") 
new_Z[,16] <- str_count(test$text,"\\b(?i)to\\b") # ignore the upper/lower case in the text
new_Z[,17] <- str_count(test$text,"\\b(?i)best\\b")
new_Z[,18] <- str_count(test$text,"\\b(?i)back\\b")
new_Z[,19] <- str_count(test$text,"\\b(?i)rib\\b")
new_Z[,20] <- str_count(test$text,"\\b(?i)when\\b")
new_Z[,21] <- str_count(test$text,"\\b(?i)beautiful\\b")
new_Z[,22] <- str_count(test$text,"\\b(?i)not\\b")
new_Z[,23] <- str_count(test$text,"\\b(?i)slow\\b")
new_Z[,24] <- str_count(test$text,"\\b(?i)salty\\b")
new_Z[,25] <- str_count(test$text,"\\b(?i)order\\b")
new_Z[,26] <- str_count(test$text,"\\b(?i)time\\b")
new_Z[,27] <- str_count(test$text,"\\b(?i)service\\b")
new_Z[,28] <- str_count(test$text,"\\b(?i)place\\b")
new_Z[,29] <- str_count(test$text,"\\b(?i)like\\b")
new_Z[,30] <- str_count(test$text,"\\b(?i)online\\b")
new_Z[,31] <- str_count(test$text,"\\b(?i)cheese\\b")
new_Z[,32] <- str_count(test$text,"\\b(?i)fries\\b")
new_Z[,33] <- str_count(test$text,"\\b(?i)menu\\b")
new_Z[,34] <- str_count(test$text,"\\b(?i)manager\\b")
new_Z[,35] <- str_count(test$text,"\\b(?i)wisconsin\\b")
new_Z[,36] <- str_count(test$text,"\\b(?i)always\\b") 
new_Z[,37] <- str_count(test$text,"\\b(?i)recommend\\b")
new_Z[,38] <- str_count(test$text,"\\b(?i)excel\\b")
new_Z[,39] <- str_count(test$text,"\\b(?i)seem\\b")
new_Z[,40] <- str_count(test$text,"\\b(?i)busy\\b")
new_Z[,41] <- str_count(test$text,"\\b(?i)everything\\b")
new_Z[,42] <- str_count(test$text,"\\b(?i)atmosphere\\b")
new_Z[,43] <- str_count(test$text,"\\b(?i)enjoy\\b")
new_Z[,44] <- str_count(test$text,"\\b(?i)better\\b")
new_Z[,45] <- str_count(test$text,"\\b(?i)salad\\b")
new_Z[,46] <- str_count(test$text,"\\b(?i)price\\b")
new_Z[,47] <- str_count(test$text,"\\b(?i)drink\\b")
new_Z[,48] <- str_count(test$text,"\\b(?i)definitely\\b")
new_Z[,49] <- str_count(test$text,"\\b(?i)definite\\b")
new_Z[,50] <- str_count(test$text,"\\b(?i)sweet\\b")
new_Z[,51] <- str_count(test$text,"\\b(?i)little\\b")
new_Z[,52] <- str_count(test$text,"\\b(?i)mean\\b")
new_Z[,53] <- str_count(test$text,"\\b(?i)get\\b")
new_Z[,54] <- str_count(test$text,"\\b(?i)fresh\\b")
new_Z[,55] <- str_count(test$text,"\\b(?i)seat\\b")
new_Z[,56] <- str_count(test$text,"\\b(?i)top\\b")
new_Z[,57] <- str_count(test$text,"\\b(?i)than\\b")
new_Z[,58] <- str_count(test$text,"\\b(?i)star\\b")
new_Z[,59] <- str_count(test$text,"\\b(?i)super\\b")
new_Z[,60] <- str_count(test$text,"\\b(?i)potato\\b")
new_Z[,61] <- str_count(test$text,"\\b(?i)visit\\b")
new_Z[,62] <- str_count(test$text,"\\b(?i)cook\\b")
new_Z[,63] <- str_count(test$text,"\\b(?i)expect\\b")
new_Z[,64] <- str_count(test$text,"\\b(?i)down\\b")
new_Z[,65] <- str_count(test$text,"\\b(?i)sure\\b")
new_Z[,66] <- str_count(test$text,"\\b(?i)taste\\b")
new_Z[,67] <- str_count(test$text,"\\b(?i)disaster\\b")
new_Z[,68] <- str_count(test$text,"\\b(?i)healthy\\b")







new_Y<-as.data.frame(new_Y)
new_Z<-as.data.frame(new_Z)



validate$notgood<-(new_Y)$notgood
validate$tip<-(new_Y)$tip
validate$her<-(new_Y)$her
validate$great<-(new_Y)$great
validate$love<-(new_Y)$love
validate$gift<-(new_Y)$gift
validate$delicious<-(new_Y)$delicious
validate$experience<-(new_Y)$experience
validate$they<-(new_Y)$they
validate$understand<-(new_Y)$understand
validate$give<-(new_Y)$give
validate$waited<-(new_Y)$waited
validate$happened<-(new_Y)$happened
validate$favorite<-(new_Y)$favorite
validate$again<-(new_Y)$again
validate$to<-(new_Y)$to
validate$best<-(new_Y)$best
validate$back<-(new_Y)$back
validate$rib<-(new_Y)$rib
validate$when<-(new_Y)$when
validate$beautiful<-(new_Y)$beautiful
validate$not<-(new_Y)$not
validate$slow<-(new_Y)$slow
validate$salty<-(new_Y)$salty
validate$order<-(new_Y)$order
validate$time<-(new_Y)$time
validate$service<-(new_Y)$service
validate$Place<-(new_Y)$Place
validate$Like<-(new_Y)$Like
validate$Online<-(new_Y)$Online
validate$Cheese<-(new_Y)$Cheese
validate$Fries<-(new_Y)$Fries
validate$Menu<-(new_Y)$Menu
validate$Manager<-(new_Y)$Manager
validate$Wisconsin<-(new_Y)$Wisconsin
validate$always<-(new_Y)$always
validate$recommend<-(new_Y)$recommend
validate$excel<-(new_Y)$excel
validate$seem<-(new_Y)$seem
validate$busy<-(new_Y)$busy
validate$everything<-(new_Y)$everything
validate$atmosphere<-(new_Y)$atmosphere
validate$enjoy<-(new_Y)$enjoy
validate$better<-(new_Y)$better
validate$salad<-(new_Y)$salad
validate$price<-(new_Y)$price
validate$drink<-(new_Y)$drink
validate$definitely<-(new_Y)$definitely
validate$definite<-(new_Y)$definite
validate$sweet<-(new_Y)$sweet
validate$little<-(new_Y)$little
validate$mean<-(new_Y)$mean
validate$get<-(new_Y)$get
validate$fresh<-(new_Y)$fresh
validate$seat<-(new_Y)$seat
validate$top<-(new_Y)$top
validate$than<-(new_Y)$than
validate$star<-(new_Y)$star
validate$super<-(new_Y)$super
validate$potato<-(new_Y)$potato
validate$visit<-(new_Y)$visit
validate$cook<-(new_Y)$cook
validate$expect<-(new_Y)$expect
validate$down<-(new_Y)$down
validate$sure<-(new_Y)$sure
validate$taste<-(new_Y)$taste
validate$disaster<-(new_Y)$disaster
validate$healthy<-(new_Y)$healthy







test$notgood<-(new_Z)$notgood
test$tip<-(new_Z)$tip
test$her<-(new_Z)$her
test$great<-(new_Z)$great
test$love<-(new_Z)$love
test$gift<-(new_Z)$gift
test$delicious<-(new_Z)$delicious
test$experience<-(new_Z)$experience
test$they<-(new_Z)$they
test$understand<-(new_Z)$understand
test$give<-(new_Z)$give
test$waited<-(new_Z)$waited
test$happened<-(new_Z)$happened
test$favorite<-(new_Z)$favorite
test$again<-(new_Z)$again
test$to<-(new_Z)$to
test$best<-(new_Z)$best
test$back<-(new_Z)$back
test$rib<-(new_Z)$rib
test$when<-(new_Z)$when
test$beautiful<-(new_Z)$beautiful
test$not<-(new_Z)$not
test$slow<-(new_Z)$slow
test$salty<-(new_Z)$salty
test$order<-(new_Z)$order
test$time<-(new_Z)$time
test$service<-(new_Z)$service
test$Place<-(new_Z)$Place
test$Like<-(new_Z)$Like
test$Online<-(new_Z)$Online
test$Cheese<-(new_Z)$Cheese
test$Fries<-(new_Z)$Fries
test$Menu<-(new_Z)$Menu
test$Manager<-(new_Z)$Manager
test$Wisconsin<-(new_Z)$Wisconsin
test$always<-(new_Z)$always
test$recommend<-(new_Z)$recommend
test$excel<-(new_Z)$excel
test$seem<-(new_Z)$seem
test$busy<-(new_Z)$busy
test$everything<-(new_Z)$everything
test$atmosphere<-(new_Z)$atmosphere
test$enjoy<-(new_Z)$enjoy
test$better<-(new_Z)$better
test$salad<-(new_Z)$salad
test$price<-(new_Z)$price
test$drink<-(new_Z)$drink
test$definitely<-(new_Z)$definitely
test$definite<-(new_Z)$definite
test$sweet<-(new_Z)$sweet
test$little<-(new_Z)$little
test$mean<-(new_Z)$mean
test$get<-(new_Z)$get
test$fresh<-(new_Z)$fresh
test$seat<-(new_Z)$seat
test$top<-(new_Z)$top
test$than<-(new_Z)$than
test$star<-(new_Z)$star
test$super<-(new_Z)$super
test$potato<-(new_Z)$potato
test$visit<-(new_Z)$visit
test$cook<-(new_Z)$cook
test$expect<-(new_Z)$expect
test$down<-(new_Z)$down
test$sure<-(new_Z)$sure
test$taste<-(new_Z)$taste
test$disaster<-(new_Z)$disaster
test$healthy<-(new_Z)$healthy







validate$Nightlife=as.numeric(grepl("Nightlife", validate$categories)==TRUE)
validate$American=as.numeric(grepl("American", validate$categories)==TRUE)
validate$Food=as.numeric(grepl("Food", validate$categories)==TRUE)
validate$Bar=as.numeric(grepl("Bar", validate$categories)==TRUE)
validate$Mexican=as.numeric(grepl("Mexican", validate$categories)==TRUE)
validate$Breakfast=as.numeric(grepl("Breakfast", validate$categories)==TRUE)
validate$Pizza=as.numeric(grepl("Pizza", validate$categories)==TRUE)
validate$Burger=as.numeric(grepl("Burger", validate$categories)==TRUE)
validate$Sandwich=as.numeric(grepl("Sandwich", validate$categories)==TRUE)
validate$Traditional=as.numeric(grepl("Traditional", validate$categories)==TRUE)
validate$Pizza=as.numeric(grepl("Pizza", validate$categories)==TRUE)
validate$Beer=as.numeric(grepl("Beer", validate$categories)==TRUE)
validate$Fast=as.numeric(grepl("Fast", validate$categories)==TRUE)
validate$New=as.numeric(grepl("New", validate$categories)==TRUE)
validate$Event=as.numeric(grepl("Event", validate$categories)==TRUE)
validate$Mexican=as.numeric(grepl("Mexican", validate$categories)==TRUE)
validate$Cafes=as.numeric(grepl("Cafes", validate$categories)==TRUE)
validate$house=as.numeric(grepl("house", validate$categories)==TRUE)




validate$Nightlife=factor(validate$Nightlife)
validate$American=factor(validate$American)
validate$Food=factor(validate$Food)
validate$Bar=factor(validate$Bar)
validate$Mexican=factor(validate$Mexican)
validate$Breakfast=factor(validate$Breakfast)
validate$Pizza=factor(validate$Pizza)
validate$Burger=factor(validate$Burger)
validate$Sandwich=factor(validate$Sandwich)
validate$Traditional=factor(validate$Traditional)
validate$Pizza=factor(validate$Pizza)
validate$Beer=factor(validate$Beer)
validate$Fast=factor(validate$Fast)
validate$New=factor(validate$New)
validate$Event=factor(validate$Event)
validate$Mexican=factor(validate$Mexican)
validate$Cafes=factor(validate$Cafes)
validate$house=factor(validate$house)


#name
validate$House1=as.numeric(grepl("House", validate$name)==TRUE)
validate$Steak1=as.numeric(grepl("Steak", validate$name)==TRUE)
validate$Grill1=as.numeric(grepl("Grill", validate$name)==TRUE)
validate$Bakery1=as.numeric(grepl("Bakery", validate$name)==TRUE)
validate$Cafe1=as.numeric(grepl("Cafe", validate$name)==TRUE)
validate$Bar1=as.numeric(grepl("Bar", validate$name)==TRUE)
validate$Seafood1=as.numeric(grepl("Seafood", validate$name)==TRUE)
validate$Mexican1=as.numeric(grepl("Mexican", validate$name)==TRUE)
validate$Restaurant1=as.numeric(grepl("Restaurant", validate$name)==TRUE)
validate$Italian1=as.numeric(grepl("Italian", validate$name)==TRUE)
validate$Pizza1=as.numeric(grepl("Pizza", validate$name)==TRUE)
validate$Tap1=as.numeric(grepl("Tap", validate$name)==TRUE)
validate$Club1=as.numeric(grepl("Club", validate$name)==TRUE)
validate$Ramen1=as.numeric(grepl("Ramen", validate$name)==TRUE)
validate$Coffee1=as.numeric(grepl("Coffee", validate$name)==TRUE)
validate$Pub1=as.numeric(grepl("Pub", validate$name)==TRUE)
validate$Cuisine1=as.numeric(grepl("Cuisine", validate$name)==TRUE)

validate$House1=factor(validate$House1)
validate$Steak1=factor(validate$Steak1)
validate$Grill1=factor(validate$Grill1)
validate$Bakery1=factor(validate$Bakery1)
validate$Cafe1=factor(validate$Cafe1)
validate$Bar1=factor(validate$Bar1)
validate$Seafood1=factor(validate$Seafood1)
validate$Mexican1=factor(validate$Mexican1)
validate$Restaurant1=factor(validate$Restaurant1)
validate$Italian1=factor(validate$Italian1)
validate$Pizza1=factor(validate$Pizza1)
validate$Tap1=factor(validate$Tap1)
validate$Club1=factor(validate$Club1)
validate$Ramen1=factor(validate$Ramen1)
validate$Coffee1=factor(validate$Coffee1)
validate$Pub1=factor(validate$Pub1)
validate$Cuisine1=factor(validate$Cuisine1)


test$Nightlife=as.numeric(grepl("Nightlife", test$categories)==TRUE)
test$American=as.numeric(grepl("American", test$categories)==TRUE)
test$Food=as.numeric(grepl("Food", test$categories)==TRUE)
test$Bar=as.numeric(grepl("Bar", test$categories)==TRUE)
test$Mexican=as.numeric(grepl("Mexican", test$categories)==TRUE)
test$Breakfast=as.numeric(grepl("Breakfast", test$categories)==TRUE)
test$Pizza=as.numeric(grepl("Pizza", test$categories)==TRUE)
test$Burger=as.numeric(grepl("Burger", test$categories)==TRUE)
test$Sandwich=as.numeric(grepl("Sandwich", test$categories)==TRUE)
test$Traditional=as.numeric(grepl("Traditional", test$categories)==TRUE)
test$Pizza=as.numeric(grepl("Pizza", test$categories)==TRUE)
test$Beer=as.numeric(grepl("Beer", test$categories)==TRUE)
test$Fast=as.numeric(grepl("Fast", test$categories)==TRUE)
test$New=as.numeric(grepl("New", test$categories)==TRUE)
test$Event=as.numeric(grepl("Event", test$categories)==TRUE)
test$Mexican=as.numeric(grepl("Mexican", test$categories)==TRUE)
test$Cafes=as.numeric(grepl("Cafes", test$categories)==TRUE)
test$house=as.numeric(grepl("house", test$categories)==TRUE)


test$Nightlife=factor(test$Nightlife)
test$American=factor(test$American)
test$Food=factor(test$Food)
test$Bar=factor(test$Bar)
test$Mexican=factor(test$Mexican)
test$Breakfast=factor(test$Breakfast)
test$Pizza=factor(test$Pizza)
test$Burger=factor(test$Burger)
test$Sandwich=factor(test$Sandwich)
test$Traditional=factor(test$Traditional)
test$Pizza=factor(test$Pizza)
test$Beer=factor(test$Beer)
test$Fast=factor(test$Fast)
test$New=factor(test$New)
test$Event=factor(test$Event)
test$Mexican=factor(test$Mexican)
test$Cafes=factor(test$Cafes)
test$house=factor(test$house)

validate$Madison=as.numeric(validate$city=="Madison")
validate$Madison=factor(validate$Madison)
test$Madison=as.numeric(test$city=="Madison")
test$Madison=factor(test$Madison)

test$Weekday<-format(test$date,format="%a")
test$weekday<-NA
test$weekday[grep("\\bMon\\b",test$Weekday)]<-1
test$weekday[grep("\\bTue\\b",test$Weekday)]<-1
test$weekday[grep("\\bWed\\b",test$Weekday)]<-1
test$weekday[grep("\\bThu\\b",test$Weekday)]<-1
test$weekday[grep("\\bFri\\b",test$Weekday)]<-1
test$weekday[grep("\\bSat\\b",test$Weekday)]<-0
test$weekday[grep("\\bSun\\b",test$Weekday)]<-0
test$weekday<-factor(test$weekday)

validate$Weekday<-format(validate$date,format="%a")
validate$weekday<-NA
validate$weekday[grep("\\bMon\\b",validate$Weekday)]<-1
validate$weekday[grep("\\bTue\\b",validate$Weekday)]<-1
validate$weekday[grep("\\bWed\\b",validate$Weekday)]<-1
validate$weekday[grep("\\bThu\\b",validate$Weekday)]<-1
validate$weekday[grep("\\bFri\\b",validate$Weekday)]<-1
validate$weekday[grep("\\bSat\\b",validate$Weekday)]<-0
validate$weekday[grep("\\bSun\\b",validate$Weekday)]<-0
validate$weekday<-factor(validate$weekday)

#test & validate snow
test$snow<-NA
test$Snow<-format(test$date,format="%b")
test$snow[grep("\\bNov\\b",test$Snow)]<-1
test$snow[grep("\\bDec\\b",test$Snow)]<-1
test$snow[grep("\\bJan\\b",test$Snow)]<-1
test$snow[grep("\\bFeb\\b",test$Snow)]<-1
test$snow[grep("\\bMar\\b",test$Snow)]<-1
test$snow[grep("\\bApr\\b",test$Snow)]<-1
test$snow[grep("\\bMay\\b",test$Snow)]<-0
test$snow[grep("\\bJun\\b",test$Snow)]<-0
test$snow[grep("\\bJul\\b",test$Snow)]<-0
test$snow[grep("\\bAug\\b",test$Snow)]<-0
test$snow[grep("\\bSep\\b",test$Snow)]<-0
test$snow[grep("\\bOct\\b",test$Snow)]<-0
test$snow<-factor(test$snow)
validate$snow<-NA
validate$Snow<-format(validate$date,format="%b")
validate$snow[grep("\\bNov\\b",validate$Snow)]<-1
validate$snow[grep("\\bDec\\b",validate$Snow)]<-1
validate$snow[grep("\\bJan\\b",validate$Snow)]<-1
validate$snow[grep("\\bFeb\\b",validate$Snow)]<-1
validate$snow[grep("\\bMar\\b",validate$Snow)]<-1
validate$snow[grep("\\bApr\\b",validate$Snow)]<-1
validate$snow[grep("\\bMay\\b",validate$Snow)]<-0
validate$snow[grep("\\bJun\\b",validate$Snow)]<-0
validate$snow[grep("\\bJul\\b",validate$Snow)]<-0
validate$snow[grep("\\bAug\\b",validate$Snow)]<-0
validate$snow[grep("\\bSep\\b",validate$Snow)]<-0
validate$snow[grep("\\bOct\\b",validate$Snow)]<-0
validate$snow<-factor(validate$snow)

#test&validate year
test$year<-NA
test$Year<-format(test$date,format="%y")
test$Year<-as.numeric(test$Year)
test$year[which(test$Year<12)]<-0
test$year[which(test$Year>=12)]<-1
test$year<-factor(test$year)
validate$year<-NA
validate$Year<-format(validate$date,format="%y")
validate$Year<-as.numeric(validate$Year)
validate$year[which(validate$Year<12)]<-0
validate$year[which(validate$Year>=12)]<-1
validate$year<-factor(validate$year)

#name
test$House1=as.numeric(grepl("House", test$name)==TRUE)
test$Steak1=as.numeric(grepl("Steak", test$name)==TRUE)
test$Grill1=as.numeric(grepl("Grill", test$name)==TRUE)
test$Bakery1=as.numeric(grepl("Bakery", test$name)==TRUE)
test$Cafe1=as.numeric(grepl("Cafe", test$name)==TRUE)
test$Bar1=as.numeric(grepl("Bar", test$name)==TRUE)
test$Seafood1=as.numeric(grepl("Seafood", test$name)==TRUE)
test$Mexican1=as.numeric(grepl("Mexican", test$name)==TRUE)
test$Restaurant1=as.numeric(grepl("Restaurant", test$name)==TRUE)
test$Italian1=as.numeric(grepl("Italian", test$name)==TRUE)
test$Pizza1=as.numeric(grepl("Pizza", test$name)==TRUE)
test$Tap1=as.numeric(grepl("Tap", test$name)==TRUE)
test$Club1=as.numeric(grepl("Club", test$name)==TRUE)
test$Ramen1=as.numeric(grepl("Ramen", test$name)==TRUE)
test$Coffee1=as.numeric(grepl("Coffee", test$name)==TRUE)
test$Pub1=as.numeric(grepl("Pub", test$name)==TRUE)
test$Cuisine1=as.numeric(grepl("Cuisine", test$name)==TRUE)

test$House1=factor(test$House1)
test$Steak1=factor(test$Steak1)
test$Grill1=factor(test$Grill1)
test$Bakery1=factor(test$Bakery1)
test$Cafe1=factor(test$Cafe1)
test$Bar1=factor(test$Bar1)
test$Seafood1=factor(test$Seafood1)
test$Mexican1=factor(test$Mexican1)
test$Restaurant1=factor(test$Restaurant1)
test$Italian1=factor(test$Italian1)
test$Pizza1=factor(test$Pizza1)
test$Tap1=factor(test$Tap1)
test$Club1=factor(test$Club1)
test$Ramen1=factor(test$Ramen1)
test$Coffee1=factor(test$Coffee1)
test$Pub1=factor(test$Pub1)
test$Cuisine1=factor(test$Cuisine1)

#emotion
#test&validate
test$exclaim<-str_count(test$text,"!")
test$question<-str_count(test$text,"\\?")
validate$exclaim<-str_count(validate$text,"!")
validate$question<-str_count(validate$text,"\\?")

newY <- matrix(0, nrow(validate), length(listing))
colnames(newY) <- listing
for (i in 1:length(listing)){
  newY[,i] <- str_count(validate$text, regex(listing[i], ignore_case=T)) # ignore the upper/lower case in the text
}
validate<-cbind(validate,newY)

newZ <- matrix(0, nrow(test), length(listing))
colnames(newZ) <- listing
for (i in 1:length(listing)){
  newZ[,i] <- str_count(test$text, regex(listing[i], ignore_case=T)) # ignore the upper/lower case in the text
}
test<-cbind(test,newZ)




validate$messingup<-str_count(validate$text,"messing up")
validate$notworking<-str_count(validate$text,"not working")
test$messingup<-str_count(test$text,"messing up")
test$notworking<-str_count(test$text,"not working")


newY2 <- matrix(0, nrow(validate), length(listing_sec))
colnames(newY2) <- listing_sec
for (i in 1:length(listing_sec)){
  newY2[,i] <- str_count(validate$text, regex(listing_sec[i], ignore_case=T)) # ignore the upper/lower case in the text
}
validate=cbind(validate, newY2)
validate=validate[, !duplicated(colnames(validate))]
validate=validate[,-1]
validate[, c(5:7,13:120,161:1905)] <- validate[, c(5:7,13:120,161:1905)]+1
validate[, c(5:7,13:120,161:1905)] <- log(validate[, c(5:7,13:120,161:1905)])

newZ2 <- matrix(0, nrow(test), length(listing_sec))
colnames(newZ2) <- listing_sec
for (i in 1:length(listing_sec)){
  newZ2[,i] <- str_count(test$text, regex(listing_sec[i], ignore_case=T)) # ignore the upper/lower case in the text
}
test=cbind(test, newZ2)
test=test[, !duplicated(colnames(test))]
test=test[,-1]
test[, c(5:7,13:120,161:1905)] <- test[, c(5:7,13:120,161:1905)]+1
test[, c(5:7,13:120,161:1905)] <- log(test[, c(5:7,13:120,161:1905)])


yelp$Expected <- predict(fit)
validate$Expected<-predict(fit,newdata = validate)
test$Expected<-predict(fit,newdata = test)

hexie=rbind(validate,test)

write.csv(hexie[,-c(2:1905)], file = "hexie.csv",row.names=FALSE)

