#Repository-link: https://github.com/lruehle/R_AgathaChristie
#install.packages("ggplot2")
library(ggplot2)

all_Data <- read.table(file = "Agatha Christie Data-Table 1.csv", header = TRUE, sep=";",fill=TRUE) #Inhalt

publish_table <- read.table(file= "timeline Agatha Christie_madeByWorkWithData.csv", header = TRUE, sep=";",fill=TRUE) #Diese Tabelle wird noch durch Daten in oberer Tabelle ersetzt


# add new data & reorder
# reorder needed because: if all occupations are 0, occupation will automatically considered medical as the occupation, instead of none. because of the ties.method=first argument
# old version: wrong_medical <- which(apply(all_Data[6:23], 1, function(x) sum(x == max(x)) > 1)) 
#old:wrong_medical_int <- as.integer(apply(all_Data[6:23], 1, function(x) sum(x == max(x)) > 1)) #check in each row, if columns 6-23 (occupations), has more than one maximum. => cases where a row has only "0"s in the occupation columns, because then every column is a maximum
  #all_zero <- which(rowSums(all_Data[6:23]<1))
  #all_Data$No_occupation <- all_zero #wrong_medical_int #create new column and fill it with the data
  #all_Data <- all_Data[,c(1:23,34,24:33)] # reorder 


#Data on book level
#stores one row for each book, to have a better overview. Data about different characters of Book gets lost in return 
book_level_data <- all_Data[!duplicated(all_Data[,"Book"]),]
books_per_year <- data.frame(table(book_level_data$PublicationDate)) #table1 Publishing Data



# murder stuff
# Data work connected to murder Details

murders <- book_level_data[,"NumberofMurders"]
murderers <- all_Data[all_Data$Murderer==1,]
max_murders <- max(murderers$NumberofMurders) #max_murders <- max(murders) (ungefiltert)
min_murders <- min(murderers$NumberofMurders)
#indx_max_murders <- which.max(murderers$NumberofMurders) #only shows first occurence & not filtered by murderer==1
index_max_murders <- which(all_Data$Murderer==1 & all_Data$NumberofMurders==max_murders)
three_andmore_murders <- which(murders>2)
mean_murders <- mean(murders)

murder_weapon <- all_Data[,c("Poisoned","Stabbed", "Shot","BluntInstrument","Strangulation","Other")]
colnames_murder_weapon <- colnames(murder_weapon)
#alternative: names_murder_weapon <- colnames(weapons_per_Book[2:7])
#as factor: factor_colnames_mw <- as.factor(colnames_murder_weapon)
weapons_per_Book <- all_Data[!duplicated(all_Data[,"Book"]),c("Book",colnames_murder_weapon)]
amount_murder_weapon <- colSums(weapons_per_Book[2:7])
amount_wpb <- colSums(weapons_per_Book[2:7]>0)

#list of murderer Occupation
amount_occupation <- colSums(all_Data[6:24])#apply(all_Data[6:24],2,sum)
array_occupation <- colnames(all_Data)[max.col(all_Data[6:24])+5] # only choose max. of occupation-columns. +5 necessary for correct index
#old: table_occupation <- table(array_occupation)

#note: sum(all_Data$Murderer[all_Data$AristocratWealthy==1])
murderer_per_occupation <- table(all_Data$Murderer,array_occupation)
mpo_df <- data.frame(innocent=murderer_per_occupation[1,],murderer=murderer_per_occupation[2,])
#old: mpo_df <- as.data.frame(murderer_per_occupation)
factor_occ <- as.factor(colnames(all_Data[6:24]))
#murderer_occ <- as.numeric(murderer_per_occupation[2,])
#no_murderer_occ <- as.numeric(murderer_per_occupation[1,])

#book stuff
all_titles <- unique(all_Data$Book)
titles_most_murders <- murderers[murderers$NumberofMurders==max_murders,"Book"] 
titles_min_murders <- murderers[murderers$NumberofMurders==min_murders,"Book"]
titles_mult_murderers <- murderers[which(duplicated(murderers$Book)),"Book"] #all books, where "Book" has the same value as another row/ check if occurence of title is >=2


#people stuff  
# who kills the most etc.
murderers_per_book =table(murderers$Book)
murderers_per_year =table(murderers$PublicationDate)
gendered_murder<- table(murderers$Gender)
gendered_murder_amount <- table(murderers$Gender, murderers$NumberofMurders)
mean_gma <- aggregate(murderers[,"NumberofMurders"], list(murderers$Gender), mean)



#functions
num_murderers_in <- function(booktitle)
{
  length(which(murderers$Book==booktitle))
}

names_murderers_in <- function(booktitle)
{
  murderers[murderers$Book==booktitle,"Name"]
}

return_occupation <- function(name)
{
  person_data <- all_Data[all_Data$Name==name,]
  line_occ <- which(person_data[6:24]==1)
  occ <- colnames(person_data[line_occ + 5])
  return(occ)
}


#Graphen

# murderweapon / Amount colour & pch not working?
#plot(factor(colnames_murder_weapon),amount_murder_weapon, col="purple", pch=3,xlab="Cause of Death", ylab="Total Amount used",main="Cause of Death throughout all books")
barplot(amount_wpb,main="Cause of Death", xlab = "Cause", ylab = "Amount", ylim=range(pretty(c(0,amount_wpb))),col = c("#CCFFFF"),density = 7,angle = 45,border = "#69b3a2")

#Barplot for killed Victims per Gender
barplot(gendered_murder_amount, beside=TRUE, main="Amount Killed by men/women", xlab="Number of Victims", ylab="Occurence", col = c("#66CCFF","#33FFFF"), ylim=range(pretty(c(0,max(gendered_murder_amount)))),legend.text = c("women", "men")) #beschriftung fehlt noch


#Barplot for no Murderer/murderer per Occupation
par(mar=c(9,4,4,4)) # adjust margin of graph
barplot(murderer_per_occupation, main="character Occupation",col = c("#69b3a2","#FF6666"), legend.text = c("no murderer", "murderer"), ylim=range(pretty(c(0,200))), las=2,cex.names = 0.8)

#book / murder Amount work in Progress
#farben <- c("salmon", "lightblue", "orange")
#plot(factor(all_titles), murders, pch=16, col=farben[names_murder_weapon])

#murderers in occupation / people working in occupation
plot(x=as.numeric(murderer_per_occupation[1,]), y=as.numeric(murderer_per_occupation[2,]), ylab="killers in occupation", xlab="ppl in occupation", col=factor_occ) #,col=bookname)
#text(murderer_per_occupation[1,]+5, (murderer_per_occupation[2,]),labels=colnames(murderer_per_occupation))
abline(lm(as.numeric(murderer_per_occupation[2,]) ~ as.numeric(murderer_per_occupation[1,])), col = "red")
legend("right",legend=factor_occ, lty=1:1, col=factor_occ, cex=0.8)


# ggplots

#basic murderers/no murderers per occupation
p <- ggplot(data=mpo_df, aes(innocent,murderer))+geom_point()+theme_minimal()  #, aes(x="pple in Occupation", y="killers in Occupation"), size=4)+ geom_point()
print(p)
#plot occupation/no murderers, circle size is amount murderers
p2 <- ggplot(data=mpo_df, aes(rownames(mpo_df),innocent,size=murderer))+geom_point()+theme_minimal()  #, aes(x="pple in Occupation", y="killers in Occupation"), size=4)+ geom_point()
print(p2)

#rearrange 
#taken & adapted from https://r-graph-gallery.com/320-the-basis-of-bubble-plot.html in progress
descending_mpo_df <- mpo_df
descending_mpo_df %>%
  arrange(desc(murderer))%>%
  ggplot(aes(rownames(mpo_df),innocent,size=murderer)) + geom_point(alpha = 0.5)+
  scale_size(range = c(0,16, name="Amount Murderers"))

#published books per year
#plot(x=publish_table$publication_date, y=publish_table$nb, type="o", lwd=1.5, lty="longdash") #years & publication wieder aus alter Tabelle ablesen

#Jahreszahlen der Veröffentlichung hinzufügen & untersuchen => evolution graph
#Kuchendiagramm Murder weapon in Murders (2/3 murders in Book by strangulation etc.)
#Gender Weapon Preferance
#Gender Murderer vs Murdered =>no data of murdered
#character Amount vs. victim amount (likeliness of dying per Book)
#Berufsgruppen & Murderer
#Berufsgruppen & Murders
# mean nchar of names
# gibt es lustige Zufälle, wie alle 3 Jahre ein gutes Buch, oder jedes 5. mehr morder
