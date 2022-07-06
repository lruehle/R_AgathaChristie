my_table <- read.table(file = "Agatha Christie Data-Table 1.csv", header = TRUE, sep=";",fill=TRUE) #Inhalt

publish_table <- read.table(file= "timeline Agatha Christie_madeByWorkWithData.csv", header = TRUE, sep=";",fill=TRUE) #Diese Tabelle wird noch durch Daten in oberer Tabelle ersetzt


# add new data & reorder
# reorder needed because: if all occupations are 0, occupation will automatically considered medical as the occupation, instead of none. because of the ties.method=first argument
# could also be done once and then resaved in the original file. Work in Progress
# old version: wrong_medical <- which(apply(my_table[6:23], 1, function(x) sum(x == max(x)) > 1)) 
wrong_medical_int <- as.integer(apply(my_table[6:23], 1, function(x) sum(x == max(x)) > 1)) #check in each row, if columns 6-23 (occupations), has more than one maximum. => cases where a row has only "0"s in the occupation columns, because then every column is a maximum
my_table$No_occupation <- wrong_medical_int #create new column and fill it with the data
my_table <- my_table[,c(1:23,34,24:33)] # reorder 

#Data on book level
#stores one row for each book, to have a better overview. Data about different characters of Book gets lost in return 
book_level_data <- my_table[!duplicated(my_table[,"Book"]),]

# murder stuff
# Data work connected to murder Details

murders <- book_level_data[,"NumberofMurders"]
murderers <- my_table[my_table$Murderer==1,]
max_murders <- max(murderers$NumberofMurders) #max_murders <- max(murders) (ungefiltert)
min_murders <- min(murderers$NumberofMurders)
#indx_max_murders <- which.max(murderers$NumberofMurders) #only shows first occurence & not filtered by murderer==1
index_max_murders <- which(my_table$Murderer==1 & my_table$NumberofMurders==max_murders)
three_andmore_murders <- which(murders>2)
mean_murders <- mean(murders)

murder_weapon <- my_table[,c("Poisoned","Stabbed", "Shot","BluntInstrument","Strangulation","Other")]
colnames_murder_weapon <- colnames(murder_weapon)
#alternative: names_murder_weapon <- colnames(weapons_per_Book[2:7])
#as factor: factor_colnames_mw <- as.factor(colnames_murder_weapon)
weapons_per_Book <- my_table[!duplicated(my_table[,"Book"]),c("Book",colnames_murder_weapon)]
amount_murder_weapon <- colSums(weapons_per_Book[2:7])
amount_wpb <- colSums(weapons_per_Book[2:7]>0)

#list of murderer Occupation
amount_occupation <- colSums(my_table[6:24])
array_occupation <- colnames(my_table)[max.col(my_table[6:24])+5] # only choose max. of occupation-columns. +5 necessary for correct index
#old: table_occupation <- table(array_occupation)
murderer_per_occupation <- table(my_table$Murderer,array_occupation)
#old: mpo_df <- as.data.frame(murderer_per_occupation)
factor_occ <- as.factor(colnames(my_table[6:23]))
#murderer_occ <- as.numeric(murderer_per_occupation[2,])
#no_murderer_occ <- as.numeric(murderer_per_occupation[1,])

#book stuff
all_titles <- unique(my_table$Book)
titles_most_murders <- murderers[murderers$NumberofMurders==max_murders,"Book"] 
titles_min_murders <- murderers[murderers$NumberofMurders==min_murders,"Book"]
titles_mult_murderers <- murderers[which(duplicated(murderers$Book)),"Book"] #all books, where "Book" has the same value as another row/ check if occurence of title is >=2


#people stuff  
# who kills the most etc.
murderers_per_book =table(murderers$Book)
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
  person_data <- my_table[my_table$Name==name,]
  line_occ <- which(person_data[6:24]==1)
  occ <- colnames(person_data[line_occ + 5])
  return(occ)
}

#Graphen

# murderweapon / Amount colour & pch not working?
#plot(factor(colnames_murder_weapon),amount_murder_weapon, col="purple", pch=3,xlab="Cause of Death", ylab="Total Amount used",main="Cause of Death throughout all books")
barplot(amount_wpb,main="Cause of Death", xlab = "Cause", ylab = "Amount", ylim=range(pretty(c(0,amount_wpb))))

barplot(gendered_murder_amount, beside=TRUE, main="Gendered Kill-Count", ylim=range(pretty(c(0,max(gendered_murder_amount))))) #beschriftung fehlt noch

barplot(murderer_per_occupation, main="Main")
#book / murder Amount work in Progress
#farben <- c("salmon", "lightblue", "orange")
#plot(factor(all_titles), murders, pch=16, col=farben[names_murder_weapon])

#murderers in occupation / people working in occupation
plot(x=as.numeric(murderer_per_occupation[2,]),y=amount_occupation, xlab="killers in occupation", ylab="ppl in occupation", col=factor_occ) #,col=bookname)
#plot(x=my_table$NumberofMurders, y=my_table$Poisoned, pch=16, col= "green")


#published books per year
plot(x=publish_table$publication_date, y=publish_table$nb, type="o", lwd=1.5, lty="longdash") #years & publication wieder aus alter Tabelle ablesen

#Jahreszahlen der Veröffentlichung hinzufügen & untersuchen => evolution graph
#Kuchendiagramm Murder weapon in Murders (2/3 murders in Book by strangulation etc.)
#Gender Weapon Preferance
#Gender Murderer vs Murdered =>no data of murdered
#character Amount vs. victim amount (likeliness of dying per Book)
#Berufsgruppen & Murderer
#Berufsgruppen & Murders
# mean nchar of names
# gibt es lustige Zufälle, wie alle 3 Jahre ein gutes Buch, oder jedes 5. mehr morder
