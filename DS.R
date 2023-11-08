library(tidyverse) 
library(dplyr) 
library(gridExtra) 
library(rlang)
library(ggplot2)
library(tm)
library(wordcloud2)
library(RColorBrewer)

#Load the Data 
DS<-read.csv("BusinessAnalyst.csv", header = TRUE, sep = ",")
head(DS,4)
summary(DS)
str(DS)

# Identify and extract mismatched rows
index_range = 0
for (i in DS$X) {
  if (!grepl("^\\d+$", i)) {
    index_range <- index_range + 1
  }
}

dt_fix <- DS[(nrow(DS) - index_range + 1):nrow(DS), ]
dt_fix <- dt_fix[, !(colnames(dt_fix) %in% c("Competitors", "Easy.Apply"))]
colnames(dt_fix) <- colnames(DS)[3:length(colnames(DS))]

# Remove mismatched rows and columns
DS <- DS[1:(nrow(DS) - index_range), ]
DS <- DS[, !(colnames(DS) %in% c("X", "index"))]

# Append the sub-dataset to the original dataset
DS <- rbind(DS, dt_fix)

#Fixing wrong data format
#Removing Unnecessary data
DS<-DS%>%
  mutate(Salary.Estimate= gsub("\\(Employer est.\\)"," ",Salary.Estimate),
         Salary.Estimate=gsub("\\(Glassdoor est.\\)"," ",Salary.Estimate),
         Company.Name= gsub("\\s\\d+\\.\\d+"," ",Company.Name),
         Type.of.ownership= gsub("Company -  ","", Type.of.ownership)
         )

#Convert -1 into unknown for easy Exploratory 
DS[DS == -1]<-"Unknown"

DS$Type.of.ownership<-gsub("Company -","",DS$Type.of.ownership)

#Spliting salary column into min, max and average 
Salary_split<-strsplit(DS$Salary.Estimate, "\\$|K\\-\\$|K")
Salary_split<-sapply(Salary_split, function(x) as.numeric(x[c(2, 3)]))

DS$Mini_Salary<-Salary_split[1, ]*1000
DS$Max_Salary<-Salary_split[2, ]*1000

#Average salary
DS$Average.Salary<-rowMeans(DS[, c("Mini_Salary", "Max_Salary")])

#Remove unwant columns 
DS<- DS%>%
  select(
    -c("Mini_Salary","Max_Salary","Salary.Estimate")
  )

# Count missing values in each column of the data frame
colSums(is.na(DS))


# Apply the function to each job description
#DS$Keywords <- lapply(DS$Job.Description, extract_keywords)

# Convert 'Rating' to a numeric (float) type
DS$Rating <- as.numeric(DS$Rating)

# Split the 'Location' column into 'StateName' and 'State'
location_split <- strsplit(DS$Location, ", ")
DS$StateName <- sapply(location_split, function(x) x[1])
DS$State <- sapply(location_split, function(x) x[2])

# Replace 'Los Angeles' with 'CA' in the 'State' column
DS$State<-gsub("Los Angeles", "CA",DS$State)

#DS<-separate(DS, Location, into = c("City", "State"), sep = ",", extra = "merge")

# Exploratory Data Analysis 

state_fill_scale<-scale_fill_manual(
  values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#bcbd22", "#17becf"),
  breaks = c("TX","PA","NY","NJ","IL","FL","DE","CA","AZ"),
  labels = c("TX","PA","NY","NJ","IL","FL","DE","CA","AZ")
)

owner_fill_scale<-scale_fill_manual(
  values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#bcbd22", "#17becf","#fb6a4a","#08519c","#bbbbbb","#d3a826","#f6f9fb","#a4bbbb"),
  breaks = c("Private","Public","College / University","Contract","Franchise","Government","Hospital",
             "Nonprofit Organizartion","Other Organization","Private Practice / Firm","School / School District",
             "Self-employed","Subsidiary or Business Segment","Unknown"),
  labels = c("Private","Public","College / University","Contract","Franchise","Government","Hospital",
             "Nonprofit Organizartion","Other Organization","Private Practice / Firm","School / School District",
             "Self-employed","Subsidiary or Business Segment","Unknown")
)

sector_fill_scale<-scale_fill_manual(
  values= c("#4292c6", "#fb6a4a","#08519c","#bbbbbb","#d3a826","#f6f9fb","#9467bd"),
  breaks = c("Business Service", "Finance","Health Care","Information Technology","Insurance","Manufacturing","Unknown"),
  labels= c("Business Service", "Finance","Health Care","Information Technology","Insurance","Manufacturing","Unknown")
)

#Histogram
histo_average<-ggplot(DS, aes(x= Average.Salary))+
  geom_histogram(fill= "skyblue", color="black")+
  labs(title = "Salary Distributaion",
       x="Average_Salary",
       y="")+
  theme_classic()
print(histo_average)
dev.copy(png,file="Histogram", width=480,height=480)
dev.off

#Bar Chart 
countState <- ggplot(DS, aes(y = State, fill= State)) +
  geom_bar() +
  state_fill_scale+
  labs(title = "Number of job offers by State",
       y = "State",
       x= "count of job offers")+
  theme_minimal()
print(countState)
dev.copy(png,file="countState.png", width=480, height=480)
dev.off()

countowner<-ggplot(DS, aes(y=Type.of.ownership, fill= Type.of.ownership))+
  geom_bar()+
  owner_fill_scale+
  labs(title = "Type of Ownership",
       x = "Count")+
  theme_classic()
dev.copy(png,file="countowner.png")
dev.off

#Box plot
options(rep.plot.width=10, repr.plot.height=10)
boxplot<-ggplot(DS, aes(x= State, y= Rating, fill=State))+
  geom_boxplot()+
  labs(title = "Distributaion between Rating and State")+
  state_fill_scale+
  theme_minimal()
print(boxplot)
dev.copy(png,file="boxplot.png", width=480, height=480)
dev.off()


boxplot_Average<-ggplot(DS, aes(x= State, y= Average.Salary, fill= State))+
  geom_boxplot()+
  state_fill_scale+
  labs(title = "Distributaion between AverageSalary and State")+
  theme_minimal()
print(boxplot_Average)
dev.copy(png,file="boxplot_Average.png", width=480, height=480)
dev.off()

#sorting sectors and boxplot on sector and Rating and Average Salary 
plus100 <- DS$Sector %in% names(sort(table(DS$Sector), decreasing = TRUE)[1:sum(table(DS$Sector) > 100)])

countsector<-ggplot(subset(DS,plus100), aes(y=Sector, fill= Sector))+
  geom_bar()+
  sector_fill_scale+
  labs(title = "Number of Sector")+
  theme_classic()
print(countsector)
dev.copy(png,file="countsector.png", width=480, height=480)
dev.off()

sectorBox<-ggplot(subset(DS, plus100),aes(x=Rating, y=Sector, fill=Sector))+
  geom_boxplot()+
  labs(title = "Distributaion between Sector and Rating")+
  sector_fill_scale+
  theme_classic()
print(sectorBox)
dev.copy(png,file="sector_boxplot.png", width=480, height=480)
dev.off()

sectorAverage<-ggplot(subset(DS, plus100), aes(x=Average.Salary, y=Sector, fill= Sector))+
  geom_boxplot()+
  labs(title = "Distributaion between Sector and Average Salary")+
  sector_fill_scale+
  theme_classic()
print(sectorAverage)
dev.copy(png,file="sectoraverage.png", width=480, height=480)
dev.off()


# Define a function to create an advanced word cloud
options(wordcloud2.colors=brewer.pal(20, "Dark2"))
get_advanced_wordcloud <- function(series, custom_stopwords = NULL, min.freq = 5, max.words = 300) {
  # Combine the text into one long string
  text <- paste(series, collapse = " ")
  
  # Create a Corpus and pre-process the text
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  
  if (!is.null(stopwords)) {
    corpus <- tm_map(corpus, removeWords, custom_stopwords)
  }
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Create a Document-Term Matrix
  dtm <- DocumentTermMatrix(corpus)
  freq <- colSums(as.matrix(dtm))
  freq <- freq[freq >= min.freq]
  freq <- head(freq[order(freq, decreasing = TRUE)], max.words)
  
  # Create the word cloud
  wordcloud2(data =data.frame(names(freq), freq = freq))
}

Job.title<-tolower(DS$Job.Title)
get_advanced_wordcloud(Job.title)
dev.copy(png,file="title2.png", width=480, height=480)
dev.off()
