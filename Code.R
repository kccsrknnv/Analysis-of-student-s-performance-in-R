data<-read.csv("StudentsPerformance.csv")
str(data)
summary(data)
View(data)
#renaming column names
colnames(data)[2]="ethnicity"
colnames(data)[3]="parent_education"
colnames(data)[5]="prep_course"
colnames(data)[6]="math"
colnames(data)[7]="reading"
colnames(data)[8]="writing"
#removing null values
data[is.na(data)] = 0
data_clean<-data
View(data_clean)
head(data_clean)
#Ranking
library(ggplot2)
data_clean <- data_clean %>% mutate (Avg.score = (math + reading + writing)/3)
data_clean%>% group_by(gender)%>% summarise(n=n()) %>% ggplot(aes(gender,n,fill=gender))+
  geom_col()
data_clean%>% group_by(ethnicity, gender) %>% summarise(n = n()) %>% 
  ggplot(aes(x = reorder(ethnicity, -n), y = n, fill = gender)) + 
  geom_col()
data_clean%>% group_by(parent_education) %>% summarise(n = n()) %>% 
  ggplot(aes(x = reorder(parent_education, n), y = n, fill = parent_education)) + 
  geom_col() 
data_clean %>% group_by(lunch) %>% summarise(n = n()) %>% ggplot(aes(lunch, n, fill = lunch)) + 
  geom_col() 
data_clean %>% group_by(gender) %>% summarise(Avg.score = sum(Avg.score)) %>% ggplot(aes(gender, 
                                Avg.score, fill = gender)) + geom_col() 
data_clean %>% group_by(prep_course) %>% summarise(n = n()) %>% ggplot(aes(prep_course, n, 
                                          fill =prep_course)) + 
  geom_col() 
data_clean %>% group_by(ethnicity, prep_course) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(ethnicity, -n),
                                                      y = n,fill = prep_course))+ geom_col()
#Histogram
ggplot(data_clean, aes(x=math))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
ggplot(data_clean, aes(x=reading))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
ggplot(data_clean, aes(x=writing))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
ggplot(data_clean, aes(x=Avg.score))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
#Corelation
library(ggcorrplot)
numeric<-data_clean[,6:9]
corr <- round(cor(numeric), 1)
print(corr)
p.mat <- cor_pmat(numeric)
ggcorrplot(corr,hc.order=TRUE,type="full",
           lab=TRUE,lab_size=3,method="square",
           colors=c("red","blue","green","grey","orange","pink"),
           title="Correlation of Placement Data",ggtheme = ggplot2::theme_gray)
#scatterplot
ggplot(data_clean, aes(x=math, y=reading)) + 
  geom_point()+
  geom_smooth(method =lm)
ggplot(data_clean, aes(x=math, y=writing)) + 
  geom_point()+
  geom_smooth(method =lm)
ggplot(data_clean, aes(x=writing, y=reading)) + 
  geom_point()+
  geom_smooth(method =lm)
#Boxplot
fact1<-factor(data_clean$gender)
fact2<-factor(data_clean$ethnicity)
fact3<-factor(data_clean$parent_education)
fact4<-factor(data_clean$lunch)
fact5<-factor(data_clean$prep_course)
ggplot(data_clean, aes(x=fact1, Avg.score)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(data_clean, aes(x=fact2, Avg.score)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(data_clean, aes(x=fact3, Avg.score)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(data_clean, aes(x=fact4, Avg.score)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(data_clean, aes(x=fact5, Avg.score)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
#Pie chart
library(lessR)
PieChart(prep_course,data = data_clean,
         rows = (gender == "female" & Avg.score > 60),
         main = NULL)
PieChart(prep_course,data = data_clean,
         rows = (gender == "male" & Avg.score > 60),
         main = NULL)
PieChart(parent_education,data = data_clean,
         rows = (gender == "female" & Avg.score > 60),
         main = NULL)
PieChart(parent_education,data = data_clean,
         rows = (gender == "male" & Avg.score > 60),
         main = NULL)
PieChart(ethnicity,data = data_clean,
         rows = (gender == "female" & Avg.score > 60),
         main = NULL)
PieChart(ethnicity,data = data_clean,
         rows = (gender == "male" & Avg.score > 60),
         main = NULL)

