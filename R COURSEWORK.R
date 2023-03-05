df <- read.csv("Coursework.csv", header = TRUE, stringsAsFactors = FALSE)

head(df)

tail(df)

summary(df)

sapply(df, class)

df$DateofTermination <- as.Date(df$DateofTermination, format = "%d/%m/%Y")

df$LastPerformanceReview_Date <- as.Date(df$LastPerformanceReview_Date, format = "%d/%m/%Y")

df$DateofHire <- as.Date(df$DateofHire, format = "%d/%m/%Y")

sapply(df, class)

sapply(df$DateofHire, class)

df %>% group_by(Department) %>% summarise(mean.Emp.Sat = mean(EmpSatisfaction))

df %>% group_by(Department) %>% summarise(mean.Absences = mean(Absences))

colnames(df)

df %>% group_by(ManagerName) %>% summarise(mean.SpecialProjectsCount = mean(SpecialProjectsCount))

df %>% group_by(ManagerName) %>% summarise(mean.Emp.Sat = mean(EmpSatisfaction))

df %>% group_by(Department) %>% summarise(mean.EngagementSurvey = mean(EngagementSurvey))

ggplot(df, aes(x = EmpSatisfaction, y = EngagementSurvey)) + geom_point()

max(df$Years_in_post)
min(df$Years_in_post)

max(df$Salary)
min(df$Salary)

df %>% group_by(Years_in_post) %>% summarise(mean.Salary = mean(Salary))

df %>% filter(Department == "Sales")

df %>% filter(PerformanceScore == "Exceed") %>% count()

df %>% filter(PerformanceScore == "Fullymeet") %>% count()

df %>% filter(PerformanceScore == "Needs improvement") %>% count()
df %>% filter(PerformanceScore == "PIP") %>% count()

pairs(df[,2 :23])

df %>% group_by(Department) %>% summarise(mean.Absences = mean(Absences))

pairs(df[,11,12,13,22,23,24])
