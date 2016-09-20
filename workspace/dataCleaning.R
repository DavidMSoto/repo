

m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)

apply(m, 2, mean)

apply(m, 1, function(x) length(x[x<0]))

summary(p_logi)

head(df.train$Name, n=10L)

tail(p_logi)

unique(people$people_group_1)

names(df.train)

df.train$Age[which(df.train$Title=="Dr")]

df.train$Age[which(df.train$Title=="Dr")]

summary(df.train$Embarked)


#Conocer la estructura de los datos.
str(train)

#Resumen basico sobre una columna podemos usar.
table(train$Survived)

#Obtener la proporción.
prop.table(table(train$Survived))
