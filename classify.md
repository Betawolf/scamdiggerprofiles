# Classification

This page covers the current performance of the demographic classifier. 

I will only use age, gender, ethnicity, the normalised occupation, and the various location values. 

The first code block sets up the data preparation code. 


```r
library(e1071)
library(ROCR)
```

```
## Loading required package: gplots
```

```
## 
## Attaching package: 'gplots'
```

```
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## Loading required package: methods
```

```r
library(PRROC)

employ <- function(level){
	if( grepl("army|milit|marine|soldier|captain|general|solda|force", level) ){
		return("military")
	} else if ( grepl("stud?ent|studi|coll", level) ){
		return("student")
	} else if ( grepl("self|own|independ|entre|freelanc|propia|autonomo", level)) {
		return("self-employed")
	} else if ( grepl("engin|ingenier|mechanic|mecanic|automot", level)) {
		return("engineering")
	} else if ( grepl("gover|civil|public|^un[$ ]", level)) {
		return("government")
	} else if ( grepl("academ|profes|research|lectur|universi|ologist|phd", level)){
		return("academic")
	} else if ( grepl("nurs|enfermer|care|trainer|nanny|baby|niñera|social", level)) {
		return("carer")
	} else if ( grepl("construc|carpent|roof|build|survey|ass?es|crane|equipment", level)){
		return("construction")
	} else if ( grepl("secur|detect|polic|investig|guard|custod|correct", level)){
		return("security")
	} else if ( grepl("econom|analy", level)){
		return("analyst")
	} else if ( grepl("farm|agri", level)){
		return("agriculture")
	} else if ( grepl("sail|sea|fish", level)){
		return("naval")
	} else if ( grepl("weld|factory|manufact|machin|industr", level)){
		return("manufacturing")
	} else if ( grepl("tech|inform|^it[$ ]|telecom|téch|software|sistem|system|tecnico|técnico|program|network|comput|electro|teck|develop", level) ){
		return("technology")
	} else if ( grepl("retail|comerci|shop|clerk|store|wait|vend|sell|cashier|assist|tender|customer|asist|mesero|restaur|camarer", level)){
		return("service")
	} else if ( grepl("tour|holiday|vacat|steward|flight|travel|turis|hotel", level)){
		return("tourism")
	} else if ( grepl("sale|market|ventas", level)) {
		return("sales")
	} else if ( grepl("writ|journal|period", level)){
		return("writing")
	} else if ( grepl("handy|repair|repare|maint|plumb|electr|manteni|hvac", level)){
		return("repair")
	} else if ( grepl("estat", level)){
		return("real estate")
	} else if ( grepl("teach|educa|docen|maestr|lehr", level)) {
		return("teacher")
	} else if ( grepl("manag|supervis", level)){
		return("manager")
	} else if ( grepl("contra", level)){
		return("contractor")
	} else if ( grepl("ama de casa|wife|mother|mom|home|hogar", level)){
		return("housewife")
	} else if ( grepl("unemploy|desempl|not work", level)){
		return("unemployed")
	} else if ( grepl("financ|bank|insur|trad|negoci|cajero", level)){
		return("finance")
	} else if ( grepl("chef|cook|bake|co[cs]iner|hospitali|food", level)){
		return("hospitality")
	} else if ( grepl("secret|admin|recep|office|human resources|clerical|profec|entry", level)){
		return("clerical")
	} else if ( grepl("driver|transport|deliver|ship|chofer|pilot|logist|cargo", level)){
		return("transport")	
	} else if ( grepl("housekeep|clean|limpi|janitor", level)){
		return("cleaner")
	} else if ( grepl("architec|arquitec", level)){
		return("architect")
	} else if ( grepl("account|contad", level)){
		return("accounting")
	} else if ( grepl("law|judge|solicitor|barrister|legal|attorney|abogad", level)){
		return("legal")
	} else if ( grepl("music|sport|play|produc|músico|deporti|conduc|soccer", level)){
		return("entertainment")
	} else if ( grepl("artist|art|paint|sculpt|boutique|photo|foto|choreo", level)){
		return("artist")
	} else if ( grepl("jewel|antiq|print", level)){
		return("specialist")
	} else if ( grepl("doctor|physic|ician|medic|psicolog|terap|therap|salud|health|médic|surgeon|denti|pharma", level)){
		return("medical")
	} else if ( grepl("beaut|styl|estili|peluquer|hair|salon|manic", level)){
		return("beauty")
	} else if ( grepl("fashion|model", level)){
		return("fashion")
	} else if ( grepl("design|decor|flower|desiñ|deisñ", level)){
		return("designer")
	} else if ( grepl("warehouse|work|opera|obrer|labor|labour|landscap|mining|mine|load|trabajo|pack|foreman", level)){
		return("manual")
	} else if ( grepl("bus[iy]?nes|empresa|execut|direct|ceo|ejecut", level)) {
		return("business")
	} else if ( grepl("consult", level)) {
		return("consultant")
	} else if ( grepl("retir|jubilad|pension", level)) {
		return("retired")
	} else if ( grepl("disab", level)) {
		return("disabled")
	} else if (is.na(level)) {
		return(NA)
	} else {
		return("other")
	}
 }


ethnise <- function(level){
	if(! level %in% c("asian", "black", "hispanic", "middle eastern", "mixed", "native american", "pacific islander", "white")){
		return("other")
	}
	return(level)
 }


loaddata <- function(filename){
	rawdata <- read.csv(filename, na.strings=c('','NA'))
	data <- data.frame(scam=rawdata$scam, age=rawdata$age, gender=rawdata$gender, latitude=rawdata$latitude, longitude=rawdata$longitude, country=rawdata$country, fold=rawdata$fold)
	data$ethnicity <- as.factor(sapply(as.character(rawdata$ethnicity), ethnise))
	data$occupation <- as.factor(sapply(as.character(rawdata$occupation), employ))
	return(data)
}

train <- loaddata("newtrain.csv")
```




```r
crossvalidate <- function(features, fitmodel, testmodel, label){
  #Make results consistent
  set.seed(2017)

  trues <- c()
  preds <- c()

  for(x in unique(features$fold)){
    #Train/test
    testindex <- which(features$fold == x)

    testset <- features[testindex,! names(features) == 'fold']
    trainset <- features[-testindex,! names(features) == 'fold']

    # Fit model
    model <- fitmodel(trainset)

    # Predict
    pred <- testmodel(model, testset)

    # Stash
    trues <- c(trues, testset$scam)
    preds <- c(preds, pred)
  }

  show_performance(preds, label, trues)
  return(preds)
}



show_performance <- function(input.pred, label, truth){
	input.eval <- prediction(input.pred, truth)

	#Produce a ROC plot.
#	input.roc <- performance(input.eval, 'tpr', 'fpr')
#	input.auc <- unlist(slot(performance(input.eval, 'auc'),'y.values'))
#	plot(input.roc, main=paste(label, "AUROC:", round(input.auc,3)), lwd=2, col='red')

	#Produce a P/R plot
#	posvals <- input.eval@predictions[[1]][truth == 1]
#	negvals <- input.eval@predictions[[1]][truth == 0]
#	input.pr <- pr.curve(scores.class0=posvals, scores.class1=negvals, curve=T)
#	input.pr.auc <- input.pr$auc.integral
#	plot(input.pr, main=paste(label, "AUPRC:", round(input.pr.auc,3)), auc.main=F, lwd=2, color='red')

	cat(label)

	#Produce a confusion matrix
	cutoffs <- input.eval@cutoffs[[1]]
	input.acc <- unlist(slot(performance(input.eval, 'f'),'y.values'))
	input.maxacc <- max(input.acc[!is.nan(input.acc)])
	input.best <- cutoffs[which(input.acc == input.maxacc)]
	given.labels <- ifelse(input.pred > input.best, "predict 1", "predict 0")
	confusion <- table(given.labels, truth)
	print(kable(confusion))
	cat("\n \n")
	
	#Calculate best precision, recall, f1
	confdf <- data.frame(confusion)	
	tn <- confdf$Freq[1]
	fp <- confdf$Freq[2]
	fn <- confdf$Freq[3]
	tp <- confdf$Freq[4]
	precision <- tp/(tp+fp)
	recall <- tp/(tp+fn)
	f1 <- 2*((precision*recall)/(precision+recall))
	accuracy <- (tp+tn)/(tp+tn+fp+fn)
	res <- data.frame(measure = c("precision","recall","f1","acc"), value = c(round(precision,3), round(recall,3), round(f1,3), round(accuracy, 3)))
	print(kable(res))
	cat("\n")
	return(given.labels)
}


fit.nb <- function(trainset){
  nb.model <- naiveBayes(trainset[,-1], trainset$scam)
  return(nb.model)
}

test.nb <- function(nb.model, testset){
  results <- predict(nb.model, testset, type='raw')
  return(results[,2])
}
```

## Prior method: Lat/Lon 

My first approach is a simple Naive Bayesian model. The advantage here is the model is quick and easy to fit,
and robust to any missing data. 


```r
null <- crossvalidate(train[,! names(train) == 'country'], fit.nb, test.nb, "Naive Bayes")
```

Naive Bayes

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 7595| 1036|
|predict 1 | 1341| 2104|

 


|measure   | value|
|:---------|-----:|
|precision | 0.611|
|recall    | 0.670|
|f1        | 0.639|
|acc       | 0.803|

This shows that we've got pretty decent precision, but terrible recall. As mentioned
before, removing rows with missing data really helps our recall:


```r
clean <- na.omit(train)
null <- crossvalidate(clean[,! names(clean) == 'country'], fit.nb, test.nb, "Naive Bayes (Subset)")
```

Naive Bayes (Subset)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 3321|  510|
|predict 1 |  823| 2271|

 


|measure   | value|
|:---------|-----:|
|precision | 0.734|
|recall    | 0.817|
|f1        | 0.773|
|acc       | 0.808|

This gets us up to an F1 in the 70s. A binomial linear regression works about as well:


```r
fit.glm <- function(trainset){
  glm.model <- glm(scam ~ ., data=trainset, family="binomial")
  return(glm.model)
}


test.glm <- function(glm.model, testset){
  glm.pred <- predict(glm.model, testset, type="response")
  return(glm.pred)
}

null <- crossvalidate(clean[! names(clean) == 'country'], fit.glm, test.glm, "LM (Subset)")
```

LM (Subset)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 3177|  411|
|predict 1 |  967| 2370|

 


|measure   | value|
|:---------|-----:|
|precision | 0.710|
|recall    | 0.852|
|f1        | 0.775|
|acc       | 0.801|

## Variable Exploration

Time to try replacing lat/lon with the new country grouping.


```r
null <- crossvalidate(train[,! names(train) %in% c('latitude','longitude')], fit.nb, test.nb, "Naive Bayes (Country)")
```

Naive Bayes (Country)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 7012|  645|
|predict 1 | 1924| 2495|

 


|measure   | value|
|:---------|-----:|
|precision | 0.565|
|recall    | 0.795|
|f1        | 0.660|
|acc       | 0.787|

Looks like country is adding a couple of percentage points over lat/lon. We could also try
adding in both of them.



```r
null <- crossvalidate(train, fit.nb, test.nb, "Naive Bayes (Country+Lat/Lon)")
```

Naive Bayes (Country+Lat/Lon)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 7149|  731|
|predict 1 | 1787| 2409|

 


|measure   | value|
|:---------|-----:|
|precision | 0.574|
|recall    | 0.767|
|f1        | 0.657|
|acc       | 0.791|

Doesn't help. It might be worth comparing performance under different combinations generally.


```r
null <- crossvalidate(train[,c('scam','fold','age','gender')], fit.nb, test.nb, "Naive Bayes (Age+Gender)")
```

Naive Bayes (Age+Gender)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 |  612|   86|
|predict 1 | 8324| 3054|

 


|measure   | value|
|:---------|-----:|
|precision | 0.268|
|recall    | 0.973|
|f1        | 0.421|
|acc       | 0.304|

```r
null <- crossvalidate(train[,c('scam','fold','age','gender','occupation')], fit.nb, test.nb, "Naive Bayes (Age+Gender+Occupation)")
```

Naive Bayes (Age+Gender+Occupation)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 7837| 1252|
|predict 1 | 1099| 1888|

 


|measure   | value|
|:---------|-----:|
|precision | 0.632|
|recall    | 0.601|
|f1        | 0.616|
|acc       | 0.805|

```r
null <- crossvalidate(train[,c('scam','fold','age','gender','ethnicity')], fit.nb, test.nb, "Naive Bayes (Age+Gender+Ethnicity)")
```

Naive Bayes (Age+Gender+Ethnicity)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 3716|  149|
|predict 1 | 5220| 2991|

 


|measure   | value|
|:---------|-----:|
|precision | 0.364|
|recall    | 0.953|
|f1        | 0.527|
|acc       | 0.555|

```r
null <- crossvalidate(train[,c('scam','fold','age','gender','occupation','ethnicity')], fit.nb, test.nb, "Naive Bayes (Age+Gender+Occupation+Ethnicity)")
```

Naive Bayes (Age+Gender+Occupation+Ethnicity)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 8080| 1190|
|predict 1 |  856| 1950|

 


|measure   | value|
|:---------|-----:|
|precision | 0.695|
|recall    | 0.621|
|f1        | 0.656|
|acc       | 0.831|

```r
null <- crossvalidate(train[,c('scam','fold','age','gender','occupation','country')], fit.nb, test.nb, "Naive Bayes (Age+Gender+Occupation+Country)")
```

Naive Bayes (Age+Gender+Occupation+Country)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 8275| 1342|
|predict 1 |  661| 1798|

 


|measure   | value|
|:---------|-----:|
|precision | 0.731|
|recall    | 0.573|
|f1        | 0.642|
|acc       | 0.834|

```r
null <- crossvalidate(train[,c('scam','fold','age','gender','occupation','ethnicity','country')], fit.nb, test.nb, "Naive Bayes (Age+Gender+Occupation+Ethnicity+Country)")
```

Naive Bayes (Age+Gender+Occupation+Ethnicity+Country)

|          |    0|    1|
|:---------|----:|----:|
|predict 0 | 7012|  645|
|predict 1 | 1924| 2495|

 


|measure   | value|
|:---------|-----:|
|precision | 0.565|
|recall    | 0.795|
|f1        | 0.660|
|acc       | 0.787|

It looks like it's actually dubious how much value the location information is adding. A lot of the power it
provides might actually be given by the ethnicity variable, which suggests that the national information is
largely just helping exclude the 'hispanic' group.


## Other Classifiers

Ongoing...
