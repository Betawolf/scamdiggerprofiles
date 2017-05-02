# Demographic Analysis

First things first, we need to load the data. I'm going to focus on only those
variables which are useful for classification (i.e. appear in both scam and 
non-scam profile data) so we can discard the others.


```r
 train <- read.csv("newtrain.csv",na.strings='')
```

This leaves us with the scam outcome variable and six predictor variables to
work with: username, age, gender, ethnicity, occupation, location. I've also geocoded the location data to produce the 
country, latitude variables. One of those variables is 'username', which
doesn't look at all predictive, so we can omit it. I'll compare each of the
remainder in turn and in various combinations
to see what we might expect to be predictive, before building a classifier and
testing it on the test data. First, divide the data on the scam/real line.


```r
 train$username <- NULL
 scam <- train[train$scam == 1,]
 real <- train[train$scam == 0,]
 dim(scam)
```

```
## [1] 3140   11
```

```r
 dim(real)
```

```
## [1] 8936   11
```

You'll note an imbalance between the data, which is fine at this stage.

## Gender

First, the gender variable. I'll quickly define a useful tabulation function.


```r
 kprop <- function(realvec, scamvec){
	rg <- as.data.frame(prop.table(sort(table(realvec), decreasing=T)))
	sg <- as.data.frame(prop.table(sort(table(scamvec), decreasing=T)))
	names(rg)[1] <- "Real Profiles"
	names(sg)[1] <- "Scam Profiles"
	v <- cbind(head(rg,10), head(sg,10))
	kable(v)
 }
```

Then we can compare the proportions for scam and real profiles.


```r
 kprop(real$gender, scam$gender)
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|male          | 0.5945276|male          | 0.5977707|
|female        | 0.4054724|female        | 0.4022293|

This doesn't look distinctive, both sets are about 60% male. Gender might be
important in combination with other variables, though. 


## Age

Initially, the age variable looks similarly unpromising. The mean of the scam
profiles is 42.48 compared to 40.4 
for the real profiles. However, when you examine
the distribution of these ages, you get a different story.


```r
 plot(table(real$age), xlab="Age", ylab="Count", main="Real Profiles: left-skewed normal spread")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
 plot(table(scam$age), xlab="Age", ylab="Count", main="Scam Profiles: bimodal?")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

The two humps in the data are suggestive. If we investigate the age difference
by gender, we see something more striking in the means:
 
 gender     | scam | real |
------- |------ | ------
female | 31.83 |  41.18
male   | 49.65 |  39.87

In the real data, the average age is around 40 for both males and females. In
the scam data, by contrast, the males are about 50 and the females are about 30.
This 10-year gap is a nice bias, although not a strong signal by itself, as the
real data still covers the same age range. Clearly the scammers perceive older
males and younger females to be more desirable targets to the 40-year-olds on
the dating site. 


## Ethnicity

Moving on to the ethnicity information.


```r
 ethnise <- function(level){
	if(! level %in% c("asian", "black", "hispanic", "middle eastern", "mixed", "native american", "pacific islander", "white")){
		return("other")
	}
	return(level)
 }

 kprop(sapply(as.character(real$ethnicity), ethnise), sapply(as.character(scam$ethnicity), ethnise))
```



|Real Profiles    |      Freq|Scam Profiles    |      Freq|
|:----------------|---------:|:----------------|---------:|
|white            | 0.4383393|white            | 0.6563694|
|hispanic         | 0.3183751|native american  | 0.1085987|
|other            | 0.0713966|mixed            | 0.0796178|
|black            | 0.0562892|black            | 0.0614650|
|mixed            | 0.0518129|other            | 0.0394904|
|asian            | 0.0419651|asian            | 0.0200637|
|native american  | 0.0097359|middle eastern   | 0.0165605|
|middle eastern   | 0.0092883|hispanic         | 0.0162420|
|pacific islander | 0.0027977|pacific islander | 0.0015924|
 
We can see a couple of important differences. Firstly, tbe scam data has a
vanishingly low reporting of hispanic ethnicity, which is the second-largest
group in the real dataset, likely predominantly US-based. This is pretty
strange, and probably says something about who it is the scammers are targeting.
The scam profiles instead are much more heavily weighted towards white and
Native American ethnicities, the latter moving from close to no representation
in the real dataset all the way up to second place in the rank of scammer
ethnicities. This is a very strong signal -- most of the NA profiles we observe
are scammers. Why this is the case is a little puzzling, but might be down to a
perceived desirability of NA partners amongst a romantic US target population.

You might also notice the presence of 'single' in the observed levels. A few
scam profiles seem to have been filled in rather hastily.


## Occupation

There's a long tail in occupation, some 2677 different
values. Many of these are minor variants of each other, so I need to group as many of
them as possible into a defined set of categories. Excuse the long code block.


```r
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

 scam$occ_group <- as.factor(sapply(scam$occupation, employ))
 real$occ_group <- as.factor(sapply(real$occupation, employ))
```

Occupations typically split on gender, so we should include that in our analysis. 


```r
kprop(real$occ_group[real$gender == 'male'], scam$occ_group[scam$gender == 'male'])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1454957|engineering   | 0.2517986|
|self-employed | 0.0817942|military      | 0.2464029|
|engineering   | 0.0704862|self-employed | 0.1061151|
|technology    | 0.0584244|business      | 0.0605516|
|student       | 0.0523935|construction  | 0.0605516|
|retired       | 0.0482473|contractor    | 0.0395683|
|construction  | 0.0437241|other         | 0.0347722|
|service       | 0.0388240|medical       | 0.0251799|
|transport     | 0.0365624|sales         | 0.0167866|
|manager       | 0.0324161|manager       | 0.0161871|

Ignoring the long tail category of 'other', which captures a lot of odd or misspelt occupations,
the largest employment group for actual males was 'self-employed', followed by careers in engineering
and technology, and then by student and retired status. 'Retired' is the most common label from
the raw occupation data.  By contrast, the scam profiles boast a whopping 25% of male
profiles as 'military' and 24% as engineers. This is a much tighter occupational clustering than
we see in the real population, no doubt again reflecting a perceived desirability of these
occupations. Students don't make the top 10 male scammer professions, and only 2 male scam profiles 
report themselves as retired.

We can also explore subgroups by age.


```r
kprop(real$occ_group[real$gender == 'male' & real$age <= 30], scam$occ_group[scam$gender == 'male' & scam$age <= 30])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|student       | 0.1818182|student       | 0.2380952|
|other         | 0.1632997|engineering   | 0.1904762|
|engineering   | 0.0774411|carer         | 0.0952381|
|technology    | 0.0471380|government    | 0.0952381|
|service       | 0.0420875|manager       | 0.0952381|
|self-employed | 0.0404040|military      | 0.0952381|
|construction  | 0.0370370|self-employed | 0.0952381|
|manual        | 0.0336700|academic      | 0.0476190|
|academic      | 0.0269360|business      | 0.0476190|
|finance       | 0.0252525|accounting    | 0.0000000|

```r
kprop(real$occ_group[real$gender == 'male' & real$age > 30 & real$age <= 40], scam$occ_group[scam$gender == 'male' & scam$age > 30 & scam$age <= 40])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1808219|military      | 0.2170543|
|engineering   | 0.0808219|engineering   | 0.1937984|
|self-employed | 0.0726027|self-employed | 0.1085271|
|technology    | 0.0643836|other         | 0.0775194|
|construction  | 0.0424658|contractor    | 0.0620155|
|manual        | 0.0383562|accounting    | 0.0465116|
|service       | 0.0369863|business      | 0.0465116|
|transport     | 0.0342466|construction  | 0.0387597|
|hospitality   | 0.0328767|analyst       | 0.0232558|
|sales         | 0.0328767|manager       | 0.0232558|

```r
kprop(real$occ_group[real$gender == 'male' & real$age > 40 ], scam$occ_group[scam$gender == 'male' & scam$age > 40 ])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1181339|engineering   | 0.2577456|
|self-employed | 0.1053424|military      | 0.2511536|
|retired       | 0.0955606|self-employed | 0.1061305|
|engineering   | 0.0617005|construction  | 0.0632828|
|technology    | 0.0601956|business      | 0.0619644|
|transport     | 0.0504138|contractor    | 0.0375742|
|construction  | 0.0474041|other         | 0.0316414|
|manager       | 0.0391272|medical       | 0.0257086|
|service       | 0.0383747|sales         | 0.0171391|
|manual        | 0.0263356|manager       | 0.0145023|

There's essentially just two groups here, the under-30s males, which are more likely to present as students 
and employees, and the over-30s males, which are much more likely to claim to be in the military or engineering
professions, with a smattering of other high-status occupations and a high incidence of self-employment. 


```r
kprop(real$occ_group[real$gender == 'female'], scam$occ_group[scam$gender == 'female'])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1532033|student       | 0.2009724|
|student       | 0.1019499|self-employed | 0.1693679|
|carer         | 0.0763231|carer         | 0.1004862|
|service       | 0.0623955|sales         | 0.0794165|
|clerical      | 0.0584958|military      | 0.0486224|
|teacher       | 0.0579387|business      | 0.0429498|
|retired       | 0.0484680|fashion       | 0.0397083|
|self-employed | 0.0395543|finance       | 0.0388979|
|medical       | 0.0362117|other         | 0.0364668|
|housewife     | 0.0323120|service       | 0.0316045|

For females, the scam profiles do manage to get the top professions right -- 'student' and 'carer' 
categories (the latter of which includes 'nurse'), but the scam data is once again highly 
concentrated compared to the real profiles, with 1 in 5 female scam
profiles purporting to be from students. The remaining female scam profiles are also
unusually concentrated, with some 16% being self-employed, and the odd appearance of
a 'military' profession in the top 10, some 5% of female scam profiles. The
prevalence of Spanish-language occupations is also notable here in the raw occupation
data for real profiles.

Again, we can explore female age subgroups.


```r
kprop(real$occ_group[real$gender == 'female' & real$age <= 30], scam$occ_group[scam$gender == 'female' & scam$age <= 30])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|student       | 0.3541147|student       | 0.3229167|
|other         | 0.1571072|self-employed | 0.1649306|
|carer         | 0.0573566|carer         | 0.0746528|
|clerical      | 0.0523691|sales         | 0.0659722|
|self-employed | 0.0399002|fashion       | 0.0555556|
|service       | 0.0399002|other         | 0.0381944|
|housewife     | 0.0274314|finance       | 0.0312500|
|medical       | 0.0249377|business      | 0.0295139|
|finance       | 0.0224439|service       | 0.0243056|
|academic      | 0.0199501|artist        | 0.0190972|

```r
kprop(real$occ_group[real$gender == 'female' & real$age > 30 & real$age <= 40], scam$occ_group[scam$gender == 'female' & scam$age > 30 & scam$age <= 40])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1493776|self-employed | 0.1816578|
|clerical      | 0.0726141|carer         | 0.1199295|
|service       | 0.0726141|student       | 0.1022928|
|teacher       | 0.0705394|sales         | 0.0899471|
|carer         | 0.0643154|business      | 0.0546737|
|sales         | 0.0456432|military      | 0.0529101|
|student       | 0.0456432|teacher       | 0.0458554|
|academic      | 0.0414938|manager       | 0.0440917|
|self-employed | 0.0394191|finance       | 0.0423280|
|medical       | 0.0373444|medical       | 0.0423280|

```r
kprop(real$occ_group[real$gender == 'female' & real$age > 40 ], scam$occ_group[scam$gender == 'female' & scam$age > 40 ])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1535088|military      | 0.2307692|
|retired       | 0.0953947|carer         | 0.1428571|
|carer         | 0.0910088|self-employed | 0.1208791|
|teacher       | 0.0679825|sales         | 0.0989011|
|service       | 0.0668860|finance       | 0.0659341|
|clerical      | 0.0537281|business      | 0.0549451|
|manager       | 0.0405702|legal         | 0.0439560|
|medical       | 0.0405702|student       | 0.0439560|
|self-employed | 0.0394737|other         | 0.0329670|
|housewife     | 0.0328947|service       | 0.0329670|

Here we see a more distinct division of the age groups. Younger females are very commonly students
(as in the real data); those aged 30-40 are mostly self-employed (unlike the real data); and, bizarrely, the 
female scam profiles over 40 are heavily military. No real female profiles were military. Whether this is a 
blind application of the same demographics used in the male scam profiles, or is a more targeted scam, is unclear.

## Location

The location data associated with the profiles is surprisingly complete. I harvested the 
lat/lon coordinates for the named locations offline, which lets us plot the points. 


```r
library(ggplot2)
library(ggmap)

profiles = ifelse(train$scam, "scam", "real")
world_map <- borders("world", colour="gray50", fill="gray50")


combined_points <- ggplot() + world_map + geom_count(data=train,  aes(x= longitude, y=latitude, fill = profiles, color=profiles), alpha=0.5, shape = 21)
plot(combined_points)
```

```
## Warning: Removed 901 rows containing non-finite values (stat_sum).
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

This combined plot is a little hard to read, but it can be helped by looking at the separate plots below:


```r
real_map <- ggplot() + world_map + geom_count(data=real,  aes(x = longitude, y = latitude), color="#F8766D", fill="#F8766D",  alpha=0.8, shape = 21, show.legend=F)
plot(real_map)
```

```
## Warning: Removed 770 rows containing non-finite values (stat_sum).
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

```r
scam_map <- ggplot() + world_map + geom_count(data=scam,  aes(x = longitude, y = latitude), color="#00BFC4", fill="#00BFC4", alpha=0.8, shape = 21, show.legend=F)
plot(scam_map)
```

```
## Warning: Removed 131 rows containing non-finite values (stat_sum).
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-2.png)

We can see the scam profiles mostly purport to be from the US or Western Europe, with a 
small but significant cluster in West Africa. Also notable is the absence of
scammer profiles purporting to be from Latin America, a sizeable population in
the real data -- this lines up with the prior observations from the ethnicity
data. 

It's worth zooming in to a couple of locations for a closer look. Below is the US:



```r
us_map <- borders("usa", color="gray50", fill="gray50")
```

```
## Warning: The plyr::rename operation has created duplicates for the
## following name(s): (`colour`)
```

```r
us_xlim <- c(-139.3, -58.8) 
us_ylim = c(13.5, 55.7)
us_data <- train[which(train$longitude > us_xlim[1] & train$longitude < us_xlim[2] & train$latitude > us_ylim[1] & train$latitude < us_ylim[2]),]
profiles = ifelse(us_data$scam, "scam", "real")

combined_points <- ggplot() + us_map + geom_count(data=us_data,  aes(x=longitude, y=latitude, fill = profiles, color=profiles), alpha=0.5, shape = 21) + scale_size(range=c(2,15))

plot(combined_points)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

We can see that the scam profiles are highly concentrated in certain well-known locations, 
to a degree much greater than the real profiles. New York is a common one, as are LA and Miami. However,
the scammers do still spread themselves across the country, even appearing to populate the Midwestern 
states more than the real profiles.


```r
europe <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom","Turkey","Ukraine","Belarus","Slovakia","Norway",
		   "Albania","Scotland","Northern Ireland","Switzerland","Yugoslavia","Croatia","Bosnia",
		   "Estonia","Serbia", "Macedonia", "Moldova","Montenegro","Kosovo","UK")

eu_xlim <-  c(-20, 45)
eu_ylim <- c(35, 60.0)
eu_map <- borders("world", europe, color="gray50", fill="gray50", xlim=eu_xlim, ylim=eu_ylim)
```

```
## Warning: The plyr::rename operation has created duplicates for the
## following name(s): (`colour`)
```

```r
eu_data <- train[which(train$longitude > eu_xlim[1] & train$longitude < eu_xlim[2] & train$latitude > eu_ylim[1] & train$latitude < eu_ylim[2]),]

profiles = ifelse(eu_data$scam, "scam", "real")

combined_points <- ggplot() + eu_map + geom_count(data=eu_data,  aes(x=longitude, y=latitude, fill = profiles, color=profiles), alpha=0.5, shape = 21) + scale_size(range=c(2,15))

plot(combined_points)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

Within Europe, we see the greatest concentration of supposed locations in the
west, with UK cities like London being the largest presence, and several
concentrations in Germany, most particularly Berlin. Scam profiles comparatively
rarely present themselves as from Eastern Europe or Spain. 

Finally, we can tabulate the most common locations. Firstly, just at the city level:


```r
kprop(real$location, scam$location)
```



|Real Profiles                         |      Freq|Scam Profiles                          |      Freq|
|:-------------------------------------|---------:|:--------------------------------------|---------:|
|Bogotá, Bogota, Colombia              | 0.0102466|New York, United States                | 0.0874003|
|Lima, Peru                            | 0.0076568|Los Angeles, California, United States | 0.0513557|
|Lima, Perú                            | 0.0065308|London, United Kingdom                 | 0.0334928|
|Quito, Ecuador                        | 0.0064182|Dallas, Texas, United States           | 0.0165869|
|Bogotá, Colombia                      | 0.0063056|Houston, Texas, United States          | 0.0165869|
|Panama City, Panama                   | 0.0061930|Miami, Florida, United States          | 0.0140351|
|Cali, Cali, Valle del Cauca, Colombia | 0.0060804|New York                               | 0.0130781|
|Santo Domingo, Dominican Republic     | 0.0060804|Berlin, Germany                        | 0.0127592|
|Bogotá, Bogotá, Colombia              | 0.0056300|Chicago, Illinois, United States       | 0.0086124|
|Distrito de Lima, Perú                | 0.0056300|Austin, Texas, United States           | 0.0079745|

As expected, the scammers are more tightly clustered (9% in just New York, compared to the 
real users' largest clustering of 1%). Once again, we see the effects of the anti-hispanic bias
in the scammer profiles. This data is a bit fiddly, so it might be better to look at the results
by the broadest component of the location.


```r
kprop(real$country, scam$country)
```



|Real Profiles            |      Freq|Scam Profiles            |      Freq|
|:------------------------|---------:|:------------------------|---------:|
|United States of America | 0.3530492|United States of America | 0.7487537|
|Colombia                 | 0.0715160|UK                       | 0.0717846|
|México                   | 0.0546167|Deutschland              | 0.0269192|
|UK                       | 0.0432280|Canada                   | 0.0132935|
|Perú                     | 0.0410238|Australia                | 0.0122964|
|España                   | 0.0356356|Ghana                    | 0.0089731|
|Venezuela                | 0.0287779|РФ                       | 0.0079761|
|РФ                       | 0.0238795|Svizra                   | 0.0069791|
|Canada                   | 0.0228998|South Africa             | 0.0063144|
|Ecuador                  | 0.0204506|Sverige                  | 0.0049850|

Once again, we see a massive concentration in scam profiles. Nearly three-quarters of them are purporting to be
in the US, compared with more like a third of the real data. The UK and Germany are the secondary targets.
The more honest answer of Ghana also makes the top list. Country names here are from the geocoding from the original
location data, which included more variation in reporting style. 

