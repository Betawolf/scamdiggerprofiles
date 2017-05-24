# Demographic Analysis

First things first, we need to load the data. I'm going to focus on only those
variables which are useful for classification (i.e. appear in both scam and 
non-scam profile data) so we can discard the others.


```r
 train <- read.csv("demographics.csv",na.strings='')
```

This leaves us with the scam outcome variable and six predictor variables to
work with: username, age, gender, ethnicity, occupation, location. I've also geocoded the location data to produce the 
country, longitude variables. One of those variables is 'username', which
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
## [1] 5402   10
```

```r
 dim(real)
```

```
## [1] 14720    10
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
 tprop <- function(realvec, scamvec){
	rg <- as.data.frame(prop.table(sort(table(realvec), decreasing=T)))
	sg <- as.data.frame(prop.table(sort(table(scamvec), decreasing=T)))
	names(rg)[1] <- "Real Profiles"
	names(sg)[1] <- "Scam Profiles"
	v <- cbind(head(rg,10), head(sg,10))
	print(xtable(v), include.rownames=F)
 }
```

Then we can compare the proportions for scam and real profiles.


```r
 kprop(real$gender, scam$gender)
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|male          | 0.5932076|male          | 0.6086635|
|female        | 0.4067924|female        | 0.3913365|

This doesn't look distinctive, both sets are about 60% male. Gender might be
important in combination with other variables, though. 


## Age

Initially, the age variable looks similarly unpromising. The mean of the scam
profiles is 42.39 compared to 40.33 
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
female | 31.47 |  41.22
male   | 49.41 |  39.72

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
|white            | 0.4387228|white            | 0.6610515|
|hispanic         | 0.3169158|native american  | 0.1106997|
|other            | 0.0709239|mixed            | 0.0710848|
|black            | 0.0563859|black            | 0.0633099|
|mixed            | 0.0512908|other            | 0.0405405|
|asian            | 0.0437500|asian            | 0.0199926|
|middle eastern   | 0.0103940|hispanic         | 0.0162903|
|native american  | 0.0087636|middle eastern   | 0.0153647|
|pacific islander | 0.0028533|pacific islander | 0.0016660|
 
We can see a couple of important differences. Firstly, the scam data has a
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

There's a long tail in occupation, some 4001 different
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
|other         | 0.1458430|military      | 0.2519525|
|self-employed | 0.0747794|engineering   | 0.2478778|
|engineering   | 0.0733860|self-employed | 0.0991511|
|technology    | 0.0543428|business      | 0.0560272|
|student       | 0.0524849|construction  | 0.0556876|
|retired       | 0.0503948|other         | 0.0390492|
|construction  | 0.0480725|contractor    | 0.0359932|
|service       | 0.0392476|medical       | 0.0322581|
|transport     | 0.0380864|manager       | 0.0190153|
|manual        | 0.0315838|sales         | 0.0169779|

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
|student       | 0.1886196|engineering   | 0.1714286|
|other         | 0.1728135|student       | 0.1428571|
|engineering   | 0.0684932|business      | 0.1142857|
|technology    | 0.0432034|architect     | 0.0857143|
|construction  | 0.0410959|military      | 0.0857143|
|service       | 0.0379347|carer         | 0.0571429|
|manual        | 0.0347734|government    | 0.0571429|
|hospitality   | 0.0337197|manager       | 0.0571429|
|self-employed | 0.0316122|other         | 0.0571429|
|finance       | 0.0263435|sales         | 0.0571429|

```r
kprop(real$occ_group[real$gender == 'male' & real$age > 30 & real$age <= 40], scam$occ_group[scam$gender == 'male' & scam$age > 30 & scam$age <= 40])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1725000|military      | 0.2744361|
|engineering   | 0.0841667|engineering   | 0.1691729|
|self-employed | 0.0633333|business      | 0.0789474|
|technology    | 0.0608333|self-employed | 0.0714286|
|construction  | 0.0516667|other         | 0.0563910|
|service       | 0.0450000|contractor    | 0.0488722|
|manual        | 0.0408333|real estate   | 0.0375940|
|transport     | 0.0358333|medical       | 0.0300752|
|hospitality   | 0.0316667|accounting    | 0.0225564|
|manager       | 0.0316667|finance       | 0.0225564|

```r
kprop(real$occ_group[real$gender == 'male' & real$age > 40 ], scam$occ_group[scam$gender == 'male' & scam$age > 40 ])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1191470|engineering   | 0.2570023|
|retired       | 0.1001391|military      | 0.2517033|
|self-employed | 0.1001391|self-employed | 0.1025738|
|engineering   | 0.0695410|construction  | 0.0601817|
|technology    | 0.0556328|business      | 0.0529902|
|transport     | 0.0500695|other         | 0.0370931|
|construction  | 0.0491423|contractor    | 0.0348221|
|service       | 0.0366249|medical       | 0.0329296|
|manager       | 0.0333797|manager       | 0.0193036|
|teacher       | 0.0259620|sales         | 0.0166540|

There's essentially just two groups here, the under-30s males, which are more likely to present as students 
and employees, and the over-30s males, which are much more likely to claim to be in the military or engineering
professions, with a smattering of other high-status occupations and a high incidence of self-employment. 


```r
kprop(real$occ_group[real$gender == 'female'], scam$occ_group[scam$gender == 'female'])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1501856|student       | 0.2055126|
|student       | 0.1015862|self-employed | 0.1644101|
|carer         | 0.0776240|carer         | 0.1025145|
|service       | 0.0644617|sales         | 0.0730174|
|clerical      | 0.0580493|military      | 0.0488395|
|teacher       | 0.0577118|fashion       | 0.0377176|
|retired       | 0.0489369|business      | 0.0367505|
|self-employed | 0.0408370|other         | 0.0362669|
|medical       | 0.0350996|finance       | 0.0343327|
|housewife     | 0.0317246|service       | 0.0323985|

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
|student       | 0.3538913|student       | 0.3170732|
|other         | 0.1556535|self-employed | 0.1463415|
|carer         | 0.0528634|carer         | 0.0819512|
|clerical      | 0.0440529|sales         | 0.0663415|
|service       | 0.0440529|fashion       | 0.0526829|
|self-employed | 0.0293686|other         | 0.0390244|
|finance       | 0.0279001|finance       | 0.0282927|
|teacher       | 0.0264317|service       | 0.0282927|
|medical       | 0.0249633|artist        | 0.0234146|
|housewife     | 0.0234949|business      | 0.0234146|

```r
kprop(real$occ_group[real$gender == 'female' & real$age > 30 & real$age <= 40], scam$occ_group[scam$gender == 'female' & scam$age > 30 & scam$age <= 40])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1557592|self-employed | 0.1877778|
|clerical      | 0.0824607|carer         | 0.1255556|
|service       | 0.0719895|student       | 0.1055556|
|teacher       | 0.0680628|sales         | 0.0822222|
|carer         | 0.0589005|military      | 0.0566667|
|student       | 0.0431937|business      | 0.0466667|
|academic      | 0.0392670|finance       | 0.0388889|
|sales         | 0.0392670|medical       | 0.0388889|
|self-employed | 0.0392670|service       | 0.0388889|
|housewife     | 0.0366492|other         | 0.0344444|

```r
kprop(real$occ_group[real$gender == 'female' & real$age > 40 ], scam$occ_group[scam$gender == 'female' & scam$age > 40 ])
```



|Real Profiles |      Freq|Scam Profiles |      Freq|
|:-------------|---------:|:-------------|---------:|
|other         | 0.1449275|military      | 0.2323944|
|carer         | 0.0981555|self-employed | 0.1478873|
|retired       | 0.0955204|carer         | 0.1056338|
|service       | 0.0698287|business      | 0.0704225|
|teacher       | 0.0665349|sales         | 0.0633803|
|clerical      | 0.0520422|engineering   | 0.0492958|
|self-employed | 0.0467721|finance       | 0.0492958|
|medical       | 0.0408432|retired       | 0.0352113|
|housewife     | 0.0329381|specialist    | 0.0352113|
|manager       | 0.0316206|student       | 0.0352113|

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


#combined_points <- ggplot() + world_map + geom_count(data=train,  aes(x= longitude, y=latitude, fill = profiles, color=profiles), alpha=0.5, shape = 21) + coord_fixed()
combined_points <- ggplot() + world_map + geom_point(data=train, aes(x=longitude, y=latitude, fill=profiles, color=profiles), shape=21, alpha=0.04) + stat_density_2d(data=train,  aes(x= longitude, y=latitude, colour=profiles), n=75, h=c(10,10)) + coord_fixed()

#png('combo.png')
#plot(combined_points)
#dev.off()
```

This combined plot is a little hard to read, but it can be helped by looking at the separate plots below:


```r
real_map <- ggplot() + world_map + geom_count(data=real,  aes(x = longitude, y = latitude), color="#F8766D", fill="#F8766D",  alpha=0.8, shape = 21, show.legend=F)
plot(real_map)
```

```
## Warning: Removed 1248 rows containing non-finite values (stat_sum).
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

```r
scam_map <- ggplot() + world_map + geom_count(data=scam,  aes(x = longitude, y = latitude), color="#00BFC4", fill="#00BFC4", alpha=0.8, shape = 21, show.legend=F)
plot(scam_map)
```

```
## Warning: Removed 224 rows containing non-finite values (stat_sum).
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
|Bogotá, Bogota, Colombia              | 0.0095668|New York, United States                | 0.0866098|
|Lima, Peru                            | 0.0071751|Los Angeles, California, United States | 0.0511869|
|London, UK                            | 0.0066967|London, United Kingdom                 | 0.0331973|
|Quito, Ecuador                        | 0.0066284|Dallas, Texas, United States           | 0.0166914|
|Lima, Perú                            | 0.0063551|Miami, Florida, United States          | 0.0166914|
|Bogotá, Bogotá, Colombia              | 0.0061501|Houston, Texas, United States          | 0.0153932|
|Panama City, Panama                   | 0.0060817|New York                               | 0.0120549|
|Cali, Cali, Valle del Cauca, Colombia | 0.0059451|Berlin, Germany                        | 0.0105712|
|Distrito de Lima, Perú                | 0.0059451|Austin, Texas, United States           | 0.0085312|
|Bogotá, Colombia                      | 0.0056034|Texas, United States                   | 0.0074184|

As expected, the scammers are more tightly clustered (9% in just New York, compared to the 
real users' largest clustering of 1%). Once again, we see the effects of the anti-hispanic bias
in the scammer profiles. This data is a bit fiddly, so it might be better to look at the results
by the broadest component of the location.


```r
kprop(real$country, scam$country)
```



|Real Profiles            |      Freq|Scam Profiles            |      Freq|
|:------------------------|---------:|:------------------------|---------:|
|United States of America | 0.3513955|United States of America | 0.7381228|
|Colombia                 | 0.0692548|UK                       | 0.0730012|
|México                   | 0.0531473|Deutschland              | 0.0254925|
|UK                       | 0.0449822|Canada                   | 0.0160294|
|Perú                     | 0.0412708|Australia                | 0.0125531|
|España                   | 0.0365202|РФ                       | 0.0090769|
|Venezuela                | 0.0288747|Ghana                    | 0.0086906|
|Canada                   | 0.0236787|Svizra                   | 0.0059869|
|РФ                       | 0.0236045|Україна                  | 0.0057937|
|Ecuador                  | 0.0201158|South Africa             | 0.0050212|

Once again, we see a massive concentration in scam profiles. Nearly three-quarters of them are purporting to be
in the US, compared with more like a third of the real data. The UK and Germany are the secondary targets.
The more honest answer of Ghana also makes the top list. Country names here are from the geocoding from the original
location data, which included more variation in reporting style. 

## Marital Status

Finally, the previously-overlooked marital status. Again a little bit of cleanup
needed.


```r
marry <- function(level){
	if (grepl('si[ng]{2}le', level)){
		return('single')
	} else if (grepl('win?doe?w', level)){
		return('widowed')
	} else if (grepl('married', level)){
		return('married')
	} else if (grepl('divorce', level)){
		return('divorced')
	} else if (grepl('sep[ae]?rat', level)){
		return('separated')
	} else if (grepl('relation|taken', level)){
		return('in relationship')
	}
	return('other')
}

 kprop(sapply(as.character(real$status), marry), sapply(as.character(scam$status), marry))
```



|Real Profiles   |      Freq|Scam Profiles   |      Freq|
|:---------------|---------:|:---------------|---------:|
|single          | 0.5669837|single          | 0.5129582|
|divorced        | 0.2067255|widowed         | 0.2837838|
|separated       | 0.0948370|divorced        | 0.1421696|
|other           | 0.0648098|other           | 0.0468345|
|widowed         | 0.0369565|separated       | 0.0118475|
|married         | 0.0154891|married         | 0.0022214|
|in relationship | 0.0141984|in relationship | 0.0001851|

Whilst both real and scam users were most commonly `single`, the scammers prefer
to present themselves as `widowed` rather than any of the other categories,
perhaps because this is the most heart-tugging option. Scammers are far less
likely than real users to have a dating profile which is married or in a
relationship, no doubt because this sort of dating is very niche and puts off
potential victims. The `other` category here includes where data was missing.
```
