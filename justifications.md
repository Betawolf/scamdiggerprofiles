# Justification Analysis

The moderators that identify the scam profiles give justification tags to explain their reasoning.
It seems worthwhile to explore that reasoning here.


```r
 justifications <- read.csv("justifications.csv", na.strings=c('NA',''))
```

There are 26108 justifications but just 2098 unique responses
for 3468 profiles, indicating that there is a high level of justification 
reuse -- which means we can group and analyse them meaningfully.


```r
 justifications$justifications <- tolower(justifications$justifications)
 justifications$contradict_location <- justifications$justifications %in% c('ip doesn’t correspond location','contradiction in location','registered on different sites with different location')
 justifications$masking_ip <- grepl('(proxy|vpn)',justifications$justifications)
 justifications$ip_location <- grepl('ip is',justifications$justifications) & ! justifications$masking_ip
 justifications$picture_known <- grepl('picture.*(scams|known|stock|found)', justifications$justifications)
 justifications$picture_contradict <- grepl('picture.*(different|correspond)', justifications$justifications)
 justifications$mass_mail <- grepl('mass.*mail', justifications$justifications)
 justifications$listed <- grepl('see here|romancescam|list', justifications$justifications) | (grepl('known', justifications$justifications) & ! justifications$picture_known) 
 justifications$inconsistencies <- grepl('different|contradict', justifications$justifications) & ! justifications$picture_contradict
 justifications$language <- grepl('wording|language|spell|description', justifications$justifications) & ! justifications$inconsistencies
	
 justdisplay <- function(jdf){

	 sumscams <- function(whc){
		length(unique(jdf$username[whc]))
	 }


	labels <- c('The IP address contradicts the given location', 
		    'The IP address is a proxy',
		    'The IP address is a suspicious location',
		    'The picture is known to be used in scams.',
		    'The picture contradicts other elements of the profile',
		    'The user was mass-mailing other users',
		    'This person has been previously identified as a scammer',
		    'The profile is self-contradictory',
		    'The profile description uses suspicious language')

	sums <- c( 	sumscams(jdf$contradict_location),
			sumscams(jdf$masking_ip),
			sumscams(jdf$ip_location),
			sumscams(jdf$picture_known),
			sumscams(jdf$picture_contradict),
			sumscams(jdf$mass_mail),
			sumscams(jdf$listed),
			sumscams(jdf$inconsistencies),
			sumscams(jdf$language))

	props <- c(round(sums/sumscams(T),3))
	df <- data.frame(reason=labels, count=sums, prop=props)
	df <- df[with(df, order(count, decreasing=T)),]
	rownames(df) <- NULL
	kable(df)
 }
```

## All Justifications


```r
 justdisplay(justifications)
```



|reason                                                  | count|  prop|
|:-------------------------------------------------------|-----:|-----:|
|The IP address contradicts the given location           |  3030| 0.874|
|The profile description uses suspicious language        |  2499| 0.721|
|The IP address is a proxy                               |  2156| 0.622|
|The picture is known to be used in scams.               |  1379| 0.398|
|This person has been previously identified as a scammer |  1368| 0.394|
|The profile is self-contradictory                       |  1145| 0.330|
|The IP address is a suspicious location                 |   968| 0.279|
|The user was mass-mailing other users                   |   761| 0.219|
|The picture contradicts other elements of the profile   |   261| 0.075|

It looks like the IP of the scammers is a major tool for the moderators. 87% of
the scam profiles were identified on the basis of their IP not corresponding to
their location, and 72% on the basis of their IP being masked. This is of
course overlapping -- all masking IPs were also flagged as non-corresponding. 

Descriptions and language matching previous scammers were also a common feature,
as well as profile reuse from other lists or moderators' own knowledge --
including picture reuse. 

Self-contradictory features cover about a third of the profile information,
slightly more if you include justifications covering pictures that contradict
given profile information or other pictures. 

Behaviour-based justifications are present only for about 20% of the scammers,
which together with the high incidence of IP-based judgement suggests that the
majority of the scammers are caught during the initial registration steps. 

## Error Analysis

It might be worth looking at the false negatives from our ensemble predictions,
to see what the justifications are for the profiles we missed. (Using labels
from my implementation of the ensemble, with 64 false negatives). 


```r
 valid <- read.csv("validationlabels.csv")
 fns <- valid$file[valid$ensemble == 0 & valid$truth == 1]
 jfns <- justifications[justifications$file %in% fns,]
 justdisplay(jfns)
```



|reason                                                  | count|  prop|
|:-------------------------------------------------------|-----:|-----:|
|The IP address contradicts the given location           |    44| 0.846|
|The profile description uses suspicious language        |    34| 0.654|
|The IP address is a proxy                               |    25| 0.481|
|The IP address is a suspicious location                 |    22| 0.423|
|The picture is known to be used in scams.               |    17| 0.327|
|This person has been previously identified as a scammer |    13| 0.250|
|The profile is self-contradictory                       |    12| 0.231|
|The user was mass-mailing other users                   |    10| 0.192|
|The picture contradicts other elements of the profile   |     4| 0.077|

The judgements which rely on IP addresses are of course invisible to our
classifiers. The others don't seem particularly altered from the ratios in the
general population, so I don't think any particular bias has been introduced.

For the curious, see below for some breakdowns for the specific justifications
for false negatives.

Within the language category:


```r
kable(as.data.frame(sort(table(jfns$justifications[jfns$language]), decreasing=T)))
```



|Var1                                                                | Freq|
|:-------------------------------------------------------------------|----:|
|nigerian wording                                                    |   28|
|profile description is widely used in scams                         |    8|
|profile description was used in scams before                        |    3|
|profile description is in bad spanish run through online translator |    2|
|name “nick” misspelled                                              |    1|

It's neat to see the online translation angle Claudia was interested in showing
up again here. 

Within the 'known pictures' category:


```r
kable(as.data.frame(sort(table(jfns$justifications[jfns$picture_known]), decreasing=T)))
```



|Var1                                                                          | Freq|
|:-----------------------------------------------------------------------------|----:|
|pictures are stolen from a known source                                       |    6|
|pictures are widely used in scams                                             |    6|
|picture is a stock photo                                                      |    2|
|pictures are stolen from a known source, from american man john m.            |    2|
|picture is stolen from a known source                                         |    1|
|picture is widely used in 419 scams                                           |    1|
|pictures are stolen from a known source, from a model and blogger charissa c. |    1|
|pictures are stolen from a known source, from analyst jesse b.                |    1|
|pictures are stolen from a known source, from businesswoman wendy a.          |    1|
|pictures are stolen from a known source, from model cindy prado               |    1|
|pictures are stolen from a known source, from ukrainian girl angelica         |    1|

Within the 'known person' category:


```r
kable(as.data.frame(sort(table(jfns$justifications[jfns$listed]), decreasing=T)))
```



|Var1                                                          | Freq|
|:-------------------------------------------------------------|----:|
|also see here:                                                |    7|
|also see here (same scammer):                                 |    3|
|also scamming on craigslist with email rayscott2008@gmail.com |    2|
|known scammer from senegal                                    |    2|
|http://www.romancescam.com/forum/viewtopic.php?f=1&t=25400    |    1|
|http://www.romancescam.com/forum/viewtopic.php?f=1&t=27393    |    1|
|http://www.romancescam.com/forum/viewtopic.php?f=3&t=12003    |    1|
|http://www.romancescam.com/forum/viewtopic.php?f=3&t=45913    |    1|
|http://www.romancescam.com/forum/viewtopic.php?f=3&t=76197    |    1|
|http://www.romancescam.com/forum/viewtopic.php?f=91&t=97227   |    1|
|http://www.romancescam.com/forum/viewtopic.php?t=12845        |    1|
|known 419 scammer                                             |    1|

(The 'also see here' is due to how the justifications are parsed -- all 7 of the
links for identifying the scammers are listed below.)

Within the self-contradictory category:


```r
kable(as.data.frame(sort(table(jfns$justifications[jfns$inconsistencies]), decreasing=T)))
```



|Var1                                                              | Freq|
|:-----------------------------------------------------------------|----:|
|registered on different sites with different details              |   14|
|contradiction in location                                         |    2|
|registered on different sites with different details and names    |    2|
|contradiction in gender                                           |    1|
|registered on different sites with different details and pictures |    1|


Within the picture inconsistency category:


```r
kable(as.data.frame(sort(table(jfns$justifications[jfns$picture_contradict]), decreasing=T)))
```



|Var1                                          | Freq|
|:---------------------------------------------|----:|
|picture doesn’t correspond her claimed race   |    2|
|picture doesn’t correspond “her” claimed age  |    1|
|picture doesn’t correspond “her” claimed race |    1|
|picture doesn’t correspond his claimed age    |    1|





