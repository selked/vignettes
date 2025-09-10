#install.packages("text2map")
#install.packages("gutenbergr")
library(ggrepel)
library(text2map)
library(text2vec)
library(gutenbergr)
library(tidyverse)
library(textclean)
library(stringi)
library(text2map.pretrained)
library(flextable)
library(huxtable)
library(sjPlot)
library(interactions)
library(car)

#Install package for pre-trained word embeddings
#remotes::install_gitlab("culturalcartography/text2map.pretrained")

#Download FastText Wiki News dataset (Only do this once)
#download_pretrained("vecs_fasttext300_wiki_news")

#Load the data you just downloaded (do this once per session)
data("vecs_fasttext300_wiki_news")

#Rename object for easier manipulation later
my_wv<-vecs_fasttext300_wiki_news
rm(vecs_fasttext300_wiki_news)

#Load job ads data
#load("ja_eschaton.rda")
#load("ja_finale.rda")
#load("ja_embedded.rda")
#load("ja_ideo.rda")
#load("ja_idfix.rda")
#load("ja_seeded.rda")
#load("ja_organ.rda")
#load("ja_processed.rda")
load("ja_reprocessed.rda")

#Build DTM with cleaned job ads
#Not quite sure why yet, but it seems that using the DTM with stopwords removed results in a less intuitive top warmth list
ja_dtm<-dtm_builder(workingjobs.df, string_lemma, doc_id)

warm_dtm<-dtm_builder(top_warm[7,], string_lemma, doc_id)
comp_dtm<-dtm_builder(top_comp[10,], string_lemma, doc_id)

#Build DTM that excludes stop words (code from Stoltz & Taylor)
dtm_ja <- workingjobs.df |>
  dtm_builder(string_lemma, doc_id) |>
  dtm_stopper(stop_list = get_stoplist("snowball2014"),
              stop_docprop = c(.01, Inf))

#Build DTM with stop words for individual ads
warm_dtm <- subset(top_warm, doc_id=="text4642")[1,] |>
  dtm_builder(string_lemma, doc_id) |>
  dtm_stopper(stop_list = get_stoplist("snowball2014"),
              stop_docprop = c(.01, Inf))

comp_dtm <- top_comp[1,] |>
  dtm_builder(string_lemma, doc_id) |>
  dtm_stopper(stop_list = get_stoplist("snowball2014"),
              stop_docprop = c(.01, Inf))

#Create embeddings matrix with all words in job ads that also occur in the pretrained embeddings

ja_vectors <- my_wv[rownames(my_wv) %in% colnames(ja_dtm), ]

#Stop words removed
ja_vectors <- my_wv[rownames(my_wv) %in% colnames(dtm_ja), ]

#Individual ads
ja_wv<-my_wv[rownames(my_wv) %in% colnames(warm_dtm),]
ja_cv<-my_wv[rownames(my_wv) %in% colnames(comp_dtm),]

#ja_vectors <- my_wv[rownames(my_wv) %in% colnames(dtm_ja), ]

#Read in words for SCM pairs
comp<-read.csv("test_competent.csv")
warm<-read.csv("test_warm.csv")

dj_seed<-read.csv("scm_ant.csv")

seed<-read.csv("seed_dictionaries.csv")

## Dakota & Jill's revised seedlist
## Remember to get rid of the row containing 'comp' and 'warm' if you work with this original file
scm_ant<-read_xlsx("scm_antonyms.xlsx")
scm_ant<-scm_ant[,1:2]

scm_ant$`Dakota's matched list`<-str_remove(scm_ant$`Dakota's matched list`, "DD: ")

names(scm_ant)[1]<-"comp"
names(scm_ant)[2]<-"warm"

scm_ant<-scm_ant[-c(1,22,31,62),]

scm_ant[38,2]<-"benevolence"

write.csv(scm_ant, file="scm_antonym_pairs.csv")

###

## work with the seedlist
warm_words<-subset(seed, Dictionary=="Sociability" & Dir=="high" | Dictionary=="Morality" & Dir=="high" )

comp_words<-subset(seed, Dictionary=="Ability" & Dir=="high" | Dictionary=="Agency" & Dir=="high")

warm_words<-subset(warm_words, term==unique(term))

comp_vec<-comp_words$term


warm_vec<-warm_words$term

scm_words<-data.frame(comp=comp_vec, warm=warm_vec)

comp_vec[69:74]<-"NA"

write.csv(scm_words, file="scm_seed_words.csv")

#Construct antonym pair vectors for competence

#I took 'dominance' out of the final spot in the 'additions' list so vector lengths match with 'adds' list

##Noticed on 3.7 that the competence list had the word 'foolish,' which doesn't quite make sense. I removed the final word from the warmth list, "good", to keep the vectors equal in length

additions<-c("competence", "competent", "competitive", "smart", "intelligent", "able", "skilled", "educated", "rational", "creative", "capable", "practical", "graceful", "imaginative", "critical", "discriminating", "wise", "efficient", "logical", "wisdom", "effective", "brilliant", "insightful", "ability", "agentic", "fearlessness", "fearless", "assertive", "secure", "active", "determined", "independent", "independence", "persistent", "enterprising", "energetic", "ambitious", "dedicated", "cautious", "resolute", "adventurous", "motivated", "untroubled", "autonomous", "dominant")

subtractions<-c("incompetence", "incompetent", "uncompetitive", "stupid", "unintelligent", "unable", "unskilled", "uneducated", "irrational", "uncreative", "incapable", "impractical", "clumsy", "unimaginative", "shrewd", "naive", "undiscriminating", "unwise", "inefficient", "illogical", "folly", "ineffective", "inept", "uninsightful", "inability", "diffident", "fearfulness", "fearful", "unassertive", "insecure", "inactive", "doubtful", "dependent", "dependence", "sporadic", "unenterprising", "lethargic", "unambitious", "undedicated", "impulsive", "wavering", "unadventurous", "unmotivated", "anxious", "helpless", "submissive", "submission")

#Construct antonym pair lists for warmth

adds <- c("sociability", "sociable", "friendliness", "friendly", "warm", "warmth", "likable", "pleasant", "unreserved", "outgoing", "sensitive", "affectionate", "caring", "sympathetic", "helpful", "supportive", "polite", "social", "popular", "agreeable", "hospitable", "welcoming", "considerate", "moral", "trustworthiness", "trustworthy", "sincere", "honest", "selfless", "softhearted", "loyal", "fair", "tolerant", "tolerance", "good", "kind", "right", "honorable", "incorrupt", "truthful", "cooperative", "forgiving", "reliable", "responsible", "unprejudiced")

subs <- c("unsociability", "unsociable", "unfriendliness", "unfriendly", "cold", "coldness", "unlikable", "unpleasant", "reserved", "shy",  "insensitive", "unaffectionate",  "uncaring", "unsympathetic", "unhelpful", "unsupportive", "impolite", "antisocial", "unpopular", "disagreeable", "inhospitable", "unwelcoming", "inconsiderate", "immoral", "untrustworthiness", "untrustworthy", "insincere", "dishonest", "selfish", "hardhearted", "disloyal", "unfair", "intolerant", "intolerance", "bad", "unkind", "wrong", "dishonorable", "corrupt", "untruthful", "uncooperative", "unforgiving", "unreliable", "irresponsible", "prejudiced", "evil")


##Note that these words need to be removed from the antonym vectors because they were not found in the pre-trained word embeddings:
##unindustrious; inconfident; inconfidence; unassertiveness; nonresilient


###Load in high-direction wordlists for all SADCAT words so we can get pooled semantic directions
high_pairs<-read.csv("dict_vecs.csv")

names(high_pairs)[1]<-"warmth"
names(high_pairs)[2]<-"competence"


##Create warmth-cold pairlist & calculate CMD
wpairs <- cbind(adds, subs)
sd_warm<-get_direction(wpairs, my_wv)
warm_closeness<- CMDist(dtm= ja_dtm, cv = sd_warm, wv= my_wv)


#Create competent-incompetent pairlist and calculate CMD
cpairs<-cbind(additions, subtractions)
sd_comp<- get_direction(pairs, my_wv)
doc_closeness <- CMDist(dtm = ja_dtm, cv = sd_comp, wv = my_wv)


#Create competence-warmth pairlist and calculate CMD
h_p<-cbind(additions,adds)
sd_stereo<-get_direction(h_p, my_wv, method="pooled")
closeness<- CMDist(dtm = ja_dtm, cv = sd_stereo, wv = ja_vectors)
#closeness<- CMDist(dtm = ja_dtm, cv = sd_stereo, wv = ja_vectors)

#Calculate CMD with original seedlist
sd_stereo<-get_direction(seed, my_wv, method="pooled")
closeness

#Calculate CMD with Dakota & Jill's list
sd_stereo<-get_direction(scm_ant, my_wv, method="paired")
closeness<-CMDist(dtm= ja_dtm, cv = sd_stereo, wv = ja_vectors)

#Rename column in output object
names(closeness)[2]<-"comp_sim"

##Check to see if last run of embeddings are accurate by testing whether these columns are identical
#all(workingjobs.df$comp_sim, closeness$competence_pole)

#Delineate centroids for warmth/competence regions & calculate CMD
warm_sd<-get_centroid(adds, my_wv)
comp_sd<-get_centroid(additions, my_wv)

w_close<-CMDist(dtm = ja_dtm, cv = warm_sd, wv = my_wv)
c_close<-CMDist(dtm=ja_dtm, cv=comp_sd, wv=my_wv)

#Rename output columns
names(w_close)[2]<-"warm_centroid"
names(c_close)[2]<-"comp_centroid"

#Attach centroid distances
workingjobs.df<-merge(workingjobs.df, w_close)
workingjobs.df<-merge(workingjobs.df, c_close)


#Create new OLS models with competence CMD as response variable
cmodel1<-lm(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate, workingjobs.df)

cmodel2<-lm(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+care_index3+mgmt_ind, workingjobs.df)

cmodel3<-lm(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+care_index3+mgmt_ind+femaleu:sal_div, workingjobs.df)

cmodel4<-lm(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+zdiverse+word.count+sal_rate+care_index3+mgmt_ind, workingjobs.df)

cmodel5<-lm(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+zdiverse+word.count+sal_rate+care_index3+mgmt_ind+femaleu:zdiverse, workingjobs.df)

summary(cmodel4)

#Create a list of all models as input for nested regression
cmodels<-list("Model 1" = cmodel1, "Model 2" = cmodel2, "Model 3" = cmodel3, "Model 4" = cmodel4, "Model 5" = cmodel5)

#Create a hugreg object for the Competence Models
ctab<-huxreg(cmodels, statistics = c('N' = "nobs",'R-squared' = "r.squared", "AIC" = "AIC", "-2x Log-likelihood"="logLik"), coefs = c("(Intercept)" ="(Intercept)","Proportion Woman"="femaleu", "Salary"="sal_div", "Management Skills"="mgmt_ind", "Care Skills"="care_index3","Proportion Woman * Salary"="femaleu:sal_div", "Diversity Commitment"="zdiverse", "Proportion Woman * Diversity Commitment"="femaleu:zdiverse", "Salary Interval: Hourly"="sal_ratehourly", "Vocational Preparation"="jobzone", "Proportion Asian"="asianu", "Proportion Black"="nhbu", "Proportion Hispanic"="hispu", "Other Race"="other.race", "Age"="ageu", "Age Squared"="age_square", "Word Count"="word.count"))%>%
  set_caption("Unstandardized Coefficients (and Standard Errors) from OLS Regression of Competence Similarity")


ctab<-as_flextable(ctab)
ctab

save_as_docx(
  "m1" = ctab,
  path = "C:/Users/selke/Desktop/Scholarship/USAJobs Project/WordEmbeddingRegs.docx")

#############Try the political ideology models with the embedding measure

#Embedding model: Ideological Rating
cmodel1 <- lmerTest::lmer(comp_sim~femaleu+ideo_rating+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+care_index3+mgmt_ind+sal_rate+(1|agency), data=test.df)

cmodel2 <- lmerTest::lmer(comp_sim~femaleu+ideo_rating+nhbu+hispu+asianu+other.race+sal_div+femaleu:ideo_rating+jobzone+ageu+age_square+word.count+care_index3+mgmt_ind+sal_rate+(1|agency), data=test.df)

cmodel3 <- lmerTest::lmer(comp_sim~femaleu*ideo_rating*sal_div+nhbu+hispu+asianu+other.race+jobzone+ageu+age_square+word.count+care_index3+mgmt_ind+sal_rate+(1|agency), data=test.df)


ci_models<-list("Model 1" = cmodel1, "Model 2" = cmodel2, "Model 3" = cmodel3)

ci_tab<-huxreg(ci_models, statistics = c('N' = "nobs", "AIC" = "AIC", "-2x Log-likelihood"="logLik"), coefs = c("(Intercept)" ="(Intercept)","Proportion Woman"="femaleu","Salary"="sal_div", "Management Skills"="mgmt_ind", "Care Skills"="care_index3", "Political Ideology"="ideo_rating", "Proportion Woman * Political Ideology"="femaleu:ideo_rating", "Proportion Woman * Political Ideology * Salary"="femaleu:ideo_rating:sal_div"))%>%
  set_caption("Table A2. Unstandardized Coefficients (and Standard Errors) from OLS Regression of Competence Similarity: Political Ideology")

ci_tab<-as_flextable(ci_tab)
ci_tab

save_as_docx(
  "m1" = ci_tab,
  path = "C:/Users/selke/Desktop/Scholarship/USAJobs Project/PolIdEmbeddingRegs.docx")


###Plot significant main effects & interactions
interact_plot(cmodel3, pred="femaleu", modx="sal_div", modx.values=c(170, 100, 30), mod2 = "ideo_rating", mod2.values = c(-2, 0, 2), x.label = "Proportion women", y.label="Predicted competence similarity", pred.labels = "Proportion Women", modx.labels = c("170k", "100k", "30k"), mod2.labels = c("Liberal (-2)", "Neutral (0)", "Conservative (+2)"), main.title = "Three-way interaction of Proportion Women, Salary, and Agency Ideology", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", bias_correction=TRUE)

plot_model(cmodel1, type="pred", terms=c("ideo_rating [-2, 0, 2]"),title="Predicted competence similarity by political ideology")  + xlab("Political Ideology") + ylab("Fitted competence similarity") + ylim(-1,1) #+ xlim(.05, .95)


### Full re-analysis with comp_sim (3.7.2025)
cmodel1<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+(1|agency), workingjobs.df)

cmodel2<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+care_index3+mgmt_ind+(1|agency), workingjobs.df)

cmodel3<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+care_index3+mgmt_ind+femaleu:sal_div+(1|agency), workingjobs.df)

cmodel4<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+ideo_rating+word.count+sal_rate+care_index3+mgmt_ind+(1|agency), workingjobs.df)

cmodel5<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+ideo_rating+word.count+sal_rate+care_index3+mgmt_ind+femaleu:ideo_rating+(1|agency), workingjobs.df)

cmodel6<-lmerTest::lmer(comp_sim~femaleu*ideo_rating*sal_div+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+ideo_rating+word.count+sal_rate+care_index3+mgmt_ind+(1|agency), workingjobs.df)

cmodels<-list("Model 1" = cmodel1, "Model 2" = cmodel2, "Model 3" = cmodel3, "Model 4" = cmodel4, "Model 5" = cmodel5, "Model 6" = cmodel6)


#Create hexreg object for our table display. Controls are included in these calculations, but we display only our independent variables.


c_tab<-huxreg(cmodels, 
              statistics = c('N' = "nobs", "AIC" = "AIC", "-2x Log-likelihood"="logLik"), coefs = c("(Intercept)" ="(Intercept)","Proportion Woman"="femaleu","Salary"="sal_div", "Management Skills"="mgmt_ind", "Care Skills"="care_index3", "Political Ideology"="ideo_rating", "Proportion Woman * Salary"="femaleu:sal_div", "Proportion Woman * Political Ideology"="femaleu:ideo_rating", "Proportion Woman * Political Ideology * Salary"="femaleu:ideo_rating:sal_div"))%>%
  set_caption("Table A2. Unstandardized Coefficients (and Standard Errors) from OLS Regressions of Competence Similarity")

#Turn these into flextable objects
c_tab<-as_flextable(c_tab)


## Take a look at our models

c_tab

save_as_docx(
   "m1" = c_tab,
   path = "C:/Users/selke/Desktop/Scholarship/USAJobs Project/PairedEmbeddingRegs.docx")

### Plot some models from this full re-analysis

plot_model(cmodel2, type="pred", terms=c("femaleu"),title="Predicted competence similarity by proportion women")  + xlab("Proportion women") + ylab("Fitted competence similarity") + ylim(-1,1)

plot_model(cmodel2, type="pred", terms=c("sal_div"),title="Predicted competence similarity by salary")  + xlab("Salary (divided by 1000)") + ylab("Fitted competence similarity") + ylim(-1,1)

plot_model(cmodel4, type="pred", terms=c("ideo_rating [-2, 0, 2]"),title="Predicted competence similarity by political ideology")  + xlab("Political Ideology") + ylab("Fitted competence similarity") + ylim(-1,1)

interact_plot(cmodel3, pred="femaleu", modx = "sal_div", modx.values = c(170,100,30), x.label = "Proportion women", y.label="Predicted competence similarity", pred.labels = "Proportion Women", modx.labels = c("170k", "100k", "30k"), main.title = "Interaction of Proportion Women and Salary", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE)

interact_plot(cmodel5, pred="femaleu", modx = "ideo_rating", modx.values = c(-1.5, 0, 1.5), x.label = "Proportion women", y.label="Predicted competence similarity", pred.labels = "Proportion Women", modx.labels = c("Liberal (-1.5)", "Neutral (0)", "Conservative (+1.5)"), main.title = "Interaction of Proportion Women and Agency Ideology", legend.main = "Political Ideology", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE) + ylim(-1,1)

interact_plot(cmodel6, pred="femaleu", modx="sal_div", modx.values=c(170, 100, 30), mod2 = "ideo_rating", mod2.values = c(-1.5, 0, 1.5), x.label = "Proportion women", y.label="Predicted competence similarity", pred.labels = "Proportion Women", modx.labels = c("170k", "100k", "30k"), mod2.labels = c("Liberal (-1.5)", "Neutral (0)", "Conservative (+1.5)"), main.title = "Interaction of Proportion Women, Salary, and Agency Ideology", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE)


##############
#Get a subset of most-warm and most-competent ads & plot them by prop. women for face validity check
c_sub<-workingjobs.df[order(workingjobs.df$comp_sim, decreasing = TRUE),]

c_sub<-c_sub[1:10,]

w_sub<-workingjobs.df[order(workingjobs.df$comp_sim, decreasing = FALSE),]

w_sub<-w_sub[1:10,]

wc_sub<-rbind(c_sub, w_sub)

plot1<-ggplot(wc_sub, aes (comp_sim, femaleu, label=cpsocc_title)) + geom_label_repel(max.overlaps = 16)

plot1 + coord_flip() + ylab("Proportion Women") + xlab("Semantic Proximity to Competence")


#Use a regex to create column with two-digit SOC codes
workingjobs.df$twodigit<-str_extract(workingjobs.df$soccps, "^\\d{2}")

workingjobs.df$threedigit<-str_extract(workingjobs.df$soccps, "^[:graph:]{4}")



#Stoltz and Taylor code to check the words that are most warm
warm_cen <- get_centroid(anchors = adds, wv = my_wv)
sims <- sim2(ja_vectors, warm_cen, method = "cosine")
sims_2<-sort(sims[1:20,1], decreasing = TRUE)

names(sims_2)




#Stoltz and Taylor code for visualizing top 30 words closest to each end of warm/comp pole
comp_warm <- get_direction(anchors = h_p, wv = my_wv, method="pooled")

#...with Dakota & Jill's list
comp_warm <- get_direction(anchors = scm_ant, wv = my_wv, method="paired")
row.names(comp_warm)[1]<-"competence_pole"

sim_dir <- sim2(comp_warm, ja_vectors, method = "cosine")
df_dir <- data.frame(competence_pole = sim_dir["competence_pole", ],
                     terms = colnames(sim_dir)) |>
  mutate(comp_warm_label = ifelse(
    competence_pole >= 0,
    "Competence", "Warmth"),
    comp_warm = abs(competence_pole)
  )

#### My attempt to get most warm words in representative job ad
scm_ant<-read.csv("scm_antonym_pairs.csv")

sim_dir <- sim2(comp_warm, ja_wv, method = "cosine")

df_dir <- data.frame(competence_pole = sim_dir["competence_pole",],
                     terms = colnames(sim_dir)) |>
  mutate(comp_warm_label = ifelse(
    competence_pole >= 0,
    "Competence", "Warmth"),
    comp_warm = abs(competence_pole)
  )

## Same for competence ad
sim_dir <- sim2(comp_warm, ja_cv, method = "cosine")

df_dir <- data.frame(competence_pole = sim_dir["competence_pole",],
                     terms = colnames(sim_dir)) |>
  mutate(comp_warm_label = ifelse(
    competence_pole >= 0,
    "Competence", "Warmth"),
    comp_warm = abs(competence_pole)
  )


#####


df_dir |>
  group_by(comp_warm_label) |>
  slice_max(comp_warm, n = 30) |>
  mutate(term = fct_reorder(terms, comp_warm)) |>
  ggplot(aes(term, comp_warm, fill = comp_warm_label, label = terms)) +
  geom_col() +
  guides(fill = "none") +
  labs(x = NULL, y = "Cosine Similarity to Pole") +
  coord_flip() +
  facet_wrap(~comp_warm_label, scale = "free")


#Create a vocab object of all words in the job ads
voc<-vocab_builder(workingjobs.df, string_lemma)

#Check to see if certain words are found in the job ad corpus
library(data.table)
any(voc %like% "loom")

summary(cmodel1)

comp_wom<-ggplot(wj, aes(zdiverse, ideo_rating^2)) + geom_point() #+geom_smooth()

comp_wom<-ggplot(test.df, aes(femaleu, cmodel1$fitted.values)) + geom_point() #+geom_smooth(method="lm")

comp_wom

plot_model(cmodel1, type="pred", terms="sal_div [30:170]", title="Predicted proximity to competence pole", show.data = TRUE) +xlab("Salary (divided by 1000)") + ylab("Fitted Similarity to Competence")

plot_model(cmodel1, type="pred", terms="femaleu", title="Predicted similarity to competence language", show.data=TRUE) + xlab("Proportion Women") + ylab("Fitted Similarity to Competence") +geom_smooth() #+ ylim(6,10)

plot_model(cmodel1, type="pred", terms="ideo_rating", title="Predicted similarity to competence language", show.data=TRUE) + xlab("Political Ideology") + ylab("Fitted Similarity to Competence") #+geom_smooth() #+ ylim(6,10)



comp_wom

workingjobs.df<-workingjobs.df[,-c(133:135)]
save(workingjobs.df, file="ja_embedded.rda")





###Some code for moving old packages over to new R installation
#Replace below with directory for older version of R
old_packages <- installed.packages(lib.loc ="C:/Users/selke/AppData/Local/R/win-library/4.4")

#Make sure that the new version of R is currently specified in the global options before running the following command
new_packages <- installed.packages()

#Create a dataframe of the packages that are in the old list but not the new one
missing_df <- as.data.frame(old_packages[
  !old_packages[, "Package"] %in% new_packages[, "Package"], 
])

#Install missing packages
install.packages(missing_df$Package, Ncpus = 3)



#######



###Messing around with Barnoulli Mixture Models
install.packages("comato")
library(comato)

#Random data generation, 100 observations, 5 dimensions, dependencies within the dimensions
data = cbind(round(runif(100)), round(runif(100)), round(runif(100)))
data = cbind(data, data[,2], 1-data[,3])

#Noisy data:
s = round(runif(2, 1, 100))
data[s, c(4,5)] = 1 - data[s, c(4,5)]

#MBMM Clustering
res = MBM.cluster(data, 1,8)

str(res)



##### Adding new skill indices
workingjobs.df<-merge(workingjobs.df, onetindices)

## Re-running full analysis with new skill indices & DEIA: 5.26.2025 (Updated 6.4.2025)

### Full re-analysis with comp_sim (3.7.2025)
nullmodel<-lmerTest::lmer(comp_sim~ 1+ (1|agency), workingjobs.df)

nullmodel<-lmerTest::lmer(comp_sim~ 1 + (1|agency/twodigit), workingjobs.df)

nullmodel2<-lmerTest::lmer(comp_sim~(1|twodigit), workingjobs.df)

nullmodel3<-lmerTest::lmer(comp_sim~(1|twodigit)+(1|agency), workingjobs.df)

mmodel1<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+(1|agency), workingjobs.df)

mmodel2<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

mmodel3<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex+femaleu:sal_div+(1|agency), workingjobs.df)

imodel1<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+rev_pol+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

imodel2<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+rev_pol+word.count+sal_rate+careindex+analyticindex+femaleu:rev_pol+(1|agency), workingjobs.df)

imodel3<-lmerTest::lmer(comp_sim~femaleu*rev_pol*sal_div+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+rev_pol+word.count+sal_rate+careindex+analyticindex+femaleu:rev_pol+femaleu:sal_div+femaleu:rev_pol:sal_div+(1|agency), workingjobs.df)

imodel4<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+zdiversity+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

imodel5<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+zdiversity+word.count+sal_rate+careindex+analyticindex+femaleu:zdiversity+(1|agency), workingjobs.df)

imodel6<-lmerTest::lmer(comp_sim~femaleu*zdiversity*sal_div+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

#cmodels<-list("Model 1" = cmodel1, "Model 2" = cmodel2, "Model 3" = cmodel3, "Model 4" = cmodel4, "Model 5" = cmodel5, "Model 6" = cmodel6)

m_models<-list("Null Model" = nullmodel, "Model 1" = mmodel1, "Model 2" = mmodel2, "Model 3" = mmodel3)

i_models<-list("Model 1" = imodel1, "Model 2" = imodel2, "Model 3" = imodel3, "Model 4"= imodel4, "Model 5" = imodel5, "Model 6"=imodel6)

#Create hexreg object for our table display. Controls are included in these calculations, but we display only our independent variables.


m_tab<-huxreg(m_models, 
              statistics = c('N' = "nobs", "AIC" = "AIC", "-2x Log-likelihood"="logLik"), coefs = c("(Intercept)" ="(Intercept)","Proportion Woman"="femaleu","Salary"="sal_div", "Care Skills"="careindex", "Analytic Skills"="analyticindex", "Proportion Woman * Salary"="femaleu:sal_div", "Non-Hispanic Black"="nhbu", "Hispanic"="hispu", "Asian"="asianu", "Other Race"="other.race", "Vocational Preparation"="jobzone", "Age"="ageu", "Age Squared"="age_square", "Word Count"="word.count", "Salary Rate: Hourly"="sal_ratehourly"))%>%
  set_caption("Table A2. Unstandardized Coefficients (and Standard Errors) from OLS Regressions of Competence/warmth Scale: Main Effects")

i_tab<-huxreg(i_models, 
              statistics = c('N' = "nobs", "AIC" = "AIC", "-2x Log-likelihood"="logLik"), coefs = c("(Intercept)" ="(Intercept)","Proportion Woman"="femaleu","Salary"="sal_div", "Care Skills"="careindex", "Analytic Skills"="analyticindex", "Agency Ideology"="rev_pol", "Agency Diversity Commitment"="zdiversity", "Proportion Woman * Salary"="femaleu:sal_div", "Proportion Woman * Policy Ideology"="femaleu:rev_pol","Policy Ideology * Salary"="rev_pol:sal_div", "Proportion Woman * Policy Ideology * Salary"="femaleu:rev_pol:sal_div", "Proportion Woman * Agency Diversity Commitment"="femaleu:zdiversity", "Agency Diversity Commitment * Salary"="zdiversity:sal_div", "Proportion Woman * Agency Diversity Commitment * Salary"="femaleu:zdiversity:sal_div", "Non-Hispanic Black"="nhbu", "Hispanic"="hispu", "Asian"="asianu", "Other Race"="other.race", "Vocational Preparation"="jobzone", "Age"="ageu", "Age Squared"="age_square", "Word Count"="word.count", "Salary Rate: Hourly"="sal_ratehourly"))%>%
  set_caption("Table A2. Unstandardized Coefficients (and Standard Errors) from OLS Regressions of Competence/warmth Scale: Agency Interactions")

#Turn these into flextable objects
#c_tab<-as_flextable(c_tab)
m_tab<-as_flextable(m_tab)
i_tab<-as_flextable(i_tab)

## Take a look at our models

#c_tab
m_tab
i_tab

save_as_docx(
  "m1" = m_tab,
  "m2" = i_tab,
  path = "C:/Users/selke/Desktop/Scholarship/USAJobs Project/ThreeDigit_RegsAndFigures.docx")

## Plot prop women coefficients w/ and w/out the skill controls
plot_coefs(cmodel1, cmodel2,
           coefs = c("Proportion Women" = "femaleu","Analytic Skills" = "analyticindex", "Care Skills" = "careindex"),
           scale = FALSE, # generates standardized coefficients when TRUE
           robust = FALSE, # robust standard errors when TRUE
           legend.title = "Predicted Competence Similarity",
           model.names = c("Without Gendered Skills","With Gendered Skills")) 


### Plot some models from this full re-analysis

plot_model(mmodel1, type="pred", terms=c("femaleu"),title="Predicted competence/warmth scale by proportion women")  + xlab("Proportion women") + ylab("Predicted competence/warmth scale") + ylim(-1,1)

plot_model(mmodel1, type="pred", terms=c("sal_div"),title="Predicted competence/warmth scale by salary")  + xlab("Salary (divided by 1000)") + ylab("Predicted competence/warmth scale") + ylim(-1,1) + xlim(15,195)

#This effect no longer significant (jk it is again as of 6.9.2025)
plot_model(imodel1, type="pred", terms=c("rev_pol [1.5, 0, -1.5]"),title="Predicted competence/warmth scale by political ideology")  + xlab("Political Ideology") + ylab("Predicted competence/warmth scale") + ylim(-1,1)

interact_plot(mmodel3, pred="femaleu", modx = "sal_div", modx.values = c(140,90,40), x.label = "Proportion women", y.label="Predicted competence/warmth scale", pred.labels = "Proportion Women", modx.labels = c("140k", "90k", "40k"), main.title = "Interaction of Proportion Women and Salary", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE) + ylim(-1,1)


# This effect no longer significant as of 6.9.2025
# JK it is now as of 6/19

interact_plot(imodel2, pred="femaleu", modx = "rev_pol", modx.values = c(-1.5, 0, 1.5), x.label = "Proportion women", y.label="Predicted competence/warmth scale", pred.labels = "Proportion Women", modx.labels = c("Conservative (-2)", "Neutral (0)", "Liberal (+2)"), main.title = "Interaction of Proportion Women and Agency Ideology", legend.main = "Political Ideology", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE) + ylim(-1,1)

# No longer significant as of 6.9.2025
# Significant as of 6.10.2025, following model syntax fix
interact_plot(imodel3, pred="femaleu", modx="sal_div", modx.values=c(140, 90, 40), mod2 = "rev_pol", mod2.values = c(-1.5, 0, 1.5), x.label = "Proportion women", y.label="Predicted competence/warmth scale", pred.labels = "Proportion Women", modx.labels = c("140k", "90k", "40k"), mod2.labels = c("Conservative (-1.5 SD)", "Neutral (0)", "Liberal (+1.5 SD)"), main.title = "Interaction of Proportion Women, Salary, and Policy Ideology", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE)+ylim(-2,2)

# No longer using zdeia as of 6/19, so that we don't have to theorize on EIA as well
#interact_plot(imodel6, pred="femaleu", modx="sal_div", modx.values=c(140, 90, 40), mod2 = "zdeia", mod2.values = c(-2, 0, 2), x.label = "Proportion women", y.label="Predicted competence similarity", pred.labels = "Proportion Women", modx.labels = c("140k", "90k", "40k"), mod2.labels = c("Low (-2 SD)", "Moderate (0)", "High (+2 SD)"), main.title = "Interaction of Proportion Women, Salary, and Agency DEIA Commitment", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE)+ylim(-2,2)

# With diversity variable
interact_plot(imodel5, pred="femaleu", modx="zdiversity",  x.label = "Proportion women", y.label="Predicted competence/warmth scale", modx.values = c(1.5, 0, -1.5), modx.labels = c("Low (-1.5 SD)", "Neutral (0)", "High (+1.5 SD)"), pred.labels = "Proportion Women", main.title = "Interaction of Proportion Women and Diversity Commitment", legend.main = "Diversity Commitment", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE)+ylim(-1,1)


interact_plot(imodel6, pred="femaleu", modx="sal_div", modx.values=c(140, 90, 40), mod2 = "zdiversity",  x.label = "Proportion women", y.label="Predicted competence/warmth scale", pred.labels = "Proportion Women", modx.labels = c("140k", "90k", "40k"), mod2.labels=c("Low (-1.5 SD)", "Neutral (0)", "High (+1.5 SD)"), main.title = "Interaction of Proportion Women, Salary, and Diversity Commitment", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE)+ylim(-2,2)




# DEIA Three-way
#interact_plot(cmodel6, pred="femaleu", modx="sal_div", modx.values=c(170, 100, 30), mod2 = "zdeia", mod2.values = c(-2, 0, 2), x.label = "Proportion women", y.label="Predicted competence similarity", pred.labels = "Proportion Women", modx.labels = c("170k", "100k", "30k"), mod2.labels = c("Low (-2 SD)", "Neutral (0)", "High (+2 SD)"), main.title = "Interaction of Proportion Women, Salary, and DEIA Committment", legend.main = "Salary", outcome.scale = "response", colors = "Qual2", interval=TRUE, bias_correction=TRUE,)


### Linear Hypothesis Testing for Skill Mediation

mmodel1<-comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+(1|agency)

mmodel1.5<-lm(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate, workingjobs.df)

mmodel2<-comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex+(1|agency)

mmodel2.5<-lm(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex, workingjobs.df)

mmodel3<-feols(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate, workingjobs.df)

mmodel4<-feols(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex, workingjobs.df)

fitsur<- systemfit(list(noskills=mmodel1, skills=mmodel2), data=workingjobs.df)

restriction<- "noskills_femaleu- skills_femaleu"

sur_hypotheses(
  list(mmodel3, mmodel4), cluster="agency",
  hypothesis = "femaleu_1-femaleu_2=0")




mmodel1$ests

####
comp_cog<-ggplot(workingjobs.df, aes(cogindex, comp_sim)) + geom_point() +geom_smooth(method="loess")
comp_cog

#### Checking correlation of zdiverse & political ideology

wj<-subset(workingjobs.df, agency!="NA" & ideo_rating!="NA" & zdiverse!="NA")

cor.test(wj$zdiverse,wj$ideo_rating, method="spearman")

cor(wj$zdiverse,wj$ideo_rating, method="spearman")

cor(wj$zdiverse,wj$ideo_rating, method="kendall")

workingjobs.df<- workingjobs.df %>% 
mutate(zdeia = scale(deia, center=TRUE, scale=TRUE))


### Fixing the random H's that 'with's get turned into

# Get subset of ads that have an 'h' surrounded by word breaks
check<-workingjobs.df[grep("\\bh\\b", workingjobs.df$string_lemma),]

# Try replacing all lone H's with the word 'with', as all the instances appear where a 'with' is expected
check$string_lemma<-str_replace_all(check$string_lemma, "\\bh\\b", "\\with\\")

# Looks good at first glance, so let's do it to the dataframe
workingjobs.df$string_lemma<-str_replace_all(workingjobs.df$string_lemma, "\\bh\\b", "\\with\\")



### Making the top- warm and comp ad subsets

top_comp <- subset(workingjobs.df, comp_sim>2 & comp_sim<2.5, select = c("doc_id", "agency", "comp_sim", "sococc_title", "soccps", "duties", "MajorDuties", "string_lemma", "word.count", "unique_words"))

top_warm <- subset(workingjobs.df, comp_sim<(-2) & comp_sim>(-2.5), select = c("doc_id", "agency", "comp_sim","sococc_title", "soccps", "duties", "MajorDuties", "string_lemma", "word.count", "unique_words"))
  


workingjobs.df$scm_label <- ifelse(workingjobs.df$comp_sim>0, "comp", "")
workingjobs.df$scm_label <- ifelse(workingjobs.df$comp_sim<0, "warm", workingjobs.df$scm_label)



### Checking the pol_id results with count data 8.7.2025
nullmodel<-lmerTest::lmer(comp_sim~ 1+ (1|agency), workingjobs.df)

nullmodel<-lmerTest::lmer(comp_sim~ 1 + (1|agency/twodigit), workingjobs.df)

nullmodel2<-lmerTest::lmer(comp_sim~(1|twodigit), workingjobs.df)

nullmodel3<-lmerTest::lmer(comp_sim~(1|twodigit)+(1|agency), workingjobs.df)

mmodel1<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+(1|agency), workingjobs.df)

mmodel2<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

mmodel3<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex+femaleu:sal_div+(1|agency), workingjobs.df)

imodel1<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+rev_pol+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

imodel2<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+rev_pol+word.count+sal_rate+careindex+analyticindex+femaleu:rev_pol+(1|agency), workingjobs.df)

imodel3<-lmerTest::lmer(comp_sim~femaleu*rev_pol*sal_div+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+rev_pol+word.count+sal_rate+careindex+analyticindex+femaleu:rev_pol+femaleu:sal_div+femaleu:rev_pol:sal_div+(1|agency), workingjobs.df)

imodel4<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+zdiversity+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

imodel5<-lmerTest::lmer(comp_sim~femaleu+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+zdiversity+word.count+sal_rate+careindex+analyticindex+femaleu:zdiversity+(1|agency), workingjobs.df)

imodel6<-lmerTest::lmer(comp_sim~femaleu*zdiversity*sal_div+nhbu+hispu+asianu+other.race+sal_div+jobzone+ageu+age_square+word.count+sal_rate+careindex+analyticindex+(1|agency), workingjobs.df)

#cmodels<-list("Model 1" = cmodel1, "Model 2" = cmodel2, "Model 3" = cmodel3, "Model 4" = cmodel4, "Model 5" = cmodel5, "Model 6" = cmodel6)

m_models<-list("Null Model" = nullmodel, "Model 1" = mmodel1, "Model 2" = mmodel2, "Model 3" = mmodel3)

i_models<-list("Model 1" = imodel1, "Model 2" = imodel2, "Model 3" = imodel3, "Model 4"= imodel4, "Model 5" = imodel5, "Model 6"=imodel6)

#Create hexreg object for our table display. Controls are included in these calculations, but we display only our independent variables.


m_tab<-huxreg(m_models, 
              statistics = c('N' = "nobs", "AIC" = "AIC", "-2x Log-likelihood"="logLik"), coefs = c("(Intercept)" ="(Intercept)","Proportion Woman"="femaleu","Salary"="sal_div", "Care Skills"="careindex", "Analytic Skills"="analyticindex", "Proportion Woman * Salary"="femaleu:sal_div", "Non-Hispanic Black"="nhbu", "Hispanic"="hispu", "Asian"="asianu", "Other Race"="other.race", "Vocational Preparation"="jobzone", "Age"="ageu", "Age Squared"="age_square", "Word Count"="word.count", "Salary Rate: Hourly"="sal_ratehourly"))%>%
  set_caption("Table A2. Unstandardized Coefficients (and Standard Errors) from OLS Regressions of Competence/warmth Scale: Main Effects")

i_tab<-huxreg(i_models, 
              statistics = c('N' = "nobs", "AIC" = "AIC", "-2x Log-likelihood"="logLik"), coefs = c("(Intercept)" ="(Intercept)","Proportion Woman"="femaleu","Salary"="sal_div", "Care Skills"="careindex", "Analytic Skills"="analyticindex", "Agency Ideology"="rev_pol", "Agency Diversity Commitment"="zdiversity", "Proportion Woman * Salary"="femaleu:sal_div", "Proportion Woman * Policy Ideology"="femaleu:rev_pol","Policy Ideology * Salary"="rev_pol:sal_div", "Proportion Woman * Policy Ideology * Salary"="femaleu:rev_pol:sal_div", "Proportion Woman * Agency Diversity Commitment"="femaleu:zdiversity", "Agency Diversity Commitment * Salary"="zdiversity:sal_div", "Proportion Woman * Agency Diversity Commitment * Salary"="femaleu:zdiversity:sal_div", "Non-Hispanic Black"="nhbu", "Hispanic"="hispu", "Asian"="asianu", "Other Race"="other.race", "Vocational Preparation"="jobzone", "Age"="ageu", "Age Squared"="age_square", "Word Count"="word.count", "Salary Rate: Hourly"="sal_ratehourly"))%>%
  set_caption("Table A2. Unstandardized Coefficients (and Standard Errors) from OLS Regressions of Competence/warmth Scale: Agency Interactions")

#Turn these into flextable objects
#c_tab<-as_flextable(c_tab)
m_tab<-as_flextable(m_tab)
i_tab<-as_flextable(i_tab)

## Take a look at our models

#c_tab
m_tab
i_tab
