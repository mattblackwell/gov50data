library(tidyverse)
library(lubridate)
library(ggthemes)
library(xtable)
library(humaniformat)
library(lfe)
library(stargazer)

options(contrasts = c("contr.treatment", "contr.treatment"))
theme_set(theme_few())
# setwd("archive_location/data/") ### Change to wherever archive is saved locally

### summary stats on ad timing and expenditures
load("fb_ads_meta.RData")
load("cands.RData")

cands <- cands %>% 
  ungroup %>%
  mutate(incumbent = replace(incumbent, is.na(incumbent), 0),
         office = recode_factor(office, us_sen = "US Senate", state_gov = "Governor", state_other = "Other Statewide", us_house = "US House", state_sen = "State Senate", state_house= "State House")
         )


### ad spending by office on FB ###
stack_ads <- stack_ads %>%
	mutate(
		office = recode_factor(office, us_sen = "US Senate", state_gov = "Governor", state_other = "Other Statewide", us_house = "US House", state_sen = "State Senate", state_house= "State House"),
		date_stop = replace(date_stop, is.na(date_stop), ymd("20181107"))
		) %>%
	filter(date_start <= date_stop)

stack_ads %>% summarize(n_unique_Creatives = length(unique(snapshot_id)),
						 n_unique_Pages = length(unique(page_id)),
						 n_unique_Candidates = length(unique(cand_id)),
						 n_unique_Races = length(unique(paste(state, office, district, sep="-")))) %>%
	gather %>%
	mutate(Item = sub("n_unique_", "N ", key)) %>%
	select(Item, Count = value) %>%
	xtable(caption = "Counts of unique creatives, pages, candidates, and races in the Facebook ads data.",
		 label = "tab:summ_stats_candcount_fb",
		 align = "llr",
		 digits=0) %>%
	print(
		file="summ_stats_candcount_fb.tex",
		  include.rownames=F,
		  hline.after = c(-1,-1,0,4,4))

cand_spend <- stack_ads %>%
	filter(!is.na(office)) %>%
	mutate(spend.mid = (spend.lb + spend.ub) / 2,
		   impressions.mid = (impressions.lb + impressions.ub) / 2) %>%
	group_by(cand_id, office) %>%
	summarize_at(.vars=vars(starts_with("spend"), starts_with("impressions")), .funs = sum)

fb_spend_dens <- ggplot(aes(group=office, x = spend.mid), data = cand_spend) + 
	geom_density(aes(colour=office)) +
	scale_colour_grey() +
	# annotate("text", label = paste("Aggregate spending: $", round(aggregate_spend/1e6,1),"M", sep=""), x = 4e+05, y = 0.6, size = 4) +
	scale_x_log10(limits=c(50,1e+07),
				  breaks=c(1e+02, 1e+03,1e+04, 1e+05,1e+06, 1e+07), 
				  labels=c("$100", "$1K", "$10K", "$100K", "$1M", "$10M")) +
	labs(x="Candidate Spending on FB", y = "Density")

ggsave(fb_spend_dens, file="fb_spend_dens.pdf", height=7, width=10)


cand_spend <- cand_spend %>% 
	inner_join(cands)

cand_spend %>% 
	group_by(office, incumbent) %>%
	summarize_at(.vars=vars(starts_with("spend"), starts_with("impressions")), .funs = funs(median(., na.rm=T))) %>%
	arrange(office, incumbent)

### fraction of candidates with positive advertising
cand_spend_0s <- cands %>%
	ungroup %>%
	left_join(cand_spend) %>%
	mutate_at(.vars=vars(starts_with("spend"), starts_with("impressions")), .funs = funs(replace(., is.na(.), 0)))

office_spend_0s <- cand_spend_0s %>%
	mutate(incumbent = recode_factor(incumbent, `0`="Challenger", `1`="Incumbent")) %>%
	group_by(incumbent, office) %>%
	summarize(Fraction_Spending = mean(spend.ub > 0)) 

fb_spend_frac <- 
	ggplot(data=office_spend_0s, aes(x=office, y=Fraction_Spending, group=incumbent)) +
	geom_bar(aes(fill=incumbent), position="dodge", stat="identity") +
	scale_fill_brewer(name = "Incumbency", palette="Paired") +
	labs(x="Office",y="Fraction Spending on FB") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fb_spend_frac, file="fb_spend_frac.pdf", height=7, width=10)


### ad timing on FB ###
allocate_time <- function(x, start, stop) {
	each_day <- seq(start, stop, by='days')
	tibble(date=each_day, val = x / length(each_day))
}

# this takes a bit
spend_timing <- stack_ads %>% 
	select(cand_id, state, office, party, incumbent, starts_with("spend"), contains("date")) %>%
	mutate(spend_allocated = pmap(.l=list(x = (spend.lb + spend.ub)/2, start=date_start, stop=date_stop), .f=allocate_time)) %>%
	unnest %>%
	group_by(cand_id, state, office, party, incumbent, primary_date, date) %>%
	summarize(fb_spend = sum(val))

fb_spend_time <- spend_timing %>%
	filter(!is.na(office)) %>%
	group_by(office, date) %>%
	summarize(fb_spend = sum(fb_spend)) %>%
	filter(date >= ymd("20180601"))


fb_spend_plot <- ggplot(data=fb_spend_time, aes(x=date, y=fb_spend, group=office)) +
	geom_line(aes(colour=office)) + 
	scale_colour_grey() +
	xlim(ymd("20180601"), ymd("20181106")) +
	scale_y_continuous(limits=c(0,1.5e+06),
				  breaks=seq(1e+05,1.5e+06,2e+05), 
				  labels= c("$100K", "$300K", "$500K", "$700K", "$900K", "$1.1M", "$1.3M", "$1.5M")) + 
	labs(x="Date", y="Daily Total FB Ad Spending")

ggsave(fb_spend_plot, file = "fb_spend_time.pdf", height=7, width=10)


#### TV ####
load('tv_ads.RData')  #### NOTE: not included in archive due to access restrictions (see README)

### ad spending by office on FB ###
tv_ads <- tv_ads %>% 
mutate(office = factor(office, levels=c("US Senate", "Governor", "Other Statewide", "US House", "State Senate", "State House")))

cand_spend_tv <- tv_ads %>%
	group_by(cand_id, office) %>%
	summarize_at(.vars=vars(starts_with("est")), .funs = funs(sum(., na.rm=T)))

ggplot(aes(x=office, y = estcost), data = cand_spend_tv) + 
	geom_boxplot() + 
	scale_y_log10()

tv_spend_dens <- ggplot(aes(group=office, x = estcost), data = cand_spend_tv) + 
	geom_density(aes(colour=office)) +
	scale_colour_grey() +
	# annotate("text", label = paste("Aggregate spending: $", round(aggregate_spend/1e6,1),"M", sep=""), x = 4e+05, y = 0.6, size = 4) +
	scale_x_log10(limits=c(50,5e+08),
				  breaks=c(1e+02, 1e+03,1e+04, 1e+05,1e+06, 1e+07, 1e+08), 
				  labels=c("$100", "$1K", "$10K", "$100K", "$1M", "$10M", "$100M")) +
	labs(x="Candidate Spending on TV", y = "Density")

ggsave(tv_spend_dens, file="tv_spend_dens.pdf", height=7, width=10)

cand_spend_tv <- cand_spend_tv %>% 
	left_join(cands)

cand_spend_tv %>% 
	group_by(office, incumbent) %>%
	summarize_at(.vars=vars(starts_with("est")), .funs = funs(median(., na.rm=T))) %>%
	arrange(office, incumbent)

### fraction of candidates with positive advertising
cand_spend_tv_0s <- cands %>%
	ungroup %>%
	left_join(cand_spend_tv) %>%
	mutate_at(.vars=vars(starts_with("est")), .funs = funs(replace(., is.na(.), 0)))

office_spend_tv_0s <- cand_spend_tv_0s %>%
	mutate(incumbent = recode_factor(incumbent, `0`="Challenger", `1`="Incumbent")) %>%
	group_by(incumbent, office) %>%
	summarize(Fraction_Spending = mean(estcost > 0)) 

tv_spend_frac <- 
	ggplot(data=office_spend_tv_0s, aes(x=office, y=Fraction_Spending, group=incumbent)) +
	geom_bar(aes(fill=incumbent), position="dodge", stat="identity") +
	scale_fill_brewer(name = "Incumbency", palette="Paired") +
	ylim(0,1)+
	labs(x="Office",y="Fraction Spending on TV") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(tv_spend_frac, file="tv_spend_frac.pdf", height=7, width=10)


### ad timing on TV ###
spend_timing <- tv_ads %>% 
	inner_join(cands, by=c("state","office","party","cand_id")) %>%
	select(cand_id, state, office, party, incumbent, starts_with("est"), contains("date")) %>%
	group_by(cand_id, state, office, party, incumbent, date) %>%
	summarize(tv_spend = sum(estcost))

tv_spend_time <- spend_timing %>%
	group_by(office, date) %>%
	summarize(tv_spend = sum(tv_spend)) %>%
	filter(date >= ymd("20180601"))


tv_spend_plot <- ggplot(data=tv_spend_time, aes(x=date, y=tv_spend, group=office)) +
	geom_line(aes(colour=office)) + 
	scale_colour_grey()+
	xlim(ymd("20180601"), ymd("20181106")) +
	scale_y_continuous(limits=c(0,1e+07),
				  breaks=seq(1e+06,1e+07,1e+06), 
				  labels= c("$1M","$2M","$3M","$4M","$5M","$6M","$7M","$8M","$9M", "$10M")) + 
	labs(x="Date", y="Daily Total TV Ad Spending")

ggsave(tv_spend_plot, file = "tv_spend_time.pdf", height=7, width=10)


