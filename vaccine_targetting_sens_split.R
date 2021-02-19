
library(readxl)
library(XLConnect)
library(ggplot2)
library(extrafont)
# loadfonts(device = "win")  # this is used to make ggplots with different font styles - not absolutely necessay

#read data from excel file
setwd("C:/covid_vaccines")     ## set wd to the location of the data files
load("data_CoMo.RData")
country_name<-"Uganda"
country_name2 <-"Uganda"

# population
wpop<-read.table("worldpop.txt")        # population age distributions for the whole world
pop<-population$country==country_name2   # UK population
pp<-population$pop[pop]                 # pp is the UK pop age structure
ihr<-read.table('ihr.txt')   # IHR
hfr<-read.table('hfr.txt')   # HFR
parameters<-read.csv('parameters_split.csv')

# deaths data
deaths<-read.table("UK_deaths.txt")     # Covid-19 deaths up to Nov 12th 2020
age_groups<-c("under 4","5 to 9", "10 to 14", "15 to 19", "20 to 24", 
              "25 to 29",  "30 to 34", "35 to 39", "40 to 44",
              "45 to 49","50 to 54", "55 to 59", "60 to 64",
              "65 to 69","70 to 74", "75 to 79", "80 to 84",
              "85 to 89","90 and over")   # generates the list of age ranges for which we have ONS data
death_age<-as.data.frame(cbind((age_groups),deaths))   #  generate a data frame with age groups as column 1 and deaths as column 2
colnames(death_age)<-c("Age_group", "Deaths")          # name the columns
death_age$Age_group<-factor(death_age$Age_group,levels=c("under 4","5 to 9", "10 to 14", "15 to 19", "20 to 24", 
                                                         "25 to 29",  "30 to 34", "35 to 39", "40 to 44",
                                                         "45 to 49","50 to 54", "55 to 59", "60 to 64",
                                                         "65 to 69","70 to 74", "75 to 79", "80 to 84",
                                                         "85 to 89","90 and over")) # this sorts the age groups in the order we want to plot them in

# makes bar plot of death by age
g<-ggplot(data=death_age,aes(x=Age_group,y = Deaths, group = Age_group))+ geom_bar(stat="identity") 

# age_group population sizes
breakpoints<-seq(4,90, by =5)
age_group_pop<-c()   # initialise vector of pop sizes for each age group

aci <- floor((breakpoints[1]/5)+1) # age class of first breakpoint 
age_group_pop<-c(age_group_pop,sum(pp[1:aci])) # adds the desired population size to the age group pop sizes vector

for (i in 2:length(breakpoints)){  # starts on two since we already have the first element filled out
  aci <- floor((breakpoints[i-1]/5)+1) # what is the upper limit of the previous age_group
  aci2 <- floor((breakpoints[i]/5)+1)   # what is the lower limit of the current age_group
  age_group_pop<-c(age_group_pop,sum(pp[(aci+1):aci2])) # adds the desired population size to the age group pop sizes vector
}
aci <- floor((breakpoints[length(breakpoints)]/5)+1) # age class of last breakpoint 
age_group_pop<-c(age_group_pop,sum(pp[(aci+1):length(pp)])) # adds over 95s pop sizes


# add colum with numbers of people per age group to the data frame
death_age<-cbind(death_age,age_group_pop)
colnames(death_age)<-c("Age_group", "Deaths","People") 
## Calculate the risk of dying  per person per month for age group - depends on the data collection time
death_age$DeathHazard<-death_age$Deaths/death_age$People/9


###################   GENERATE INDIVIDUALS   #################################################
#sample age from age distribution
samplesize=1e6
ages=with(death_age,sample(length(death_age$Age_group),samplesize,p=death_age$People,replace=TRUE)) # choose a bin
# barplot(death_age$People/sum(death_age$People))
# hist(ages,breaks=seq(0.5,19.5,by=1),freq=F,add=T,bord=3)

#sample susceptibility from gamma distribution
susc=rgamma(samplesize, shape=1.25, scale = 0.065)
# hist(susc)


#serostatus by age group - sample from age dependent seroprevalence
#seroprevalence data
sero<-read.table("UK_sero.txt")

#adjust by looking at contact matrices
c_home <- rowSums(as.matrix(contact_home[[country_name]]))
c_school <- rowSums(as.matrix(contact_school[[country_name]]))
c_work <- rowSums(as.matrix(contact_work[[country_name]]))
c_other <- rowSums(as.matrix(contact_other[[country_name]]))

nce <-max(ages)-length(c_home)

contact_home<-c(c_home,rep(c_home[length(c_home)],nce))
contact_school<-c(c_school,rep(c_school[length(c_school)],nce))
contact_work<-c(c_work,rep(c_work[length(c_work)],nce))
contact_other<-c(c_work,rep(c_work[length(c_work)],nce))

c_all<- contact_home+contact_school+contact_work+contact_other
c_all_norm<- c_all/mean(c_all)
# barplot(c_all_norm)
# title('Relative expected prevalence per age group')


## SEROPREVALENCE BY START OF VACCINE DELIVERY PROGRAM
prev<-0.12
sero_base<-ages
sero_age<-death_age[,c(1,3)]
sero_age$Prob<-sero_age$People*prev*c_all_norm
sero_age$Prev<-(sero_age$Prob)/(sero_age$People)
prev_sim<-sum(sero_age$Prob)/sum(sero_age$People)

## serostatus at baseline for each person
for (i in 1:max(ages)){
  ww<-which(ages==i)
  sero_base[ww]<-runif(length(ww))<sero_age$Prev[i]
}
hist(sero_base)
sum(sero_base)


#####################################################################################################################
####
####        MAIN INPUTS
####
#####################################################################################################################

# for (j in 1:dim(parameters)[1]){
# for (j in 1:1){
  #sample doses taken per age group - oldest first
  v1<-sort(ages,decreasing=T,index.return=T)
  prct_2dose<-parameters$Dose_split[j]
  prct_1dose<-1-prct_2dose
  cumsum(rev(age_group_pop))
  totaldoses<-samplesize*parameters$Target_doses[j]
  targetted_pop<-floor(totaldoses/(1+(prct_2dose/(prct_1dose+prct_2dose))))
  refusals<-targetted_pop*0.05   
  
  agesort<-v1$ix[1:(targetted_pop+refusals)]
  doses<-rep(0,samplesize)
  agesort_rand<-sort(agesort)
  doses[agesort_rand[1:round(prct_1dose*targetted_pop)]]<-1
  doses[agesort_rand[round(prct_1dose*targetted_pop):round((prct_1dose+prct_2dose)*targetted_pop)]]<-2
  
  # dosing schedule
  timelimit<-180
  prct_1dose<-0.3
  vac_coverage<-0.8
  first_day_doses<-1e3
  lin_doses<-c()
  for(i in 1:timelimit){
    lin_doses[i] <- totaldoses - (timelimit-i)*(totaldoses)/(timelimit)
  }
  # plot(lin_doses)
  
  exp_doses<-c()
  for(i in 1:timelimit){
    exp_doses[i] <- (totaldoses)*(1 - exp(-1/timelimit*5.85*(i))) + first_day_doses
  }
  # exp_doses
  
  exp_doses2<-c()
  for(i in 1:timelimit){
    exp_doses2[i] <- (totaldoses*0.001)*(exp(1/timelimit*6.905*(i))) 
  }
  # plot(exp_doses2,col="red",type="l")
  # lines(exp_doses,col="dodgerblue2")
  # lines(lin_doses,col="green3")
  
  
  df_doses<-as.data.frame(cbind(1:timelimit,lin_doses,exp_doses,exp_doses2))
  colnames(df_doses)<-c("Time","Linear","Exponential","Exponential2")
  df_doses$lin<-df_doses$Linear
  df_doses$exp<-df_doses$Exponential
  df_doses$exp2<-df_doses$Exponential2
  for(i in 2:timelimit){
    df_doses$exp[i] <- ceiling(df_doses$Exponential[i] - df_doses$Exponential[i-1])
    df_doses$exp2[i]<- ceiling(df_doses$Exponential2[i] - df_doses$Exponential2[i-1])
    df_doses$lin[i] <- ceiling(df_doses$Linear[i] - df_doses$Linear[i-1])
  }
  # plot(df_doses$exp2,col="red",type="l")
  # lines(df_doses$exp,col="dodgerblue2")
  # lines(df_doses$lin,col="green3")
  
  dose1<-which(doses==1)
  dose2<-which(doses==2)
  speed<-parameters$Delivery[j]
  if(speed==1){dose_t=with(df_doses,sample(length(df_doses$Time),length(c(dose1,dose2)),p=df_doses$lin,replace=TRUE))}
  if(speed==2){dose_t=with(df_doses,sample(length(df_doses$Time),length(c(dose1,dose2)),p=df_doses$exp,replace=TRUE))}
  if(speed==3){dose_t=with(df_doses,sample(length(df_doses$Time),length(c(dose1,dose2)),p=df_doses$exp2,replace=TRUE))}
  dt<-sort(dose_t)
  da<-sort(ages[c(dose1,dose2)],decreasing=T,index.return=T)
  dose_timing1<-rep(0,length(dt))
  dose_timing1[da$ix]<-dt
  time_dose1<-rep(0,samplesize)
  ww<-which(doses>0)
  time_dose1[ww[da$ix]]<-dt
  ww2<-which(doses==2)
  
  # define time between doses
  dose_interval_fixed=21
  dose_interval_sample <- parameters$Boost_interval[j]-1+ceiling(parameters$Boost_interval[j]*rbeta(length(dose2), shape1 = 0.15, shape2=0.95))
  # hist(dose_interval_sample)
  da2<-sort(ages[dose2],decreasing=T,index.return=T)
  # dose_timing2<-rep(0,length(da2))
  # dose_timing2[dose2[da2$ix]]<-dose_timing1[da2$ix]+dose_interval_sample
  time_dose2<-rep(0,samplesize)
  time_dose2[ww2]<-time_dose1[ww2]+dose_interval_sample
  time_interval<-rep(0,samplesize)
  time_interval[dose2[da2$ix]]<-dose_interval_sample
  gap<-(time_interval<56)
  weight_gap<-rep(1,samplesize)
  weight_gap[gap]<-0.75 
  
  #### Simulation process 
  ### EVERY DAY : 
  # 
  #     1) CHECK ALIVE STATUS - death vector
  #     2) CHECK VACC STATUS - vacc vector - & VACCINATE BASED ON TIME OF DOSES - time_dose1 and time_dose2
  #     3) CALCULATE NEW INFECTION RISK - depends on prop vaccinated and immune?
  #     4) INFECTION PROCESS - inf, clin, alive vectors
  # 
  ##################   
  
  death<-rep(0,samplesize)
  vacc<-rep(0,samplesize)
  timelastvacc<-rep(0,samplesize)
  clin<-rep(0,samplesize)
  inf<-sero_base
  days_immune<-ceiling(inf*(0.1+200*rbeta(samplesize, shape1 = 1.1, shape2=3)))
  # hist(0.1+200*rbeta(samplesize, shape1 = 1.1, shape2=3))
  
  death_c<-death
  clin_c<-clin
  inf_c<-inf
  days_immune_c<-ceiling(inf*(0.1+200*rbeta(samplesize, shape1 = 1.1, shape2=3)))
  
  time_inf<-rep(0,samplesize); time_inf_c<-rep(0,samplesize)
  time_clin<-rep(0,samplesize); time_clin_c<-rep(0,samplesize)
  time_death<-rep(0,samplesize); time_death_c<-rep(0,samplesize)
  
  vacc_reduction_infection<-c(0.0,0.05,0.05)
  me<-0
  vacc_effect2<-parameters$PD2[j]
  vacc_effect<-vacc_effect2-me
  p1<-parameters$Dose2_rel_boost[j]; p2<-parameters$Dose2_rel_boost[j]
  vacc_reduction_clinical<-c(0.0,vacc_effect*p1,vacc_effect)
  vacc_reduction_death<-c(0.0,vacc_effect2*p1,vacc_effect2)
  vacc_delay<-0
  
  risk_infection<-parameters$Attack_rate[j]
  risk_clinical<-risk_infection*ihr[ages,1]
  risk_death<-risk_infection*ihr[ages,1]*hfr[ages,1]
  
  nat_immunity<-365*2                               ## how long does natural immunity last
  vacc_immunity_inf<-parameters$Vacc_waning[j]      ## how long does vaccine immunity last
  vacc_immunity_clin<-parameters$Vacc_waning[j]     ## how long does vaccine immunity last
  vacc_immunity_death<-parameters$Vacc_waning[j]    ## how long does vaccine immunity last
  
  cc<-0;ff<-0;ff2<-0
  for(i in 1: (timelimit)){
    
    wv1<-which(time_dose1==i & death==0)  ## of those, who should get vaccinated today - 1st dose
    wv2<-which(time_dose2==i & death==0)  ## of those, who should get vaccinated today - 2nd dose
    # cc<-cc+length(wv2)
    
    vacc[wv1]<-1 # allocate one dose - this is the time when the vaccine takes effect, not the time of the shot
    vacc[wv2]<-2 # allocate second dose
    timelastvacc[c(wv1,wv2)]<-i
    
    daily_risk_infection<-risk_infection
    daily_risk_clinical<-risk_clinical
    daily_risk_death<-risk_death
    
    vacc_effect_infection<-(1-vacc_reduction_infection[vacc+1]*weight_gap*exp(-(i-timelastvacc)/vacc_immunity_inf))
    vacc_effect_clin<-(1-vacc_reduction_clinical[vacc+1]*weight_gap*exp(-(i-timelastvacc)/vacc_immunity_clin))
    vacc_effect_death<-(1-vacc_reduction_death[vacc+1]*weight_gap*exp(-(i-timelastvacc)/vacc_immunity_death))
    
    # vacc_effect_infection<-(1-vacc_reduction_infection[vacc+1])
    # vacc_effect_clin<-(1-vacc_reduction_clinical[vacc+1])
    # vacc_effect_death<-(1-vacc_reduction_death[vacc+1])
    
    randset<-runif(samplesize)
    ri<-(randset<(daily_risk_infection*vacc_effect_infection))#*rnorm(samplesize,mean=1,sd=.05))
    rc<-(randset<(daily_risk_clinical*vacc_effect_clin))#*rnorm(samplesize,mean=1, sd=.05))
    rd<-(randset<(daily_risk_death*vacc_effect_death))#*rnorm(samplesize,mean=1,sd=.05))
    
    ri_c<-(randset<(daily_risk_infection))#*rnorm(samplesize,mean=1,sd=.05))
    rc_c<-(randset<(daily_risk_clinical))#*rnorm(samplesize,mean=1, sd=.05))
    rd_c<-(randset<(daily_risk_death))#*rnorm(samplesize,mean=1,sd=.05))
    
    wri<-which(ri==T & inf==0 & death==0 )       ## of those susceptible, who should get infected today
    wrc<-which(rc==T & clin==0 & death==0)       ## of those susceptible, who should get clinical today
    wrd<-which(rd==T & death==0 )                ## of those alive, who should die today
    
    wri_c<-which(ri_c==T & inf_c==0 & death_c==0)   ## of those susceptible, who should get infected today
    wrc_c<-which(rc_c==T & clin_c==0 & death_c==0 ) ## of those susceptible, who should get clinical today
    wrd_c<-which(rd_c==T & death_c==0)              ## of those alive, who should die today
    
    
    # #### REINFECTION
    # daily_risk_infection_imm<-max(risk_infection*(1-exp(-days_immune/nat_immunity)),0)
    # daily_risk_clinical_imm<-max(risk_clinical*(1-exp(-days_immune/nat_immunity)),0)
    # daily_risk_death_imm<-max(risk_death*(1-exp(-days_immune/nat_immunity)),0)
    # 
    # randset<-runif(samplesize)
    # ri_imm<-(randset<(daily_risk_infection_imm*vacc_effect_infection))#*rnorm(samplesize,mean=1,sd=.05))
    # rc_imm<-(randset<(daily_risk_clinical_imm*vacc_effect_clin))#*rnorm(samplesize,mean=1, sd=.05))
    # rd_imm<-(randset<(daily_risk_death_imm*vacc_effect_death))#*rnorm(samplesize,mean=1,sd=.05))
    # ri_c_imm<-(randset<(daily_risk_infection_imm))#*rnorm(samplesize,mean=1,sd=.05))
    # rc_c_imm<-(randset<(daily_risk_clinical_imm))#*rnorm(samplesize,mean=1, sd=.05))
    # rd_c_imm<-(randset<(daily_risk_death_imm))#*rnorm(samplesize,mean=1,sd=.05))
    # 
    # wri_imm<-which(ri_imm==T & inf==1 & death==0)                ## of those susceptible, who should get infected today
    # wrc_imm<-which(rc_imm==T & inf==1 & clin==0 & death==0)      ## of those susceptible, who should get clinical today
    # wrd_imm<-which(rd_imm==T & inf==1 & death==0)                ## of those alive, who should die today
    # wri_c_imm<-which(ri_c_imm==T & inf==1 & death_c==0)          ## of those susceptible, who should get infected today
    # wrc_c_imm<-which(rc_c_imm==T & inf==1 & clin_c==0 & death_c==0)  ## of those susceptible, who should get clinical today
    # wrd_c_imm<-which(rd_c_imm==T& inf==1 & death_c==0)               ## of those alive, who should die today
    
    
    inf[wri]<-1; inf_c[wri_c]<-1;
    time_inf[wri]<-i; time_inf_c[wri_c]<-i
    days_immune[wri]<-1; days_immune_c[wri_c]<-1
    # inf[wri_imm]<-1; inf_c[wri_c_imm]<-1 ; time_inf[wri_imm]<-i; time_inf_c[wri_c_imm]<-i 
    
    clin[wrc]<-1; clin_c[wrc_c]<-1
    time_clin[wrc]<-i; time_clin_c[wrc_c]<-i
    days_immune[wrc]<-1; days_immune_c[wrc_c]<-1; 
    # clin[wrc_imm]<-1; clin_c[wrc_c_imm]<-1; time_clin[wrc_imm]<-i; time_clin_c[wrc_c_imm]<-i
    
    death[wrd]<-1; death_c[wrd_c]<-1
    time_death[wrd]<-i; time_death_c[wrd_c]<-i
    # death[wrd_imm]<-1; death_c[wrd_c_imm]<-1; time_death[wrd_imm]<-i; time_death_c[wrd_c_imm]<-i
    
    days_immune[days_immune>0]<-days_immune[days_immune>0]+1
    days_immune_c[days_immune_c>0]<-days_immune_c[days_immune_c>0]+1
    
  }
  
  ### HOW MANY PEOPLE GOT VACCINCATED
  Vacc1<-length(which(vacc==1))
  Vacc2<-length(which(vacc==2))
  
  ## CALCAULATE ATTACK RATES
  attack_rate_unvacc_inf<-sum(inf_c)
  attack_rate_vacc_inf<-sum(inf)
  
  attack_rate_unvacc_clin<-sum(clin_c)
  attack_rate_vacc_clin<-sum(clin)
  
  attack_rate_unvacc_death<-sum(death_c)
  attack_rate_vacc_death<-sum(death)
  
  LS<-attack_rate_unvacc_death-attack_rate_vacc_death
  CA<-attack_rate_unvacc_clin-attack_rate_vacc_clin
  IA<-attack_rate_unvacc_inf-attack_rate_vacc_inf
  VE_inf<-100*(attack_rate_unvacc_inf-attack_rate_vacc_inf)/attack_rate_unvacc_inf
  VE_clin<-100*(attack_rate_unvacc_clin-attack_rate_vacc_clin)/attack_rate_unvacc_clin
  VE_death<-100*(attack_rate_unvacc_death-attack_rate_vacc_death)/attack_rate_unvacc_death
  
  write.table(c(VE_inf,VE_clin,VE_death,IA,CA,LS,Vacc1,Vacc2),file = paste("Run_",as.character(j),".txt"),row.names = F, col.names = F)
# }





