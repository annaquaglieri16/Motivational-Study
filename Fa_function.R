
library(ggplot2)
library(psych)
library(corrplot)
library(reshape2)

detach("package:ggplot2", unload=TRUE)





fa_all <- function(data4,rot,minL,maxL,nfac=0,seed=5){
  
  set.seed(seed)
  
  # Save the orignal dataset
  data_orig <- data4
  
  # Define the rotations
  orth <- c("varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" , "bifactor" )
  obl <- c("Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin" ,"cluster")
  
  
  
  alpha2 <- function(x){
    
    alpha.1 <- function(C, R) {
      n <- dim(C)[2]
      alpha.raw <- (1 - tr(C)/sum(C)) * (n/(n - 1))
      sumR <- sum(R)
      alpha.std <- (1 - n/sumR) * (n/(n - 1))
      smc.R <- smc(R)
      G6 <- (1 - (n - sum(smc.R))/sumR)
      av.r <- (sumR - n)/(n * (n - 1))
      mod1 <- matrix(av.r, n, n)
      Res1 <- R - mod1
      GF1 = 1 - sum(Res1^2)/sum(R^2)
      Rd <- R - diag(R)
      diag(Res1) <- 0
      GF1.off <- 1 - sum(Res1^2)/sum(Rd^2)
      sn <- n * av.r/(1 - av.r)
      Q = (2 * n^2/((n - 1)^2 * (sum(C)^3))) * (sum(C) * (tr(C %*% 
                                                               C) + (tr(C))^2) - 2 * (tr(C) * sum(C %*% C)))
      result <- list(raw = alpha.raw, std = alpha.std, G6 = G6, 
                     av.r = av.r, sn = sn, Q = Q, GF1, GF1.off)
      return(result)
    }
    
    
    if (!isCorrelation(x)) {
      item.var <- apply(x, 2, sd, na.rm = T)
      bad <- which((item.var <= 0) | is.na(item.var))
      if ((length(bad) > 0) && delete) {
        for (baddy in 1:length(bad)) {
          warning("Item = ", colnames(x)[bad][baddy], " had no variance and was deleted")
        }
        x <- x[, -bad]
        nvar <- nvar - length(bad)
      }
      response.freq <- response.frequencies(x, max = 10)
      C <- cov(x, use = "pairwise")
    }
    else {
      C <- x
    }
    
    
    
    R <- cov2cor(C)
    
    alpha.total <- alpha.1(C, R)
    
    return(alpha.total)
  }
  
  
  cicl<-0
  par1<-1
  par2<-1
  par3<-1
  while(par3>0){
    
    
    # Selezione il numero di fattori consigliati
    
    cat("Calculating the number of factors needed \n")
    
    fact <- nfac
    if(nfac==0){
      fact<-fa.parallel(data4)$nfact
    }
    
    if(fact==1){
      stop("Only one factor remains. Check the data or reduce the threshold")
    }
    
    #rot<-c("none", "varimax", "quartimax", "bentlerT", "geominT" , "bifactor", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ" , "biquartimin" , "cluster" )
    
    # Matrice uota che conterra le miedie degli alpha
    
    range <- length(seq(minL,maxL,0.01))
    
    
    mean.al<-matrix(0,range,length(rot))
    
    # Sottociclo per il calcolo delle medie degli alpha (sui fattori) al variare sia della soglia sia della della rotazione
    
    cat("Calculating the loadings thresholds \n")
    
    for(b in 1:length(rot)){
      cat("- Calculating the loadings thresholds for rotation",rot[b],"\n")
      fa1<-fa(data4,fact,rotate=rot[b])
      
      a<-fa1$loadings
      class(a)<-"matrix"
      colnames(a)<-paste("F",1:fact,sep="")
      a<-as.data.frame(a)
      a<-round(a,2)
      a$D<-rownames(a)
      
      ls1<-minL
      m<-rep(0,range)
      i<-1
      nv<-fact
      
      while(ls1<=maxL+0.01){
        
        var<-lapply(1:nv,function(f)unique(a$D[abs(a[,f])>ls1]))
        names(var)<-paste(colnames(a[,1:nv]))
        
        
        # Do this if there are factors with length less than 2
        if(any(as.numeric(summary(var)[,1])<2)){
          
          # If a certain threshold and rotation gives only factors composed by 1 give it alpha=0
          if(sum(as.numeric(summary(var)[,1])>1)==0){
            m[i] <- 0
          }else{
            var1<-var[as.numeric(summary(var)[,1])>1]
            al<-sapply(1:length(var1),function(v)alpha2(data4[,var1[[v]]])$std)
            al<-data.frame(al)
            #m[i]<-mean(t(al))
            m[i]<-median(t(al))
          }
          
        }
        # Do this if ALL the factors have length greater than 1
        else{
          al<-sapply(1:nv,function(v)alpha2(data4[,var[[v]]])$std)
          al<-data.frame(al)
          #m[i]<-mean(t(al))
          m[i]<-median(t(al))
        }
        
        i<-i+1
        ls1<-ls1+0.01
        #cat(ls1)
      }
      mean.al[,b]<-m
      #cat("\n")
    }
    
    # Creazione e stampa degli andamenti delle medie degli alpha
    cat("Producing the threshold plot \n")
    
    mean.al<-as.data.frame(mean.al)
    colnames(mean.al)<-rot
    mean.al$sl<-seq(minL,maxL,0.01)
    mean.al1<-melt(mean.al,id.vars="sl")
    zp<-ggplot(mean.al1,aes(x=sl,y=value,colour=variable))+geom_line()+labs(x="Soglia Loading",y="Alpha Medio",colour="Rotazione")
    
    
    # Display the plot
    print(zp)
    
    max(mean.al)
    #print(mean.al)
    
    # Selezione della rotazione e della soglia che massimizzano le medie degli alpha
    cat("Choosing the best rotation and threshold \n")
    ind<-which(mean.al==max(mean.al),arr.ind=T)
    if(class(ind)=="matrix"){
      ind<-ind[1,]
    }
    
    rota<-names(mean.al)[ind[2]]
    sogl<-mean.al$sl[ind[1]]
    
    
    # Keep the same rotation that was selected in the first run
    rot <- rota
    
    
    # Calcolo fattoriale
    
    fa1<-fa(data4,fact,rotate=rota)
    a<-fa1$loadings
    class(a)<-"matrix"
    colnames(a)<-paste("F",1:fact,sep="")
    a<-as.data.frame(a)
    a<-round(a,2)
    a$D<-rownames(a)
    
    # Creazione dei fattori
    
    var<-lapply(1:fact,function(f)unique(a$D[abs(a[,f])>sogl]))
    names(var)<-paste(colnames(a[,1:fact]))
    
    # Display the factors
    cat("Displaying the factors \n")
    print(var)
    
    # togliamo le variabili che non entrano nei fattori al secondo ciclo
    
    nc<- ncol(data4)
    par1_names <- names(data4)[!(names(data4) %in% unlist(var))]
    data4<-data4[,names(data4) %in% unlist(var)]
    
    # Aggiorniamo il parametro di ciclo
    
    par1<-nc-ncol(data4) 
    
    # togliamo le variabili che entrano in un fattore da sole (solo se non entrano in un altro fattore)
    par4 <- 0
    par4_names <- character()
    
    len_fac <- cbind(1:fact,sapply(1:fact,function(v)length(var[[v]])))
    
    if(any(len_fac[,2]==1)){
      par4_nam<- data.frame(variable=as.character(unlist(var[ c(len_fac[len_fac[,2]==1,1])  ])))
      par4_nam$variable <- as.character(par4_nam$variable)
      par4_nam$ntimes= sapply(1:nrow(par4_nam), function(v)sum(unlist(var)%in%par4_nam[v,"variable"] ) )
      # Do this only if the single variable enetrs in a variable alone
      
      if(any(par4_nam$ntimes>1) & rot%in%obl ){
        cat("- Will NOT remove variable(s)", par4_nam$variable[par4_nam$ntimes>1],"since they contribute to multiple factors","\n")
      }
      
      par4_names <- par4_nam$variable[par4_nam$ntimes==1]
      par4 <- length(par4_names)
      data4<-data4[,!names(data4) %in% par4_names]
    }
    
    
    # Togliamo le variabili che compaiono in piu fattori (se la rotazione e obliqua non effetuiamo tale operazione)
    
    if(rota%in%obl){
      par2<-0
      s <- character()
    }else{
      s<-c(as.vector(unlist(var)))
      s<-unique(s[duplicated(s)])
      par2<-length(s)
      data4<-data4[,!names(data4) %in% s]
    }
    
    
    
    
    cicl<-cicl+1
    
    # Aggiorniamo il parametro di ciclo
    
    par3<-par1+par2+par4
    
    # Stampa diagnosi ciclo
    
    cat(paste("Unused Variable:",par1),"\n")
    cat("-",paste(par1_names),"\n")
    cat(paste("Repeated variables:",par2),"\n")
    cat("-",paste(s),"\n")
    cat(paste("Single variables:",par4),"\n")
    cat("-",paste(par4_names),"\n")
    cat(paste("Rotation used:",rota),"\n")
    cat(paste("Threshold chosen:",sogl),"\n")
    cat(paste("End interation",cicl),"\n","\n")
    
  }
  
  cat(paste("Variables excluded in the process:"),names(data_orig)[!names(data_orig)%in%names(data4)],"\n")
  return(data4)
}



#

likert_variables2 <- names(dat)

data1 <- dat[,likert_variables1[!(likert_variables1 %in% c("necessity1","educated1","reconnect.comm1", "speakersmelb.comm1", "comecloser.comm1"))]]

# Plot the correlations
corrplot(cor(data1,use = "pair"))


# check which variable does not correlated with any other vaiable
r1 <- cor(data1,use = "pair")
diag(r1) <- 0
r1 <- data.frame(r1)
temp <- data.frame(name=names(r1),cor=sapply(1:ncol(r1),function(v)any(r1[,v]>=0.3))  )
as.character(temp$name[temp$cor==F])




# Keep just the itemes with a r.cor greater or equal to 0.3
as<-alpha(data1,check.keys=F)$item.stats
as$n1<-1:nrow(as)
summary(as$r.cor)
no_corr_macu <- rownames(as[abs(as$r.cor)<0.3,])
no_corr_macu
data1<-data1[,!names(data1)%in%no_corr_macu]




# check which items will decrease the alpha
al<-alpha(data1)
drop<-al$alpha.drop
tot<-as.numeric(al$total$std.alpha)
drop <- drop[drop$std.alpha>tot,]
drop


# Check how much
drop$std.alpha-tot
# They don;t drop enough so leave it

#drop_alpha_macu <- rownames(drop)
#data1<-data1[,!names(data1)%in%drop_alpha_macu]




# check how many factors should be used
fap <- fa.parallel(data1)
fap


data_macu <- data1


## Perform the anlaysis for macular thickness



#Check again the out_maculiers
out_macu <- outlier(data_macu)
#abline(h=800)

#dat_imp_pheno[out_macu>800,1:20]
#out_maculiers_macu <- dat_imp_pheno$id1[out_macu>800]
#data_macu <- data_macu[out_macu<800,]




# Run the first set of analysisi and check what is cleaned out_macu!
rot=c("oblimin","promax")


# Take off the variables that give problem
#problem_var_macu <- c( "v07_spectralis" ,"v01_cyrrus" )
#data_macu <- data_macu[,!names(data_macu)%in%problem_var_macu]


library(ggplot2)
data_macu_facleaned <- fa_all(data_macu,rot,0.2,0.5)

# 0 Variable do not enter the factors
not_used_fact_macu <- names(data_macu)[!names(data_macu)%in%names(data_macu_facleaned)]
not_used_fact_macu



# Check again how many factors you need
#fa.parallel(data_macu_facleaned)

nfact_macu <- 6

# Run the actual factorial analysis on the final dataset
fa_macu<-fa(data_macu_facleaned,nfact_macu,rot="promax")





# Check whther any variable do not enter in any factor
lod <- fa_macu$loadings
class(lod) <- "matrix"
lod <- data.frame(lod)
lod$any <- apply(lod,1,function(v)any(abs(v)>=0.2) )
apply(lod[,1:nfact_macu],1,max )
lod[lod$any==F,]
# NONE, good!




# Plot the results
a<-fa_macu$loadings
class(a)<-"matrix"
colnames(a)<-paste("F",1:nfact_macu,sep="")
a<-as.data.frame(a)
a<-round(a,2)
a$D<-rownames(a)
a1 <- a
a1$D
a1 <- melt(a1,id.vars=c("D"))
a1$x <- runif(nrow(a1))
a1$inv <- ifelse(a1$value<0,"neg","pos")
a1$value[abs(a1$value)<0.2] <- 0
a1 <- a1[a1$value!=0,]



library(ggplot2)
ggplot(a1)+geom_bar(aes(x=reorder(D, value) ,y=value),stat="identity")+facet_wrap(~variable,ncol = 4,scales = "free_y")+coord_flip()
detach("package:ggplot2", unload=TRUE)




var<-lapply(1:nfact_macu,function(f)unique(a$D[abs(a[,f])>0.2]))
names(var)<-paste(colnames(a[,1:nfact_macu]))

al <- sapply(1:length(var),function(v)alpha(data_macu_facleaned[,var[[v]]])$total$std.alpha)
al
# Alpha total
alpha(data_macu_facleaned)$total$std.alpha
# they are ok...
alpha(data_macu)$total$std.alpha


# Table of the factors
a$D <- NULL
a[abs(a)<0.2] <- 0
for(i in 1:ncol(a)){a[,i] <- as.character(a[,i])}

a[a=="0"] <- ""
loading_fact_macu <- a
loading_fact_macu





# Discriminant analysis: In this section i will test if the values of the variables kept in the dataframe by the factorial analysis are able to discriminate between subjects for which their total score lie in the 1st and 3rd quartile.
discr<-unique(data_macu_facleaned)

# Detrmine the total score

discr$punteggio<-rowSums(discr)

# Dividce the groups of people who lie in the 4rth and 1st quarile

hist(discr$punteggio)
quantile(discr$punteggio,na.rm = T)

discr1<-unique(discr[discr[,ncol(discr)]<=quantile(discr[,ncol(discr)],na.rm = T)[2],])
discr2<-unique(discr[discr[,ncol(discr)]>=quantile(discr[,ncol(discr)],na.rm = T)[4],])

# Wilcox Test the values of each single variable comparing the group of pople lien in the 1st and 3rd quartile

test<-data.frame(Item=colnames(discr[,1:(ncol(discr)-1)]),p.value=rep(0,(ncol(discr)-1)))

for(i in 1:(ncol(discr)-1)){
  test[i,2]<-wilcox.test(discr1[,i],discr2[,i],alternative="two.sided")$p.value
}

test <- test[order(test$p.value),]
test
# they all discriminate!












# Calculate factors on the discarded variables

dat_disc <- dat[,no_corr_macu]


# check how many factors should be used
fap <- fa.parallel(dat_disc)
fap



library(ggplot2)
fa_disc <- fa(dat_disc,nfactors = 1,rotate = "oblimin")



# Plot the results
a<-fa_disc$loadings
class(a)<-"matrix"
colnames(a)<-paste("F",1,sep="")
a<-as.data.frame(a)
a<-round(a,2)
a$D<-rownames(a)
a1 <- a
a1$D
a1 <- melt(a1,id.vars=c("D"))
a1$x <- runif(nrow(a1))
a1$inv <- ifelse(a1$value<0,"neg","pos")
a1$value[abs(a1$value)<0.2] <- 0
a1 <- a1[a1$value!=0,]



library(ggplot2)
ggplot(a1)+geom_bar(aes(x=reorder(D, value) ,y=value),stat="identity")+facet_wrap(~variable,ncol = 4,scales = "free_y")+coord_flip()
detach("package:ggplot2", unload=TRUE)






# Predict the factors

pred <- as.data.frame(predict(fa_macu,dat[,names(data_macu_facleaned)]))
names(pred) <- paste("Factor",1:nfact_macu,sep = "")

# Predict the factor from the discarded variables
pred_disc <- as.data.frame(predict(fa_disc,dat[,names(dat_disc)]))
names(pred_disc) <- paste("Factor",7,sep = "")



factors <- c(names(pred),names(pred_disc))


dat_complete <- cbind(dat,scale(pred),scale(pred_disc))


corrplot(cor(dat_complete[,likert_variables2],dat_complete[,factors],use = "pair"))


all_complete <-  cbind(all,pred,pred_disc)

all_complete$Context

dat_plot <- melt(all_complete,id.vars = "Context",measure.vars = factors)



library(ggplot2)
ggplot(dat_plot)+geom_boxplot(aes(x=Context,y=value,color=Context))+facet_wrap(~variable)+coord_flip()+guides(color=F)





mod <- lm(Factor1~Context,data=all_complete)
summary(mod)
plot()



summary(lm(Factor2~Context,data=all_complete))

summary(lm(Factor4~Context,data=all_complete))

summary(lm(Factor6~Context,data=all_complete))

summary(lm(Factor7~Context,data=all_complete))





