#' ---
#' title: "Appendix E: Simulation Codes for Comparative Statics"
#' author: "Gento Kato"
#' date: "December 2, 2018"
#' ---

#' # Preparation

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create("./figure")

## Import Relevant packages
library(ggplot2)
library(gridExtra)
library(grid)

## Clear Workspace
rm(list = ls())


#' # Figure 2: The Visual Depiction of Discouraged and Delegatory Abstention Interval
#'
#' ## Relevant Functions

# The acceptance threshold
Fphix <- function(pi,b,e,c) 0.5 - (pi*b)/(2*(pi+e))
# The lower bound of the abstention interval
Fphivr <- function(pi,k1,k0,b,e,c) {
  (pi*k0*(1-b) + e -c)/(pi*((1-k1)*(1+b) + k0*(1-b)) + e) }
Fphiv1x0 <- function(pi,k1,k0,b,e,c) {
  apply(cbind(Fphix(pi,b,e,c),Fphivr(pi,k1,k0,b,e,c)), 1, min) }
# The upper bound of the abstention interval
Fphiva <- function(pi,k1,k0,b,e,c) {
  (pi*(1-k0)*(1-b) + c)/(pi*(k1*(1+b) + (1-k0)*(1-b)) + e) }
Fphiv1x1 <- function(pi,k1,k0,b,e,c) {
  apply(cbind(Fphix(pi,b,e,c),Fphiva(pi,k1,k0,b,e,c)), 1, max) }

#' ## Setting Parameters  

# Moving value = pi
pi <- seq(0,1,length=500)

# Fixed Values: (Discouraged, Delegatory)
k1 <- c(0.48, 0.28)
k0 <- c(0.48, 0.28)
e <- c(0.5, 0.5)
c <- c(0.3, 0.3)
b <- c(0, 0)

#' ## Simulations

## The Discouraged Abstention Interval & Approval Threshold
DIphix <- Fphix(pi,b[1],e[1],c[1])
DIphiv1x0 <- Fphiv1x0(pi,k1[1],k0[1],b[1],e[1],c[1])
DIphiv1x1 <- Fphiv1x1(pi,k1[1],k0[1],b[1],e[1],c[1])

## The Delegatory Abstention Interval & Approval Threshold
DEphix <- Fphix(pi,b[2],e[2],c[2])
DEphiv1x0 <- Fphiv1x0(pi,k1[2],k0[2],b[2],e[2],c[2])
DEphiv1x1 <- Fphiv1x1(pi,k1[2],k0[2],b[2],e[2],c[2])

#' ## Plot

## Dataset
da <- data.frame(pi=rep(pi,2),
                 DIDE=rep(c("2","3"), each=length(pi)),
                 phix=c(DIphix,DEphix),
                 phiv1x0=c(DIphiv1x0,DEphiv1x0),
                 phiv1x1=c(DIphiv1x1,DEphiv1x1),
                 catk1 = rep(c(k1[1],k1[2]),each=length(pi)),
                 catk0 = rep(c(k0[1],k0[2]),each=length(pi)) )

## Plot
abgraph <- 
  ggplot(da,aes(x=pi)) +
  geom_ribbon(aes(ymin=phiv1x0, ymax=phiv1x1, fill=DIDE)) +
  geom_line(aes(y=phix, linetype="2"), size=0.5) +
  facet_grid( . ~ (catk0*-1) + catk0 + catk1,
              labeller = label_bquote(cols=kappa[a] ~ "=" ~ 
                                           kappa[r] ~ "=" ~ 
                                           .(catk0))) +
  scale_x_continuous(expand=c(0,0)) +
  geom_vline(aes(xintercept=0), size=0.5) +
  coord_cartesian(xlim=c(0,1.1), ylim=c(0.3,0.7)) +
  ylab(bquote(phi ~ "(The Prob. of High-Quality Policy)")) +
  xlab(bquote(pi ~ "(The Pivot Probability of Uninformed Voters)")) +
  theme_classic() +
  scale_linetype_manual(name="", 
                        values=c(2),
                        labels=c("The Approval Threshold")) +
  scale_fill_manual(name="",
                    labels=c("Discouraged (Left) ",
                             "Delegatory (Right) "),
                    values=c("#99d8c9","#f03b20")) +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face="bold", size=12),
        axis.line.y = element_line(colour="black"),
        axis.line.x = element_line(colour="black"),
        legend.position = "bottom")
abgraph              

ggsave("figure/abgraph-1.jpeg", abgraph, width=6.5, height=3.5, dpi=500)

#' # Figure 3: d:c Ratio and the Probability of Ideologues ka, kr Explain 
#' the Available Form of Abstention
#' 
#' ## Relevant Functions 

# Moving Value = k1
k1 <- seq(0,1, length=500)

# k0 threshold for the discouraged abstention 
k0loin <- function(k1,b,ec){
  k0 <- 1-(k1*(1-b))/((1+b)*(ec-1))
  k0s <- sapply(k0,function(x){if(x<0){return(0)}else{return(x)}})
  return(k0s)
}

# k0 threshold for the delegatory abstention
k0upde <- function(k1,b,ec){
  k0 <- ((1-k1)*(1-b)*(ec-1))/(1+b)
  k0s <- sapply(k0,function(x){if(x<0){return(0)}else{return(x)}})
  return(k0s)
  }

#' ## Plot

## Dataset
abtypedt <- data.frame(
  k1 = rep(k1,3),
  cxt = factor(rep(c("2:1","5:3","10:9"), each=length(k1)),
               levels=c("10:9","5:3","2:1")),
  cxte = rep(c(2,5,10), each=length(k1)),
  cxtc = rep(c(1,3,9), each=length(k1)),
  DIk0 = c(apply(cbind(k0upde(k1,0,2/1),k0loin(k1,0,2/1)),1,max),
           apply(cbind(k0upde(k1,0,5/3),k0loin(k1,0,5/3)),1,max), 
           apply(cbind(k0upde(k1,0,10/9),k0loin(k1,0,10/9)),1,max)),
  DEk0 = c(apply(cbind(k0upde(k1,0,2/1),k0loin(k1,0,2/1)),1,min),
           apply(cbind(k0upde(k1,0,5/3),k0loin(k1,0,5/3)),1,min),
           apply(cbind(k0upde(k1,0,10/9),k0loin(k1,0,10/9)),1,min)),
  limk0 = rep(1-k1,3)
)

## Plot
abtype <- 
  ggplot(abtypedt, aes(x=k1)) + 
  geom_ribbon(aes(ymin=0,ymax=limk0, fill="3"),alpha=1, colour="black") + 
  geom_ribbon(aes(ymin=DIk0,ymax=1, fill="1"),alpha=1, colour="black") + 
  geom_ribbon(aes(ymin=0,ymax=DEk0, fill="2"),alpha=1, colour="black") + 
  geom_line(aes(y=DIk0)) + geom_line(aes(y=DEk0)) +
  geom_vline(aes(xintercept=1),size=1.5, colour="white") + 
  geom_hline(aes(yintercept=1),size=1.5, colour="white") +
  geom_ribbon(aes(ymin=limk0,ymax=1),fill="white", colour="white") + 
  geom_line(aes(y=limk0)) + 
  geom_vline(aes(xintercept=0), size=0.5) +
  facet_grid(.~cxt + cxte + cxtc,
             labeller = label_bquote(cols = d:c == .(cxte):.(cxtc))) + 
  scale_x_continuous(expand=c(0,0), limits=c(0,1.05)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1.05)) +
  scale_fill_manual(name="",
                    labels=c("Discouraged ",
                             "Delegatory ",
                             "Mixed "),
                    values=c("#99d8c9","#f03b20","#fff7bc")) + 
  xlab(bquote(kappa[a] ~ "(The Probability of Approval Ideologues)")) + 
  ylab(bquote(kappa[r] ~ "(The Prob. of Rejection Ideologues)")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face="bold", size=12),
        legend.position = "bottom")
abtype

ggsave("figure/abtype-1.jpeg", abtype, width=7, height=3.5, dpi=500)


#' # Figure 4: For Sufficiently High pi, PH Proposes the Higher-Quality Policy 
#' under the Delegatory Abstention Context than the Discouraged Abstention Context
#' 
#' Relevant Functions

## Eu function for the Policymaker
Feup <- function(phi,pi,k1,k0,vu,xu){
  (1-pi*(vu + (1-vu)*(k1+k0))) * 
    (2*k1 + phi*(1-2*(k1+k0))) + 
    pi*(2*k1 - phi*(vu + (1-vu)*(k1+k0)) + 
          (1-(k1+k0))*vu*2*xu )
}


## Threshold for Uninformed Voters 1: phiv1x0
Fpvx0 <- function(pi,k1,k0,e,c,b=0) {
  # the acceptance threshold
  phix <- 0.5 - (pi*b)/(2*pi + 2*e)
  # the abstention interval 
  phiv0 <- (pi*k1*(1-b) + e - c)/(pi*((1-k0)*(1+b) + k1*(1-b)) + e)
  pvx0 <- min(phix,phiv0)
  return(pvx0)
}

## Threshold for Uninformed Voters 2: phiv1x1
Fpvx1 <- function(pi,k1,k0,e,c,b=0) {
  # the acceptance threshold
  phix <- 0.5 - (pi*b)/(2*pi + 2*e)
  # the abstention interval 
  phiv1 <- (pi*(1-k1)*(1-b) + c)/(pi*(k0*(1+b) + (1-k1)*(1-b)) + e)
  pvx1 <- max(phix,phiv1)
  return(pvx1)
}


# phi Thresholds for the Policymaker
Ftphi <- function(phi,pH,pvx0,pvx1){
  pvx0pH <- ifelse(pvx0<pH, pvx0/pH, 1)
  pvx1pH <- ifelse(pvx1<pH, pvx1/pH, 1)
  store <- data.frame(c1=rep(NA,length(phi)), c2=NA, c3=NA)
  for(i in 1:length(phi)){
    if(phi[i] >= pvx1pH){
      store$c3[i] <- 1
    } else if (phi[i] < pvx1pH & phi[i] >= pvx0pH) {
      store$c2[i] <- 1
    } else {
      store$c1[i] <- 1
    }
  }
  return(store)
}

# Expected Utilities in EQ
Feufin <- function(eu11,eu10,eu0,tphi){
  ifelse(!is.na(eu10 * tphi$c3), eu11 * tphi$c3,
         ifelse(!is.na(eu10 * tphi$c2), eu0 * tphi$c2, 
                ifelse(!is.na(eu10 * tphi$c1), eu10 * tphi$c1, NA)))
}

## Define Color by the Type of Abstention
Fdedicol <- function(k1,k0,e,c,b=0){
  colset <- c("#99d8c9","#f03b20","#fff7bc")
  names(colset) <- c("Discouraged ","Delegatory ","Mixed ")
  DIk0 <- max(k0upde(k1,b,e/c),k0loin(k1,b,e/c))
  DEk0 <- min(k0upde(k1,b,e/c),k0loin(k1,b,e/c))
  if (k1 > DIk0){ colfin <- colset[1] 
  } else if (k1 <= DEk0) { colfin <- colset[2]
  } else { colfin <- colset[3] }
  return(colfin)
}

## Graphing Function  
Fgphi <- function(tphi,eu,phi,plr,phr,pi,dedicol){
  # The Equilibrium Strategy
  
  if (round(plr,5)==round(phr,5)){
    philab <- c("0","",expression( over(phi[x]^"*",p)),"","1")
    px <- plr; plr <- NA; phr <- NA
  } else { 
    px <- NA
    philab <- c("0",expression( over(phi[v1x0]^"*",p)),"",
                expression( over(phi[v1x1]^"*",p)),"1")
    }

  dphi <- tphi
  dphi$eu <- eu
  dphi$phi <- phi
  dphi$eu1 <- dphi$eu*dphi$c1
  dphi$eu2 <- dphi$eu*dphi$c2
  dphi$eu3 <- dphi$eu*dphi$c3
  
  phimaxlist <-
    list(bquote(pi == .(pi) ~ " then " ~ phi[H]^"*" == 0),
         bquote(pi == .(pi) ~ " then " ~ phi[H]^"*" == phi[v1x0]^"*"/p),
         bquote(pi == .(pi) ~ " then " ~ phi[H]^"*" == phi[x]^"*"/p),
         bquote(pi == .(pi) ~ " then " ~ phi[H]^"*" == phi[v1x1]^"*"/p),
         bquote(pi == .(pi) ~ " then " ~ phi[H]^"*" == 1))
  dif <- phi[which(eu==max(eu))] - c(0,plr,px,phr,1)
  phimax <- phimaxlist[[which(abs(dif)==min(abs(dif),na.rm=T))]]
  phimax
  
  if (is.na(plr)){ plr <- px } 
  if (is.na(phr)){ phr <- px }
  if (is.na(px)){ px <- phr }
  
  graph2x <-
    ggplot(dphi, aes(x=phi)) +
    annotate("rect", fill = dedicol, alpha = 0.7, 
             xmin = plr, xmax = phr,
             ymin = -Inf, ymax = Inf) + 
    geom_line(aes(y=eu1), size=1) +
    geom_line(aes(y=eu2), size=1) +
    geom_line(aes(y=eu3), size=1) +
    geom_vline(aes(xintercept=plr),linetype=2) +
    geom_vline(aes(xintercept=px),linetype=2) +
    geom_vline(aes(xintercept=phr),linetype=2) +
    scale_x_continuous(limits=c(0,1),
                       breaks=c(0,plr,px,phr,1),
                       labels=philab) +
    ylab(expression(Eu[P])) +
    xlab(expression(phi)) +
    ggtitle(phimax) +
    theme_classic() + theme(plot.title = element_text(hjust = 0.5, size=13),
                            plot.background=element_rect(fill="white", colour="white"),
                            rect = element_rect(fill = 'white'))
  return(graph2x)
}

## The Canned Function of Everything
Fgphi2 <- function(phi,pH,pi,k1,k0,e,c,b=0){
  eu11 <- Feup(phi,pi,k1,k0,vu=1,xu=1)
  eu10 <- Feup(phi,pi,k1,k0,vu=1,xu=0)
  eu0 <- Feup(phi,pi,k1,k0,vu=0,xu=1)
  pvx0 <- Fpvx0(pi,k1,k0,e,c,b)
  pvx1 <- Fpvx1(pi,k1,k0,e,c,b)
  tphi <- Ftphi(phi,pH,pvx0,pvx1)
  eufin <- Feufin(eu11,eu10,eu0,tphi)
  dedicol <- Fdedicol(k1,k0,e,c)
  graph2x <- Fgphi(tphi,eufin,phi,pvx0/pH,pvx1/pH,pi,dedicol)
  return(graph2x)
}

#' ## Setting Parameters

## Moving Value: phi
phi <- seq(0,1,length=500)

## Fixed Values: 
pils <- c(0,0.4,0.8) #(low pi, moderate pi, high pi)
k0 <- 0.3
k1 <- 0.3
e <- 0.5
els <- c(0.5, 0.8)
c <- 0.4
#cls <- c(0.4,0.2) # (Discouraged, Delegatory)
pH <- 0.85

## Graph Y axis Limits
yl <- 0; yh <- 0.75

#' ## Plot

## Under the Discouraged Abstention Context 

# Set c
e <- els[1]
#c <- cls[1]

# Title Text
titletxt123 <- bquote(bold("The Discouraged Abstention Context (" ~ 
                             d == .(e) ~ ", " ~  c == .(c) ~ ")"))

# Plot
p1 <- Fgphi2(phi,pH,pi=pils[1],k1,k0,e,c) +
  coord_cartesian(ylim=c(yl,yh)); p1
p2 <- Fgphi2(phi,pH,pi=pils[2],k1,k0,e,c) +
  coord_cartesian(ylim=c(yl,yh)); p2
p3 <- Fgphi2(phi,pH,pi=pils[3],k1,k0,e,c) +
  coord_cartesian(ylim=c(yl,yh)); p3
p123 <- arrangeGrob(p1 + xlab(NULL) + ylab(NULL),
                    p2 + xlab(NULL) + ylab(NULL),
                    p3 + xlab(NULL) + ylab(NULL),
                    ncol=3,
                    top = textGrob(titletxt123,
                                   gp = gpar(fontsize = 15)))
grid.arrange(p123)


## Under the Delegatory Abstention Context

# Set c
e <- els[2]
#c <- cls[2]

# Title
titletxt456 <- bquote(bold("The Delegatory Abstention Context (" ~ 
                             d == .(e) ~ ", " ~  c == .(c) ~ ")"))
titletxt456

# Plot
p4 <- Fgphi2(phi,pH,pi=pils[1],k1,k0,e,c) +
  coord_cartesian(ylim=c(yl,yh)); p4
p5 <- Fgphi2(phi,pH,pi=pils[2],k1,k0,e,c) +
  coord_cartesian(ylim=c(yl,yh)); p5
p6 <- Fgphi2(phi,pH,pi=pils[3],k1,k0,e,c) +
  coord_cartesian(ylim=c(yl,yh)); p6
p456 <- arrangeGrob(p4 + xlab(NULL) + ylab(NULL),
                    p5 + xlab(NULL) + ylab(NULL),
                    p6 + xlab(NULL) + ylab(NULL),
                    ncol=3,
                    top = textGrob(titletxt456,
                                   gp = gpar(fontsize = 15)))
grid.arrange(p456)


## The Final Graph 

# Plot
p123456 <- arrangeGrob(p123,p456,
                       bottom = textGrob(expression(phi[H]),
                                         gp = gpar(fontsize = 13)),
                       left = textGrob(expression(Eu(P[H])),
                                       gp = gpar(fontsize = 13), 
                                       rot=90))
grid.arrange(p123456)

ggsave("figure/eupgraph-1.jpeg", p123456, width=10, height=6.5, dpi=500)


#' # Prevalence of Accountability Improvement

## Solution Formula for quadratic function
kaifm <- function(a,b,c,mode="plus"){
  den <- 2*a
  if (mode=="plus") {
    num <- -b + sqrt(b^2 - 4*a*c)
  } else if (mode=="minus") {
    num <- -b - sqrt(b^2 - 4*a*c)
  }
  return(num/den)
}

## Pi threshold EU_H(phi_v1x0) > EU_H(0) 
gpith <- function(p,ka,kr,d,c, mode="plus", exportadv=FALSE) {
  
  fa = (1-ka-kr)*ka*(p*(1+ka-kr)-(ka+kr))
  if (any(fa<0) & mode=="plus") warning("mode should be minus for some values")
  fb = (1-ka-kr)*(p*ka*d - (ka+kr)*(d-c))-ka*(ka+kr-0.5)
  fc = -(ka+kr-0.5)*(d-c)
  pith = kaifm(fa,fb,fc,mode=mode)
  
  if (exportadv==TRUE) {
    pi = seq(0,1,length=50)
    adv = 2*(fa*pi^2 + fb*pi + fc)
    names(adv) = pi
    return(list(pith=pith,adv=adv))
  } else {
    return(pith)
  }
  
}

#gpith(0.85,0.3,0.3,0.5,0.4)

## Pi threshold EU_H(phi_v1x1) > EU_H(0) 
tpith <- function(p,ka,kr,d,c,mode="plus", exportadv=FALSE) {

  fa = (1-ka-kr)*(p*kr-(1-p)*(1-ka))
  if (any(fa<0) & mode=="plus") warning("mode should be minus for some values")
  fb = (1-ka-kr)*(p*d-c)-(1-ka)*(ka+kr-0.5)
  fc = -(ka+kr-0.5)*c
  pith = kaifm(fa,fb,fc,mode=mode)
  
  if (exportadv==TRUE) {
    pi = seq(0,1,length=50)
    adv = 2*(fa*pi^2 + fb*pi + fc)
    names(adv) = pi
    return(list(pith=pith,adv=adv))
  } else {
    return(pith)
  }
  
}

#tpith(0.85,0.3,0.3,0.8,0.4)

## Pi threshold EU_H(phi_v1x1) > EU_H(phi_v1x0) 
tvsgpith <- function(p,ka,kr,d,c,mode="plus", exportadv=FALSE) {
  
  fa = (1-ka-kr)*(p*kr-(1-p)*(1-ka)) - (1-ka-kr)*ka*(p*(1+ka-kr)-(ka+kr))
  if (any(fa<0) & mode=="plus") warning("mode should be minus for some values")
  fb = (1-ka-kr)*(p*d-c)-(1-ka)*(ka+kr-0.5) - ((1-ka-kr)*(p*ka*d - (ka+kr)*(d-c))-ka*(ka+kr-0.5))
  fc = -(ka+kr-0.5)*c - (-(ka+kr-0.5)*(d-c))
  pith = kaifm(fa,fb,fc,mode=mode)
  
  if (exportadv==TRUE) {
    pi = seq(0,1,length=50)
    adv = 2*(fa*pi^2 + fb*pi + fc)
    names(adv) = pi
    return(list(pith=pith,adv=adv))
  } else {
    return(pith)
  }
  
}

#tvsgpith(seq(0.75,1,by=0.05),0.3,0.3,0.6,0.4)

## Simulation 1
simdt <- data.frame(p=rep(c(0.75,0.85,0.95),each=50*3),
                    ka = rep(c(0.26,0.3,0.35),50*3),
                    kr = rep(c(0.26,0.3,0.35),50*3),
                    d = rep(rep(seq(0.45,0.95,length=50),each=3),3),
                    c = 0.4)

simdt$gpi_fa_minus <- ifelse(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr)<0,1,0)
all(simdt$gpi_fa_minus==0)
simdt$gpi_low <- gpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpi_fa_minus <- ifelse(simdt$p*kr-(1-simdt$p)*(1-simdt$ka)<0,1,0)
all(simdt$tpi_fa_minus)==0
simdt$tpi_low <- tpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpiovgpi_fa_minus <- ifelse((1-simdt$ka-simdt$kr)*(simdt$p*simdt$kr-(1-simdt$p)*(1-simdt$ka)) - (1-simdt$ka-simdt$kr)*simdt$ka*(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr))<0,1,0)
simdt$tpiovgpi_plus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)
simdt$tpiovgpi_minus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c, mode="minus")
(simdt$tpiovgpi_fa_minus == 1)*1 + (simdt$tpiovgpi_minus > simdt$tpiovgpi_plus | is.na(simdt$tpiovgpi_minus))*1 
simdt$tpiovgpi_low <- simdt$tpiovgpi_plus
simdt$tpiovgpi_high <- ifelse(simdt$tpiovgpi_fa_minus==1,simdt$tpiovgpi_minus,Inf)
simdt$tpiovgpi_low <- ifelse(simdt$tpiovgpi_high<0|is.nan(simdt$tpiovgpi_high),
                             NA,simdt$tpiovgpi_low)

simdt$tpi_low <- ifelse(simdt$tpi_low>1,simdt$tpi_low,
                        ifelse(is.na(simdt$tpiovgpi_low),simdt$tpi_low,
                               ifelse(simdt$tpiovgpi_low>simdt$tpi_low,
                                      simdt$tpiovgpi_low,simdt$tpi_low)))

simdt$delegatory_d1 <- (simdt$ka/(1-simdt$kr)+1)*simdt$c
simdt$delegatory_d2 <- (simdt$kr/(1-simdt$ka)+1)*simdt$c
simdt$delegatory_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                             simdt$delegatory_d1,simdt$delegatory_d2)
simdt$discouraged_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                              simdt$delegatory_d2,simdt$delegatory_d1)

prange <- ggplot(simdt) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=min(simdt$d),xmax=max(simdt$d),fill="a",colour="a")) + 
  geom_ribbon(aes(ymin=gpi_low,ymax=Inf,x=d,fill="gpi",colour="gpi"),alpha=1) + 
  geom_ribbon(aes(ymin=tpi_low,ymax=Inf,x=d,fill="tpi",colour="tpi"),alpha=1) +
  geom_rect(aes(ymax=Inf,ymin=-Inf,
                xmax=delegatory_d,xmin=discouraged_d),
            linetype=2,colour="black",fill=NA) + 
  facet_grid(ka~p,
             labeller = label_bquote(
               rows = kappa[a] ~ "=" ~ kappa[r] ~ "=" ~  .(ka),
               cols = p ~ "=" ~ .(p))) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=min(simdt$d),xmax=max(simdt$d)),fill=NA,colour="black") + 
  coord_cartesian(xlim=c(min(simdt$d),max(simdt$d)),ylim=c(0,1), expand=FALSE) + 
  xlab("d (c=0.4)\n\nFrom the vertical line, the left is discouraged and the right is delegatory context") + 
  ylab(bquote(pi)) +
  scale_fill_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                    labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                    values=c("white","#99d8c9","#f03b20")) +
  scale_colour_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                    labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                    values=c("black","black","black")) +
  scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8)) + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size=13),
                          panel.background = element_rect(fill="white",colour="black"),
                          plot.background=element_rect(fill="white", colour="white"),
                          legend.position = "bottom") +
  annotate("text",x=0.94,y=0.2,label="Delegatory\nContext",size=3,hjust=1) + 
  annotate("text",x=0.46,y=0.2,label="Discouraged\nContext",size=3,hjust=0)

prange

ggsave("figure/rangegraph-1.jpeg", prange, width=8, height=5.5, dpi=500)

## Simulation 2 (smaller c)
0.45/0.4; 0.95/0.4
simdt <- data.frame(p=rep(c(0.75,0.85,0.95),each=50*3),
                    ka = rep(c(0.26,0.3,0.35),50*3),
                    kr = rep(c(0.26,0.3,0.35),50*3),
                    d = rep(rep(seq(0.2*1.125,0.2*2.375,length=50),each=3),3),
                    c = 0.2)

simdt$gpi_fa_minus <- ifelse(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr)<0,1,0)
all(simdt$gpi_fa_minus==0)
simdt$gpi_low <- gpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpi_fa_minus <- ifelse(simdt$p*kr-(1-simdt$p)*(1-simdt$ka)<0,1,0)
all(simdt$tpi_fa_minus)==0
simdt$tpi_low <- tpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpiovgpi_fa_minus <- ifelse((1-simdt$ka-simdt$kr)*(simdt$p*simdt$kr-(1-simdt$p)*(1-simdt$ka)) - (1-simdt$ka-simdt$kr)*simdt$ka*(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr))<0,1,0)
simdt$tpiovgpi_plus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)
simdt$tpiovgpi_minus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c, mode="minus")
(simdt$tpiovgpi_fa_minus == 1)*1 + (simdt$tpiovgpi_minus > simdt$tpiovgpi_plus)*1 
(simdt$tpiovgpi_fa_minus == 1)*1 + (simdt$tpiovgpi_minus > simdt$tpiovgpi_plus | is.na(simdt$tpiovgpi_minus))*1 
simdt$tpiovgpi_low <- simdt$tpiovgpi_plus
simdt$tpiovgpi_high <- ifelse(simdt$tpiovgpi_fa_minus==1,simdt$tpiovgpi_minus,Inf)
simdt$tpiovgpi_low <- ifelse(is.nan(simdt$tpiovgpi_high) & simdt$tpiovgpi_fa_minus==0,-Inf,
                             ifelse(simdt$tpiovgpi_high<0 | is.nan(simdt$tpiovgpi_high),
                             NA,simdt$tpiovgpi_low))


simdt$tpi_low <- ifelse(simdt$tpi_low>1,simdt$tpi_low,
                        ifelse(is.na(simdt$tpiovgpi_low),simdt$tpi_low,
                               ifelse(simdt$tpiovgpi_low>simdt$tpi_low,
                                      simdt$tpiovgpi_low,simdt$tpi_low)))

simdt$delegatory_d1 <- (simdt$ka/(1-simdt$kr)+1)*simdt$c
simdt$delegatory_d2 <- (simdt$kr/(1-simdt$ka)+1)*simdt$c
simdt$delegatory_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                             simdt$delegatory_d1,simdt$delegatory_d2)
simdt$discouraged_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                              simdt$delegatory_d2,simdt$delegatory_d1)

prange2 <- ggplot(simdt) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=min(simdt$d),xmax=max(simdt$d),fill="a",colour="a")) + 
  geom_ribbon(aes(ymin=gpi_low,ymax=Inf,x=d,fill="gpi",colour="gpi"),alpha=1) + 
  geom_ribbon(aes(ymin=tpi_low,ymax=Inf,x=d,fill="tpi",colour="tpi"),alpha=1) +
  geom_rect(aes(ymax=Inf,ymin=-Inf,
                xmax=delegatory_d,xmin=discouraged_d),
            linetype=2,colour="black",fill=NA) +
  facet_grid(ka~p,
             labeller = label_bquote(
               rows = kappa[a] ~ "=" ~ kappa[r] ~ "=" ~  .(ka),
               cols = p ~ "=" ~ .(p))) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=min(simdt$d),xmax=max(simdt$d)),fill=NA,colour="black") + 
  coord_cartesian(xlim=c(min(simdt$d),max(simdt$d)),ylim=c(0,1), expand=FALSE) + 
  xlab("d (c=0.2)\n\nFrom the vertical line, the left is discouraged and the right is delegatory context") + 
  ylab(bquote(pi)) +
  scale_fill_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                    labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                    values=c("white","#99d8c9","#f03b20")) +
  scale_colour_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                      labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                      values=c("black","black","black")) +
  scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8)) + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size=13),
                          panel.background = element_rect(fill="white",colour="black"),
                          plot.background=element_rect(fill="white", colour="white"),
                          legend.position = "bottom") +
  annotate("text",x=max(simdt$d)-0.01,y=0.2,label="Delegatory\nContext",size=3,hjust=1) + 
  annotate("text",x=min(simdt$d)+0.01,y=0.2,label="Discouraged\nContext",size=3,hjust=0)
prange2

ggsave("figure/rangegraph-2.jpeg", prange2, width=8, height=5.5, dpi=500)

## Simulation 3 (larger c)
0.45/0.4; 0.95/0.4
simdt <- data.frame(p=rep(c(0.75,0.85,0.95),each=50*3),
                    ka = rep(c(0.26,0.3,0.35),50*3),
                    kr = rep(c(0.26,0.3,0.35),50*3),
                    d = rep(rep(seq(0.6*1.125,0.6*2.375,length=50),each=3),3),
                    c = 0.6)

simdt$gpi_fa_minus <- ifelse(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr)<0,1,0)
all(simdt$gpi_fa_minus==0)
simdt$gpi_low <- gpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpi_fa_minus <- ifelse(simdt$p*kr-(1-simdt$p)*(1-simdt$ka)<0,1,0)
all(simdt$tpi_fa_minus)==0
simdt$tpi_low <- tpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpiovgpi_fa_minus <- ifelse((1-simdt$ka-simdt$kr)*(simdt$p*simdt$kr-(1-simdt$p)*(1-simdt$ka)) - (1-simdt$ka-simdt$kr)*simdt$ka*(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr))<0,1,0)
simdt$tpiovgpi_plus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)
simdt$tpiovgpi_minus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c, mode="minus")
(simdt$tpiovgpi_fa_minus == 1)*1 + (simdt$tpiovgpi_minus > simdt$tpiovgpi_plus | is.na(simdt$tpiovgpi_minus))*1 
simdt$tpiovgpi_low <- simdt$tpiovgpi_plus
simdt$tpiovgpi_high <- ifelse(simdt$tpiovgpi_fa_minus==1,simdt$tpiovgpi_minus,Inf)
simdt$tpiovgpi_low <- ifelse(simdt$tpiovgpi_high<0|is.nan(simdt$tpiovgpi_high),
                             NA,simdt$tpiovgpi_low)

simdt$tpi_low <- ifelse(simdt$tpi_low>1,simdt$tpi_low,
                        ifelse(is.na(simdt$tpiovgpi_low),simdt$tpi_low,
                               ifelse(simdt$tpiovgpi_low>simdt$tpi_low,
                                      simdt$tpiovgpi_low,simdt$tpi_low)))

simdt$delegatory_d1 <- (simdt$ka/(1-simdt$kr)+1)*simdt$c
simdt$delegatory_d2 <- (simdt$kr/(1-simdt$ka)+1)*simdt$c
simdt$delegatory_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                             simdt$delegatory_d1,simdt$delegatory_d2)
simdt$discouraged_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                              simdt$delegatory_d2,simdt$delegatory_d1)

prange3 <- ggplot(simdt) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=min(simdt$d),xmax=max(simdt$d),fill="a",colour="a")) + 
  geom_ribbon(aes(ymin=gpi_low,ymax=Inf,x=d,fill="gpi",colour="gpi"),alpha=1) + 
  geom_ribbon(aes(ymin=tpi_low,ymax=Inf,x=d,fill="tpi",colour="tpi"),alpha=1) +
  geom_rect(aes(ymax=Inf,ymin=-Inf,
                xmax=delegatory_d + 0.0001,xmin=delegatory_d),
            linetype=2,colour="black",fill=NA) +
  facet_grid(ka + kr ~p,
             labeller = label_bquote(
               rows = kappa[a] ~ "=" ~ kappa[r] ~ "=" ~  .(ka),
               cols = p ~ "=" ~ .(p))) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=min(simdt$d),xmax=max(simdt$d)),fill=NA,colour="black") + 
  coord_cartesian(xlim=c(min(simdt$d),max(simdt$d)),ylim=c(0,1), expand=FALSE) + 
  xlab("d (c=0.6)\n\nFrom the vertical line, the left is discouraged and the right is delegatory context") + 
  ylab(bquote(pi)) +
  scale_fill_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                    labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                    values=c("white","#99d8c9","#f03b20")) +
  scale_colour_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                      labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                      values=c("black","black","black")) +
  scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8)) + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size=13),
                          panel.background = element_rect(fill="white",colour="black"),
                          plot.background=element_rect(fill="white", colour="white"),
                          legend.position = "bottom") +
  annotate("text",x=max(simdt$d)-0.01,y=0.2,label="Delegatory\nContext",size=3,hjust=1) + 
  annotate("text",x=min(simdt$d)+0.01,y=0.2,label="Discouraged\nContext",size=3,hjust=0)
prange3

ggsave("figure/rangegraph-3.jpeg", prange3, width=8, height=5.5, dpi=500)

## Simulation 4
simdt <- data.frame(p=rep(c(0.75,0.85,0.95),each=50*3),
                    ka = rep(c(0.35,0.3,0.25),50*3),
                    kr = rep(c(0.25,0.3,0.35),50*3),
                    d = rep(rep(seq(0.45,0.95,length=50),each=3),3),
                    c = 0.4)

simdt$gpi_fa_minus <- ifelse(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr)<0,1,0)
all(simdt$gpi_fa_minus==0)
simdt$gpi_low <- gpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpi_fa_minus <- ifelse(simdt$p*kr-(1-simdt$p)*(1-simdt$ka)<0,1,0)
all(simdt$tpi_fa_minus)==0
simdt$tpi_low <- tpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)

simdt$tpiovgpi_fa_minus <- ifelse((1-simdt$ka-simdt$kr)*(simdt$p*simdt$kr-(1-simdt$p)*(1-simdt$ka)) - (1-simdt$ka-simdt$kr)*simdt$ka*(simdt$p*(1+simdt$ka-simdt$kr)-(simdt$ka+simdt$kr))<0,1,0)
simdt$tpiovgpi_plus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c)
simdt$tpiovgpi_minus <- tvsgpith(simdt$p,simdt$ka,simdt$kr,simdt$d,simdt$c, mode="minus")
(simdt$tpiovgpi_fa_minus == 1)*1 + (simdt$tpiovgpi_minus > simdt$tpiovgpi_plus | is.na(simdt$tpiovgpi_minus))*1 
simdt$tpiovgpi_low <- simdt$tpiovgpi_plus
simdt$tpiovgpi_high <- ifelse(simdt$tpiovgpi_fa_minus==1,simdt$tpiovgpi_minus,Inf)
simdt$tpiovgpi_low <- ifelse(simdt$tpiovgpi_high<0|is.nan(simdt$tpiovgpi_high),
                             NA,simdt$tpiovgpi_low)

simdt$tpi_low <- ifelse(simdt$tpi_low>1,simdt$tpi_low,
                        ifelse(is.na(simdt$tpiovgpi_low),simdt$tpi_low,
                               ifelse(simdt$tpiovgpi_low>simdt$tpi_low,
                                      simdt$tpiovgpi_low,simdt$tpi_low)))

simdt$delegatory_d1 <- (simdt$ka/(1-simdt$kr)+1)*simdt$c
simdt$delegatory_d2 <- (simdt$kr/(1-simdt$ka)+1)*simdt$c
simdt$delegatory_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                             simdt$delegatory_d1,simdt$delegatory_d2)
simdt$discouraged_d <- ifelse(simdt$delegatory_d1>simdt$delegatory_d2,
                             simdt$delegatory_d2,simdt$delegatory_d1)

prange4 <- ggplot(simdt) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=0.45,xmax=0.95,fill="a",colour="a")) + 
  geom_ribbon(aes(ymin=gpi_low,ymax=Inf,x=d,fill="gpi",colour="gpi"),alpha=1) + 
  geom_ribbon(aes(ymin=tpi_low,ymax=Inf,x=d,fill="tpi",colour="tpi"),alpha=1) +
  geom_rect(aes(ymax=Inf,ymin=-Inf,
                xmax=delegatory_d,xmin=discouraged_d),
            linetype=2,colour="black",fill=NA) + 
  facet_grid(ka + kr ~p,
             labeller = label_bquote(
               rows = kappa[a] ~ "=" ~ .(ka) ~ ";" ~ kappa[r] ~ "=" ~  .(kr),
               cols = p ~ "=" ~ .(p))) + 
  geom_rect(aes(ymin=0,ymax=1,xmin=0.45,xmax=0.95),fill=NA,colour="black") + 
  coord_cartesian(xlim=c(0.45,0.95),ylim=c(0,1), expand=FALSE) + 
  xlab("d (c=0.4)\n\nFrom the vertical line, the left is discouraged and the right is delegatory context") + 
  ylab(bquote(pi)) +
  scale_fill_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                    labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                    values=c("white","#99d8c9","#f03b20")) +
  scale_colour_manual(name=bquote("Equilibrium Policy Quality Decision ("~phi[H]~") "),
                      labels = c(0,bquote(phi[v1x0]/p),bquote(phi[v1x1]/p)),
                      values=c("black","black","black")) +
  scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8)) + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size=13),
                          panel.background = element_rect(fill="white",colour="black"),
                          plot.background=element_rect(fill="white", colour="white"),
                          legend.position = "bottom") +
  annotate("text",x=0.94,y=0.2,label="Delegatory\nContext",size=3,hjust=1) + 
  annotate("text",x=0.46,y=0.2,label="Discouraged\nContext",size=3,hjust=0)
prange4

ggsave("figure/rangegraph-4.jpeg", prange4, width=8, height=5.5, dpi=500)

#' # Save Workspace

save.image("Kato2018thlo_simulations.RData")

