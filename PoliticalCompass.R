library(ggplot2)
library(ggrepel)    # https://github.com/slowkow/ggrepel
library(dplyr)
library(lpSolve)

d <- read.csv("votecompass.csv", header = TRUE) %>%
  mutate(EconAbs = abs(Econ),SocAbs = abs(Soc),
         EconPct = Econ/sum(abs(.[["Econ"]]))*100,
         SocPct = Soc/sum(abs(.[["Soc"]]))*100)

zoomedInArrows <- ggplot(d) +
  geom_segment(aes(x=0,y=0,xend=EconPct,yend=SocPct),
               arrow = arrow(length = unit(0.3,"cm")), colour="grey25") +
  geom_text_repel(aes(x=EconPct,y=SocPct,label=Label), segment.size=0, force=1) +
  xlab("Economic") + ylab("Social") + coord_fixed(ylim=c(-12,12), xlim=c(-12,12))
ggsave(zoomedInArrows, filename = "zoomedInArrows.png", width=6, height=6)


wholePicture <- ggplot(d) +
  geom_segment(aes(x=0,y=0,xend=EconPct,yend=SocPct), arrow = arrow(length = unit(0.15,"cm")), colour="grey25") +
  ylim(c(-100,100)) + xlim(c(-100,100)) +
  aes(x=EconPct, y=SocPct, label=Label) +
  xlab("Economic") + ylab("Social") +
  geom_text(data=data.frame(SocPct=-75, EconPct=-80, Label="Nostalgic\nRacists"), fontface="bold") + 
  geom_text(data=data.frame(SocPct=75, EconPct=-80, Label="Literally\nCommunists"), fontface="bold") +
  geom_text(data=data.frame(SocPct=75, EconPct=80, Label="Glibertarians"), fontface="bold") +
  geom_text(data=data.frame(SocPct=-75, EconPct=80, Label="Culture\nWarriors"), fontface="bold") +
  geom_text(data=data.frame(SocPct=0, EconPct=80, Label="Fuck You,\nGot Mine")) +
  geom_text(data=data.frame(SocPct=-75, EconPct=0, Label="FWD: Fwd: Fwd:\nFwd: RE: Islam")) +
  geom_text(data=data.frame(SocPct=75, EconPct=0, Label="Ukulele-strumming\ncold-drip sippers")) +
  geom_text(data=data.frame(SocPct=0, EconPct=-80, Label="Eat the Rich"))

ggsave(plot=wholePicture, filename = "politicalCompass.png", width=5, height=5)

# Use integer programming to find the smallest set of opinions to get above 50.
f.obj <- rep(1,30)
econValues <- abs(d$EconPct)
socValues <- abs(d$SocPct)
pureEconValues <- econValues
pureSocValues <- socValues
mixedOpinions <- sign(d$EconPct) * sign(d$SocPct) == -1
pureEconValues[mixedOpinions] <- 0
pureSocValues[mixedOpinions] <- 0

glibSoln <- lp("min", f.obj, rbind(pureEconValues, pureSocValues),
               c(">=", ">="), c(50,50), all.bin=TRUE)
racistSoln <- lp("min", f.obj, rbind(-pureEconValues, -pureSocValues),
               c("<=", "<="), c(-50,-50), all.bin=TRUE)
commoSoln <- lp("min", f.obj, rbind(-econValues, socValues),
               c("<=", ">="), c(-50,50), all.bin=TRUE)
abbottSoln <- lp("min", f.obj, rbind(econValues, -socValues),
               c(">=", "<="), c(50,-50), all.bin=TRUE)

print(d$FullText[glibSoln$solution == 1])
print(d$FullText[commoSoln$solution == 1])


