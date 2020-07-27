#Packages
Packages <- c("geomorph", "ggplot2","magrittr","dplyr","Morpho","rgl","ape","abind")
lapply(Packages, library, character.only = TRUE)

#Refmesh cranium
mshape<-mshape(Y.gpa$coords)
mesh<-read.table("mesh.dta")
mesh<-as.matrix(mesh)
ply<-read.ply("mesh.ply")
refmesh<-warpRefMesh(ply,mesh,mshape)
grandmean <-gpca$Grandmean
gpcavis2sd<- showPC(2*sd(gpca$Scores[,1]), gpca$groupPCs[,1], grandmean)
gpcavis2sd.neg<- showPC(-2*sd(gpca$Scores[,1]), gpca$groupPCs[,1], grandmean)
deformGrid3d(gpcavis2sd, gpcavis2sd.neg, ngrid = 0)
plotRefToTarget(mshape,gpcavis2sd,refmesh,method="surface")
plotRefToTarget(mshape,gpcavis2sd.neg,refmesh,method="surface")

#Between groups PCA for the crania of all specimens
LM<-readland.nts("Cranium1.txt")
LM<-estimate.missing(LM,method="Reg")
LM2<-readland.nts("Cranium2.txt")
LM3<-readland.nts("Cranium3.txt")
LM<-abind(LM,LM2,LM3)
curv<- as.matrix(read.csv("curveslide_cranium.csv", header=T))
classifier<-read.csv("Cranium_groups.csv", header=T, row.names=1, sep=";")
Type<-classifier$TS
Type2<-factor(Type,levels=c("Wild boar - Stabling","Wild Boar - Enclosure","Wild Boar - Free - Germany","Wild Boar - Free - France","Wild Boar - Free - Control","Traditional Domestic Pigs - Captive","Traditional Domestic Pigs - Free", "Intensive Domestic Pigs - Captive","Hybrids - Captive","Feral Pig - Free","Wild Boar - Captive - France"))
Y.gpa<-gpagen(LM, Proj = TRUE, ProcD = FALSE, surfaces = NULL,curves = curv)
gpca<-groupPCA(Y.gpa$coords,Type2)
PC1<-as.matrix(gpca$Scores[,1])
PC2<-as.matrix(gpca$Scores[,2])
PCA<-data.frame(PC1,PC2,Type2)
hull_PCA<- PCA %>%
  group_by(Type2) %>%
  slice(chull(PC1, PC2))
ggplot(PCA, aes(PC1, PC2,shape=Type2))+
  xlab("PC1 (82.7%)")+ylab("PC3 (3.5%)") + geom_point() +
  scale_shape_manual(values=c(22,24,3,21,25,23,4,8,7,1,2))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  aes(fill = factor(Type2)) + scale_fill_manual(values=c("#00516F","#507D8D","#14252C","#40E029","#A7D379","#E7184E","#7C4149","#FF72ED","chocolate2"),name = "Groups")+ 
  geom_polygon(data = hull_PCA, alpha = 0.5)

#Between groups PCA for the crania of wild boar
LM<-readland.nts("Cranium1.txt")
LM<-estimate.missing(LM,method="Reg")
LM2<-readland.nts("Cranium2.txt")
LM<-abind(LM,LM2)
curv<- as.matrix(read.csv("curveslide_cranium.csv", header=T))
classifier<-read.csv("Cranium_groups.csv", header=T, row.names=1, sep=",")
Type<-classifier$TS
Type2<-factor(Type,levels=c("Wild boar - Stabling","Wild Boar - Enclosure","Wild Boar - Free - Germany","Wild Boar - Free - France","Wild Boar - Free - Control","Traditional Domestic Pigs - Captive","Traditional Domestic Pigs - Free", "Intensive Domestic Pigs - Captive","Hybrids - Captive","Feral Pig - Free","Wild Boar - Captive - France"))
Y.gpa<-gpagen(LM, Proj = TRUE, ProcD = FALSE, surfaces = NULL,curves = curv)
gpca<-groupPCA(Y.gpa$coords,Type2)
PC1<-as.matrix(gpca$Scores[,1])
PC2<-as.matrix(gpca$Scores[,2])
PCA<-data.frame(PC1,PC2,Type2)
hull_PCA<- PCA %>%
  group_by(Type2) %>%
  slice(chull(PC1, PC2))
ggplot(PCA, aes(PC1, PC2,shape=Type2))+xlab("PC1 (50.1%)")+ylab("PC2 (33.8%)") + geom_point() + scale_shape_manual(values=c(22,24,3,21,25,23,4,8,7,1,2))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + aes(fill = factor(Type2)) + scale_fill_manual(values=c("#00516F","#507D8D","#14252C","#40E029","#A7D379","#E7184E","#7C4149","#FF72ED","chocolate2"),name = "Groups")+ geom_polygon(data = hull_PCA, alpha = 0.5)

#Between groups PCA for the mandible of all specimens
LM<-readland.nts("Mandible1.dta")
LM2<-readland.nts("Mandible2.dta")
LM3<-readland.nts("Mandible3.dta")
LM<-abind(LM,LM2,LM3)
curv <- as.matrix(read.csv("curveslide_mandibule.csv", header=T))
classifier<-read.csv("Mandible_groups.csv", header=T, row.names=1, sep=";")
Y.gpa<-gpagen(LM, Proj = TRUE, ProcD = FALSE, curves = curv, surfaces = NULL)
Type<-classifier$TS
Type2<-factor(Type,levels=c("Wild boar - Stabling","Wild Boar - Enclosure","Wild Boar - Free - France","Wild Boar - Free - Control","Traditional Domestic Pigs - Captive","Traditional Domestic Pigs - Free", "Intensive Domestic Pigs - Captive","Hybrids - Captive","Feral Pig - Free","Wild Boar - captive"))
gpca<-groupPCA(Y.gpa$coords,Type2)
PC1<-as.matrix(gpca$Scores[,1])
PC2<-as.matrix(gpca$Scores[,2])
PCA<-data.frame(PC1,PC2,Type2)
hull_PCA<- PCA %>%
  group_by(Type2) %>%
  slice(chull(PC1, PC2))
ggplot(PCA, aes(PC1, PC2,shape=Type2))+xlab("PC1 (67.5%)")+ylab("PC2 (20.5%)") + geom_point() + scale_shape_manual(values=c(22,24,21,25,23,4,8,7,1,2,3))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + aes(fill = factor(Type2)) + scale_fill_manual(values=c("#00516F","#507D8D","#40E029","#A7D379","#E7184E","#7C4149","#FF72ED","chocolate2"),name = "Groups")+ geom_polygon(data = hull_PCA, alpha = 0.5)

#Between groups PCA for the mandible of wild boar
LM<-readland.nts("Mandible1.dta")
LM3<-readland.nts("Mandible3.dta")
LM<-abind(LM,LM3)
curv <- as.matrix(read.csv("curveslide_mandibule.csv", header=T))
classifier<-read.csv("Mandible_groups.csv", header=T, row.names=1, sep=";")
Y.gpa<-gpagen(LM, Proj = TRUE, ProcD = FALSE, curves = curv, surfaces = NULL)
Type<-classifier$TS
Type2<-factor(Type,levels=c("Wild boar - Stabling","Wild Boar - Enclosure","Wild Boar - Free - France","Wild Boar - Free - Control","Traditional Domestic Pigs - Captive","Traditional Domestic Pigs - Free", "Intensive Domestic Pigs - Captive","Hybrids - Captive","Feral Pig - Free","Wild Boar - captive"))
gpca<-groupPCA(Y.gpa$coords,Type2)
PC1<-as.matrix(gpca$Scores[,1])
PC2<-as.matrix(gpca$Scores[,2])
PCA<-data.frame(PC1,PC2,Type2)
hull_PCA<- PCA %>%
  group_by(Type2) %>%
  slice(chull(PC1, PC2))
ggplot(PCA, aes(PC1, PC2,shape=Type2))+ coord_equal()+xlab("PC1 (80.4.5%)")+ylab("PC2 (14.7%)") + geom_point() + scale_shape_manual(values=c(22,24,21,25,23,4,8,7,1,2,3))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + aes(fill = factor(Type2)) + scale_fill_manual(values=c("#00516F","#507D8D","#40E029","#A7D379","#E7184E","#7C4149","#FF72ED","chocolate2"),name = "Groups")+ geom_polygon(data = hull_PCA, alpha = 0.5)

#Boxplot LogCS
logcsize<-log10(Y.gpa$Csize)
vc1<-data.frame(logcsize,Type2)
ggplot(vc1,aes(x=Type2,y=logcsize))+ggtitle("Boxplot Size Cranium")+
  geom_boxplot() +
  xlab("")+ylab("Log Centroid Size")+theme_classic() 

#PLS Crania
LM<-readland.nts("Cranium1_PLS.txt")
LM<-estimate.missing(LM,method="Reg")
curv<- as.matrix(read.csv("curveslide_cranium.csv", header=T))
Y.gpa<-gpagen(LM, Proj = TRUE, ProcD = FALSE, surfaces = NULL,curves = curv)
classifier<-read.csv("Mesures_muscles.csv", header=T, row.names=1, sep=";")
Masseter_Superficialis<-log10(classifier$PCSA.C..masseter.superficialis)
PCSA<-as.data.frame(Masseter_Superficialis,row.names=dimnames(Y.gpa$coords)[[3]])
masseter_profundus<-log10(classifier$PCSA.C..masseter.profundus)
PCSA$Masseter_Profundus<-as.data.frame(masseter_profundus)
Anterior_Zygomaticomandibularis<-log10(classifier$PCSA.C..ZMA)
PCSA$Anterior_Zygomaticomandibularis<-as.data.frame(Anterior_Zygomaticomandibularis)
Posterior_Zygomaticomandibularis<-log10(classifier$PCSA.C..ZMP)
PCSA$Posterior_Zygomaticomandibularis<-as.data.frame(Posterior_Zygomaticomandibularis)
Temporalis<-log10(classifier$PCSA.C..temporalis)
PCSA$Temporalis<-as.data.frame(Temporalis)
Pterygoideus_Lateralis<-log10(classifier$PCSA.C..pterygoideus.pars.lateralis)
PCSA$Pterygoideus_Lateralis<-as.data.frame(Pterygoideus_Lateralis)
Pterygoideus_Medialis<-log10(classifier$PCSA.C..pterygoideus.pars.medialis)
PCSA$Pterygoideus_Medialis<-as.data.frame(Pterygoideus_Medialis)
PLS<-two.b.pls(Y.gpa$coords,PCSA_md,iter = 999)
plot(PLS_md,shapes=T)
PLS1B1<-PLS_md$XScores[,1]
PLS1B2<-PLS_md$YScores[,1]
Type<-classifier$groupe2
Type2<-factor(Type,levels=c("Wild boar - Stabling","Wild Boar - Enclosure","Wild Boar - Free","Wild Boar - Free - France","Wild Boar - Free - Control","Traditional Domestic Pigs - Captive","Traditional Domestic Pigs - Free", "Intensive Domestic Pigs - Captive","Hybrids - Captive"))
PCA<-data.frame(PLS1B1,PLS1B2,Type2)
hull_PCA<- PCA %>%
  group_by(Type2) %>%
  slice(chull(PLS1B1, PLS1B2))
ggplot(PCA, aes(PLS1B1, PLS1B2,shape=Type2))+geom_polygon(data = hull_PCA, alpha = 0.5)+geom_point()+ggtitle("PLS Cranium/PCSA")+xlab("Shape")+ylab("PCSA") + scale_shape_manual(values=c(22,24,3,21,25,23,4,8,7))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + aes(fill = factor(Type2)) + scale_fill_manual(values=c("#00516F","#507D8D","yellow","#40E029","#A7D379","#E7184E","#7C4149","#FF72ED","#255108"),name = "Groups")


#PLS Mandibles
LM<-readland.nts("Mandible1.txt")
curv<- as.matrix(read.csv("curveslide_mandibule.csv", header=T))
Y.gpa<-gpagen(LM, Proj = TRUE, ProcD = FALSE, surfaces = NULL,curves = curv)
classifier<-read.csv("Mesures_muscles_md.csv", header=T, row.names=1, sep=";")
Masseter_Superficialis<-log10(classifier$PCSA.C..masseter.superficialis)
PCSA_md<-as.data.frame(Masseter_Superficialis,row.names=dimnames(Y.gpa$coords)[[3]])
masseter_profundus<-log10(classifier$PCSA.C..masseter.profundus)
PCSA_md$Masseter_Profundus<-as.data.frame(masseter_profundus)
Zygomaticomandibularis_anterior<-log10(classifier$PCSA.C..ZMA)
PCSA_md$Anterior_Zygomaticomandibularis<-as.data.frame(Zygomaticomandibularis_anterior)
Zygomaticomandibularis_posterior<-log10(classifier$PCSA.C..ZMP)
PCSA_md$Posterior_Zygomaticomandibularis<-as.data.frame(Zygomaticomandibularis_posterior)
temporalis<-log10(classifier$PCSA.C..temporalis)
PCSA_md$Temporalis<-as.data.frame(temporalis)
pterygoideus_lateralis<-log10(classifier$PCSA.C..pterygoideus.pars.lateralis)
PCSA_md$Pterygoideus_Lateralis<-as.data.frame(pterygoideus_lateralis)
pterygoideus_medialis<-log10(classifier$PCSA.C..pterygoideus.pars.medialis)
PCSA_md$Pterygoideus_Medialis<-as.data.frame(pterygoideus_medialis)
PLS_md<-two.b.pls(Y.gpa$coords,PCSA_md,iter = 999)
plot(PLS_md,shapes=T)
PLS1B1<-PLS_md$XScores[,1]
PLS1B2<-PLS_md$YScores[,1]
Type<-classifier$groupe2
Type2<-factor(Type,levels=c("Wild boar - Stabling","Wild Boar - Enclosure","Wild Boar - Free","Wild Boar - Free - France","Wild Boar - Free - Control","Traditional Domestic Pigs - Captive","Traditional Domestic Pigs - Free", "Intensive Domestic Pigs - Captive","Hybrids - Captive"))
PCA<-data.frame(PLS1B1,PLS1B2,Type2)
hull_PCA<- PCA %>%
  group_by(Type2) %>%
  slice(chull(PLS1B1, PLS1B2))
ggplot(PCA, aes(PLS1B1, PLS1B2,shape=Type2))+geom_polygon(data = hull_PCA, alpha = 0.5)+geom_point()+ggtitle("PLS Cranium/PCSA")+xlab("Shape")+ylab("PCSA") + scale_shape_manual(values=c(22,24,3,21,25,23,4,8,7))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + aes(fill = factor(Type2)) + scale_fill_manual(values=c("#00516F","#507D8D","yellow","#40E029","#A7D379","#E7184E","#7C4149","#FF72ED","#255108"),name = "Groups")
