# type of onset ���ϱ� (�ٿ��� �ڵ�)
alshistory<-read.csv("C:/Temp/AlsHistory_r.CSV")
for (i in 1:length(alshistory$subject_id))
{
  if (alshistory$Site_of_Onset[i]=="Onset: Bulbar"){
    alshistory$Onsetsite[i]="Bulbar"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Limb"){
    alshistory$Onsetsite[i]="Limb"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Limb and Bulbar"){
    alshistory$Onsetsite[i]="Limb and Bulbar"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Other"){
    alshistory$Onsetsite[i]="Other"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Spine"){
    alshistory$Onsetsite[i]="Limb"
  }
  if (!is.na(alshistory$Site_of_Onset___Bulbar[i])) {
    if (alshistory$Site_of_Onset___Bulbar[i]==1) {
      alshistory$Onsetsite[i]="Bulbar"
    }
  }
  if (!is.na(alshistory$Site_of_Onset___Limb[i])) {
    if (alshistory$Site_of_Onset___Limb[i]==1) {
      alshistory$Onsetsite[i]="Limb"
    }
  }
  if (!is.na(alshistory$Site_of_Onset___Bulbar[i]) & !is.na(alshistory$Site_of_Onset___Limb[i])){
    if (alshistory$Site_of_Onset___Bulbar[i]==1 & alshistory$Site_of_Onset___Limb[i]==1) {
      alshistory$Onsetsite[i]="Limb and Bulbar"
    }
  }
  
}
head(alshistory)
class(alshistory$Onsetsite)
alshistory_sub <- subset(alshistory, subset=!is.na(Onsetsite),select=c(subject_id, Onsetsite,Onset_Delta,Diagnosis_Delta))
head(alshistory_sub, n=30)
#merge
ALSFRS_df_firsttime2 <- merge(x=ALSFRS_df_firsttime1, y=alshistory_sub, by="subject_id", all.x=TRUE)
head(ALSFRS_df_firsttime2, n=50)
#cluster���� onsetsite Ȯ��
table(ALSFRS_df_firsttime2$Cluster1, ALSFRS_df_firsttime2$Onsetsite)
mosaicplot(Onsetsite~Cluster1, data=ALSFRS_df_firsttime2, las=1)
# ���̰� �������� �������� chi-square ���� ����
chisq.test(ALSFRS_df_firsttime2$Cluster1, ALSFRS_df_firsttime2$Onsetsite, correct=FALSE)