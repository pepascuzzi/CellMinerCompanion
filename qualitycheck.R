# define cancer cell line names
name <- c("BR:MCF7","BR:MDA_MB_231","BR:HS578T","BR:BT_549","BR:T47D","CNS:SF_268","CNS:SF_295","CNS:SF_539","CNS:SNB_19","CNS:SNB_75","CNS:U251","CO:COLO205","CO:HCC_2998","CO:HCT_116","CO:HCT_15","CO:HT29","CO:KM12","CO:SW_620","LE:CCRF_CEM","LE:HL_60","LE:K_562","LE:MOLT_4","LE:RPMI_8226","LE:SR","ME:LOXIMVI","ME:MALME_3M","ME:M14","ME:SK_MEL_2","ME:SK_MEL_28","ME:SK_MEL_5","ME:UACC_257","ME:UACC_62","ME:MDA_MB_435","ME:MDA_N","LC:A549","LC:EKVX","LC:HOP_62","LC:HOP_92","LC:NCI_H226","LC:NCI_H23","LC:NCI_H322M","LC:NCI_H460","LC:NCI_H522","OV:IGROV1","OV:OVCAR_3","OV:OVCAR_4","OV:OVCAR_5","OV:OVCAR_8","OV:SK_OV_3","OV:NCI_ADR_RES","PR:PC_3","PR:DU_145","RE:786_0","RE:A498","RE:ACHN","RE:CAKI_1","RE:RXF_393","RE:SN12C","RE:TK_10","RE:UO_31")

# define quality check function
qualitycheck <- function(dataset,counts){
	
	report <- matrix(0,ncol=dim(dataset)[2],nrow=6)
	colnames(report) <- colnames(dataset)
	rownames(report) <- c("Number of experiments","Min","Max","Number of repeated values","Number of missing values","Summary")
	report[1,] <- counts
	report[2,] <- apply(dataset,2,min,na.rm=T)
	report[3,] <- apply(dataset,2,max,na.rm=T)
	report[4,] <- apply(dataset,2,function(x)(max(table(as.factor(x)))))
	report[5,] <- apply(dataset,2,function(x)(length(which(is.na(x)) == TRUE)))
	
	if ((dim(dataset)[1] != 60) | (identical(name,rownames(dataset)) == FALSE)) {
		 report[6,] <- ("Error: data format is not correct") 
	}

	for (i in 1:dim(dataset)[2]) {
		tmp <- rep(F, 3)
		exp.no <- as.numeric(report[1,i])
		if(exp.no > 1) tmp[1] <- T
		repeats.no <- as.numeric(report[4,i])
		if(repeats.no <= 30) tmp[2] <- T
		na.no <- as.numeric(report[5,i])
		if(na.no <= 6) tmp[3] <- T
		
		if(all(tmp)) {
			report[6,i] <- "OK"
		}else{
			report[6,i] <- "Warning"
		}
		if(!tmp[1]) 
			report[6,i] <- paste(report[6,i], "too few experiments", sep=";")
		if(!tmp[2]) 
			report[6,i] <- paste(report[6,i], "too many repeated values", sep=";")
		if(!tmp[3]) 
			report[6,i] <- paste(report[6,i], "too many NAs", sep=";")
		
	}
	return(report)
}