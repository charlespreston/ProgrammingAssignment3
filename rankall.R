rankall<-function(outcome, num = "best") {

	# read outcome data
	data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	outcomes<-c("heart attack", "heart failure", "pneumonia")
	colnums <- c(11, 17, 23)
	colnum <- colnums[grep(outcome, outcomes, ignore.case=TRUE)]

	#data <- data[,c(2, 7, colnum)]
 	if ( num == "best" ) { num = 1 }

	if (!toupper(outcome) %in% toupper(outcomes)) stop("invalid outcome")

	# instance a data frame to append to later
	df <- data.frame(hospital=character(), state=character(), stringsAsFactors=F)
	#df <- data.frame(hospital=character(), state=character())
	mun=num

	states<-sort(unique(data[,7]))

	for ( state in states ) {
	
		ourstate <- subset(data, data$State == state)[,c(2, colnum)]
		ourstate[,2] <- suppressWarnings(as.numeric(as.character(ourstate[,2])))
		ourstate<-na.omit(ourstate)

		ourstate<-ourstate[order(ourstate[,2],ourstate[,1]),]	

		ourstate[,3] <- rank(ourstate[,2], ties.method="first")

        	if ( num == "worst" ) { mun = nrow(ourstate) }
		mun = as.numeric(mun)
		hosp<-subset(ourstate, ourstate$V3 == mun)[1,1]
		
		df<-rbind(df, data.frame(hospital=hosp, state=state))
	}
	
	row.names(df) <- df[,2]
	return(df)

}
