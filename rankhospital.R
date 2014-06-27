rankhospital<-function(state, outcome, num = "best") {

	# read outcome data
	data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	# setup an index to help find the right column
	outcomes<-c("heart attack", "heart failure", "pneumonia")
	colnums <- c(11, 17, 23)

	# determine the requested column
	colnum <- colnums[grep(outcome, outcomes, ignore.case=TRUE)]

	# extract just the data we want
	ourstate <- subset(data, data$State == toupper(state))[, c( 2, colnum)]

	# test the input
	if (!toupper(state) %in% data$State) stop("invalid state")
	if (!toupper(outcome) %in% toupper(outcomes)) stop("invalid outcome")

	# coerce the data into a useful class
	ourstate[,2] <- suppressWarnings(as.numeric(as.character(ourstate[,2])))
	
	# exclude hospitals without data - going to do this by getting rid of NAs 
	# any hospital with all NAs (or no data) will be eliminated
	ourstate<-na.omit(ourstate)

	# order the list prior to ranking so that ties are broken alphabetically
	ourstate<-ourstate[order(ourstate[,2],ourstate[,1]),]	

	# rank the list and add it as a column
	# use the first method since the previous step already broke ties
	ourstate[,3] <- rank(ourstate[,2], ties.method="first")

	# set the rank
 	if ( num == "best" ) { num = 1 }
        if ( num == "worst" ) { num = nrow(ourstate) }
	num = as.numeric(num)

	#return the one we want
	return(subset(ourstate, ourstate$V3 == num)[1,1])

}
