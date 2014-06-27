best<-function(state, outcome) {

	# read outcome data
	data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	# setup an index to help find the right column
	outcomes<-c("heart attack", "heart failure", "pneumonia")
	colnums <- c(11, 17, 23)

	# find the requested column
	colnum <- colnums[grep(outcome, outcomes, ignore.case=TRUE)]

	# test the input
	if (!toupper(state) %in% data$State) stop("invalid state")
	if (!toupper(outcome) %in% toupper(outcomes)) stop("invalid outcome")

	# extract just the data we want
	ourstate <- subset(data, data$State == toupper(state))[, c( 2, colnum)]

	# coerce the data into a useful class
	ourstate[,2] <- suppressWarnings(as.numeric(as.character(ourstate[,2])))
	
	# exclude hospitals without data - going to do this by getting rid of NAs 
	# any hospital with all NAs (or no data) will be eliminated
	ourstate<-na.omit(ourstate)
	
	# build list of lowest 30 day death rate
	# return only the first hospital in the list when sorted alphabetically
	return(ourstate[order(ourstate[,2],ourstate[,1]),][1,1])
}
