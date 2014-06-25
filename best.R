best<-function(state, outcome) {

	# read outcome data
	data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	# just grab the rows for the state we want, the hospital names, and 30 day death rate for the requested outcome
	colint <- paste("^Hospital.30.Day.Death..Mortality..Rates.from", sub(" ", ".", paste(outcome, "$", sep="")), sep=".")
	colnum <- grep(colint, names(data), ignore.case=TRUE)
	ourstate <- subset(data, data$State == toupper(state))[, c(2,7, grep(colint, names(data), ignore.case=TRUE))]

	# validation check of input to function
	if (nrow(ourstate) == 0) { stop("invalid state") } # there were no states matching 'state'
	if (length(colnum) == 0) { stop("invalid outcome") } # there were no outcomes matching 'outcome'

	# exclude hospitals without data - going to do this by getting rid of NAs 
	# any hospital with all NAs (or no data) will be eliminated
	new.data<-na.omit(ourstate)
	
	# build list of lowest 30 day death rate
	# return only the first hospital in the list when sorted alphabetically
	return(new.data[order(x[,3],x[,1]),][1,1])
}