
runbench <- function(benchmark, output="log.txt", version="rjit", repeats=10){
        e <- (function () {
                source(benchmark, local=TRUE);
                return (function() {
                        execute()
                });
        })()

	result = paste(version, ",", benchmark)
	for(i in 1:repeats){
		time <- round(system.time(e())[3]*1000)
		result  <- paste(result, "," , time)
                write(paste("   [", version ,"]", i, ":", time), stderr())
	}

	write(result, file=output, append=TRUE)
}


calclog <- function(fileNames){

	#opening file of the name:fileNames
	conn <- file(fileNames,open="r")
	#stores every line in fileNames
	linn <-readLines(conn)
	
	close(conn)
	#counters used to parse the file
	i = 1
	m = 1

	#vector of the path without "/"
	nameTable <- strsplit(fileNames, "[/]")[1]

	#name of the .txt file
	testName <- strsplit(nameTable[[1]][length(nameTable[[1]])], "[.]")[[1]][1]

	#path to the .txt file
	pathName <- nameTable[[1]][1]
	for(y in 2:(length(nameTable[[1]])-1)){
		pathName <- paste(pathName, nameTable[[1]][y] ,sep="/")
	}

	#total number of times the execute in each game is ran
	numberofruns = length(strsplit(linn[1], "[,]")[[1]]) - 2

	#total number of files being ran
	numberoffiles = length(readLines(fileNames))/2

	#df of the complete raw, includes rjit and gnur
	rawdata <- data.frame(matrix(ncol=1,nrow=numberofruns))

	#the median of normalised rjit runtime
	medNR <- data.frame(matrix(ncol=7,nrow=numberoffiles), stringsAsFactors=FALSE)
	#setting up the labels for medNR
	names(medNR) <- c("name", "compilation_time", "median_time", "top_quantile", "bottom_quantile", "large_CT", "graph_CT") 

	#result of rjit normalised against the median of the corresponding gnur
	norm <- data.frame(matrix(ncol=numberoffiles+1, nrow=numberofruns))

	#setting up the labels for norm
	names(norm) <- "runs"
	norm[1, 1] <- "compilation time"
	for(p in 2:numberofruns){
		norm[p, 1] <- paste("run", p, sep="-")
	}

	#loop to traverse over every line in fileName
	while (i < length(linn)){
		
	   #vector with rjit and gnur result of line i
	   rjit <- strsplit(linn[i], "[,]")[1]
	   gnur <- strsplit(linn[i+1], "[,]")[1]

	   #matrix containing values parsed from the ith line in fileName
	   datas <- array(0, dim=c(length(rjit[[1]]) - 2,2))

	   #parsing the name of the benchmark
	   testN <- strsplit(strsplit(trim(rjit[[1]][2]),"[.]")[[1]][1], "[/]")
	   filen <- testN[[1]][length(testN[[1]])]
	   rjitn <- paste(filen, "rjit", sep="-")
	   gnurn <- paste(filen, "gnur", sep="-")


	   #temporarily storing the raw value of rjit and gnur run
	   for (j in 1:length(rjit[[1]])-2) {
	   	datas[j, 1] = strtoi(trim(rjit[[1]][j+2]))
	   	datas[j, 2] = strtoi(trim(gnur[[1]][j+2]))
	   }

	   #calculating the median for gnur
   	   med <- apply(datas, 2, median)[2]

   	   #normalising rjit and gnur against the median of gnur
   	   normRjit <- (datas/med)
   	      	  
   	   #storing the raw information into the rawdata df
   	   tempdatas <- data.frame(datas[, 1], datas[, 2]) 
	   names(tempdatas) <- c(rjitn, gnurn)
	   rawdata <- cbind(rawdata, tempdatas)

	   #storing the median, max and min values of the normalised rjit runtime
	   medNR$name[m] = filen
	   medNR$median_time[m] = median(normRjit[2:numberofruns])
	   medNR$top_quantile[m] = quantile(normRjit[2:numberofruns], c(0.75))
	   medNR$bottom_quantile[m] = quantile(normRjit[2:numberofruns], c(0.25))

	   #if the compilation time (run1) is two times greater than the largest non-compilation run then it is stored in large_CT 
	   if(normRjit[1] > max(normRjit[2:numberofruns])*2){
	   	medNR$compilation_time[m] = NA
	   	medNR$large_CT[m] = paste(round(normRjit[1], digits=1)*100, "%", sep="")
	   } else{
	   	medNR$compilation_time[m] = normRjit[1]
	   	medNR$large_CT[m] = NA 	
	   }

	   #storing all the normalised value of rjit runtime
	   for(n1 in 1:numberofruns){
	   	norm[n1,m+1] = normRjit[n1]
	   	names(norm)[m+1] = filen
	   }

	   i = i + 2
	   m = m + 1
	}

	#removing the vectors of zero, because of cbhind
	rawdata <- rawdata[, -1]

	#ordering medNR by the median for ggplot
	medNR$name <- factor(medNR$name, levels = medNR$name[order(medNR$median_time)])
	
	#ordering medNR by the median
	medNR <- medNR[with(medNR, order(median_time)),]

	#setting the x-axis for large_CT
	for(r in 1:numberoffiles){
		if(is.na(medNR$compilation_time[r])){
			medNR$graph_CT[r] = r
		}
	}

	# the location of the .csv files
	# probably easiest to upload it into the drop box

	# printing medNR and norm to .csv files
	write.csv(x=rawdata, file=paste(pathName, paste(testName, "rjitrawdata.csv", sep="-"), sep="/"))
	write.csv(x=medNR, file=paste(pathName, paste(testName, "rjitNormalisedMed.csv", sep="-"), sep="/"))
	write.csv(x=norm, file=paste(pathName, paste(testName, "rjitNormalisedComplete.csv", sep="-"), sep="/"))

	medNR
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
