
generateData <- function(nCols, nRows) {
    t <- list()

    multFactor <- 1000

    for (i in 1:nCols) {

        if (i %% 2 == 1)   # first column will be Int
            dataCol = as.integer(runif(nRows) *  multFactor)
        else
            dataCol = runif(nRows) * multFactor
        # complex(real=runif(nRows), imaginary=runif(nRows))

        t[[i]] = dataCol

    }

    myData <<- t

}


h <- function(colIndex, t) {

    dataCol <- t[[colIndex]]

    res <- 0

    for (i in 1:length(dataCol)) {
        res <- res + dataCol[[i]]
    }
    res


}
rir.compile(h)
rir.markFunction(h, DisableInline=TRUE)


f <-  function(colIndex, t) {
    cat("\n")
    print(paste("Running for col ", colIndex, ": ",  typeof(t[[colIndex]]) ))
    time = system.time({

        res <- h(colIndex, t)
    })
    print(paste("Sum for col: ", colIndex, ": ", res))
    print(time)
}



cols = 50L
rows  = 10000000L

generateData(cols, rows)

totalTime = system.time(
{
    #warm up
    print("WARMUP")
    f(2, myData)
    f(2, myData)
    f(2, myData)
    f(2, myData)
    f(2, myData)
    f(2, myData)
    f(2, myData)


    # run
    print("RUN")
    f(	1	,myData)
    f(	2	,myData)
    f(	3	,myData)
    f(	4	,myData)
    f(	5	,myData)
    f(	6	,myData)
    f(	7	,myData)
    f(	8	,myData)
    f(	9	,myData)
    f(	10	,myData)
    f(	11	,myData)
    f(	12	,myData)
    f(	13	,myData)
    f(	14	,myData)
    f(	15	,myData)
    f(	16	,myData)
    f(	17	,myData)
    f(	18	,myData)
    f(	19	,myData)
    f(	20	,myData)
    f(	21	,myData)
    f(	22	,myData)
    f(	23	,myData)
    f(	24	,myData)
    f(	25	,myData)
    f(	26	,myData)
    f(	27	,myData)
    f(	28	,myData)
    f(	29	,myData)
    f(	30	,myData)
    f(	31	,myData)
    f(	32	,myData)
    f(	33	,myData)
    f(	34	,myData)
    f(	35	,myData)
    f(	36	,myData)
    f(	37	,myData)
    f(	38	,myData)
    f(	39	,myData)
    f(	40	,myData)
    f(	41	,myData)
    f(	42	,myData)
    f(	43	,myData)
    f(	44	,myData)
    f(	45	,myData)
    f(	46	,myData)
    f(	47	,myData)
    f(	48	,myData)
    f(	49	,myData)
    f(	50	,myData)

    # f(	51	,myData)
    # f(	52	,myData)
    # f(	53	,myData)
    # f(	54	,myData)
    # f(	55	,myData)
    # f(	56	,myData)
    # f(	57	,myData)
    # f(	58	,myData)
    # f(	59	,myData)
    # f(	60	,myData)
    # f(	61	,myData)
    # f(	62	,myData)
    # f(	63	,myData)
    # f(	64	,myData)
    # f(	65	,myData)
    # f(	66	,myData)
    # f(	67	,myData)
    # f(	68	,myData)
    # f(	69	,myData)
    # f(	70	,myData)
    # f(	71	,myData)
    # f(	72	,myData)
    # f(	73	,myData)
    # f(	74	,myData)
    # f(	75	,myData)
    # f(	76	,myData)
    # f(	77	,myData)
    # f(	78	,myData)
    # f(	79	,myData)
    # f(	80	,myData)
    # f(	81	,myData)
    # f(	82	,myData)
    # f(	83	,myData)
    # f(	84	,myData)
    # f(	85	,myData)
    # f(	86	,myData)
    # f(	87	,myData)
    # f(	88	,myData)
    # f(	89	,myData)
    # f(	90	,myData)
    # f(	91	,myData)
    # f(	92	,myData)
    # f(	93	,myData)
    # f(	94	,myData)
    # f(	95	,myData)
    # f(	96	,myData)
    # f(	97	,myData)
    # f(	98	,myData)
    # f(	99	,myData)
    # f(	100	,myData)



    # print("BEGIN LOOP")
    # for (i in 1L:cols) {

    #     if (i%%2 ==1)
    #         f(i, myData)
    #     #if (i%%2 ==1)
    #          #f(1, myData)
    #     # else
    #     #     f(2, myData)
    # }


}
)
print("TOTAL TIME:")
print(totalTime)
