#!/bin/bash
set -e
set -o pipefail


# SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# echo $SCRIPT_DIR


rshFolder="$(dirname "$(pwd)")"  #########

rshMasterBuildFolder="$rshFolder/build"  #########
rshMasterBinFolder="$rshFolder/build/release"  #########
#rshMasterBinFolder="$rshFolder/build/debug"  #########
rshMasterRScript="${rshMasterBinFolder}/bin/Rscript"
rshMasterRBin="${rshMasterBinFolder}/bin/R"


#rshDebugFlags="PIR_DEBUG=PrintEarlyPir,PrintPirAfterOpt,PrintToStdout,ShowWarnings PIR_DEBUG_DEOPTS=1 PIR_OSR=1"   #########
#rshDebugFlags="PIR_DEBUG=PrintPirAfterOpt,PrintToStdout PIR_OSR=0"   #########


experimentFlagsDefault="PIR_DEFAULT_SPECULATION=0"
experimentFlagsEmptyFeedback_CD_Off="PIR_GLOBAL_SPECIALIZATION_LEVEL=0 PIR_TRANSFER_FEEDBACK=0 PIR_DEFAULT_SPECULATION=0 PIR_INLINE=0"
#experimentFlagsEmptyFeedbackNoCDSpec="PIR_GLOBAL_SPECIALIZATION_LEVEL=0 PIR_TRANSFER_FEEDBACK=0 PIR_DEFAULT_SPECULATION=1 PIR_INLINE=0"

experimentFlagsRirOnly="PIR_ENABLE=off"
experimentFlagsRirOnlyNoRecording="RIR_PROFILING=off PIR_ENABLE=off"

experimentFlags=$experimentFlagsDefault   #####

rshDebugFlags="$experimentFlags STATS_VERBOSE=0 PIR_WARMUP=10 PIR_OSR=0 PIR_DEBUG=PrintEarlyPir,PrintPirAfterOpt,ShowWarnings,PrintToStdout"   #########



### BENCH SETTINGS ####################################################


#benchmarkFolder="$HOME/benchmarks/RBenchmarking/Benchmarks/shootout"
#benchmarkArgs="-f harness.r --args spectralnorm/spectralnorm_naive 15 150"
#benchmarkArgs="-f harness.r --args pidigits/pidigits 15 30"
#benchmarkArgs="-f harness.r --args nbody/nbody 15 25000"






#benchmarkFolder="$rshMasterBinFolder/../"
#benchmarkArgs="-f trivialAssignment.R"
# benchmarkArgs="-f dependentsFail.R"


benchmarkFolder="$rshMasterBinFolder/../../rir/tests/"
benchmarkArgs="-f pir_simple_range.R"
#benchmarkArgs="-f matrix_regression.r"
#benchmarkArgs="-f pir_regression_splines.R"
#benchmarkArgs="-f regression_reg-packages.R"


####################################################################


runMaster() {
    runBench "$rshMasterRBin"
}



runMasterWithRecorder() {
    runBenchWithRecorder "$rshMasterRBin" "master_recording"
}


runBench() {
    local RExecPath=$1


    cd $benchmarkFolder
    runR "${RExecPath}" "${benchmarkArgs}" ""

}


runBenchWithRecorder() {

    local RExecPath=$1
    local recordingFileName_noExtension=$2


    here=`pwd`


    local rdsFile="${here}/${recordingFileName_noExtension}.rds"
    local csvFile="${here}/${recordingFileName_noExtension}.csv"
    rm -f ${csvFile}
    rm -f ${rdsFile}

    cd $benchmarkFolder

    #Compile,Deopt,TypeFeedback,Invoke
    local recordingFlags="RIR_RECORD=${rdsFile} RIR_RECORD_FILTER=Compile,Deopt,TypeFeedback,Invoke"

    runR "${RExecPath}" "${benchmarkArgs}" "${recordingFlags}"
    cd $here

    echo "Converting Rds to CSV"
    PIR_ENABLE=off ${RExecPath} -f recordRdsToCsv.R --args "${rdsFile}" "${csvFile}"
    echo "Converting Rds to CSV.. DONE"

}



runR() {

    local RExecPath=$1
    local args=$2
    local extraFlags=$3


    pushd $rshMasterBinFolder
    ninja
    popd

    local allEnv="${extraFlags} ${rshDebugFlags}"

    echo ${allEnv} ${RExecPath} ${args}
    mkdir -p ${rshMasterBuildFolder}/output/
    env ${allEnv} ${RExecPath} ${args} &> ${rshMasterBuildFolder}/output/feednotempty.txt


}
