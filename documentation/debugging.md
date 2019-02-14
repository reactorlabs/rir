## Debugging PIR
PIR comes with a wide variety of options to analyze the output of the compiler in very different
stages of the compilation. Currently, to manage the different options we use environment 
variables. For instance: `PIR_ENABLE=force PIR_DEBUG=PrintPirAfterOpt bin/R -f yourScript.r`
will run your script, compile each function actually called with PIR and output the result
of the PIR IR just after PIR performed all its optimization passes.

### Environment Variables

* PIR_ENABLE: Default value is `on`.
    * `on`: 
    * `off`: 
    * `force`: Usually PIR is triggered after a function becomes hot, i.e., it was called more than 
               n times. `force` makes PIR to trigger in each function's first activation.

* PIR_DEBUG: By default PIR does not print any kind of debugging output. 
    * PrintEarlyRir:
    * PrintEarlyPir:
    * PrintOptimizationPasses:
    * PrintPirAfterOpt:
    * PrintCSSA:
    * PrintAllocator:
    * PrintFinalPir:
    * PrintFinalRir: 

FILTER?

### Printing Annotation Semantics

#### Assumptions
* EA: Function/Closure has eager arguments
* EAn: Argument n IsEager
* NO: Function/Closure has non-object arguments
* NOn: Argument n is not an object
* NTA: Function/Closure does not have more than 3 args
* NMA: All arguments are supplied
* COA: Arguments are supplied in the correct order

#### Types Annotations
*$: Is scalar
*^: May be lazy
*~: May be wrapped in a promise
*?: May be missing
*': May be an object
    
