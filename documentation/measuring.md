# Measuring # of Opcodes

**You must compile with `ENABLE_MEASURE` to use these features**

RIR will count how many times each opcode is executed for each closure. Each closure has its own row, each opcode has its own column. Additionally, there's a "sum" row and column. Here is an example:

   <fileName>, invalid_, ... push_, ... Total
   print              0,        42,       345
   write              0,        37,     12123
   Total              0,        79,     12468

Call `rir.getMeasure()` to get a data frame containing this info. Call `rir.resetMeasure()` to reset measurements. Set the env variable `RIR_MEASURE=<fileName>` to send measurements to `<fileName>.csv` in CSV form.
