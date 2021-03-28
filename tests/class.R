library(Rllvm)
library(NativeCodeAnalysis)

m = parseIR("class.ir")

funNames = getDefinedRoutines(m)

mk2 = compReturnType(m$mk2)
# doesn't get the class names.

bob = compReturnType(m$doS4)
# gets class name.


#√  but want to connect the dimension of the second element to the argument in the first routine that called mk2
# i.e. mk2() creates y with length = length(x)
#  listEls() calls mk(a) so length of the result is length(a) not length(x). i.e. need to map
#  parameter a to parameter x
# Can pass the argument names as a separate character vector to getCallType()
# and then we have 
a = compReturnType(m$listEls)
