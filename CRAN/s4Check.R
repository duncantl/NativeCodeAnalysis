setClass("int", contains = "integer")
a = new("int", 10L)

dyn.load("typeof.so")
.Call("R_typeof", a)
# 13 so INTSXP

setClass("r", list(unit = "character"), contains = "numeric")
obj = new("r", pi, unit = "?")
.Call("R_typeof", obj)
# REALSXP

setClass("bob", list(a = "integer", b = "character"))
b = new("bob")
.Call("R_typeof", b)
# 25 - S4SXP

setClass("jane", contains = c("bob", "numeric"))
j  = new("jane")
.Call("R_typeof", j)
# 14  - REALSXP

