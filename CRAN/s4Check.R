setClass("int", contains = "integer")
a = new("int", 10L)

dyn.load("typeof.so")
.Call("R_typeof", a)
# 13 so INTSXP

setClass("r", list(unit = "character"), contains = "numeric")
obj = new("r", pi, unit = "?")
.Call("R_typeof", obj)

setClass("bob", list(a = "integer", b = "character"))
b = new("bob")
