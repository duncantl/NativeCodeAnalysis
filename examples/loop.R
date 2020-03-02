l = parseIR("loop.ir")
f = l$R_loop2
p = getParameters(f)
inferLength(p$ry)


#u1 = lapply(p, getAllUsers)
#u2 = unlist(lapply(u1, getAllUsers))
#u3 = unlist(lapply(u2, getAllUsers))
