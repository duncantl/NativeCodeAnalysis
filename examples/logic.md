
#

library(Rllvm)
m = parseIR("logic.ll")



rt = getReturnValues(m$do_logic)
rt
$cleanup39
[1] "  %retval.1 = phi %struct.SEXPREC* [ %retval.0.i, %if.then28 ], [ %x.1.i, %lbinary.exit ],[ %8, %if.then ], [ %x.0.i76, %sw.epilog.i ], [ %call.i.i68, %if.then10.i ]"


There is only return value and that is a PHI node with 5 incoming block-values.
So
rt = rt[[1]]

To see the return Value expressions.

```
rt[]
```
```
[[1]]
[1] "  %retval.0.i = load %struct.SEXPREC*, %struct.SEXPREC** %retval.0.in.i, align 8, !tbaa !12"

[[2]]
[1] "  %x.1.i = phi %struct.SEXPREC* [ %call.i365.i, %land.end.i ], [ %call.i.i.i, %if.then.i.i ], [ %call.i119.i.i, %if.end.i.i ], [ %call.i119.i.i, %for.cond.preheader.i.i ], [ %call.i119.i.i, %for.cond34.preheader.i.i ], [ %call.i.i355.i, %if.then.i356.i ], [ %call.i161.i.i, %if.end.i358.i ], [ %call.i161.i.i, %for.cond.preheader.i359.i ], [ %call.i161.i.i, %for.cond43.preheader.i.i ], [ %call.i161.i.i, %for.inc.i.i ], [ %call.i161.i.i, %for.inc65.i.i ], [ %call.i119.i.i, %for.body.i.i ], [ %call.i119.i.i, %for.body37.i.i ]"

[[3]]
[1] "  %8 = load %struct.SEXPREC*, %struct.SEXPREC** %ans, align 8"

[[4]]
[1] "  %x.0.i76 = phi %struct.SEXPREC* [ %call26.i, %if.then24.i ], [ %call33.i69, %if.end51.i ]"

[[5]]
[1] "  %call.i.i68 = call %struct.SEXPREC* @Rf_allocVector3(i32 10, i64 0, %struct.R_allocator* null) #5"
```


```
sapply(rt[], class)
[1] "LoadInst" "PHINode"  "LoadInst" "PHINode"  "CallInst"
```

We have two PHI nodes adding to the complexity.
However, the one CallInst is a call to Rf_allocVector() with SEXPTYPE 10 which corresponds to a
logical vector. And the length is 0. So this is a constant logical(0). This is probably the
degenerate case!

rt[[1]]
[1] "  %retval.0.i = load %struct.SEXPREC*, %struct.SEXPREC** %retval.0.in.i, align 8, !tbaa !12"

rt[[3]]
[1] "  %8 = load %struct.SEXPREC*, %struct.SEXPREC** %ans, align 8"






rt[c(1, 3)]
[[1]]
[1] "  %retval.0.i = load %struct.SEXPREC*, %struct.SEXPREC** %retval.0.in.i, align 8, !tbaa !12"

[[2]]
[1] "  %8 = load %struct.SEXPREC*, %struct.SEXPREC** %ans, align 8"

*** FIX rt[c(1, 3)]  
   Warning
   In if (i < 0) (x[])[i] else lapply(i, function(i) getOperand(x,  :
  the condition has length > 1 and only the first element will be used


