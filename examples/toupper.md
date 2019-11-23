# tolower()

We don't show how we know tolower() maps
to the C routine do_tolower,
now how toupper also maps to that same C routine.
That is a separate task based on understanding of the architectire of R.

```
tolower
function (x) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(tolower(x))
}
```


```
library(Rllvm); library(NativeCodeAnalysis)
```

```
m = parseIR("~/R-devel/build/src/main/character.ll")
m$do_tolower
```
```
struct SEXPREC * do_tolower ( struct SEXPREC * call, struct SEXPREC * op, struct SEXPREC * args, struct SEXPREC * env )
```

So now we get the return value(s) for this routine, i.e. all
the possible values that come through the single return terminator:
```
rv = getReturnValues(m$do_tolower)
```
```
$if.end138
[1] "  %call.i = tail call %struct.SEXPREC* @Rf_allocVector3(i32 16, i64 %call3, %struct.R_allocator* null) #11"
```

There is only one in this case, so we process it.
It is a call to Rf_allocVector3, is often the case.
To get the type of this R object, we get the value of the first argument
```
rv[[1]][[1]]
```
```
[1] "i32 16"
````
This is a ConstantInt object. We can get the actual value with
```
getValue(rv[[1]][[1]])
```
```
[1] 16
```

Now we want to match this to an R type
```
rtype = getRType(getValue(rv[[1]][[1]]))
```
```
     STRSXP 
"character" 
```

So we have a character() vector.

# The dimension

The length of the character vector being returned
is available in the second argument in the call to Rf_allocVector3:
```
len = rv[[1]][[2]]
```
This looks like
```
[1] "  %call3 = tail call i64 @XLENGTH_EX(%struct.SEXPREC* %2)"
```
So it is a call to XLENGTH_EX. This used to be Rf_length, but this
call handls alt-rep objects, large vectors, etc.
But it is the length of an R object.
So our next task is to find which R object.
This is %2 from the string representation.
But we get this from the argument Value in the call:
```
len[[1]]
```
```
[1] "  %2 = load %struct.SEXPREC*, %struct.SEXPREC** %carval, align 8, !tbaa !4"
```
This is a Load instruction, so we look at its first argument to see where it came from:
```
len[[1]][[1]]
```
```
[1] "  %carval = getelementptr inbounds %struct.SEXPREC, %struct.SEXPREC* %args, i64 0, i32 4, i32 0, i32 0"
```

So the is one of the famed GEP instructions.
We get the object being indexed via 
```
len[[1]][[1]][[1]]
```
```
[1] "%struct.SEXPREC* %args"
```
This is the 3 argument in the routine, the one named args as is always the case for a .Internal
routine in R. We can get its name directly with 
```
getName(len[[1]][[1]][[1]]) 
```
```
[1] "args"
```
So we know we are working with the args parameter in a .Internal.


# The indices
The next step is to determine which element of args the call to XLENGTH_EX is operating on.
We can determine this from the indices, i.e. dropping the first argument which is args:
```
len[[1]][[1]][-1] 
```
```
[[1]]
[1] "i64 0"

[[2]]
[1] "i32 4"

[[3]]
[1] "i32 0"

[[4]]
[1] "i32 0"
```
Again these are ConstantInt values. 
So we get the integer values with
```
sapply(len[[1]][[1]][-1] , getValue)
```
```
[1] 0 4 0 0
```
To interpret this as the first argument, 
we need to "manually" know from our understanding of R that this is the CAR() of the args vector.

## Solution

At the end of this we know the return value is a
`character(length(args[[1]]])`
and from the R code we know a`rgs[[1]]` is x in the function
`toupper(x)`.

`character(length(x))`


Also from the R code, we know x must be a character vector, or coercible to one 
via `as.character()` and not `as(, "character")`.


