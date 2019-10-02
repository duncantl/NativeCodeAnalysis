; ModuleID = 'convolve.c'
source_filename = "convolve.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local void @convolve(double* %a, i32* %na, double* %b, i32* %nb, double* %ab) #0 {
entry:
  %a.addr = alloca double*, align 8
  %na.addr = alloca i32*, align 8
  %b.addr = alloca double*, align 8
  %nb.addr = alloca i32*, align 8
  %ab.addr = alloca double*, align 8
  %nab = alloca i32, align 4
  %i = alloca i32, align 4
  %i1 = alloca i32, align 4
  %j = alloca i32, align 4
  store double* %a, double** %a.addr, align 8
  store i32* %na, i32** %na.addr, align 8
  store double* %b, double** %b.addr, align 8
  store i32* %nb, i32** %nb.addr, align 8
  store double* %ab, double** %ab.addr, align 8
  %0 = load i32*, i32** %na.addr, align 8
  %1 = load i32, i32* %0, align 4
  %2 = load i32*, i32** %nb.addr, align 8
  %3 = load i32, i32* %2, align 4
  %add = add nsw i32 %1, %3
  %sub = sub nsw i32 %add, 1
  store i32 %sub, i32* %nab, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %4 = load i32, i32* %i, align 4
  %5 = load i32, i32* %nab, align 4
  %cmp = icmp slt i32 %4, %5
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %6 = load double*, double** %ab.addr, align 8
  %7 = load i32, i32* %i, align 4
  %idxprom = sext i32 %7 to i64
  %arrayidx = getelementptr inbounds double, double* %6, i64 %idxprom
  store double 0.000000e+00, double* %arrayidx, align 8
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %8 = load i32, i32* %i, align 4
  %inc = add nsw i32 %8, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  store i32 0, i32* %i1, align 4
  br label %for.cond2

for.cond2:                                        ; preds = %for.inc19, %for.end
  %9 = load i32, i32* %i1, align 4
  %10 = load i32*, i32** %na.addr, align 8
  %11 = load i32, i32* %10, align 4
  %cmp3 = icmp slt i32 %9, %11
  br i1 %cmp3, label %for.body4, label %for.end21

for.body4:                                        ; preds = %for.cond2
  store i32 0, i32* %j, align 4
  br label %for.cond5

for.cond5:                                        ; preds = %for.inc16, %for.body4
  %12 = load i32, i32* %j, align 4
  %13 = load i32*, i32** %nb.addr, align 8
  %14 = load i32, i32* %13, align 4
  %cmp6 = icmp slt i32 %12, %14
  br i1 %cmp6, label %for.body7, label %for.end18

for.body7:                                        ; preds = %for.cond5
  %15 = load double*, double** %a.addr, align 8
  %16 = load i32, i32* %i1, align 4
  %idxprom8 = sext i32 %16 to i64
  %arrayidx9 = getelementptr inbounds double, double* %15, i64 %idxprom8
  %17 = load double, double* %arrayidx9, align 8
  %18 = load double*, double** %b.addr, align 8
  %19 = load i32, i32* %j, align 4
  %idxprom10 = sext i32 %19 to i64
  %arrayidx11 = getelementptr inbounds double, double* %18, i64 %idxprom10
  %20 = load double, double* %arrayidx11, align 8
  %mul = fmul double %17, %20
  %21 = load double*, double** %ab.addr, align 8
  %22 = load i32, i32* %i1, align 4
  %23 = load i32, i32* %j, align 4
  %add12 = add nsw i32 %22, %23
  %idxprom13 = sext i32 %add12 to i64
  %arrayidx14 = getelementptr inbounds double, double* %21, i64 %idxprom13
  %24 = load double, double* %arrayidx14, align 8
  %add15 = fadd double %24, %mul
  store double %add15, double* %arrayidx14, align 8
  br label %for.inc16

for.inc16:                                        ; preds = %for.body7
  %25 = load i32, i32* %j, align 4
  %inc17 = add nsw i32 %25, 1
  store i32 %inc17, i32* %j, align 4
  br label %for.cond5

for.end18:                                        ; preds = %for.cond5
  br label %for.inc19

for.inc19:                                        ; preds = %for.end18
  %26 = load i32, i32* %i1, align 4
  %inc20 = add nsw i32 %26, 1
  store i32 %inc20, i32* %i1, align 4
  br label %for.cond2

for.end21:                                        ; preds = %for.cond2
  ret void
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 8.0.1 (tags/RELEASE_801/final)"}
