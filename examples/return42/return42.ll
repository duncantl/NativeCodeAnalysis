; ModuleID = 'return42.c'
source_filename = "return42.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.SEXPREC = type opaque

; Function Attrs: nounwind sspstrong uwtable
define dso_local %struct.SEXPREC* @return42() local_unnamed_addr #0 {
entry:
  %call = tail call %struct.SEXPREC* @Rf_allocVector(i32 13, i64 1) #2
  %call1 = tail call %struct.SEXPREC* @Rf_protect(%struct.SEXPREC* %call) #2
  %call2 = tail call i32* @INTEGER(%struct.SEXPREC* %call1) #2
  store i32 42, i32* %call2, align 4, !tbaa !4
  tail call void @Rf_unprotect(i32 1) #2
  ret %struct.SEXPREC* %call1
}

declare %struct.SEXPREC* @Rf_protect(%struct.SEXPREC*) local_unnamed_addr #1

declare %struct.SEXPREC* @Rf_allocVector(i32, i64) local_unnamed_addr #1

declare i32* @INTEGER(%struct.SEXPREC*) local_unnamed_addr #1

declare void @Rf_unprotect(i32) local_unnamed_addr #1

attributes #0 = { nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 8.0.1 (tags/RELEASE_801/final)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
