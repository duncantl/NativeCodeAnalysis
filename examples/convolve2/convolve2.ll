; ModuleID = 'convolve2.c'
source_filename = "convolve2.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.SEXPREC = type opaque

; Function Attrs: nounwind sspstrong uwtable
define dso_local %struct.SEXPREC* @convolve2(%struct.SEXPREC* %a, %struct.SEXPREC* %b) local_unnamed_addr #0 {
entry:
  %call = tail call %struct.SEXPREC* @Rf_coerceVector(%struct.SEXPREC* %a, i32 14) #3
  %call1 = tail call %struct.SEXPREC* @Rf_protect(%struct.SEXPREC* %call) #3
  %call2 = tail call %struct.SEXPREC* @Rf_coerceVector(%struct.SEXPREC* %b, i32 14) #3
  %call3 = tail call %struct.SEXPREC* @Rf_protect(%struct.SEXPREC* %call2) #3
  %call4 = tail call i32 @Rf_length(%struct.SEXPREC* %call1) #3
  %call5 = tail call i32 @Rf_length(%struct.SEXPREC* %call3) #3
  %add = add i32 %call4, -1
  %sub = add i32 %add, %call5
  %conv = sext i32 %sub to i64
  %call6 = tail call %struct.SEXPREC* @Rf_allocVector(i32 14, i64 %conv) #3
  %call7 = tail call %struct.SEXPREC* @Rf_protect(%struct.SEXPREC* %call6) #3
  %call8 = tail call double* @REAL(%struct.SEXPREC* %call1) #3
  %call9 = tail call double* @REAL(%struct.SEXPREC* %call3) #3
  %call10 = tail call double* @REAL(%struct.SEXPREC* %call7) #3
  %cmp69 = icmp sgt i32 %sub, 0
  br i1 %cmp69, label %for.cond13.preheader.loopexit, label %for.cond13.preheader

for.cond13.preheader.loopexit:                    ; preds = %entry
  %call1080 = bitcast double* %call10 to i8*
  %0 = add i32 %call4, %call5
  %1 = add i32 %0, -2
  %2 = zext i32 %1 to i64
  %3 = shl nuw nsw i64 %2, 3
  %4 = add nuw nsw i64 %3, 8
  call void @llvm.memset.p0i8.i64(i8* align 8 %call1080, i8 0, i64 %4, i1 false)
  br label %for.cond13.preheader

for.cond13.preheader:                             ; preds = %for.cond13.preheader.loopexit, %entry
  %cmp1466 = icmp sgt i32 %call4, 0
  br i1 %cmp1466, label %for.cond18.preheader.lr.ph, label %for.cond.cleanup16

for.cond18.preheader.lr.ph:                       ; preds = %for.cond13.preheader
  %cmp1964 = icmp sgt i32 %call5, 0
  %wide.trip.count = zext i32 %call5 to i64
  %wide.trip.count74 = zext i32 %call4 to i64
  br label %for.cond18.preheader

for.cond18.preheader:                             ; preds = %for.cond.cleanup21, %for.cond18.preheader.lr.ph
  %indvars.iv72 = phi i64 [ 0, %for.cond18.preheader.lr.ph ], [ %indvars.iv.next73, %for.cond.cleanup21 ]
  br i1 %cmp1964, label %for.body22.lr.ph, label %for.cond.cleanup21

for.body22.lr.ph:                                 ; preds = %for.cond18.preheader
  %arrayidx24 = getelementptr inbounds double, double* %call8, i64 %indvars.iv72
  br label %for.body22

for.cond.cleanup16:                               ; preds = %for.cond.cleanup21, %for.cond13.preheader
  tail call void @Rf_unprotect(i32 3) #3
  ret %struct.SEXPREC* %call7

for.cond.cleanup21:                               ; preds = %for.body22, %for.cond18.preheader
  %indvars.iv.next73 = add nuw nsw i64 %indvars.iv72, 1
  %exitcond75 = icmp eq i64 %indvars.iv.next73, %wide.trip.count74
  br i1 %exitcond75, label %for.cond.cleanup16, label %for.cond18.preheader

for.body22:                                       ; preds = %for.body22, %for.body22.lr.ph
  %indvars.iv = phi i64 [ 0, %for.body22.lr.ph ], [ %indvars.iv.next, %for.body22 ]
  %5 = load double, double* %arrayidx24, align 8, !tbaa !4
  %arrayidx26 = getelementptr inbounds double, double* %call9, i64 %indvars.iv
  %6 = load double, double* %arrayidx26, align 8, !tbaa !4
  %mul = fmul double %5, %6
  %7 = add nuw nsw i64 %indvars.iv, %indvars.iv72
  %arrayidx29 = getelementptr inbounds double, double* %call10, i64 %7
  %8 = load double, double* %arrayidx29, align 8, !tbaa !4
  %add30 = fadd double %8, %mul
  store double %add30, double* %arrayidx29, align 8, !tbaa !4
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %exitcond = icmp eq i64 %indvars.iv.next, %wide.trip.count
  br i1 %exitcond, label %for.cond.cleanup21, label %for.body22
}

declare %struct.SEXPREC* @Rf_protect(%struct.SEXPREC*) local_unnamed_addr #1

declare %struct.SEXPREC* @Rf_coerceVector(%struct.SEXPREC*, i32) local_unnamed_addr #1

declare i32 @Rf_length(%struct.SEXPREC*) local_unnamed_addr #1

declare %struct.SEXPREC* @Rf_allocVector(i32, i64) local_unnamed_addr #1

declare double* @REAL(%struct.SEXPREC*) local_unnamed_addr #1

declare void @Rf_unprotect(i32) local_unnamed_addr #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1) #2

attributes #0 = { nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { argmemonly nounwind }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 8.0.1 (tags/RELEASE_801/final)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"double", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
