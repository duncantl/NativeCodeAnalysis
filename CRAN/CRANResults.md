## Calls to Routines in R

```
 
                Rf_error                  Rprintf           Rf_allocVector                    R_pow                Rf_length          Rf_coerceVector                Rf_mkChar                 REprintf 
                   17005                    15079                    12878                     6263                     5619                     4815                     4679                     3531 
            Rf_getAttrib             Rf_asInteger               Rf_install                  DATAPTR             Rf_setAttrib                unif_rand     R_CheckUserInterrupt           Rf_allocMatrix 
                    3004                     2754                     2540                     2461                     2078                     1950                     1931                     1920 
             PutRNGstate              GetRNGstate               Rf_warning                 R_finite       R_registerRoutines      R_useDynamicSymbols                 Rf_nrows             Rf_errorcall 
                    1545                     1534                     1383                     1223                     1214                     1191                     1151                     1110 
            Rf_isInteger                 Rf_ncols                LENGTH_EX                   R_IsNA      R_RegisterCCallable                  Rf_eval              Rf_mkString              Rf_isMatrix 
                    1084                     1021                      923                      782                      757                      636                      632                      630 
            Rf_duplicate                Rf_asReal               Rf_xlength                      CAR                Rf_asChar             Rf_asLogical                R_do_slot         Rf_ScalarInteger 
                     627                      600                      589                      581                      553                      547                      512                      501 
                R_pow_di         Rf_ScalarLogical              Rf_inherits             Rf_type2char          Rf_ScalarString             Rf_isNumeric               R_isnancpp            Rf_isFunction 
                     495                      452                      389                      347                      319                      314                      269                      258 
                Rf_lang2         R_do_slot_assign            Rf_ScalarReal             Rf_defineVar           R_forceSymbols              Rf_isVector     Rf_translateCharUTF8                Rf_rPsort 
                     250                      242                      237                      193                      188                      187                      182                      160 
               R_qsort_I            R_qsort_int_I              Rf_mkCharCE               Rf_findVar                 Rf_lcons             Rf_isNewList        Rf_findVarInFrame        Rf_isVectorAtomic 
                     153                      145                      142                      138                      136                      125                      124                      119 
              Rf_mkNamed                Rf_iPsort           Rf_mkCharLenCE             Rf_namesgets            Rf_lengthgets                 Rf_lang3         rsort_with_index           R_GetCCallable 
                     114                      109                      109                      103                      102                       92                       91                       84 
            Rf_classgets              R_qsort_int        Rf_copyMostAttrib           Rf_GetRowNames                  R_rsort          Rf_alloc3DArray                  R_isort              Rf_isFactor 
                      78                       74                       70                       70                       63                       56                       54                       54 
              Rf_revsort         Rf_translateChar            Rf_allocArray              XTRUELENGTH             Rf_mkCharLen          Rf_isVectorList               XLENGTH_EX                 Rf_lang4 
                      54                       52                       50                       48                       47                       46                       46                       45 
               Rf_onintr          R_do_MAKE_CLASS          R_do_new_object                     push                  R_IsNaN            R_PromiseExpr         R_ContinueUnwind             Rf_getCharCE 
                      44                       43                       43                       40                       35                       35                       34                       33 
                Rf_lang1             R_unif_index         Rf_installTrChar                     scmp                R_tryEval                 Rf_list1               Rf_psmatch           Rf_warningcall 
                      33                       30                       30                       30                       28                       28                       28                       28 
         Rf_dimnamesgets       Rf_findVarInFrame3               Rf_isFrame            R_orderVector                REvprintf               savetl_end                   call_R                Rf_nthcdr 
                      27                       24                       23                       19                       19                       19                       18                       18 
     R_compute_identical               Rf_isArray                 Rf_list2               Rf_findFun              GEgetDevice           InStringBinary             R_CheckStack                 Rvprintf 
                      17                       17                       17                       15                       14                       14                       14                       14 
       strptime_internal                 dtwiddle                 mktime00          R_FindNamespace      Rf_VectorToPairList             R_FindSymbol           R_ToplevelExec              validate_tm 
                      14                       13                       13                       13                       13                       12                       12                       12 
    Rf_shallow_duplicate              Rf_str2type              Rf_type2str                   savetl          InIntegerBinary                  R_qsort          Rf_acopy_string                 Rf_lang5 
                      11                       10                       10                       10                        9                        9                        9                        9 
         GEcreateDevDesc         integerSubscript         logicalSubscript   R_CheckDeviceAvailable             R_GE_str2col     Rf_asCharacterFactor                  Rf_isS4         Rf_isValidString 
                       8                        8                        8                        8                        8                        8                        8                        8 
                Rf_match    Rf_NonNullStringMatch            Rf_PrintValue              Riconv_open          GEcurrentDevice                   R_atof            R_getClassDef                Rf_isList 
                       8                        8                        8                        8                        7                        7                        7                        7 
     Rf_PairToVectorList                  xerbla_             GEaddDevice2                   makelt            Rf_copyVector            Rf_GetOption1             Rf_isOrdered            Rf_isPairList 
                       7                        7                        6                        6                        6                        6                        6                        6 
           Rf_ndevNumber                   Riconv             Riconv_close        GEinitDisplayList               localtime0             mkStringUTF8   R_GE_checkVersionOrDie                 reset_tz 
                       6                        6                        6                        5                        5                        5                        5                        5 
       Rf_any_duplicated             Rf_GetOption            Rf_isLanguage           Rf_xlengthgets                   set_tz        GEfromDeviceWidth                  mktime0         R_InitOutPStream 
                       5                        5                        5                        5                        5                        4                        4                        4 
           R_ParseVector              R_Serialize            R_Unserialize          R_UnwindProtect        R_WriteConnection            Rf_duplicated           Rf_installChar                 Rf_lang6 
                       4                        4                        4                        4                        4                        4                        4                        4 
                Rf_list3                Rf_setVar Rf_warningcall_immediate                 cradix_r                  destroy        GEplayDisplayList       get_locale_strings          R_GetConnection 
                       4                        4                        4                        3                        3                        3                        3                        3 
           Rf_copyMatrix             Rf_curDevice               Rf_dimgets            Rf_NumDevices                 Rf_reEnc                Rf_RGBpar             Rf_ScalarRaw           Rf_StringFalse 
                       3                        3                        3                        3                        3                        3                        3                        3 
           Rf_StringTrue                    anyNA            asLogicalNoNA          day_of_the_week                  dinsert                   do_one                 dradix_r               FrameNames 
                       3                        2                        2                        2                        2                        2                        2                        2 
             GEaddDevice                   getvar                   inside            INTEGER_NO_NA            LOGICAL_NO_NA                   match5        R_ExecWithCleanup           R_forceAndCall 
                       2                        2                        2                        2                        2                        2                        2                        2 
              R_has_slot          R_InitInPStream            R_LockBinding            R_lsInternal3                  R_nchar                 R_strtod          R_tryEvalSilent               REAL_NO_NA 
                       2                        2                        2                        2                        2                        2                        2                        2 
             Rf_col2name           Rf_GetColNames               Rf_gsetVar                 Rf_list4                 Rf_list5           Rf_StringBlank       Rf_type2str_nowarn             Rf_ucstoutf8 
                       2                        2                        2                        2                        2                        2                        2                        2 
                 seed_in                 seed_out             STRING_NO_NA              clipPolygon          DATAPTR_OR_NULL                    equal         GEcreateSnapshot           GEdeviceNumber 
                       2                        2                        2                        1                        1                        1                        1                        1 
            GEkillDevice              GEStrMetric               GEStrWidth            installAttrib        INTEGER_IS_SORTED        R_BindingIsActive            R_curErrorBuf    R_EnvironmentIsLocked 
                       1                        1                        1                        1                        1                        1                        1                        1 
         R_GetCurrentEnv      R_InitFileInPStream         R_IsNamespaceEnv           R_IsPackageEnv             R_lsInternal  R_new_custom_connection         R_ReadConnection                 R_tmpnam 
                       1                        1                        1                        1                        1                        1                        1                        1 
         REAL_GET_REGION           REAL_IS_SORTED                    REAL0       Rf_any_duplicated3           Rf_desc2GEDesc                   Rf_elt         Rf_isBlankString        Rf_isVectorizable 
                       1                        1                        1                        1                        1                        1                        1                        1 
       Rf_KillAllDevices            Rf_killDevice                 Rf_list6            Rf_listAppend               Rf_nlevels             Rf_NoDevices         Rf_ScalarComplex         STRING_IS_SORTED 
                       1                        1                        1                        1                        1                        1                        1                        1 
             StringValue 
                       1 
```


## R Routines that return SEXP that are Called from Packages

```
 [1] "Rf_install"              "Rf_lang2"                "Rf_eval"                 "Rf_allocVector"          "Rf_mkChar"               "Rf_mkString"             "Rf_setAttrib"           
 [8] "Rf_lang1"                "CAR"                     "Rf_findFun"              "Rf_lang3"                "Rf_lang4"                "Rf_nthcdr"               "Rf_coerceVector"        
[15] "Rf_getAttrib"            "Rf_duplicate"            "Rf_type2str_nowarn"      "logicalSubscript"        "integerSubscript"        "Rf_match"                "Rf_ScalarLogical"       
[22] "R_do_MAKE_CLASS"         "R_do_new_object"         "Rf_allocMatrix"          "R_do_slot"               "Rf_ScalarInteger"        "Rf_mkCharCE"             "Rf_mkCharLenCE"         
[29] "Rf_ScalarString"         "Rf_mkCharLen"            "Rf_ScalarReal"           "Rf_asChar"               "R_do_slot_assign"        "Rf_findVarInFrame"       "Rf_namesgets"           
[36] "Rf_findVarInFrame3"      "R_tryEval"               "Rf_lang5"                "Rf_allocArray"           "Rf_GetRowNames"          "Rf_VectorToPairList"     "Rf_lcons"               
[43] "Rf_installTrChar"        "Rf_GetColNames"          "Rf_lengthgets"           "Rf_findVar"              "Rf_duplicated"           "Rf_asCharacterFactor"    "Rf_GetOption"           
[50] "do_one"                  "R_Unserialize"           "Rf_lang6"                "Rf_xlengthgets"          "Rf_mkNamed"              "Rf_PairToVectorList"     "Rf_classgets"           
[57] "R_UnwindProtect"         "Rf_list5"                "Rf_list2"                "Rf_list6"                "Rf_list4"                "Rf_list1"                "match5"                 
[64] "R_FindNamespace"         "R_PromiseExpr"           "Rf_installChar"          "Rf_shallow_duplicate"    "Rf_type2str"             "Rf_GetOption1"           "R_lsInternal3"          
[71] "R_ParseVector"           "Rf_alloc3DArray"         "Rf_dimnamesgets"         "R_tryEvalSilent"         "R_getClassDef"           "R_ExecWithCleanup"       "Rf_dimgets"             
[78] "R_GetCurrentEnv"         "R_forceAndCall"          "GEcreateSnapshot"        "Rf_elt"                  "installAttrib"           "mkStringUTF8"            "Rf_ScalarRaw"           
[85] "R_new_custom_connection" "Rf_list3"                "R_lsInternal"            "getvar"                  "Rf_ScalarComplex"        "Rf_listAppend"          
```


## Rf_setAttrib Symbol names
See RAPI.R
```
            R_NamesSymbol             R_ClassSymbol 
                      846                       269 
              R_DimSymbol          R_DimNamesSymbol 
                      244                       138 
         R_RowNamesSymbol            R_LevelsSymbol 
                       52                        29 
          xts_IndexSymbol                <Argument> 
                       23                        16 
                        i                     class 
                       16                        15 
                    names                         p 
                       10                        10 
                    tzone                  Dimnames 
                       10                         9 
                row.names                    Labels 
                        9                         8 
                     Size                       Dim 
                        8                         7 
                        x                  channels 
                        7                         6 
                file.name                sym_sorted 
                        5                         5 
                    index                     niter 
                        4                         4 
           representation                    status 
                        4                         4 
         sym_colClassesAs                   areaMax 
                        4                         3 
                  indroot                    istate 
                        3                         3 
              label.table                    levels 
                        3                         3 
                  maxSize                     plane 
                        3                         3 
                   precis                    steady 
                        3                         3 
               syms_tzone                     troot 
                        3                         3 
                  valroot                       win 
                        3                         3 
          xts_ClassSymbol                  accuracy 
                        3                         2 
                algorithm ALIKEC_SYM_syntacticnames 
                        2                         2 
                     bits                       col 
                        2                         2 
               columnType                  dimnames 
                        2                         2 
      endStateDifferences                       err 
                        2                         2 
                        F                 frequency 
                        2                         2 
                 gradient                handle_ptr 
                        2                         2 
                       hs   initialStateDifferences 
                        2                         2 
                    iroot               LFE_inplace 
                        2                         2 
                 missings                         n 
                        2                         2 
               namespaces                     nroot 
                        2                         2 
                   oclass          oclContextSymbol 
                        2                         2 
            oclModeSymbol                      rate 
                        2                         2 
                   rstate          Ryaml_KeysSymbol 
                        2                         2 
          Ryaml_TagSymbol               sample_rate 
                        2                         2 
            scaled:center              scaled:scale 
                        2                         2 
     sym_datatable_locked               sym_maxgrpn 
                        2                         2 
               sym_starts                syms_class 
                        2                         2 
                    table                       tol 
                        2                         2 
                     type                      uplo 
                        2                         2 
          variable.labels      xts_IndexTzoneSymbol 
                        2                         2 
                .compiled              .Environment 
                        1                         1 
                      asp                background 
                        1                         1 
                  badconv                      base 
                        1                         1 
                 bitdepth                      C_PI 
                        1                         1 
             capabilities                   changed 
                        1                         1 
              clipAsPolys             cluster_count 
                        1                         1 
                 codepage               color.space 
                        1                         1 
                     comm                   comment 
                        1                         1 
                    count                   crt.DER 
                        1                         1 
               data_types                 datalabel 
                        1                         1 
                     Diag                 distances 
                        1                         1 
                      dpi                      drop 
                        1                         1 
              effectNames               effectTypes 
                        1                         1 
                 encoding                 endRecord 
                        1                         1 
                  endtime          expansion.fields 
                        1                         1 
           FANSI_warn_sym                  fileInfo 
                        1                         1 
                 filePath                  filesize 
                        1                         1 
      finalReciprocalRate                     flows 
                        1                         1 
                  formats               genericVars 
                        1                         1 
               groupNames                    groups 
                        1                         1 
                   height                       ian 
                        1                         1 
                      ids                      info 
                        1                         1 
               interlaced                       jan 
                        1                         1 
                        k                     label 
                        1                         1 
                    match                   matched 
                        1                         1 
                    maxbb                    method 
                        1                         1 
                    minbb                        mu 
                        1                         1 
                na.action                    nbtype 
                        1                         1 
              networkName               networkType 
                        1                         1 
                     next                   nGroups 
                        1                         1 
                    nshps           oclDeviceSymbol 
                        1                         1 
           oclEventSymbol             oclNameSymbol 
                        1                         1 
           oclQueueSymbol                    offset 
                        1                         1 
                  options                  origFreq 
                        1                         1 
                  overlap                   package 
                        1                         1 
              padded.bits                   palette 
                        1                         1 
                   pixdim                  pixunits 
                        1                         1 
                  pointer                  PolyData 
                        1                         1 
                      pos                      prob 
                        1                         1 
                   qgrams              r_srcref_sym 
                        1                         1 
                    range                  readonly 
                        1                         1 
      Rkdnearest.dist_sym        Rkdnearest.kd_symb 
                        1                         1 
          Rkdtree.kd_symb          Rkdtree.kd_symbi 
                        1                         1 
         Rkradius.kd_symb                       rsc 
                        1                         1 
               sampleRate                   SAStype 
                        1                         1 
            SelfRefSymbol                  shp.type 
                        1                         1 
                   sigma2                      size 
                        1                         1 
              startRecord                 startTime 
                        1                         1 
                sym_index                    syntax 
                        1                         1 
                     text            thor_size_name 
                        1                         1 
               time.stamp              trackFormats 
                        1                         1 
                    types                     Upper 
                        1                         1 
               val.labels                     value 
                        1                         1 
               var.labels                   version 
                        1                         1 
                    width     xts_IndexTclassSymbol 
                        1                         1 
   xts_IndexTformatSymbol 
                        1 
```						


## Sets S3 Class Attribute

```
 [1] "arulesSequences" "stringi"         "datamap"        
 [4] "tdigest"         "data.table"      "restfulr"       
 [7] "scclust"         "libsoc"          "cba"            
[10] "rjson"           "slam"            "jsonlite"       
[13] "fastmatch"       "proxy"           "git2r"          
[16] "thor"            "loder"           "rphast"         
[19] "xts"             "RCurl"           "rlang"          
[22] "iotools"         "corpus"          "meanr"          
[25] "listdtr"         "tibble"          "coRanking"      
[28] "rbamtools"       "lazyeval"        "RCassandra"     
[31] "nseval"          "Rgb"             "Cairo"          
[34] "phreeqc"         "png"             "mongolite"      
[37] "XML"             "RJSONIO"         "tikzDevice"     
[40] "OpenCL"          "curl"            "audio"          
[43] "zoo"             "profdpm"         "spdep"          
[46] "TPmsm"           "warp"            "yaml"           
[49] "Rbeast"          "praznik"         "RSclient"       
[52] "RNetCDF"         "processx"        "pingr"          
[55] "RPostgreSQL"     "jpeg"            "dbarts"         
[58] "ore"             "rJava"           "openssl"        
[61] "parsedate"       "topicmodels"     "sylcount"       
[64] "foreign"         "rtfbs"           "DSL"            
[67] "tiff"            "refGenome"       "PKI"            
```




## S4 classes in C code
```
 [1] "arulesSequences" "catnet"          "sp"             
 [4] "Runuran"         "pomp"            "tuneR"          
 [7] "RCurl"           "arules"          "memisc"         
[10] "rbamtools"       "SiMRiv"          "XML"            
[13] "zoo"             "sdnet"           "float"          
[16] "ouch"            "pedigreemm"      "purrr"          
[19] "rJPSGCS"         "JavaGD"          "pedigreeTools"  
[22] "nimble"          "dbarts"          "rJava"          
[25] "topicmodels"     "SearchTrees"     "rmatio"         
[28] "splusTimeDate"   "Rcpp"            "SimInf"         
```
