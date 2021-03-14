## Calls to Routines in R

```
                Rf_error                  Rprintf           Rf_allocVector                    R_pow                Rf_length          Rf_coerceVector 
                   16176                    13236                    12059                     6263                     5351                     4591 
               Rf_mkChar                 REprintf             Rf_asInteger             Rf_getAttrib               Rf_install                  DATAPTR 
                    4254                     2925                     2720                     2625                     2295                     2211 
            Rf_setAttrib     R_CheckUserInterrupt                unif_rand           Rf_allocMatrix              GetRNGstate              PutRNGstate 
                    1965                     1857                     1847                     1723                     1392                     1361 
              Rf_warning       R_registerRoutines      R_useDynamicSymbols                 R_finite                 Rf_nrows                 Rf_ncols 
                    1292                     1214                     1191                     1157                     1150                     1020 
            Rf_isInteger             Rf_errorcall      R_RegisterCCallable                   R_IsNA                LENGTH_EX              Rf_isMatrix 
                     899                      798                      757                      752                      750                      619 
             Rf_mkString                  Rf_eval               Rf_xlength                Rf_asReal                Rf_asChar             Rf_asLogical 
                     608                      596                      559                      537                      536                      535 
                     CAR                 R_pow_di             Rf_duplicate                R_do_slot         Rf_ScalarInteger         Rf_ScalarLogical 
                     506                      495                      495                      482                      478                      413 
             Rf_inherits             Rf_type2char             Rf_isNumeric          Rf_ScalarString            Rf_isFunction               R_isnancpp 
                     372                      329                      311                      306                      254                      241 
        R_do_slot_assign                 Rf_lang2            Rf_ScalarReal             Rf_defineVar           R_forceSymbols              Rf_isVector 
                     236                      236                      223                      191                      188                      161 
               Rf_rPsort                R_qsort_I     Rf_translateCharUTF8            R_qsort_int_I                 Rf_lcons              Rf_mkCharCE 
                     160                      153                      151                      145                      135                      133 
       Rf_isVectorAtomic             Rf_isNewList               Rf_findVar               Rf_mkNamed                Rf_iPsort        Rf_findVarInFrame 
                     119                      117                      114                      110                      109                      103 
        rsort_with_index                 Rf_lang3            Rf_lengthgets              R_qsort_int             Rf_classgets        Rf_copyMostAttrib 
                      91                       88                       75                       74                       70                       70 
          Rf_GetRowNames                  R_rsort          Rf_alloc3DArray                  R_isort           Rf_mkCharLenCE              Rf_isFactor 
                      70                       63                       55                       54                       53                       52 
            Rf_namesgets               Rf_revsort         Rf_translateChar           R_GetCCallable            Rf_allocArray              XTRUELENGTH 
                      50                       50                       50                       49                       48                       48 
              XLENGTH_EX          Rf_isVectorList                Rf_onintr          R_do_MAKE_CLASS          R_do_new_object             Rf_mkCharLen 
                      46                       44                       44                       42                       42                       41 
                Rf_lang4                  R_IsNaN            R_PromiseExpr         R_ContinueUnwind             Rf_getCharCE                 Rf_lang1 
                      36                       35                       35                       34                       33                       31 
            R_unif_index         Rf_installTrChar                R_tryEval                 Rf_list1               Rf_psmatch           Rf_warningcall 
                      30                       30                       28                       28                       28                       28 
      Rf_findVarInFrame3          Rf_dimnamesgets               Rf_isFrame                REvprintf                   call_R            R_orderVector 
                      24                       23                       23                       19                       18                       18 
     R_compute_identical               Rf_isArray                 Rf_list2              GEgetDevice             R_CheckStack                 Rvprintf 
                      17                       17                       15                       14                       14                       14 
         R_FindNamespace               Rf_findFun      Rf_VectorToPairList             R_FindSymbol           R_ToplevelExec     Rf_shallow_duplicate 
                      13                       13                       13                       12                       12                       11 
             Rf_str2type                  R_qsort          Rf_acopy_string                 Rf_lang5              Rf_type2str          GEcreateDevDesc 
                      10                        9                        9                        9                        9                        8 
  R_CheckDeviceAvailable             R_GE_str2col     Rf_asCharacterFactor         Rf_isValidString                 Rf_match    Rf_NonNullStringMatch 
                       8                        8                        8                        8                        8                        8 
             Riconv_open          GEcurrentDevice                   R_atof            R_getClassDef      Rf_PairToVectorList            Rf_PrintValue 
                       8                        7                        7                        7                        7                        7 
            GEaddDevice2            Rf_GetOption1                Rf_isList             Rf_isOrdered            Rf_isPairList            Rf_ndevNumber 
                       6                        6                        6                        6                        6                        6 
                  Riconv        GEinitDisplayList   R_GE_checkVersionOrDie        Rf_any_duplicated             Rf_GetOption            Rf_isLanguage 
                       6                        5                        5                        5                        5                        5 
          Rf_xlengthgets             Riconv_close        GEfromDeviceWidth         R_InitOutPStream              R_Serialize            R_Unserialize 
                       5                        5                        4                        4                        4                        4 
         R_UnwindProtect        R_WriteConnection            Rf_copyVector            Rf_duplicated           Rf_installChar                  Rf_isS4 
                       4                        4                        4                        4                        4                        4 
                Rf_lang6                 Rf_list3                Rf_nthcdr                Rf_setVar Rf_warningcall_immediate        GEplayDisplayList 
                       4                        4                        4                        4                        4                        3 
         R_GetConnection            R_ParseVector            Rf_copyMatrix             Rf_curDevice               Rf_dimgets            Rf_NumDevices 
                       3                        3                        3                        3                        3                        3 
                Rf_reEnc                Rf_RGBpar             Rf_ScalarRaw           Rf_StringFalse            Rf_StringTrue              GEaddDevice 
                       3                        3                        3                        3                        3                        2 
           INTEGER_NO_NA            LOGICAL_NO_NA                   match5        R_ExecWithCleanup           R_forceAndCall               R_has_slot 
                       2                        2                        2                        2                        2                        2 
         R_InitInPStream            R_LockBinding            R_lsInternal3                  R_nchar                 R_strtod          R_tryEvalSilent 
                       2                        2                        2                        2                        2                        2 
              REAL_NO_NA              Rf_col2name           Rf_GetColNames               Rf_gsetVar                 Rf_list4                 Rf_list5 
                       2                        2                        2                        2                        2                        2 
          Rf_StringBlank       Rf_type2str_nowarn             Rf_ucstoutf8                  seed_in                 seed_out             STRING_NO_NA 
                       2                        2                        2                        2                        2                        2 
         DATAPTR_OR_NULL         GEcreateSnapshot           GEdeviceNumber             GEkillDevice              GEStrMetric               GEStrWidth 
                       1                        1                        1                        1                        1                        1 
       INTEGER_IS_SORTED        R_BindingIsActive            R_curErrorBuf    R_EnvironmentIsLocked          R_GetCurrentEnv      R_InitFileInPStream 
                       1                        1                        1                        1                        1                        1 
        R_IsNamespaceEnv           R_IsPackageEnv             R_lsInternal  R_new_custom_connection         R_ReadConnection                 R_tmpnam 
                       1                        1                        1                        1                        1                        1 
         REAL_GET_REGION           REAL_IS_SORTED                    REAL0       Rf_any_duplicated3           Rf_desc2GEDesc                   Rf_elt 
                       1                        1                        1                        1                        1                        1 
        Rf_isBlankString        Rf_isVectorizable        Rf_KillAllDevices            Rf_killDevice                 Rf_list6            Rf_listAppend 
                       1                        1                        1                        1                        1                        1 
              Rf_nlevels             Rf_NoDevices         Rf_ScalarComplex         STRING_IS_SORTED 
                       1                        1                        1                        1 
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
