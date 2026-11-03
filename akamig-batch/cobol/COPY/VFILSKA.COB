      *                              /*   CS SEIKYUSYO DATA AFTER   */
      *                              /*   LENGTH =>  250            */
      *                              /*   DATE  19/06/19   V-007    */
           02 SKA-CNTRL.
              03 SKA-KAINO                  PIC  X(02).
              03 SKA-YTKBN                  PIC  X(01).
              03 SKA-YSYBN.
                 04 SKA-YSYB1               PIC  X(03).
                 04 SKA-YSYB2               PIC  X(04).
              03 SKA-YUSBNO                 PIC  X(03).
              03 SKA-YUSNO                  PIC  X(03).
              03 SKA-YUSCKK                 PIC  X(05).
              03 SKA-YUSCD                  PIC  X(07).
              03 SKA-YUNCD                  PIC  X(02).
              03 SKA-YSALL                  PIC  9(04).
              03 SKA-INFLG                  PIC  X(01).
              03 SKA-YSTPG                  PIC  9(04).
              03 SKA-PAGEN.
                 04 SKA-PAGE1               PIC  9(04).
                 04 SKA-PAGE2               PIC  9(04).
              03 SKA-SESCD                  PIC  X(07).
              03 SKA-SQNCD                  PIC  X(02).
           02 SKA-GYOBG                     PIC  9(02).
           02 SKA-RECKB                     PIC  X(02).
           02 SKA-MEISAI                    PIC  X(188).
           02 SKA-DAT03 REDEFINES SKA-MEISAI.
              03 SKA-YUADD.
                 04 SKA-YUAD1               PIC  X(40).
                 04 SKA-YUAD2               PIC  X(30).
                 04 SKA-YUADB               PIC  X(30).
              03 FILLER                     PIC  X(88).
           02 SKA-DAT04 REDEFINES SKA-MEISAI.
              03 SKA-TKMJO                  PIC  X(40).
              03 SKA-TKMGE                  PIC  X(40).
              03 SKA-NSKNJ                  PIC  X(40).
              03 SKA-YUSBN                  PIC  X(03).
              03 SKA-YUSTN                  PIC  X(03).
              03 SKA-YUTKG                  PIC  X(05).
              03 SKA-SIMYMD.
                 04 SKA-SIMYY               PIC  9(04).
                 04 SKA-SIMMM               PIC  9(02).
                 04 SKA-SIMDD               PIC  9(02).
              03 FILLER                     PIC  X(49).
           02 SKA-DAT05 REDEFINES SKA-MEISAI.
              03 SKA-HJPTN                  PIC  X(01).
              03 SKA-MIDAK5                 PIC  X(08).
              03 SKA-TGNME5  OCCURS  3.
                 04 SKA-GSKAJ5              PIC  X(30).
                 04 SKA-KOZKJ5              PIC  X(04).
                 04 SKA-KZABG5              PIC  X(07).
              03 SKA-BNKSU5                 PIC  9(01).
              03 FILLER                     PIC  X(55).
           02 SKA-DAT06 REDEFINES SKA-MEISAI.
              03 SKA-STNMJ6                 PIC  X(18).
              03 SKA-STYBN6                 PIC  X(07).
              03 SKA-STTEL6                 PIC  X(13).
              03 SKA-NSTJJ6.
                 04 SKA-NSTJ16              PIC  X(36).
                 04 SKA-NSTJ26              PIC  X(36).
              03 FILLER                     PIC  X(78).
           02 SKA-DAT07 REDEFINES SKA-MEISAI.
              03 SKA-ZENGT7.
                 04 SKA-ZSKDK7              PIC S9(13)  COMP-3.
                 04 SKA-ZNKDK7              PIC S9(13)  COMP-3.
                 04 SKA-ZSYNB7              PIC S9(09)  COMP-3.
                 04 SKA-ZKRDK7              PIC S9(13)  COMP-3.
              03 SKA-TOGTU7.
                 04 SKA-TKIDK7              PIC S9(13)  COMP-3.
                 04 SKA-TSKDK7              PIC S9(13)  COMP-3.
                 04 SKA-MSKSK7              PIC S9(13)  COMP-3.
                 04 SKA-TURZN7              PIC S9(13)  COMP-3.
              03 SKA-SHZEI7                 PIC S9(13)  COMP-3.
              03 SKA-TKAIG7                 PIC S9(13)  COMP-3.
              03 SKA-SHZSK7                 PIC  X(01).
              03 FILLER                     PIC  X(119).
           02 SKA-DAT0H REDEFINES SKA-MEISAI.
              03 SKA-ZRGKI.
                 04 SKA-MIDSI1              PIC  X(22).
                 04 SKA-OKIDKH              PIC S9(13)   COMP-3.
                 04 SKA-MIDSI2              PIC  X(08).
                 04 SKA-SHZEIH              PIC S9(13)   COMP-3.
                 04 SKA-ZEIRTH              PIC S9(02)V9 COMP-3.
                 04 SKA-KGZRTH              PIC  X(01).
              03 FILLER                     PIC  X(141).
           02 SKA-DAT08 REDEFINES SKA-MEISAI.
              03 SKA-ZENGT8.
                 04 SKA-ZSKDK8              PIC S9(13)  COMP-3.
                 04 SKA-ZNKDK8              PIC S9(13)  COMP-3.
                 04 SKA-ZSYNB8              PIC S9(09)  COMP-3.
                 04 SKA-ZKRDK8              PIC S9(13)  COMP-3.
              03 SKA-TOGTU8.
                 04 SKA-TKIDK8              PIC S9(13)  COMP-3.
                 04 SKA-TSKDK8              PIC S9(13)  COMP-3.
                 04 SKA-MSKSK8              PIC S9(13)  COMP-3.
                 04 SKA-TURZN8              PIC S9(13)  COMP-3.
              03 SKA-SHZEI8                 PIC S9(13)  COMP-3.
              03 SKA-TKAIG8                 PIC S9(13)  COMP-3.
              03 SKA-SGSM1                  PIC  X(40).
              03 SKA-SGSM2                  PIC  X(40).
              03 SKA-SGNME                  PIC  X(30).
              03 SKA-HIFLG                  PIC  X(01).
              03 FILLER                     PIC  X(09).
           02 SKA-DAT10 REDEFINES SKA-MEISAI.
              03 SKA-SEADD.
                 04 SKA-SEAD1               PIC  X(40).
                 04 SKA-SEAD2               PIC  X(30).
                 04 SKA-SEADB               PIC  X(30).
              03 SKA-SEYBN.
                 04 SKA-SEYB1               PIC  X(03).
                 04 SKA-SEYB2               PIC  X(04).
              03 FILLER                     PIC  X(81).
           02 SKA-DAT20 REDEFINES SKA-MEISAI.
              03 SKA-TNMJO                  PIC  X(40).
              03 SKA-TNMGE                  PIC  X(40).
              03 SKA-NOSKA                  PIC  X(40).
              03 SKA-SQBNO                  PIC  X(03).
              03 SKA-SQSTN                  PIC  X(03).
              03 SKA-SQCKK                  PIC  X(05).
              03 SKA-SIMEY.
                 04 SKA-SIYY                PIC  9(04).
                 04 SKA-SIMM                PIC  9(02).
                 04 SKA-SIDD                PIC  9(02).
              03 FILLER                     PIC  X(49).
           02 SKA-DAT30 REDEFINES SKA-MEISAI.
              03 SKA-HPTN1                  PIC  X(01).
              03 SKA-MIDAK                  PIC  X(08).
              03 SKA-TGNME  OCCURS  3.
                 04 SKA-GSKAJ               PIC  X(30).
                 04 SKA-KOZKJ               PIC  X(04).
                 04 SKA-KZABG               PIC  X(07).
              03 SKA-BNKSU                  PIC  9(01).
              03 FILLER                     PIC  X(55).
           02 FILLER REDEFINES SKA-MEISAI.
              03 SKA-HPTN2                  PIC  X(01).
              03 SKA-HYOJI1                 PIC  X(22).
              03 SKA-HYOJI2                 PIC  X(16).
              03 SKA-HYOJI3                 PIC  X(26).
              03 SKA-HYOJI4                 PIC  X(20).
              03 FILLER                     PIC  X(103).
           02 FILLER REDEFINES SKA-MEISAI.
              03 SKA-HPTN3                  PIC  X(01).
              03 FILLER                     PIC  X(187).
           02 SKA-DAT40 REDEFINES SKA-MEISAI.
              03 SKA-STNMJ4                 PIC  X(18).
              03 SKA-STYBN                  PIC  X(07).
              03 SKA-STTEL                  PIC  X(13).
              03 SKA-NSTJJ.
                 04 SKA-NSTJ1               PIC  X(36).
                 04 SKA-NSTJ2               PIC  X(36).
              03 SKA-HKJBG                  PIC  X(14).
              03 FILLER                     PIC  X(64).
           02 SKA-DAT50 REDEFINES SKA-MEISAI.
              03 SKA-ZENGT.
                 04 SKA-ZSKDK               PIC S9(13)  COMP-3.
                 04 SKA-ZNKDK               PIC S9(13)  COMP-3.
                 04 SKA-ZSYNB               PIC S9(09)  COMP-3.
                 04 SKA-ZKRDK               PIC S9(13)  COMP-3.
              03 SKA-TOGTU.
                 04 SKA-TKIDK               PIC S9(13)  COMP-3.
                 04 SKA-TSKDK               PIC S9(13)  COMP-3.
                 04 SKA-MSKSK               PIC S9(13)  COMP-3.
                 04 SKA-TURZN               PIC S9(13)  COMP-3.
              03 SKA-ZNITK                  PIC  X(01).
              03 SKA-SHZEI                  PIC S9(13)  COMP-3.
              03 SKA-TKAIG                  PIC S9(13)  COMP-3.
              03 SKA-SHZSK                  PIC  X(01).
              03 FILLER                     PIC  X(118).
           02 SKA-DAT60 REDEFINES SKA-MEISAI.
              03 SKA-DNHI1.
                 04 SKA-DNMM1               PIC  9(02).
                 04 SKA-DNDD1               PIC  9(02).
              03 SKA-DNBN1                  PIC  X(07).
              03 SKA-TRKBN                  PIC  X(08).
              03 SKA-DNKIN                  PIC S9(09)  COMP-3.
              03 SKA-SYOME.
                 04 SKA-SYOMKN              PIC  X(50).
                 04 SKA-SYOMKY              PIC  X(40).
              03 SKA-SSURYO                 PIC S9(05)  COMP-3.
              03 SKA-STANKA                 PIC S9(09)  COMP-3.
              03 SKA-SKNGK                  PIC S9(09)  COMP-3.
              03 SKA-SMSHZ                  PIC S9(09)  COMP-3.
              03 SKA-SBIKO.
                 04 SKA-DNHI2.
                    05 SKA-DNMM2            PIC  9(02).
                    05 SKA-DNDD2            PIC  9(02).
                 04 SKA-DNBN2               PIC  X(07).
                 04 SKA-STRKB               PIC  X(01).
                 04 SKA-BNKKB               PIC  X(01).
              03 SKA-DSHZ3                  PIC S9(09)  COMP-3.
              03 SKA-DSHZFG                 PIC  X(01).
              03 SKA-MSHZUM                 PIC  X(01).
              03 SKA-MSZRT                  PIC S9(02)V9 COMP-3.
              03 SKA-DPGK60                 PIC S9(09)   COMP-3.
              03 SKA-KGZH60                 PIC  X(02).
              03 FILLER                     PIC  X(27).
           02 SKA-DAT6X REDEFINES SKA-MEISAI.
              03 SKA-DNPH1                  PIC  9(04).
              03 SKA-MIDA1                  PIC  X(50).
              03 SKA-MIDA2                  PIC  X(50).
              03 SKA-DNPKG                  PIC S9(11)   COMP-3.
              03 SKA-DSHZ4                  PIC S9(11)   COMP-3.
              03 SKA-DPGK6X                 PIC S9(11)   COMP-3.
              03 SKA-ZERT6X                 PIC S9(02)V9 COMP-3.
              03 SKA-KGZH6X                 PIC  X(02).
              03 FILLER                     PIC  X(62).
           02 SKA-DAT65 REDEFINES SKA-MEISAI.
              03 SKA-MIDAU                  PIC  X(50).
              03 SKA-MIDAD                  PIC  X(50).
              03 SKA-CDEDT                  PIC  X(14).
              03 SKA-TNGOK                  PIC S9(09)  COMP-3.
              03 SKA-TSHZK                  PIC S9(09)  COMP-3.
              03 SKA-MTOKC                  PIC  X(07).
              03 SKA-MNSCD                  PIC  X(02).
              03 SKA-TNGKK                  PIC S9(09)  COMP-3.
              03 FILLER                     PIC  X(50).
           02 SKA-DAT66 REDEFINES SKA-MEISAI.
              03 SKA-MIDAU6                 PIC  X(50).
              03 SKA-MIDAD6                 PIC  X(50).
              03 SKA-ZOKDK                  PIC S9(11)   COMP-3.
              03 SKA-ZSHZI                  PIC S9(11)   COMP-3.
              03 SKA-ZOKGDK                 PIC S9(11)   COMP-3.
              03 SKA-ZEIRT6                 PIC S9(02)V9 COMP-3.
              03 SKA-KGZRT6                 PIC  X(01).
              03 FILLER                     PIC  X(67).
           02 SKA-PGMSU                     PIC  X(02).
