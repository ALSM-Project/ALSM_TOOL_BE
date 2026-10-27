./ ADD NAME=VBKA5090  01.00 20150805 20150805 164654 00502 00502 MCSK013
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5090'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : KA                            *
      *              <> PG.OUTLINE     : YUSOTANNI SOU PAGE SUU SETTEI *
      *                                                                *
      *                 * INPUT FILE   : ITIRANSE SEKYUSYO DATA (DWKB) *
      *                 * INPUT FILE   : ITIRANSE SEKYUSYO PAGE DATA   *
      *                                                         (DWKC) *
      *                 * OUTPUT FILE  : ITIRANSE SEKYUSYO DATA (DWKA) *
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/08/07                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1780             ÍÝº³   : 15/08/05  CSK-Y.YOSHIGA       *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(21).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKB-R.
           02 WKB-CNTKEY.
             03 WKB-NRKNO                   PIC  X(02).
             03 WKB-YTKBN                   PIC  X(01).
             03 WKB-YSYBN.
                04 WKB-YSYB1                PIC  X(03).
                04 WKB-YSYB2                PIC  X(04).
             03 WKB-YUSBNO                  PIC  X(03).
             03 WKB-YUSNO                   PIC  X(03).
             03 WKB-YUSCKK                  PIC  X(05).
             03 WKB-YUSCD                   PIC  X(07).
             03 WKB-YUNCD                   PIC  X(02).
             03 WKB-YSALL                   PIC  9(04).
             03 WKB-INFLG                   PIC  X(01).
             03 WKB-YSTPG                   PIC  9(04).
             03 WKB-PAGEN.
                04 WKB-PAGE1                PIC  9(04).
                04 WKB-PAGE2                PIC  9(04).
             03 WKB-SESCD                   PIC  X(07).
             03 WKB-SQNCD                   PIC  X(02).
           02 WKB-LPDAT                     PIC  X(219).
           02 WKB-DATA10  REDEFINES  WKB-LPDAT.
              03 WKB-GNO10                  PIC  9(02).
              03 WKB-RKB10                  PIC  X(02).
              03 WKB-MESAI1.
                 04 WKB-TJYSO.
                    05 WKB-TJYSA            PIC  X(40).
                    05 WKB-TJYSB            PIC  X(30).
                    05 WKB-TKBLMJ           PIC  X(30).
                 04 FILLER                  PIC  X(115).
           02 WKB-DATA20  REDEFINES  WKB-LPDAT.
              03 WKB-GNO20                  PIC  9(02).
              03 WKB-RKB20                  PIC  X(02).
              03 WKB-MESAI2.
                 04 WKB-TNMJO               PIC  X(40).
                 04 WKB-TNMGE               PIC  X(40).
                 04 WKB-NOSKA               PIC  X(40).
                 04 WKB-SQBNO               PIC  X(03).
                 04 WKB-SQSTN               PIC  X(03).
                 04 WKB-SQCKK               PIC  X(05).
                 04 WKB-SMEYMD.
                    05 WKB-SIMYY            PIC  9(04).
                    05 WKB-SIMMM            PIC  9(02).
                    05 WKB-SIMDD            PIC  9(02).
                 04 WKB-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKB-DATA30  REDEFINES  WKB-LPDAT.
              03 WKB-GNO30                  PIC  9(02).
              03 WKB-RKB30                  PIC  X(02).
              03 WKB-MESAI3.
                 04 WKB-HJPTN1              PIC  X(01).
                 04 WKB-MIDAK               PIC  X(08).
                 04 WKB-TGNME  OCCURS  3.
                    05 WKB-GSKAJ            PIC  X(30).
                    05 WKB-KOZKJ            PIC  X(04).
                    05 WKB-KZABG            PIC  X(07).
                 04 WKB-BNKSU               PIC  9(01).
                 04 FILLER                  PIC  X(82).
              03 FILLER  REDEFINES  WKB-MESAI3.
                 04 WKB-HJPTN2              PIC  X(01).
                 04 WKB-HYOJI1              PIC  X(22).
                 04 WKB-HYOJI2              PIC  X(16).
                 04 WKB-HYOJI3              PIC  X(26).
                 04 WKB-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 FILLER  REDEFINES  WKB-MESAI3.
                 04 WKB-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKB-DATA40  REDEFINES  WKB-LPDAT.
              03 WKB-GNO40                  PIC  9(02).
              03 WKB-RKB40                  PIC  X(02).
              03 WKB-MESAI4.
                 04 WKB-STNMJ4              PIC  X(18).
                 04 WKB-STYBN               PIC  X(07).
                 04 WKB-STTEL               PIC  X(13).
                 04 WKB-NSTJJ.
                    05 WKB-NSTJ1            PIC  X(36).
                    05 WKB-NSTJ2            PIC  X(36).
                 04 FILLER                  PIC  X(105).
           02 WKB-DATA50  REDEFINES  WKB-LPDAT.
              03 WKB-GNO50                  PIC  9(02).
              03 WKB-RKB50                  PIC  X(02).
              03 WKB-MESAI5.
                 04 WKB-ZENGT.
                    05 WKB-ZSKDK            PIC S9(13)  COMP-3.
                    05 WKB-ZNKDK            PIC S9(13)  COMP-3.
                    05 WKB-ZSYNB            PIC S9(09)  COMP-3.
                    05 WKB-ZKRDK            PIC S9(13)  COMP-3.
                 04 WKB-TOGTU.
                    05 WKB-TKIDK            PIC S9(13)  COMP-3.
                    05 WKB-TSKDK            PIC S9(13)  COMP-3.
                    05 WKB-MSKSK            PIC S9(13)  COMP-3.
                    05 WKB-TURZN            PIC S9(13)  COMP-3.
                 04 WKB-ZNITK               PIC  X(01).
                 04 WKB-SHZEI               PIC S9(13)  COMP-3.
                 04 WKB-TKAIG               PIC S9(13)  COMP-3.
                 04 WKB-SHZSK               PIC  X(01).
                 04 FILLER                  PIC  X(145).
           02 WKB-DATA60  REDEFINES  WKB-LPDAT.
              03 WKB-GNO60                  PIC  9(02).
              03 WKB-RKB60                  PIC  X(02).
              03 WKB-MESAI6.
                 04 WKB-DNHI1.
                    05 WKB-DNMM1            PIC  9(02).
                    05 WKB-DNDD1            PIC  9(02).
                 04 WKB-DNBN1               PIC  X(07).
                 04 WKB-TRKBN               PIC  X(08).
                 04 WKB-DNKIN               PIC S9(09)  COMP-3.
                 04 WKB-SYOME.
                    05 WKB-SYOMKN           PIC  X(50).
                    05 WKB-SYOMKY           PIC  X(40).
                 04 WKB-SSURYO              PIC S9(05)  COMP-3.
                 04 WKB-STANKA              PIC S9(08)  COMP-3.
                 04 WKB-SKNGK               PIC S9(09)  COMP-3.
                 04 WKB-SMSHZ               PIC S9(09)  COMP-3.
                 04 WKB-SYOCD               PIC  X(09).
                 04 WKB-SYOKK.
                    05 WKB-SYOHK            PIC  X(25).
                    05 WKB-SYOYK            PIC  X(20).
                 04 WKB-SBIKO.
                    05 WKB-DNHI2.
                       06 WKB-DNMM2         PIC  9(02).
                       06 WKB-DNDD2         PIC  9(02).
                    05 WKB-DNBN2            PIC  X(07).
                    05 WKB-STRKB            PIC  X(01).
                    05 WKB-BNKKB            PIC  X(01).
                 04 WKB-DSHZ3               PIC S9(09)  COMP-3.
                 04 WKB-DSHZFG              PIC  X(01).
                 04 WKB-MSHZUM              PIC  X(01).
                 04 FILLER                  PIC  X(09).
           02 WKB-DATA6X  REDEFINES  WKB-LPDAT.
              03 WKB-GNO6X                  PIC  9(02).
              03 WKB-RKB6X                  PIC  X(02).
              03 WKB-MESAIX.
                 04 WKB-DNPH1               PIC  9(04).
                 04 WKB-MIDA1               PIC  X(50).
                 04 WKB-MIDA2               PIC  X(50).
                 04 WKB-DNPKG               PIC S9(11)  COMP-3.
                 04 WKB-DSHZ4               PIC S9(11)  COMP-3.
                 04 FILLER                  PIC  X(99).
           02 WKB-DATA65  REDEFINES  WKB-LPDAT.
              03 WKB-GNO65                  PIC  9(02).
              03 WKB-RKB65                  PIC  X(02).
              03 WKB-MESI65.
                 04 WKB-MIDAU               PIC  X(50).
                 04 WKB-MIDAD               PIC  X(50).
                 04 WKB-CDEDT               PIC  X(14).
                 04 WKB-GOKGK               PIC S9(09)  COMP-3.
                 04 WKB-ZEGAK               PIC S9(09)  COMP-3.
                 04 WKB-MTOKC               PIC  X(07).
                 04 WKB-MNSCD               PIC  X(02).
                 04 FILLER                  PIC  X(82).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  WKC-R.
           02 WKC-YTKBN                     PIC  X(01).
           02 WKC-YUSCD                     PIC  X(07).
           02 WKC-YUNCD                     PIC  X(02).
           02 WKC-YSALL                     PIC  X(04).
           02 FILLER                        PIC  X(07).
       01  WKA-R.
           02 WKA-CNTKEY.
             03 WKA-NRKNO                   PIC  X(02).
             03 WKA-YTKBN                   PIC  X(01).
             03 WKA-YSYBN.
                04 WKA-YSYB1                PIC  X(03).
                04 WKA-YSYB2                PIC  X(04).
             03 WKA-YUSBNO                  PIC  X(03).
             03 WKA-YUSNO                   PIC  X(03).
             03 WKA-YUSCKK                  PIC  X(05).
             03 WKA-YUSCD                   PIC  X(07).
             03 WKA-YUNCD                   PIC  X(02).
             03 WKA-YSALL                   PIC  9(04).
             03 WKA-INFLG                   PIC  X(01).
             03 WKA-YSTPG                   PIC  9(04).
             03 WKA-PAGEN.
                04 WKA-PAGE1                PIC  9(04).
                04 WKA-PAGE2                PIC  9(04).
             03 WKA-SESCD                   PIC  X(07).
             03 WKA-SQNCD                   PIC  X(02).
           02 WKA-LPDAT                     PIC  X(219).
           02 WKA-DATA10  REDEFINES  WKA-LPDAT.
              03 WKA-GNO10                  PIC  9(02).
              03 WKA-RKB10                  PIC  X(02).
              03 WKA-MESAI1.
                 04 WKA-TJYSO.
                    05 WKA-TJYSA            PIC  X(40).
                    05 WKA-TJYSB            PIC  X(30).
                    05 WKA-TKBLMJ           PIC  X(30).
                 04 FILLER                  PIC  X(115).
           02 WKA-DATA20  REDEFINES  WKA-LPDAT.
              03 WKA-GNO20                  PIC  9(02).
              03 WKA-RKB20                  PIC  X(02).
              03 WKA-MESAI2.
                 04 WKA-TNMJO               PIC  X(40).
                 04 WKA-TNMGE               PIC  X(40).
                 04 WKA-NOSKA               PIC  X(40).
                 04 WKA-SQBNO               PIC  X(03).
                 04 WKA-SQSTN               PIC  X(03).
                 04 WKA-SQCKK               PIC  X(05).
                 04 WKA-SMEYMD.
                    05 WKA-SIMYY            PIC  9(04).
                    05 WKA-SIMMM            PIC  9(02).
                    05 WKA-SIMDD            PIC  9(02).
                 04 WKA-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKA-DATA30  REDEFINES  WKA-LPDAT.
              03 WKA-GNO30                  PIC  9(02).
              03 WKA-RKB30                  PIC  X(02).
              03 WKA-MESAI3.
                 04 WKA-HJPTN1              PIC  X(01).
                 04 WKA-MIDAK               PIC  X(08).
                 04 WKA-TGNME  OCCURS  3.
                    05 WKA-GSKAJ            PIC  X(30).
                    05 WKA-KOZKJ            PIC  X(04).
                    05 WKA-KZABG            PIC  X(07).
                 04 WKA-BNKSU               PIC  9(01).
                 04 FILLER                  PIC  X(82).
              03 FILLER  REDEFINES  WKA-MESAI3.
                 04 WKA-HJPTN2              PIC  X(01).
                 04 WKA-HYOJI1              PIC  X(22).
                 04 WKA-HYOJI2              PIC  X(16).
                 04 WKA-HYOJI3              PIC  X(26).
                 04 WKA-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 FILLER  REDEFINES  WKA-MESAI3.
                 04 WKA-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKA-DATA40  REDEFINES  WKA-LPDAT.
              03 WKA-GNO40                  PIC  9(02).
              03 WKA-RKB40                  PIC  X(02).
              03 WKA-MESAI4.
                 04 WKA-STNMJ4              PIC  X(18).
                 04 WKA-STYBN               PIC  X(07).
                 04 WKA-STTEL               PIC  X(13).
                 04 WKA-NSTJJ.
                    05 WKA-NSTJ1            PIC  X(36).
                    05 WKA-NSTJ2            PIC  X(36).
                 04 FILLER                  PIC  X(105).
           02 WKA-DATA50  REDEFINES  WKA-LPDAT.
              03 WKA-GNO50                  PIC  9(02).
              03 WKA-RKB50                  PIC  X(02).
              03 WKA-MESAI5.
                 04 WKA-ZENGT.
                    05 WKA-ZSKDK            PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK            PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB            PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK            PIC S9(13)  COMP-3.
                 04 WKA-TOGTU.
                    05 WKA-TKIDK            PIC S9(13)  COMP-3.
                    05 WKA-TSKDK            PIC S9(13)  COMP-3.
                    05 WKA-MSKSK            PIC S9(13)  COMP-3.
                    05 WKA-TURZN            PIC S9(13)  COMP-3.
                 04 WKA-ZNITK               PIC  X(01).
                 04 WKA-SHZEI               PIC S9(13)  COMP-3.
                 04 WKA-TKAIG               PIC S9(13)  COMP-3.
                 04 WKA-SHZSK               PIC  X(01).
                 04 FILLER                  PIC  X(145).
           02 WKA-DATA60  REDEFINES  WKA-LPDAT.
              03 WKA-GNO60                  PIC  9(02).
              03 WKA-RKB60                  PIC  X(02).
              03 WKA-MESAI6.
                 04 WKA-DNHI1.
                    05 WKA-DNMM1            PIC  9(02).
                    05 WKA-DNDD1            PIC  9(02).
                 04 WKA-DNBN1               PIC  X(07).
                 04 WKA-TRKBN               PIC  X(08).
                 04 WKA-DNKIN               PIC S9(09)  COMP-3.
                 04 WKA-SYOME.
                    05 WKA-SYOMKN           PIC  X(50).
                    05 WKA-SYOMKY           PIC  X(40).
                 04 WKA-SSURYO              PIC S9(05)  COMP-3.
                 04 WKA-STANKA              PIC S9(08)  COMP-3.
                 04 WKA-SKNGK               PIC S9(09)  COMP-3.
                 04 WKA-SMSHZ               PIC S9(09)  COMP-3.
                 04 WKA-SYOCD               PIC  X(09).
                 04 WKA-SYOKK.
                    05 WKA-SYOHK            PIC  X(25).
                    05 WKA-SYOYK            PIC  X(20).
                 04 WKA-SBIKO.
                    05 WKA-DNHI2.
                       06 WKA-DNMM2         PIC  9(02).
                       06 WKA-DNDD2         PIC  9(02).
                    05 WKA-DNBN2            PIC  X(07).
                    05 WKA-STRKB            PIC  X(01).
                    05 WKA-BNKKB            PIC  X(01).
                 04 WKA-DSHZ3               PIC S9(09)  COMP-3.
                 04 WKA-DSHZFG              PIC  X(01).
                 04 WKA-MSHZUM              PIC  X(01).
                 04 FILLER                  PIC  X(09).
           02 WKA-DATA6X  REDEFINES  WKA-LPDAT.
              03 WKA-GNO6X                  PIC  9(02).
              03 WKA-RKB6X                  PIC  X(02).
              03 WKA-MESAIX.
                 04 WKA-DNPH1               PIC  9(04).
                 04 WKA-MIDA1               PIC  X(50).
                 04 WKA-MIDA2               PIC  X(50).
                 04 WKA-DNPKG               PIC S9(11)  COMP-3.
                 04 WKA-DSHZ4               PIC S9(11)  COMP-3.
                 04 FILLER                  PIC  X(99).
           02 WKA-DATA65  REDEFINES  WKA-LPDAT.
              03 WKA-GNO65                  PIC  9(02).
              03 WKA-RKB65                  PIC  X(02).
              03 WKA-MESI65.
                 04 WKA-MIDAU               PIC  X(50).
                 04 WKA-MIDAD               PIC  X(50).
                 04 WKA-CDEDT               PIC  X(14).
                 04 WKA-GOKGK               PIC S9(09)  COMP-3.
                 04 WKA-ZEGAK               PIC S9(09)  COMP-3.
                 04 WKA-MTOKC               PIC  X(07).
                 04 WKA-MNSCD               PIC  X(02).
                 04 FILLER                  PIC  X(82).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-YTKBN              PIC  X(01).
                 04 WIN1-YUSCD              PIC  X(07).
                 04 WIN1-YUNCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-YTKBN              PIC  X(01).
                 04 WIN2-YUSCD              PIC  X(07).
                 04 WIN2-YUNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-YTKBN                 PIC  X(01).
              03 WSV1-YUSCD                 PIC  X(07).
              03 WSV1-YUNCD                 PIC  X(02).
       01  EDIT-AREA.
           02 WED1-PAGE.
              03 WED1-PAGEX                 PIC  X(04).
           02 WED2-PAGER  REDEFINES  WED1-PAGE.
              03 WED2-PAGE9                 PIC  9(04).
           02 WED3-YSALL.
              03 FILLER                     PIC  X(02).
              03 WED3-YSALL3                PIC  X(02).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE OR
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                       WKCFILE
                OUTPUT WKAFILE.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
       M11-L01.
           MOVE WKB-YTKBN TO WIN1-YTKBN.
           MOVE WKB-YUSCD TO WIN1-YUSCD.
           MOVE WKB-YUNCD TO WIN1-YUNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
       M12-L01.
           MOVE WKC-YTKBN TO WIN2-YTKBN.
           MOVE WKC-YUSCD TO WIN2-YUSCD.
           MOVE WKC-YUNCD TO WIN2-YUNCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-WKBONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKCONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M211-EDIT-WKA THRU M211-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT WKAFILE                                       *
      *----------------------------------------------------------------*
       M211-EDIT-WKA.
           MOVE SPACE     TO WKA-R.
           INITIALIZE        WKA-R.
           MOVE WKB-R     TO WKA-R.
           MOVE WKC-YSALL TO WKA-YSALL.
           MOVE WKC-YSALL TO WED1-PAGEX.
           IF WED2-PAGE9 NOT < 4
             MOVE '04'      TO WKA-PGMSU
           ELSE
             MOVE WKC-YSALL TO WED3-YSALL
             MOVE WED3-YSALL3
                            TO WKA-PGMSU.
           WRITE WKA-REC  FROM WKA-R.
       M211-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKBONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M11-READ-WKB THRU M11-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKCFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKCONL.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 WKCFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5090 END                                           *
      *----------------------------------------------------------------*
