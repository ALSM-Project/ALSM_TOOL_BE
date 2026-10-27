./ ADD NAME=VBKA5100  01.07 20150805 20190726 133402 00581 00560 MCSK013
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5100'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : KA                            *
      *              <> PG.OUTLINE     : ITIRANSE SEKYUSYO             *
      *                                               DATA KAKUTEI     *
      *                 * INPUT FILE   : SEKYUSYO DATA (SEKYU KAGAMI)  *
      *                                :                        (DWKA) *
      *                 * INPUT FILE   : SEKYUSYO DATA (SEKYU MEISAI)  *
      *                                                         (DWKB) *
      *                 * OUTPUT FILE  : ITIRANSE SEKYUSYO DATA (DWKC) *
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/08/08                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1780             ÍÝº³   : 15/08/05  CSK-Y.YOSHIGA       *
      * (02) K-02163            ÍÝº³   : 19/07/10  SCSK-A.ISIKAWA      *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           02 WKA-CNTKEY.
              03 WKA-NRKNO                  PIC  X(02).
              03 WKA-YTKBN                  PIC  X(01).
              03 WKA-YSYBN.
                 04 WKA-YSYB1               PIC  X(03).
                 04 WKA-YSYB2               PIC  X(04).
              03 WKA-YUSBNO                 PIC  X(03).
              03 WKA-YUSNO                  PIC  X(03).
              03 WKA-YUSCKK                 PIC  X(05).
              03 WKA-YUSCD                  PIC  X(07).
              03 WKA-YUNCD                  PIC  X(02).
              03 WKA-YSALL                  PIC  9(04).
              03 WKA-INFLG                  PIC  X(01).
              03 WKA-YSTPG                  PIC  9(04).
              03 WKA-PAGEN.
                 04 WKA-PAGE1               PIC  9(04).
                 04 WKA-PAGE2               PIC  9(04).
              03 WKA-SESCD                  PIC  X(07).
              03 WKA-SQNCD                  PIC  X(02).
           02 WKA-LPDAT                     PIC  X(219).
           02 WKA-DATA10  REDEFINES  WKA-LPDAT.
              03 WKA-GNO10                  PIC  9(02).
              03 WKA-RKB10                  PIC  X(02).
              03 WKA-MESAI1.
                 04 WKA-TJYSO.
                    05 WKA-TJYSA            PIC  X(40).
                    05 WKA-TJYSB            PIC  X(30).
                    05 WKA-TKBLMJ           PIC  X(30).
                 04 WKA-SEYBN.
                    05 WKA-SEYB1            PIC  X(03).
                    05 WKA-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
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
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
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
           02 WKC-CNTKEY.
              03 WKC-NRKNO                  PIC  X(02).
              03 WKC-YTKBN                  PIC  X(01).
              03 WKC-YSYBN.
                 04 WKC-YSYB1               PIC  X(03).
                 04 WKC-YSYB2               PIC  X(04).
              03 WKC-YUSBNO                 PIC  X(03).
              03 WKC-YUSNO                  PIC  X(03).
              03 WKC-YUSCKK                 PIC  X(05).
              03 WKC-YUSCD                  PIC  X(07).
              03 WKC-YUNCD                  PIC  X(02).
              03 WKC-YSALL                  PIC  9(04).
              03 WKC-INFLG                  PIC  X(01).
              03 WKC-YSTPG                  PIC  9(04).
              03 WKC-PAGEN.
                 04 WKC-PAGE1               PIC  9(04).
                 04 WKC-PAGE2               PIC  9(04).
              03 WKC-SESCD                  PIC  X(07).
              03 WKC-SQNCD                  PIC  X(02).
           02 WKC-LPDAT                     PIC  X(219).
           02 WKC-DATA10  REDEFINES  WKC-LPDAT.
              03 WKC-GNO10                  PIC  9(02).
              03 WKC-RKB10                  PIC  X(02).
              03 WKC-MESAI1.
                 04 WKC-TJYSO.
                    05 WKC-TJYSA            PIC  X(40).
                    05 WKC-TJYSB            PIC  X(30).
                    05 WKC-TKBLMJ           PIC  X(30).
                 04 WKC-SEYBN.
                    05 WKC-SEYB1            PIC  X(03).
                    05 WKC-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
           02 WKC-DATA20  REDEFINES  WKC-LPDAT.
              03 WKC-GNO20                  PIC  9(02).
              03 WKC-RKB20                  PIC  X(02).
              03 WKC-MESAI2.
                 04 WKC-TNMJO               PIC  X(40).
                 04 WKC-TNMGE               PIC  X(40).
                 04 WKC-NOSKA               PIC  X(40).
                 04 WKC-SQBNO               PIC  X(03).
                 04 WKC-SQSTN               PIC  X(03).
                 04 WKC-SQCKK               PIC  X(05).
                 04 WKC-SMEYMD.
                    05 WKC-SIMYY            PIC  9(04).
                    05 WKC-SIMMM            PIC  9(02).
                    05 WKC-SIMDD            PIC  9(02).
                 04 WKC-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKC-DATA30  REDEFINES  WKC-LPDAT.
              03 WKC-GNO30                  PIC  9(02).
              03 WKC-RKB30                  PIC  X(02).
              03 WKC-MESAI3.
                 04 WKC-HJPTN1              PIC  X(01).
                 04 WKC-MIDAK               PIC  X(08).
                 04 WKC-TGNME  OCCURS  3.
                    05 WKC-GSKAJ            PIC  X(30).
                    05 WKC-KOZKJ            PIC  X(04).
                    05 WKC-KZABG            PIC  X(07).
                 04 WKC-BNKSU               PIC  9(01).
                 04 FILLER                  PIC  X(82).
              03 FILLER  REDEFINES  WKC-MESAI3.
                 04 WKC-HJPTN2              PIC  X(01).
                 04 WKC-HYOJI1              PIC  X(22).
                 04 WKC-HYOJI2              PIC  X(16).
                 04 WKC-HYOJI3              PIC  X(26).
                 04 WKC-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 FILLER  REDEFINES  WKC-MESAI3.
                 04 WKC-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKC-DATA40  REDEFINES  WKC-LPDAT.
              03 WKC-GNO40                  PIC  9(02).
              03 WKC-RKB40                  PIC  X(02).
              03 WKC-MESAI4.
                 04 WKC-STNMJ4              PIC  X(18).
                 04 WKC-STYBN               PIC  X(07).
                 04 WKC-STTEL               PIC  X(13).
                 04 WKC-NSTJJ.
                    05 WKC-NSTJ1            PIC  X(36).
                    05 WKC-NSTJ2            PIC  X(36).
                 04 WKC-HKJBG               PIC  X(14).
                 04 FILLER                  PIC  X(91).
           02 WKC-DATA50  REDEFINES  WKC-LPDAT.
              03 WKC-GNO50                  PIC  9(02).
              03 WKC-RKB50                  PIC  X(02).
              03 WKC-MESAI5.
                 04 WKC-ZENGT.
                    05 WKC-ZSKDK            PIC S9(13)  COMP-3.
                    05 WKC-ZNKDK            PIC S9(13)  COMP-3.
                    05 WKC-ZSYNB            PIC S9(09)  COMP-3.
                    05 WKC-ZKRDK            PIC S9(13)  COMP-3.
                 04 WKC-TOGTU.
                    05 WKC-TKIDK            PIC S9(13)  COMP-3.
                    05 WKC-TSKDK            PIC S9(13)  COMP-3.
                    05 WKC-MSKSK            PIC S9(13)  COMP-3.
                    05 WKC-TURZN            PIC S9(13)  COMP-3.
                 04 WKC-ZNITK               PIC  X(01).
                 04 WKC-SHZEI               PIC S9(13)  COMP-3.
                 04 WKC-TKAIG               PIC S9(13)  COMP-3.
                 04 WKC-SHZSK               PIC  X(01).
                 04 FILLER                  PIC  X(145).
           02 WKC-DATA60  REDEFINES  WKC-LPDAT.
              03 WKC-GNO60                  PIC  9(02).
              03 WKC-RKB60                  PIC  X(02).
              03 WKC-MESAI6.
                 04 WKC-DNHI1.
                    05 WKC-DNMM1            PIC  9(02).
                    05 WKC-DNDD1            PIC  9(02).
                 04 WKC-DNBN1               PIC  X(07).
                 04 WKC-TRKBN               PIC  X(08).
                 04 WKC-DNKIN               PIC S9(09)  COMP-3.
                 04 WKC-SYOME.
                    05 WKC-SYOMKN           PIC  X(50).
                    05 WKC-SYOMKY           PIC  X(40).
                 04 WKC-SSURYO              PIC S9(05)  COMP-3.
                 04 WKC-STANKA              PIC S9(08)  COMP-3.
                 04 WKC-SKNGK               PIC S9(09)  COMP-3.
                 04 WKC-SMSHZ               PIC S9(09)  COMP-3.
                 04 WKC-SYOCD               PIC  X(09).
                 04 WKC-SYOKK.
                    05 WKC-SYOHK            PIC  X(25).
                    05 WKC-SYOYK            PIC  X(20).
                 04 WKC-SBIKO.
                    05 WKC-DNHI2.
                       06 WKC-DNMM2         PIC  9(02).
                       06 WKC-DNDD2         PIC  9(02).
                    05 WKC-DNBN2            PIC  X(07).
                    05 WKC-STRKB            PIC  X(01).
                    05 WKC-BNKKB            PIC  X(01).
                 04 WKC-DSHZ3               PIC S9(09)  COMP-3.
                 04 WKC-DSHZFG              PIC  X(01).
                 04 WKC-MSHZUM              PIC  X(01).
                 04 WKC-MSZRT               PIC S9(02)V9 COMP-3.
                 04 WKC-DPGK60              PIC S9(09)   COMP-3.
                 04 WKC-KGZH60              PIC  X(02).
           02 WKC-DATA6X  REDEFINES  WKC-LPDAT.
              03 WKC-GNO6X                  PIC  9(02).
              03 WKC-RKB6X                  PIC  X(02).
              03 WKC-MESAIX.
                 04 WKC-DNPH1               PIC  9(04).
                 04 WKC-MIDA1               PIC  X(50).
                 04 WKC-MIDA2               PIC  X(50).
                 04 WKC-DNPKG               PIC S9(11)  COMP-3.
                 04 WKC-DSHZ4               PIC S9(11)  COMP-3.
                 04 WKC-DPGK6X              PIC S9(11)   COMP-3.
                 04 WKC-ZERT6X              PIC S9(02)V9 COMP-3.
                 04 WKC-KGZH6X              PIC  X(02).
                 04 FILLER                  PIC  X(89).
           02 WKC-DATA65  REDEFINES  WKC-LPDAT.
              03 WKC-GNO65                  PIC  9(02).
              03 WKC-RKB65                  PIC  X(02).
              03 WKC-MESI65.
                 04 WKC-MIDAU               PIC  X(50).
                 04 WKC-MIDAD               PIC  X(50).
                 04 WKC-CDEDT               PIC  X(14).
                 04 WKC-GOKGK               PIC S9(09)  COMP-3.
                 04 WKC-ZEGAK               PIC S9(09)  COMP-3.
                 04 WKC-MTOKC               PIC  X(07).
                 04 WKC-MNSCD               PIC  X(02).
                 04 WKC-TNGKK               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(77).
           02 WKC-DAT66   REDEFINES  WKC-LPDAT.
              03 WKC-GNO66                  PIC  9(02).
              03 WKC-RKB66                  PIC  X(02).
              03 WKC-MESI66.
                 04 WKC-MIDAU6              PIC  X(50).
                 04 WKC-MIDAD6              PIC  X(50).
                 04 WKC-ZOKDK               PIC S9(11)   COMP-3.
                 04 WKC-ZSHZI               PIC S9(11)   COMP-3.
                 04 WKC-ZOKGDK              PIC S9(11)   COMP-3.
                 04 WKC-ZEIRT6              PIC S9(02)V9 COMP-3.
                 04 WKC-KGZRT6              PIC  X(01).
                 04 FILLER                  PIC  X(94).

           02 WKC-PGMSU                     PIC  X(02).
           02 WKC-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              04 WIN1-SESCD                 PIC  X(07).
              04 WIN1-SQNCD                 PIC  X(02).
           02 WIN2-KEY.
              04 WIN2-SESCD                 PIC  X(07).
              04 WIN2-SQNCD                 PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-SESCD                 PIC  X(07).
              03 WSV1-SQNCD                 PIC  X(02).
           02 WSV2-KEY.
              03 WSV2-SESCD                 PIC  X(07).
              03 WSV2-SQNCD                 PIC  X(02).
           02 WSV3-WKA.
              03 WSV3-NRKNO                 PIC  X(02).
              03 WSV3-YTKBN                 PIC  X(01).
              03 WSV3-YSYBN                 PIC  X(07).
              03 WSV3-YUSBNO                PIC  X(03).
              03 WSV3-YUSNO                 PIC  X(03).
              03 WSV3-YUSCKK                PIC  X(05).
              03 WSV3-YUSCD                 PIC  X(07).
              03 WSV3-YUNCD                 PIC  X(02).
              03 WSV3-SORTJ                 PIC  X(20).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE AND
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                       WKBFILE
                OUTPUT WKCFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
       M11-L01.
           MOVE WKA-SESCD TO WIN1-SESCD.
           MOVE WKA-SQNCD TO WIN1-SQNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
       M12-L01.
           MOVE WKB-SESCD TO WIN2-SESCD.
           MOVE WKB-SQNCD TO WIN2-SQNCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY < WIN2-KEY
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKBONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE  WIN1-KEY TO WSV1-KEY.
           PERFORM M211-EDIT-WKA THRU M211-EXIT
             UNTIL (WIN1-KEY NOT = WSV1-KEY).
       M21-L01.
           MOVE  WIN2-KEY TO WSV2-KEY.
           PERFORM M212-EDIT-WKB THRU M212-EXIT
             UNTIL (WIN2-KEY NOT = WSV2-KEY).
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT WKAFILE                                       *
      *----------------------------------------------------------------*
       M211-EDIT-WKA.
           MOVE WKA-R      TO WKC-R.
           WRITE WKC-REC   FROM WKC-R.
           IF WKA-RKB10 = '50'
              MOVE WKA-NRKNO  TO WSV3-NRKNO
              MOVE WKA-YTKBN  TO WSV3-YTKBN
              MOVE WKA-YSYBN  TO WSV3-YSYBN
              MOVE WKA-YUSBNO TO WSV3-YUSBNO
              MOVE WKA-YUSNO  TO WSV3-YUSNO
              MOVE WKA-YUSCKK TO WSV3-YUSCKK
              MOVE WKA-YUSCD  TO WSV3-YUSCD
              MOVE WKA-YUNCD  TO WSV3-YUNCD
              MOVE WKA-SORTJ  TO WSV3-SORTJ.
       M211-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : EDIT WKBFILE                                       *
      *----------------------------------------------------------------*
       M212-EDIT-WKB.
           MOVE SPACE       TO WKC-R.
           INITIALIZE          WKC-R.
           MOVE WSV3-NRKNO  TO WKC-NRKNO.
           MOVE WSV3-YTKBN  TO WKC-YTKBN.
           MOVE WSV3-YSYBN  TO WKC-YSYBN.
           MOVE WSV3-YUSBNO TO WKC-YUSBNO.
           MOVE WSV3-YUSNO  TO WKC-YUSNO.
           MOVE WSV3-YUSCKK TO WKC-YUSCKK.
           MOVE WSV3-YUSCD  TO WKC-YUSCD.
           MOVE WSV3-YUNCD  TO WKC-YUNCD.
           MOVE WKB-YSALL   TO WKC-YSALL.
           MOVE WKB-INFLG   TO WKC-INFLG.
           MOVE WKB-YSTPG   TO WKC-YSTPG.
           MOVE WKB-PAGEN   TO WKC-PAGEN.
           MOVE WKB-SESCD   TO WKC-SESCD.
           MOVE WKB-SQNCD   TO WKC-SQNCD.
           MOVE WKB-LPDAT   TO WKC-LPDAT.
           MOVE WKB-PGMSU   TO WKC-PGMSU.
           MOVE WSV3-SORTJ  TO WKC-SORTJ.
           WRITE WKC-REC  FROM WKC-R.
       M212-L01.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE  WIN1-KEY TO WSV1-KEY.
           PERFORM M211-EDIT-WKA THRU M211-EXIT
             UNTIL (WIN1-KEY NOT = WSV1-KEY).
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           DISPLAY 'R VBKA5100 SEIKYU-KAGAMI NASI ! = '
           WIN2-KEY
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5100 END                                           *
      *----------------------------------------------------------------*
