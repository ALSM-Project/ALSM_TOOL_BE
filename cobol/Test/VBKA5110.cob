./ ADD NAME=VBKA5110  01.06 20150805 20190708 161328 00819 00694 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5110'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : CS YOU SEIKYUSYO DATA         *
      *                                  FURIWAKE                      *
      *                                                                *
      *                 * INPUT FILE   : ICHIRANSEI SEIKYU SYO DATA    *
      *                                                          (DWKA)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA   (DWKC)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA         *
      *                                  ( NAKANO )              (DWKD)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA         *
      *                                  ( OKINAWA )             (DWKE)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA         *
      *                                  ( IWATE )               (DWKF)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA         *
      *                                  ( SANKI )               (DWKG)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA         *
      *                                  ( ASTIS )               (DWKH)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA         *
      *                                  ( AISEIKAN )            (DWKI)*
      *                 * OUTPUT FILE  : CS YOU SEIKYUSYO DATA         *
      *                                  ( SYOYAKU )             (DWKJ)*
      *                                                                *
      *              <> AUTHOR         : CSK-S.OKA                     *
      *              <> DATE           : 03/08/04                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) C-258              ÍÝº³   : 05/10/19  CSK-Y.MATSUOKA      *
      * (02) K-1555         ±½Ã¨½Â²¶   : 14/06/10  CSK-K.KOYAMA        *
      * (03) K-1780             ÍÝº³   : 15/08/05  CSK-Y.YOSHIGA       *
      * (04) K-1759             ÍÝº³   : 15/08/31  CSK-N.EGUCHI        *
      * (05) K-1771             ÍÝº³   : 15/12/04  CSK-N.EGUCHI        *
      * (06) K-2163             ÍÝº³   : 19/07/08  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
           SELECT WKFFILE ASSIGN    TO S-DWKF.
           SELECT WKGFILE ASSIGN    TO S-DWKG.
           SELECT WKHFILE ASSIGN    TO S-DWKH.
           SELECT WKIFILE ASSIGN    TO S-DWKI.
           SELECT WKJFILE ASSIGN    TO S-DWKJ.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(250).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(250).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(250).
       FD  WKFFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKF-REC.
           02 FILLER                        PIC  X(250).
       FD  WKGFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKG-REC.
           02 FILLER                        PIC  X(250).
       FD  WKHFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKH-REC.
           02 FILLER                        PIC  X(250).
       FD  WKIFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKI-REC.
           02 FILLER                        PIC  X(250).
       FD  WKJFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKJ-REC.
           02 FILLER                        PIC  X(250).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKC-R.
           COPY VFILSKA SUPPRESS.
       01  WKD-R.
           COPY VFILSKA SUPPRESS.
       01  WKE-R.
           COPY VFILSKA SUPPRESS.
       01  WKF-R.
           COPY VFILSKA SUPPRESS.
       01  WKG-R.
           COPY VFILSKA SUPPRESS.
       01  WKH-R.
           COPY VFILSKA SUPPRESS.
       01  WKI-R.
           COPY VFILSKA SUPPRESS.
       01  WKJ-R.
           COPY VFILSKA SUPPRESS.
       01  WKA-R.
           02 WKA-CNTRL.
              03 WKA-KAINO                  PIC  X(02).
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
           02 WKA-DATA03  REDEFINES  WKA-LPDAT.
              03 WKA-GNO03                  PIC  9(02).
              03 WKA-RKB03                  PIC  X(02).
              03 WKA-MESAI3.
                 04 WKA-YUADD.
                    05 WKA-YUAD1            PIC  X(40).
                    05 WKA-YUAD2            PIC  X(30).
                    05 WKA-YUADB            PIC  X(30).
                 04 FILLER                  PIC  X(115).
           02 WKA-DATA04  REDEFINES  WKA-LPDAT.
              03 WKA-GNO04                  PIC  9(02).
              03 WKA-RKB04                  PIC  X(02).
              03 WKA-MESAI4.
                 04 WKA-TNMJO4              PIC  X(40).
                 04 WKA-TNMGE4              PIC  X(40).
                 04 WKA-NOSKA4              PIC  X(40).
                 04 WKA-YSBN04              PIC  X(03).
                 04 WKA-YSSN04              PIC  X(03).
                 04 WKA-YSCK04              PIC  X(05).
                 04 WKA-SMEYMD.
                    05 WKA-SIMYY            PIC  9(04).
                    05 WKA-SIMMM            PIC  9(02).
                    05 WKA-SIMDD            PIC  9(02).
                 04 FILLER                  PIC  X(76).
           02 WKA-DATA05  REDEFINES  WKA-LPDAT.
              03 WKA-GNO05                  PIC  9(02).
              03 WKA-RKB05                  PIC  X(02).
              03 WKA-MESAI5.
                 04 WKA-HJPTN5              PIC  X(01).
                 04 WKA-MIDAK5              PIC  X(08).
                 04 WKA-TGNME5  OCCURS  3.
                    05 WKA-GSKAJ5           PIC  X(30).
                    05 WKA-KOZKJ5           PIC  X(04).
                    05 WKA-KZABG5           PIC  X(07).
                 04 WKA-BNKSU5              PIC  9(01).
                 04 FILLER                  PIC  X(82).
           02 WKA-DATA06  REDEFINES  WKA-LPDAT.
              03 WKA-GNO06                  PIC  9(02).
              03 WKA-RKB06                  PIC  X(02).
              03 WKA-MESAI6.
                 04 WKA-STNMJ6              PIC  X(18).
                 04 WKA-STYBN6              PIC  X(07).
                 04 WKA-STTEL6              PIC  X(13).
                 04 WKA-NSTJJ6.
                    05 WKA-NSTJ16           PIC  X(36).
                    05 WKA-NSTJ26           PIC  X(36).
                 04 FILLER                  PIC  X(105).
           02 WKA-DATA07  REDEFINES  WKA-LPDAT.
              03 WKA-GNO07                  PIC  9(02).
              03 WKA-RKB07                  PIC  X(02).
              03 WKA-MESAI7.
                 04 WKA-ZENGT7.
                    05 WKA-ZSKDK7           PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK7           PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB7           PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK7           PIC S9(13)  COMP-3.
                 04 WKA-TOGTU7.
                    05 WKA-TKIDK7           PIC S9(13)  COMP-3.
                    05 WKA-TSKDK7           PIC S9(13)  COMP-3.
                    05 WKA-MSKSK7           PIC S9(13)  COMP-3.
                    05 WKA-TURZN7           PIC S9(13)  COMP-3.
                 04 WKA-SHZEI7              PIC S9(13)  COMP-3.
                 04 WKA-TKAIG7              PIC S9(13)  COMP-3.
                 04 WKA-SHZSK1              PIC  X(01).
                 04 FILLER                  PIC  X(146).
           02 WKA-DATA08  REDEFINES  WKA-LPDAT.
              03 WKA-GNO08                  PIC  9(02).
              03 WKA-RKB08                  PIC  X(02).
              03 WKA-MESAI8.
                 04 WKA-ZRTBG8.
                    05 WKA-MIDA18           PIC  X(22).
                    05 WKA-OKAIG8           PIC S9(13)  COMP-3.
                    05 WKA-MIDA28           PIC  X(08).
                    05 WKA-SHZEI8           PIC S9(13)  COMP-3.
                    05 WKA-ZEIRT8           PIC S9(02)V9 COMP-3.
                    05 WKA-KGZKB8           PIC  X(01).
                 04 FILLER                  PIC  X(168).
           02 WKA-DATA09  REDEFINES  WKA-LPDAT.
              03 WKA-GNO09                  PIC  9(02).
              03 WKA-RKB09                  PIC  X(02).
              03 WKA-MESAI9.
                 04 WKA-ZENGT9.
                    05 WKA-ZSKDK9           PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK9           PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB9           PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK9           PIC S9(13)  COMP-3.
                 04 WKA-TOGTU9.
                    05 WKA-TKIDK9           PIC S9(13)  COMP-3.
                    05 WKA-TSKDK9           PIC S9(13)  COMP-3.
                    05 WKA-MSKSK9           PIC S9(13)  COMP-3.
                    05 WKA-TURZN9           PIC S9(13)  COMP-3.
                 04 WKA-SHZEI9              PIC S9(13)  COMP-3.
                 04 WKA-TKAIG9              PIC S9(13)  COMP-3.
                 04 WKA-TNMJO9              PIC  X(40).
                 04 WKA-TNMGE9              PIC  X(40).
                 04 WKA-NOSKA9              PIC  X(30).
                 04 WKA-HIFLG9              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKA-DATA10  REDEFINES  WKA-LPDAT.
              03 WKA-GNO10                  PIC  9(02).
              03 WKA-RKB10                  PIC  X(02).
              03 WKA-MESAI10.
                 04 WKA-SEADD.
                    05 WKA-SEAD1            PIC  X(40).
                    05 WKA-SEAD1            PIC  X(30).
                    05 WKA-SEADB            PIC  X(30).
                 04 WKA-SEYBN.
                    05 WKA-SEYB1            PIC  X(03).
                    05 WKA-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
           02 WKA-DATA20  REDEFINES  WKA-LPDAT.
              03 WKA-GNO20                  PIC  9(02).
              03 WKA-RKB20                  PIC  X(02).
              03 WKA-MESAI20.
                 04 WKA-TNMJO               PIC  X(40).
                 04 WKA-TNMGE               PIC  X(40).
                 04 WKA-NOSKA               PIC  X(40).
                 04 WKA-SQBNO               PIC  X(03).
                 04 WKA-SQSTN               PIC  X(03).
                 04 WKA-SQCKK               PIC  X(05).
                 04 WKA-SIMEY.
                    05 WKA-SIMYY            PIC  9(04).
                    05 WKA-SIMMM            PIC  9(02).
                    05 WKA-SIMDD            PIC  9(02).
                 04 WKA-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKA-DATA30  REDEFINES  WKA-LPDAT.
              03 WKA-GNO30                  PIC  9(02).
              03 WKA-RKB30                  PIC  X(02).
              03 WKA-MESAI30.
                 04 WKA-HJPTN1              PIC  X(01).
                 04 WKA-MIDAK               PIC  X(08).
                 04 WKA-TGNME  OCCURS  3.
                    05 WKA-GSKAJ            PIC  X(30).
                    05 WKA-KOZKJ            PIC  X(04).
                    05 WKA-KZABG            PIC  X(07).
                 04 WKA-BNKSU               PIC  9(01).
                 04 FILLER                  PIC  X(82).
              03 FILLER  REDEFINES  WKA-MESAI30.
                 04 WKA-HJPTN2              PIC  X(01).
                 04 WKA-HYOJI1              PIC  X(22).
                 04 WKA-HYOJI2              PIC  X(16).
                 04 WKA-HYOJI3              PIC  X(26).
                 04 WKA-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 FILLER  REDEFINES  WKA-MESAI30.
                 04 WKA-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKA-DATA40  REDEFINES  WKA-LPDAT.
              03 WKA-GNO40                  PIC  9(02).
              03 WKA-RKB40                  PIC  X(02).
              03 WKA-MESAI40.
                 04 WKA-STNMJ4              PIC  X(18).
                 04 WKA-STYBN               PIC  X(07).
                 04 WKA-STTEL               PIC  X(13).
                 04 WKA-NSTJJ.
                    05 WKA-NSTJ1            PIC  X(36).
                    05 WKA-NSTJ2            PIC  X(36).
                 04 WKA-HJTRB               PIC  X(14).
                 04 FILLER                  PIC  X(91).
           02 WKA-DATA50  REDEFINES  WKA-LPDAT.
              03 WKA-GNO50                  PIC  9(02).
              03 WKA-RKB50                  PIC  X(02).
              03 WKA-MESAI50.
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
                 04 WKA-SHZSK2              PIC  X(01).
                 04 FILLER                  PIC  X(145).
           02 WKA-DATA60  REDEFINES  WKA-LPDAT.
              03 WKA-GNO60                  PIC  9(02).
              03 WKA-RKB60                  PIC  X(02).
              03 WKA-MESAI60.
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
                 04 WKA-ZEIRT1              PIC S9(02)V9 COMP-3.
                 04 WKA-DENGK1              PIC S9(09)  COMP-3.
                 04 WKA-KGZRH1              PIC  X(02).
           02 WKA-DATA6X  REDEFINES  WKA-LPDAT.
              03 WKA-GNO6X                  PIC  9(02).
              03 WKA-RKB6X                  PIC  X(02).
              03 WKA-MESAIX.
                 04 WKA-DNPH1               PIC  9(04).
                 04 WKA-MIDA1               PIC  X(50).
                 04 WKA-MIDA2               PIC  X(50).
                 04 WKA-DNPKG               PIC S9(11)  COMP-3.
                 04 WKA-DSHZ4               PIC S9(11)  COMP-3.
                 04 WKA-DENGK2              PIC S9(11)  COMP-3.
                 04 WKA-ZEIRT2              PIC S9(02)V9 COMP-3.
                 04 WKA-KGZRH2              PIC  X(02).
                 04 FILLER                  PIC  X(89).
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
                 04 WKA-TKNGK               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(77).
           02 WKA-DATA66  REDEFINES  WKA-LPDAT.
              03 WKA-GNO66                  PIC  9(02).
              03 WKA-RKB66                  PIC  X(02).
              03 WKA-MESAI66.
                 04 WKA-MIDAU2              PIC  X(50).
                 04 WKA-MIDAD2              PIC  X(50).
                 04 WKA-ZOKKD               PIC S9(11)  COMP-3.
                 04 WKA-ZSZEI               PIC S9(11)  COMP-3.
                 04 WKA-ZOKKG               PIC S9(11)  COMP-3.
                 04 WKA-ZEIRT3              PIC S9(02)V9 COMP-3.
                 04 WKA-KGZKB               PIC  X(01).
                 04 FILLER                  PIC  X(94).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  EDIT-AREA.
           02 WED1-R.
              03 WED1-CNTRL                 PIC  X(56).
              03 WED1-GYOBG                 PIC  9(02).
              03 WED1-RECKB                 PIC  X(02).
              03 WED1-MEISAI                PIC  X(188).
              03 WED1-DAT03 REDEFINES WED1-MEISAI.
                 04 WED1-YUADD              PIC  X(100).
                 04 FILLER                  PIC  X(88).
              03 WED1-DAT04 REDEFINES WED1-MEISAI.
                 04 WED1-TKMJO              PIC  X(40).
                 04 WED1-TKMGE              PIC  X(40).
                 04 WED1-NSKNJ              PIC  X(40).
                 04 WED1-YUSBN              PIC  X(03).
                 04 WED1-YUSTN              PIC  X(03).
                 04 WED1-YUTKG              PIC  X(05).
                 04 WED1-SIMYMD             PIC  X(08).
                 04 FILLER                  PIC  X(49).
              03 WED1-DAT05 REDEFINES WED1-MEISAI.
                 04 WED1-HJPTN              PIC  X(01).
                 04 WED1-MIDAK5             PIC  X(08).
                 04 WED1-TGNME5 OCCURS  3.
                    05 WED1-GSKAJ5          PIC  X(30).
                    05 WED1-KOZKJ5          PIC  X(04).
                    05 WED1-KZABG5          PIC  X(07).
                 04 WED1-BNKSU5             PIC  9(01).
                 04 FILLER                  PIC  X(55).
              03 WED1-DAT06 REDEFINES WED1-MEISAI.
                 04 WED1-STNMJ6             PIC  X(18).
                 04 WED1-STYBN6             PIC  X(07).
                 04 WED1-STTEL6             PIC  X(13).
                 04 WED1-NSTJJ6             PIC  X(72).
                 04 FILLER                  PIC  X(78).
              03 WED1-DAT07 REDEFINES WED1-MEISAI.
                 04 WED1-ZENGT7             PIC  X(26).
                 04 WED1-TOGTU7             PIC  X(28).
                 04 WED1-SHZEI7             PIC S9(13)  COMP-3.
                 04 WED1-TKAIG7             PIC S9(13)  COMP-3.
                 04 WED1-SHZSK1             PIC  X(01).
                 04 FILLER                  PIC  X(119).
              03 WED1-DAT08 REDEFINES WED1-MEISAI.
                 04 WED1-ZRTBG.
                    05 WED1-MIDA18          PIC  X(22).
                    05 WED1-OKAIG8          PIC S9(13)  COMP-3.
                    05 WED1-MIDA28          PIC  X(08).
                    05 WED1-SHZEI8          PIC S9(13)  COMP-3.
                    05 WED1-ZEIRT8          PIC S9(02)V9 COMP-3.
                    05 WED1-KGZKB8          PIC  X(01).
                 04 FILLER                  PIC  X(141).
              03 WED1-DAT09 REDEFINES WED1-MEISAI.
                 04 WED1-ZENGT9             PIC  X(26).
                 04 WED1-TOGTU9             PIC  X(28).
                 04 WED1-SHZEI9             PIC S9(13)  COMP-3.
                 04 WED1-TKAIG9             PIC S9(13)  COMP-3.
                 04 WED1-SGSM1              PIC  X(40).
                 04 WED1-SGSM2              PIC  X(40).
                 04 WED1-SGNME              PIC  X(30).
                 04 WED1-HIFLG              PIC  X(01).
                 04 FILLER                  PIC  X(09).
              03 WED1-DAT10 REDEFINES WED1-MEISAI.
                 04 WED1-SEADD              PIC  X(100).
                 04 WED1-SEYBN              PIC  X(07).
                 04 FILLER                  PIC  X(81).
              03 WED1-DAT20 REDEFINES WED1-MEISAI.
                 04 WED1-TNMJO              PIC  X(40).
                 04 WED1-TNMGE              PIC  X(40).
                 04 WED1-NOSKA              PIC  X(40).
                 04 WED1-SQBNO              PIC  X(03).
                 04 WED1-SQSTN              PIC  X(03).
                 04 WED1-SQCKK              PIC  X(05).
                 04 WED1-SIMEY              PIC  X(08).
                 04 FILLER                  PIC  X(49).
              03 WED1-DAT30 REDEFINES WED1-MEISAI.
                 04 WED1-HPTN1              PIC  X(01).
                 04 WED1-MIDAK              PIC  X(08).
                 04 WED1-TGNME OCCURS  3.
                    05 WED1-GSKAJ           PIC  X(30).
                    05 WED1-KOZKJ           PIC  X(04).
                    05 WED1-KZABG           PIC  X(07).
                 04 WED1-BNKSU              PIC  9(01).
                 04 FILLER                  PIC  X(55).
              03 FILLER REDEFINES WED1-DAT30.
                 04 WED1-HPTN2              PIC  X(01).
                 04 WED1-HYOJI1             PIC  X(22).
                 04 WED1-HYOJI2             PIC  X(16).
                 04 WED1-HYOJI3             PIC  X(26).
                 04 WED1-HYOJI4             PIC  X(20).
                 04 FILLER                  PIC  X(103).
              03 FILLER REDEFINES WED1-DAT30.
                 04 WED1-HPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(187).
              03 WED1-DAT40 REDEFINES WED1-MEISAI.
                 04 WED1-STNMJ4             PIC  X(18).
                 04 WED1-STYBN              PIC  X(07).
                 04 WED1-STTEL              PIC  X(13).
                 04 WED1-NSTJJ              PIC  X(72).
                 04 WED1-HJTRB              PIC  X(14).
                 04 FILLER                  PIC  X(64).
              03 WED1-DAT50 REDEFINES WED1-MEISAI.
                 04 WED1-ZENGT              PIC  X(26).
                 04 WED1-TOGTU              PIC  X(28).
                 04 WED1-ZNITK              PIC  X(01).
                 04 WED1-SHZEI              PIC S9(13)  COMP-3.
                 04 WED1-TKAIG              PIC S9(13)  COMP-3.
                 04 WED1-SHZSK2             PIC  X(01).
                 04 FILLER                  PIC  X(118).
             03 WED1-DAT60 REDEFINES WED1-MEISAI.
                 04 WED1-DNHI1              PIC  X(04).
                 04 WED1-DNBN1              PIC  X(07).
                 04 WED1-TRKBN              PIC  X(08).
                 04 WED1-DNKIN              PIC S9(09)  COMP-3.
                 04 WED1-SYOME              PIC  X(90).
                 04 WED1-SSURYO             PIC S9(05)  COMP-3.
                 04 WED1-STANKA             PIC S9(09)  COMP-3.
                 04 WED1-SKNGK              PIC S9(09)  COMP-3.
                 04 WED1-SMSHZ              PIC S9(09)  COMP-3.
                 04 WED1-SBIKO              PIC  X(13).
                 04 WED1-DSHZ3              PIC S9(09)  COMP-3.
                 04 WED1-DSHZFG             PIC  X(01).
                 04 WED1-MSHZUM             PIC  X(01).
                 04 WED1-ZEIRT1             PIC S9(02)V9 COMP-3.
                 04 WED1-DENGK1             PIC S9(09)  COMP-3.
                 04 WED1-KGZRH1             PIC  X(02).
                 04 FILLER                  PIC  X(27).
              03 WED1-DAT6X REDEFINES WED1-MEISAI.
                 04 WED1-DNPH1              PIC  9(04).
                 04 WED1-MIDA1              PIC  X(50).
                 04 WED1-MIDA2              PIC  X(50).
                 04 WED1-DNPKG              PIC S9(11)  COMP-3.
                 04 WED1-DSHZ4              PIC S9(11)  COMP-3.
                 04 WED1-DENGK2             PIC S9(11)  COMP-3.
                 04 WED1-ZEIRT2             PIC S9(02)V9 COMP-3.
                 04 WED1-KGZRH2             PIC  X(02).
                 04 FILLER                  PIC  X(62).
              03 WED1-DAT65 REDEFINES WED1-MEISAI.
                 04 WED1-MIDAU              PIC  X(50).
                 04 WED1-MIDAD              PIC  X(50).
                 04 WED1-CDEDT              PIC  X(14).
                 04 WED1-TNGOK              PIC S9(09)  COMP-3.
                 04 WED1-TSHZK              PIC S9(09)  COMP-3.
                 04 WED1-MTOKC              PIC  X(07).
                 04 WED1-MNSCD              PIC  X(02).
                 04 WED1-TKNGK              PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(50).
              03 WED1-DAT66 REDEFINES WED1-MEISAI.
                 04 WED1-MIDAU2             PIC  X(50).
                 04 WED1-MIDAD2             PIC  X(50).
                 04 WED1-ZOKKD              PIC S9(11)  COMP-3.
                 04 WED1-ZSZEI              PIC S9(11)  COMP-3.
                 04 WED1-ZOKKG              PIC S9(11)  COMP-3.
                 04 WED1-ZEIRT3             PIC S9(02)V9 COMP-3.
                 04 WED1-KGZKB              PIC  X(01).
                 04 FILLER                  PIC  X(67).
              03 WED1-PGMSU                 PIC  X(02).
           02 I                             PIC  9(03)  COMP.
           02 WED1-TBL.
              03 WED1-KAINO                 PIC  X(02).
              03 WED1-KENSU                 PIC  ZZZZ,ZZ9.
       01  TABLE-AREA.
           02 WTB1-TBL OCCURS 100.
              03 WTB1-KAINO                 PIC  X(02).
              03 WTB1-KENSU                 PIC  9(07).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL  WKA-R = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           MOVE SPACE  TO TABLE-AREA.
           INITIALIZE  TABLE-AREA.
           OPEN INPUT  WKAFILE
                OUTPUT WKCFILE
                       WKDFILE
                       WKEFILE
                       WKFFILE
                       WKGFILE
                       WKHFILE
                       WKIFILE
                       WKJFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WKA-R.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           EVALUATE WKA-KAINO
      ***  KAISYA-KOYU  ***
              WHEN '01'   PERFORM M21-EDIT  THRU M21-EXIT
                          IF WKA-YUSBNO = '011' OR '016'
                             MOVE WED1-R   TO WKI-R
                             WRITE WKI-REC FROM WKI-R
                          ELSE
                             MOVE WED1-R   TO WKC-R
                             WRITE WKC-REC FROM WKC-R
                          END-IF,
              WHEN '35'   PERFORM M21-EDIT  THRU M21-EXIT
                          MOVE WED1-R   TO WKD-R
                          WRITE WKD-REC FROM WKD-R,
              WHEN '11'   PERFORM M21-EDIT  THRU M21-EXIT
                          MOVE WED1-R   TO WKE-R
                          WRITE WKE-REC FROM WKE-R,
              WHEN '22'   PERFORM M21-EDIT  THRU M21-EXIT
                          MOVE WED1-R   TO WKF-R
                          WRITE WKF-REC FROM WKF-R,
              WHEN '37'   PERFORM M21-EDIT  THRU M21-EXIT
                          MOVE WED1-R   TO WKG-R
                          WRITE WKG-REC FROM WKG-R,
              WHEN '58'   PERFORM M21-EDIT  THRU M21-EXIT
                          MOVE WED1-R   TO WKH-R
                          WRITE WKH-REC FROM WKH-R,
              WHEN '82'   PERFORM M21-EDIT  THRU M21-EXIT
                          MOVE WED1-R   TO WKJ-R
                          WRITE WKJ-REC FROM WKJ-R,
              WHEN OTHER  PERFORM M22-COUNT THRU M22-EXIT
           END-EVALUATE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : EDIT                                                 *
      *----------------------------------------------------------------*
       M21-EDIT.
           MOVE SPACE  TO WED1-R.
           INITIALIZE  WED1-R.
           MOVE WKA-CNTRL  TO WED1-CNTRL.
           MOVE WKA-PGMSU  TO WED1-PGMSU.
           EVALUATE WKA-RKB03
               WHEN '03'
                    MOVE WKA-GNO03  TO WED1-GYOBG
                    MOVE WKA-RKB03  TO WED1-RECKB
                    MOVE WKA-YUADD  TO WED1-YUADD,
               WHEN '04'
                    MOVE WKA-GNO04  TO WED1-GYOBG
                    MOVE WKA-RKB04  TO WED1-RECKB
                    MOVE WKA-TNMJO4 TO WED1-TKMJO
                    MOVE WKA-TNMGE4 TO WED1-TKMGE
                    MOVE WKA-NOSKA4 TO WED1-NSKNJ
                    MOVE WKA-YSBN04 TO WED1-YUSBN
                    MOVE WKA-YSSN04 TO WED1-YUSTN
                    MOVE WKA-YSCK04 TO WED1-YUTKG
                    MOVE WKA-SMEYMD TO WED1-SIMYMD,
               WHEN '05'
                    MOVE WKA-GNO05  TO WED1-GYOBG
                    MOVE WKA-RKB05  TO WED1-RECKB
                    MOVE WKA-HJPTN5 TO WED1-HJPTN
                    MOVE WKA-MIDAK5 TO WED1-MIDAK5
                    MOVE WKA-TGNME5(1) TO WED1-TGNME5(1)
                    MOVE WKA-TGNME5(2) TO WED1-TGNME5(2)
                    MOVE WKA-TGNME5(3) TO WED1-TGNME5(3)
                    MOVE WKA-BNKSU5 TO WED1-BNKSU5,
               WHEN '06'
                    MOVE WKA-GNO06  TO WED1-GYOBG
                    MOVE WKA-RKB06  TO WED1-RECKB
                    MOVE WKA-STNMJ6 TO WED1-STNMJ6
                    MOVE WKA-STYBN6 TO WED1-STYBN6
                    MOVE WKA-STTEL6 TO WED1-STTEL6
                    MOVE WKA-NSTJJ6 TO WED1-NSTJJ6,
               WHEN '07'
                    MOVE WKA-GNO07  TO WED1-GYOBG
                    MOVE WKA-RKB07  TO WED1-RECKB
                    MOVE WKA-ZENGT7 TO WED1-ZENGT7
                    MOVE WKA-TOGTU7 TO WED1-TOGTU7
                    MOVE WKA-SHZEI7 TO WED1-SHZEI7
                    MOVE WKA-TKAIG7 TO WED1-TKAIG7
                    MOVE WKA-SHZSK1 TO WED1-SHZSK1,
               WHEN '08'
                    MOVE WKA-GNO08  TO WED1-GYOBG
                    MOVE WKA-RKB08  TO WED1-RECKB
                    MOVE WKA-MIDA18 TO WED1-MIDA18
                    MOVE WKA-OKAIG8 TO WED1-OKAIG8
                    MOVE WKA-MIDA28 TO WED1-MIDA28
                    MOVE WKA-SHZEI8 TO WED1-SHZEI8
                    MOVE WKA-ZEIRT8 TO WED1-ZEIRT8
                    MOVE WKA-KGZKB8 TO WED1-KGZKB8,
               WHEN '09'
                    MOVE WKA-GNO09  TO WED1-GYOBG
                    MOVE WKA-RKB09  TO WED1-RECKB
                    MOVE WKA-ZENGT9 TO WED1-ZENGT9
                    MOVE WKA-TOGTU9 TO WED1-TOGTU9
                    MOVE WKA-SHZEI9 TO WED1-SHZEI9
                    MOVE WKA-TKAIG9 TO WED1-TKAIG9
                    MOVE WKA-TNMJO9 TO WED1-SGSM1
                    MOVE WKA-TNMGE9 TO WED1-SGSM2
                    MOVE WKA-NOSKA9 TO WED1-SGNME
                    MOVE WKA-HIFLG9 TO WED1-HIFLG,
               WHEN '10'
                    MOVE WKA-GNO10  TO WED1-GYOBG
                    MOVE WKA-RKB10  TO WED1-RECKB
                    MOVE WKA-SEADD  TO WED1-SEADD
                    MOVE WKA-SEYBN  TO WED1-SEYBN,
               WHEN '20'
                    MOVE WKA-GNO20  TO WED1-GYOBG
                    MOVE WKA-RKB20  TO WED1-RECKB
                    MOVE WKA-TNMJO  TO WED1-TNMJO
                    MOVE WKA-TNMGE  TO WED1-TNMGE
                    MOVE WKA-NOSKA  TO WED1-NOSKA
                    MOVE WKA-SQBNO  TO WED1-SQBNO
                    MOVE WKA-SQSTN  TO WED1-SQSTN
                    MOVE WKA-SQCKK  TO WED1-SQCKK
                    MOVE WKA-SIMEY  TO WED1-SIMEY,
               WHEN '30'
                    MOVE WKA-GNO30  TO WED1-GYOBG
                    MOVE WKA-RKB30  TO WED1-RECKB
                    MOVE WKA-MESAI30 TO WED1-DAT30,
               WHEN '40'
                    MOVE WKA-GNO40  TO WED1-GYOBG
                    MOVE WKA-RKB40  TO WED1-RECKB
                    MOVE WKA-STNMJ4 TO WED1-STNMJ4
                    MOVE WKA-STYBN  TO WED1-STYBN
                    MOVE WKA-STTEL  TO WED1-STTEL
                    MOVE WKA-NSTJJ  TO WED1-NSTJJ
                    MOVE WKA-HJTRB  TO WED1-HJTRB,
               WHEN '50'
                    MOVE WKA-GNO50  TO WED1-GYOBG
                    MOVE WKA-RKB50  TO WED1-RECKB
                    MOVE WKA-ZENGT  TO WED1-ZENGT
                    MOVE WKA-TOGTU  TO WED1-TOGTU
                    MOVE WKA-ZNITK  TO WED1-ZNITK
                    MOVE WKA-SHZEI  TO WED1-SHZEI
                    MOVE WKA-TKAIG  TO WED1-TKAIG
                    MOVE WKA-SHZSK2 TO WED1-SHZSK2,
               WHEN '60'
                    MOVE WKA-GNO60  TO WED1-GYOBG
                    MOVE WKA-RKB60  TO WED1-RECKB
                    MOVE WKA-DNHI1  TO WED1-DNHI1
                    MOVE WKA-DNBN1  TO WED1-DNBN1
                    MOVE WKA-TRKBN  TO WED1-TRKBN
                    MOVE WKA-DNKIN  TO WED1-DNKIN
                    MOVE WKA-SYOME  TO WED1-SYOME
                    MOVE WKA-SSURYO TO WED1-SSURYO
                    MOVE WKA-STANKA TO WED1-STANKA
                    MOVE WKA-SKNGK  TO WED1-SKNGK
                    MOVE WKA-SMSHZ  TO WED1-SMSHZ
                    MOVE WKA-SBIKO  TO WED1-SBIKO
                    MOVE WKA-DSHZ3  TO WED1-DSHZ3
                    MOVE WKA-DSHZFG TO WED1-DSHZFG
                    MOVE WKA-MSHZUM TO WED1-MSHZUM
                    MOVE WKA-ZEIRT1 TO WED1-ZEIRT1
                    MOVE WKA-DENGK1 TO WED1-DENGK1
                    MOVE WKA-KGZRH1 TO WED1-KGZRH1,
               WHEN '63'
               WHEN '64'
                    MOVE WKA-GNO6X  TO WED1-GYOBG
                    MOVE WKA-RKB6X  TO WED1-RECKB
                    MOVE WKA-DNPH1  TO WED1-DNPH1
                    MOVE WKA-MIDA1  TO WED1-MIDA1
                    MOVE WKA-MIDA2  TO WED1-MIDA2
                    MOVE WKA-DNPKG  TO WED1-DNPKG
                    MOVE WKA-DSHZ4  TO WED1-DSHZ4
                    MOVE WKA-DENGK2 TO WED1-DENGK2
                    MOVE WKA-ZEIRT2 TO WED1-ZEIRT2
                    MOVE WKA-KGZRH2 TO WED1-KGZRH2,
               WHEN '65'
                    MOVE WKA-GNO65  TO WED1-GYOBG
                    MOVE WKA-RKB65  TO WED1-RECKB
                    MOVE WKA-MIDAU  TO WED1-MIDAU
                    MOVE WKA-MIDAD  TO WED1-MIDAD
                    MOVE WKA-CDEDT  TO WED1-CDEDT
                    MOVE WKA-GOKGK  TO WED1-TNGOK
                    MOVE WKA-ZEGAK  TO WED1-TSHZK
                    MOVE WKA-MTOKC  TO WED1-MTOKC
                    MOVE WKA-MNSCD  TO WED1-MNSCD
                    MOVE WKA-TKNGK  TO WED1-TKNGK,
               WHEN '66'
                    MOVE WKA-GNO66  TO WED1-GYOBG
                    MOVE WKA-RKB66  TO WED1-RECKB
                    MOVE WKA-MIDAU2 TO WED1-MIDAU2
                    MOVE WKA-MIDAD2 TO WED1-MIDAD2
                    MOVE WKA-ZOKKD  TO WED1-ZOKKD
                    MOVE WKA-ZSZEI  TO WED1-ZSZEI
                    MOVE WKA-ZOKKG  TO WED1-ZOKKG
                    MOVE WKA-ZEIRT3 TO WED1-ZEIRT3
                    MOVE WKA-KGZKB  TO WED1-KGZKB
           END-EVALUATE.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : COUNT                                                *
      *----------------------------------------------------------------*
       M22-COUNT.
           MOVE 1  TO I.
           PERFORM UNTIL (WTB1-KAINO(I) = WKA-KAINO OR SPACE)
                         OR I NOT < 100
              ADD 1  TO I
           END-PERFORM.
           IF WTB1-KAINO(I) = SPACE
              MOVE  WKA-KAINO  TO WTB1-KAINO(I)
              MOVE  1          TO WTB1-KENSU(I)
           ELSE
              IF WTB1-KAINO(I) = WKA-KAINO
                 ADD   1          TO WTB1-KENSU(I)
              END-IF
           END-IF.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           MOVE 1  TO I.
           PERFORM UNTIL WTB1-KAINO(I) = SPACE OR I > 100
              MOVE WTB1-KAINO(I) TO WED1-KAINO
              MOVE WTB1-KENSU(I) TO WED1-KENSU
              DISPLAY 'M VBKA5110 TAISYOU-GAI KAISYA ARI ! = '
              WED1-TBL UPON CONSOLE
              ADD 1 TO I
           END-PERFORM.
           CLOSE WKAFILE
                 WKCFILE
                 WKDFILE
                 WKEFILE
                 WKFFILE
                 WKGFILE
                 WKHFILE
                 WKIFILE
                 WKJFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5110 END                                           *
      *----------------------------------------------------------------*
