./ ADD NAME=VBKA5080  01.17 20150805 20190828 131826 00695 00504 MCSK004
       ID  DIVISION.
       PROGRAM-ID. 'VBKA5080'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : YUSOTANNI PAGE SETTEI         *
      *                                                                *
      *                 * INPUT FILE   : ITIRANSEI SEKYUSYO DATA (DWKA)*
      *                                : KOKYAKU MASTER        (SI.KMS)*
      *                 * OUTPUT FILE  : ITIRANSEI SEKYUSYO DATA (DWKB)*
      *                 * OUTPUT FILE  : ITIRANSEI SEKYOSYO            *
      *                                                PAGE DATA (DWKC)*
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/08/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1780             ÍÝº³   : 15/08/27  CSK-Y.YOSHIGA       *
      * (02) H-010  O-10315     ÍÝº³   : 17/11/28  CSK-M.SAWADA        *
      * (03) H-010  O-10341     ÍÝº³   : 18/02/05  CSK-M.SAWADA        *
      * (04) K-02163            ÍÝº³   : 19/07/11  SCSK-A.ISIKAWA      *
      * (04) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
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
           02 FILLER                        PIC  X(21).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKMS END-EXEC.
       01  KMS REDEFINES DCLKMS.
           COPY VTABKMS SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
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
                 04 WKA-SHZSK7              PIC  X(01).
                 04 FILLER                  PIC  X(146).
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
                 04 WKA-HKJBG               PIC  X(14).
                 04 FILLER                  PIC  X(91).
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
                 04 WKA-MSZRT               PIC S9(02)V9 COMP-3.
                 04 WKA-DPGK60              PIC S9(09)   COMP-3.
                 04 WKA-KGZH60              PIC  X(02).
           02 WKA-DATA6X  REDEFINES  WKA-LPDAT.
              03 WKA-GNO6X                  PIC  9(02).
              03 WKA-RKB6X                  PIC  X(02).
              03 WKA-MESAIX.
                 04 WKA-DNPH1               PIC  9(04).
                 04 WKA-MIDA1               PIC  X(50).
                 04 WKA-MIDA2               PIC  X(50).
                 04 WKA-DNPKG               PIC S9(11)  COMP-3.
                 04 WKA-DSHZ4               PIC S9(11)  COMP-3.
                 04 WKA-DPGK6X              PIC S9(11)   COMP-3.
                 04 WKA-ZERT6X              PIC S9(02)V9 COMP-3.
                 04 WKA-KGZH6X              PIC  X(02).
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
                 04 WKA-TNGKK               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(77).
           02 WKA-DAT66   REDEFINES  WKA-LPDAT.
              03 WKA-GNO66                  PIC  9(02).
              03 WKA-RKB66                  PIC  X(02).
              03 WKA-MESI66.
                 04 WKA-MIDAU6              PIC  X(50).
                 04 WKA-MIDAD6              PIC  X(50).
                 04 WKA-ZOKDK               PIC S9(11)   COMP-3.
                 04 WKA-ZSHZI               PIC S9(11)   COMP-3.
                 04 WKA-ZOKGDK              PIC S9(11)   COMP-3.
                 04 WKA-ZEIRT6              PIC S9(02)V9 COMP-3.
                 04 WKA-KGZRT6              PIC  X(01).
                 04 FILLER                  PIC  X(94).

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
           02 WKB-DATA07  REDEFINES  WKB-LPDAT.
              03 WKB-GNO07                  PIC  9(02).
              03 WKB-RKB07                  PIC  X(02).
              03 WKB-MESAI7.
                 04 WKB-ZENGT7.
                    05 WKB-ZSKDK7           PIC S9(13)  COMP-3.
                    05 WKB-ZNKDK7           PIC S9(13)  COMP-3.
                    05 WKB-ZSYNB7           PIC S9(09)  COMP-3.
                    05 WKB-ZKRDK7           PIC S9(13)  COMP-3.
                 04 WKB-TOGTU7.
                    05 WKB-TKIDK7           PIC S9(13)  COMP-3.
                    05 WKB-TSKDK7           PIC S9(13)  COMP-3.
                    05 WKB-MSKSK7           PIC S9(13)  COMP-3.
                    05 WKB-TURZN7           PIC S9(13)  COMP-3.
                 04 WKB-SHZEI7              PIC S9(13)  COMP-3.
                 04 WKB-TKAIG7              PIC S9(13)  COMP-3.
                 04 WKB-SHZSK7              PIC  X(01).
                 04 FILLER                  PIC  X(146).
           02 WKB-DATA10  REDEFINES  WKB-LPDAT.
              03 WKB-GNO10                  PIC  9(02).
              03 WKB-RKB10                  PIC  X(02).
              03 WKB-MESAI1.
                 04 WKB-TJYSO.
                    05 WKB-TJYSA            PIC  X(40).
                    05 WKB-TJYSB            PIC  X(30).
                    05 WKB-TKBLMJ           PIC  X(30).
                 04 WKB-SEYBN.
                    05 WKB-SEYB1            PIC  X(03).
                    05 WKB-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
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
                 04 WKB-HKJBG               PIC  X(14).
                 04 FILLER                  PIC  X(91).
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
                 04 WKB-MSZRT               PIC S9(02)V9 COMP-3.
                 04 WKB-DPGK60              PIC S9(09)   COMP-3.
                 04 WKB-KGZH60              PIC  X(02).
           02 WKB-DATA6X  REDEFINES  WKB-LPDAT.
              03 WKB-GNO6X                  PIC  9(02).
              03 WKB-RKB6X                  PIC  X(02).
              03 WKB-MESAIX.
                 04 WKB-DNPH1               PIC  9(04).
                 04 WKB-MIDA1               PIC  X(50).
                 04 WKB-MIDA2               PIC  X(50).
                 04 WKB-DNPKG               PIC S9(11)  COMP-3.
                 04 WKB-DSHZ4               PIC S9(11)  COMP-3.
                 04 WKB-DPGK6X              PIC S9(11)   COMP-3.
                 04 WKB-ZERT6X              PIC S9(02)V9 COMP-3.
                 04 WKB-KGZH6X              PIC  X(02).
                 04 FILLER                  PIC  X(89).
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
                 04 WKB-TNGKK               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(77).
           02 WKB-DAT66   REDEFINES  WKB-LPDAT.
              03 WKB-GNO66                  PIC  9(02).
              03 WKB-RKB66                  PIC  X(02).
              03 WKB-MESI66.
                 04 WKB-MIDAU6              PIC  X(50).
                 04 WKB-MIDAD6              PIC  X(50).
                 04 WKB-ZOKDK               PIC S9(11)   COMP-3.
                 04 WKB-ZSHZI               PIC S9(11)   COMP-3.
                 04 WKB-ZOKGDK              PIC S9(11)   COMP-3.
                 04 WKB-ZEIRT6              PIC S9(02)V9 COMP-3.
                 04 WKB-KGZRT6              PIC  X(01).
                 04 FILLER                  PIC  X(94).

           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  WKC-R.
           02 WKC-YTKBN                     PIC  X(01).
           02 WKC-YUSCD                     PIC  X(07).
           02 WKC-YUNCD                     PIC  X(02).
           02 WKC-YSALL                     PIC  X(04).
           02 FILLER                        PIC  X(07).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-YTKBN                 PIC  X(01).
              03 WIN1-YUSCD                 PIC  X(07).
              03 WIN1-YUNCD                 PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-YTKBN                 PIC  X(01).
              03 WSV1-YUSCD                 PIC  X(07).
              03 WSV1-YUNCD                 PIC  X(02).
           02 WSV2-KEY.
              03 WSV2-RECKB                 PIC  X(02).
              03 WSV2-YSTPG                 PIC  9(04).
              02 WSV2-YSTPG1                PIC  9(04).
              02 WSV2-YSTPG5                PIC  9(04).
              02 WSV2-YSTPGC                PIC  9(04).
           02 WSV3-KEY                      PIC  X(07).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-PAGE1                    PIC  9(04).
           02 WED1-PAGE2                    PIC  9(04).
           02 WED1-PAGEC                    PIC  ZZ,ZZ9.
       01  COUNT-AREA.
           02 WCT1-PAGEA                    PIC  9(05).
           02 WCT1-PAGEB                    PIC  9(05).
           02 WCT1-PAGEC                    PIC  9(05).
       01  FLAG-AREA.
           03 WFG1-OVER                     PIC  X(01).
       01  SYS-CARD1.
           02 SYS-ACCEPT                    PIC  X(01).
           02 FILLER                        PIC  X(79).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *    LINKAGE SECTION                                             *
      *----------------------------------------------------------------*
       01  PARM-AREA.
           02 FILLER                        PIC S9(04)  COMP.
           02 PRM-DATA                      PIC  X(01).
       PROCEDURE DIVISION USING PARM-AREA.
      *-------------------------------------------------------------*
      *    M0 : CONTROL                                             *
      *-------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *-------------------------------------------------------------*
      *    M1 : INIT                                                *
      *-------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE
                       WKCFILE.
       M1-L01.
           MOVE ZERO          TO WFG1-OVER.
           MOVE LOW-VALUE     TO WSV3-KEY.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '    KMS'     TO P89-TBLID(1).
           MOVE 'S'           TO P89-LTYPE(1).
           MOVE ALL '*'       TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
       M11-L01.
           MOVE WKA-YTKBN   TO WIN1-YTKBN.
           MOVE WKA-YUSCD   TO WIN1-YUSCD.
           MOVE WKA-YUNCD   TO WIN1-YUNCD.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-KEY  TO WSV1-KEY.
           MOVE LOW-VALUE TO WSV2-RECKB.
           MOVE ZERO      TO WED1-PAGE1.
           MOVE ZERO      TO WED1-PAGE2.
           MOVE ZERO      TO WCT1-PAGEA.
           MOVE ZERO      TO WCT1-PAGEB.
           MOVE ZERO      TO WCT1-PAGEC.
           PERFORM M21-WRITE-WKB THRU M21-EXIT
             UNTIL WIN1-KEY NOT = WSV1-KEY.
           PERFORM M22-WRITE-WKC THRU M22-EXIT.
       M2-L01.
           IF PRM-DATA = '1'
              GO TO M2-EXIT.
           IF WCT1-PAGEC > 9999 AND
              WFG1-OVER  = ZERO
              MOVE '1'   TO WFG1-OVER
              DISPLAY 'C VBKA5080 SEIKYU-MEISAI 9,999 PAGE OVER'
              UPON CONSOLE.
           IF WCT1-PAGEC > 9999
              MOVE  WCT1-PAGEC   TO  WED1-PAGEC
              DISPLAY 'C VBKA5080 YUSOU-SAKI=' WSV1-YUSCD '('
                          WSV1-YUNCD ') PAGE=' WED1-PAGEC
              UPON CONSOLE.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1 : WRITE WKBFILE                                     *
      *-------------------------------------------------------------*
       M21-WRITE-WKB.
           MOVE WKA-R TO WKB-R.
           EVALUATE WKA-RKB10
             WHEN '03'
             WHEN '04'
             WHEN '05'
             WHEN '06'
             WHEN '07'
             WHEN '08'
             WHEN '09'
                  MOVE WKA-PAGE2 TO WED1-PAGE1
                  MOVE WKA-PAGE2 TO WCT1-PAGEA,
             WHEN '10'
                  IF WSV2-RECKB = '09'
                     COMPUTE WED1-PAGE1 = WED1-PAGE1 + 1
                     COMPUTE WCT1-PAGEA = WCT1-PAGEA + 1
                  ELSE
                     IF WSV2-RECKB = LOW-VALUE
                        MOVE WKA-PAGE2 TO WED1-PAGE1
                        MOVE WKA-PAGE2 TO WCT1-PAGEA
                     ELSE
                        IF WSV2-RECKB NOT < '60'
                           COMPUTE WED1-PAGE1 = WED1-PAGE2 + 1
                           COMPUTE WCT1-PAGEA = WCT1-PAGEB + 1
                        ELSE
                           COMPUTE WED1-PAGE1 = WED1-PAGE1 + 1
                           COMPUTE WCT1-PAGEA = WCT1-PAGEA + 1,
                        END-IF
                     END-IF
                  END-IF,
             WHEN '20'
             WHEN '30'
             WHEN '40'
             WHEN '50'
                  GO TO M21-L01,
             WHEN '60'
             WHEN '63'
             WHEN '64'
             WHEN '65'
             WHEN '66'
                  COMPUTE WED1-PAGE2 = WED1-PAGE1 + WKA-PAGE2 - 1
                  COMPUTE WCT1-PAGEB = WCT1-PAGEA + WKA-PAGE2 - 1
           END-EVALUATE.
       M21-L01.
           IF WKA-RKB10 < '60'
              MOVE WED1-PAGE1 TO WKB-YSTPG
              MOVE WCT1-PAGEA TO WCT1-PAGEC
              IF WKA-RKB10 = '07'
                 IF WKA-YUSCD NOT = WSV3-KEY
                    MOVE WKA-YUSCD  TO WSV3-KEY
                    MOVE WKA-YUSCD  TO TOKCD OF KMS
                    PERFORM M211-SELECT-KMS THRU M211-EXIT
                 END-IF
                 MOVE SHZSK OF KMS  TO WKB-SHZSK7
              END-IF
           ELSE
              MOVE WED1-PAGE2 TO WKB-YSTPG
              MOVE WCT1-PAGEB TO WCT1-PAGEC.
           WRITE WKB-REC FROM WKB-R.
           MOVE WKA-RKB10 TO WSV2-RECKB.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SELECT-KMS                                         *
      *----------------------------------------------------------------*
       M211-SELECT-KMS.
           EXEC SQL WHENEVER NOT FOUND  GO TO M211-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M211-ERR END-EXEC.
           EXEC SQL SELECT C5
              INTO :DCLKMS.C5
              FROM    KMS
              WHERE A0=:DCLKMS.A0
           END-EXEC.
           GO TO M211-EXIT.
       M211-L01.
           MOVE SPACE   TO SHZSK OF KMS.
           GO TO M211-EXIT.
       M211-ERR.
           DISPLAY 'R VBKA5080 TABLE(SI.KMS) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.2 : WRITE WKCFILE                                     *
      *-------------------------------------------------------------*
       M22-WRITE-WKC.
           MOVE SPACE       TO WKC-R.
           INITIALIZE          WKC-R.
           MOVE WSV1-YTKBN  TO WKC-YTKBN.
           MOVE WSV1-YUSCD  TO WKC-YUSCD.
           MOVE WSV1-YUNCD  TO WKC-YUNCD.
           MOVE WKB-YSTPG   TO WKC-YSALL.
           WRITE WKC-REC FROM WKC-R.
       M22-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE.
       M3-L01.
           IF PRM-DATA = '1'
              GO TO M3-EXIT.
           IF WFG1-OVER = '1'
              DISPLAY 'C VBKA5080 KAIHATUKA-CALL!! Y KEY-IN'
              UPON CONSOLE
              ACCEPT SYS-CARD1 FROM CONSOLE
              IF SYS-ACCEPT = 'Y'
                 CONTINUE
              ELSE
                 GO TO M3-L01
              END-IF
           ELSE
              DISPLAY '*** ´×- Å¼ ***'.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA5080  END                                          *
      *-------------------------------------------------------------*
