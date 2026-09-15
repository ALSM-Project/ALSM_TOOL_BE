./ ADD NAME=VBKA1610  01.07 20150805 20190726 141914 00610 00602 MCSK013
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1610'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN    SYSTEM             *
      *              <> SUBSYSTEM      : K.A                           *
      *              <> PG.OUTLINE     : Ä¸²»· ¼ÞÕ³¼Ö É ÎÝÔ¸           *
      *                                                                *
      *                 * INPUT FILE   : Ä¸²»· ¼ÞÕ³¼Ö Ï½À-   (TJM)     *
      *                 * INPUT FILE   : ¾²·Õ³¼Ö ÃÞ-À       (DWKB)     *
      *                 * OUTPUT FILE  : ¾²·Õ³¼Ö ÃÞ-À                  *
      *                                  ¾²·Õ³Ò²»²          (DWKA)     *
      *                 * OUTPUT FILE  : ¾²·Õ³¼Ö ÃÞ-À                  *
      *                                  ¾²·Õ³ ¶¶ÞÐ         (DWKC)     *
      *                 * OUTPUT FILE  : ÄØÄÒ ÃÞ-À2         (DWKE)     *
      *                                                                *
      *              <> AUTHOR         : SUZUKEN-M.KITAGAWA            *
      *              <> DATE           : 86/08/29                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) TJFFILE KARA SCMFILE Æ ÍÝº³  : 86/09/12 SUZUKEN-M.K       *
      * (02) PAGE MAISU TUIKA             : 86/10/31 CSK-K.OTSUKA      *
      * (03) SYOHIZEI TUIKA         Â²¶   : 89/03/17 CSK-S.MUNEISHI    *
      * (04) YOUKEN NO. K-263       ÍÝº³  : 90/11/01 SUZ-H.MIZUTANI    *
      * (05) YOUKEN NO. K-263       ÍÝº³  : 90/12/07 SUZ-H.MIZUTANI    *
      * (06) K-261                  ÍÝº³  : 97/10/06 CSK-H.YOSHIMOTO   *
      * (07) K-275C                 ÍÝº³  : 98/04/27 CSK-S.KOJIMA      *
      * (08) K-275C                 ÍÝº³  : 98/05/26 CSK-S.KOJIMA      *
      * (09) K-568                  ÍÝº³  : 03/08/07 CSK-C.YAMAGATA    *
      * (10) K-1780                 ÍÝº³  : 15/08/05 CSK-Y.YOSHIGA     *
      * (11) K-02163                ÍÝº³  : 19/06/26 SCSK-A.ISIKAWA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TJMFILE ASSIGN    TO TJMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TJM
                          FILE  STATUS S-CODE.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
       DATA DIVISION.
       FILE SECTION.
       FD  TJMFILE
           LABEL RECORD STANDARD.
       01  TJM-REC.
           02 KEY-TJM                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(300).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(30).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  TJM-R.
           COPY VFILTJM SUPPRESS.
       01  WKB-R.
           02 WKB-CTLKEY.
              03 WKB-KAINO                  PIC  X(02).
              03 WKB-YTKBN                  PIC  X(01).
              03 WKB-YSYBN.
                 04 WKB-YSYB1               PIC  X(03).
                 04 WKB-YSYB2               PIC  X(04).
              03 WKB-YSBNO                  PIC  X(03).
              03 WKB-YSSTN                  PIC  X(03).
              03 WKB-YSCKG                  PIC  X(05).
              03 WKB-YUSCD                  PIC  X(07).
              03 WKB-YUNCD                  PIC  X(02).
              03 WKB-YSALL                  PIC  9(04).
              03 WKB-INFLG                  PIC  X(01).
              03 WKB-YSTPG                  PIC  9(04).
              03 WKB-PAGEN.
                 04 WKB-PAGE1               PIC  9(04).
                 04 WKB-PAGE2               PIC  9(04).
              03 WKB-SESCD                  PIC  X(07).
              03 WKB-SQNCD                  PIC  X(02).
           02 WKB-LPDAT                     PIC  X(219).
           02 WKB-DATA20  REDEFINES  WKB-LPDAT.
              03 WKB-GNO20                  PIC  9(02).
              03 WKB-RKB20                  PIC  X(02).
              03 WKB-MESAI2.
                 04 WKB-TMSJU               PIC  X(40).
                 04 WKB-TMSJL               PIC  X(40).
                 04 WKB-NOSMJ               PIC  X(40).
                 04 WKB-SQBNO               PIC  X(03).
                 04 WKB-SQSTN               PIC  X(03).
                 04 WKB-SQCKK               PIC  X(05).
                 04 WKB-SMEYMD.
                    05 WKB-SIMYY            PIC  9(04).
                    05 WKB-SIMMM            PIC  9(02).
                    05 WKB-SIMDD            PIC  9(02).
                 04 WKB-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKB-DATA50  REDEFINES  WKB-LPDAT.
              03 WKB-GNO50                  PIC  9(02).
              03 WKB-RKB50                  PIC  X(02).
              03 WKB-MEISA5.
                 04 WKB-ZENGT.
                    05 WKB-ZSKDK            PIC S9(13) COMP-3.
                    05 WKB-ZNKDK            PIC S9(13) COMP-3.
                    05 WKB-ZSYNB            PIC S9(09) COMP-3.
                    05 WKB-ZKRDK            PIC S9(13) COMP-3.
                 04 WKB-TOGTU.
                    05 WKB-TKIDK            PIC S9(13) COMP-3.
                    05 WKB-TSKDK            PIC S9(13) COMP-3.
                    05 WKB-MSKSK            PIC S9(13) COMP-3.
                    05 WKB-TURZN            PIC S9(13) COMP-3.
                 04 WKB-ZANKB               PIC  X(01).
                 04 WKB-SHZEI               PIC S9(13) COMP-3.
                 04 WKB-TKAIG               PIC S9(13) COMP-3.
                 04 WKB-SHZSK               PIC  X(01).
                 04 FILLER                  PIC  X(145).
           02 WKB-DATA60  REDEFINES  WKB-LPDAT.
              03 WKB-GNO60                  PIC  9(02).
              03 WKB-RKB60                  PIC  X(02).
              03 WKB-MEISA6.
                 04 WKB-DNHI1.
                    05 WKB-DNHMM            PIC  9(02).
                    05 WKB-DNHDD            PIC  9(02).
                 04 WKB-DNBN1               PIC  X(07).
                 04 WKB-TRKBN               PIC  X(08).
                 04 WKB-DNKIN               PIC S9(09) COMP-3.
                 04 WKB-SYMKJ.
                    05 WKB-SHMKJ            PIC  X(50).
                    05 WKB-SKYKJ            PIC  X(40).
                 04 WKB-SURYO               PIC S9(05) COMP-3.
                 04 WKB-TANKA               PIC S9(08) COMP-3.
                 04 WKB-KNGAK               PIC S9(09) COMP-3.
                 04 WKB-MESHZ               PIC S9(09) COMP-3.
                 04 WKB-SYOCD               PIC  X(09).
                 04 WKB-SYOME.
                    05 WKB-SHMKK            PIC  X(25).
                    05 WKB-SKYKK            PIC  X(20).
                 04 WKB-BIKOU.
                    05 WKB-DNHI2.
                       06 WKB-DNMM2         PIC  9(02).
                       06 WKB-DNDD2         PIC  9(02).
                    05 WKB-DNBN2            PIC  X(07).
                    05 WKB-THKBN            PIC  X(01).
                    05 WKB-BNKKB            PIC  X(01).
                 04 WKB-DSHZ3               PIC S9(09) COMP-3.
                 04 WKB-DSHZFG              PIC  X(01).
                 04 WKB-MSHZUM              PIC  X(01).
                 04 WKB-MSZRT               PIC S9(02)V9 COMP-3.
                 04 WKB-DPGK60              PIC S9(09)   COMP-3.
                 04 WKB-KGZH60              PIC  X(02).
           02 WKB-DATA6X  REDEFINES  WKB-LPDAT.
              03 WKB-GNO63                  PIC  9(02).
              03 WKB-RKB63                  PIC  X(02).
              03 WKB-MEISAX.
                 04 WKB-DNPH1               PIC  9(04).
                 04 WKB-MIDA1               PIC  X(50).
                 04 WKB-MIDA2               PIC  X(50).
                 04 WKB-DNPKG               PIC S9(11) COMP-3.
                 04 WKB-DSHZ4               PIC S9(11) COMP-3.
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
                 04 WKB-GOKGK               PIC S9(09) COMP-3.
                 04 WKB-ZEGAK               PIC S9(09) COMP-3.
                 04 WKB-TOKCD               PIC  X(07).
                 04 WKB-NOSCD               PIC  X(02).
                 04 WKB-TNGKK               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(77).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKA-R.
           02 WKA-CTLKEY.
              03 WKA-KAINO                  PIC  X(02).
              03 WKA-YTKBN                  PIC  X(01).
              03 WKA-YSYBN.
                 04 WKA-YSYB1               PIC  X(03).
                 04 WKA-YSYB2               PIC  X(04).
              03 WKA-YSBNO                  PIC  X(03).
              03 WKA-YSSTN                  PIC  X(03).
              03 WKA-YSCKG                  PIC  X(05).
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
           02 WKA-DATA60  REDEFINES  WKA-LPDAT.
              03 WKA-GNO60                  PIC  9(02).
              03 WKA-RKB60                  PIC  X(02).
              03 WKA-MEISA6.
                 04 WKA-DNHI1.
                    05 WKA-DNHMM            PIC  9(02).
                    05 WKA-DNHDD            PIC  9(02).
                 04 WKA-DNBN1               PIC  X(07).
                 04 WKA-TRKBN               PIC  X(08).
                 04 WKA-DNKIN               PIC S9(09) COMP-3.
                 04 WKA-SYMKJ.
                    05 WKA-SHMKJ            PIC  X(50).
                    05 WKA-SKYKJ            PIC  X(40).
                 04 WKA-SURYO               PIC S9(05) COMP-3.
                 04 WKA-TANKA               PIC S9(08) COMP-3.
                 04 WKA-KNGAK               PIC S9(09) COMP-3.
                 04 WKA-MESHZ               PIC S9(09) COMP-3.
                 04 WKA-SYOCD               PIC  X(09).
                 04 WKA-SYOME.
                    05 WKA-SHMKK            PIC  X(25).
                    05 WKA-SKYKK            PIC  X(20).
                 04 WKA-BIKOU.
                    05 WKA-DNHI2.
                       06 WKA-DNMM2         PIC  9(02).
                       06 WKA-DNDD2         PIC  9(02).
                    05 WKA-DNBN2            PIC  X(07).
                    05 WKA-THKBN            PIC  X(01).
                    05 WKA-BNKKB            PIC  X(01).
                 04 WKA-DSHZ3               PIC S9(09) COMP-3.
                 04 WKA-DSHZFG              PIC  X(01).
                 04 WKA-MSHZUM              PIC  X(01).
                 04 FILLER                  PIC  X(09).
           02 WKA-DATA6X  REDEFINES  WKA-LPDAT.
              03 WKA-GNO63                  PIC  9(02).
              03 WKA-RKB63                  PIC  X(02).
              03 WKA-MEISAX.
                 04 WKA-DNPH1               PIC  9(04).
                 04 WKA-MIDA1               PIC  X(50).
                 04 WKA-MIDA2               PIC  X(50).
                 04 WKA-DNPKG               PIC S9(11) COMP-3.
                 04 WKA-DSHZ4               PIC S9(11) COMP-3.
                 04 FILLER                  PIC  X(99).
           02 WKA-DATA65  REDEFINES  WKA-LPDAT.
              03 WKA-GNO65                  PIC  9(02).
              03 WKA-RKB65                  PIC  X(02).
              03 WKA-MESI65.
                 04 WKA-MIDAU               PIC  X(50).
                 04 WKA-MIDAD               PIC  X(50).
                 04 WKA-CDEDT               PIC  X(14).
                 04 WKA-GOKGK               PIC S9(09) COMP-3.
                 04 WKA-ZEGAK               PIC S9(09) COMP-3.
                 04 WKA-TOKCD               PIC  X(07).
                 04 WKA-NOSCD               PIC  X(02).
                 04 FILLER                  PIC  X(82).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKC-R.
           02 WKC-CTLKEY.
              03 WKC-KAINO                  PIC  X(02).
              03 WKC-YTKBN                  PIC  X(01).
              03 WKC-YSYBN.
                 04 WKC-YSYB1               PIC  X(03).
                 04 WKC-YSYB2               PIC  X(04).
              03 WKC-YSBNO                  PIC  X(03).
              03 WKC-YSSTN                  PIC  X(03).
              03 WKC-YSCKG                  PIC  X(05).
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
                    05 WKC-BILME            PIC  X(30).
                 04 WKC-SEYBN.
                    05 WKC-SEYB1            PIC  X(03).
                    05 WKC-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
           02 WKC-DATA20  REDEFINES  WKC-LPDAT.
              03 WKC-GNO20                  PIC  9(02).
              03 WKC-RKB20                  PIC  X(02).
              03 WKC-MESAI2.
                 04 WKC-TMSJU               PIC  X(40).
                 04 WKC-TMSJL               PIC  X(40).
                 04 WKC-NOSMJ               PIC  X(40).
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
                 04 WKC-MIDAS               PIC  X(08).
                 04 WKC-TRBNK  OCCURS  3.
                    05 WKC-TGSME            PIC  X(30).
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
              03 WKC-MEISA5.
                 04 WKC-ZENGT.
                    05 WKC-ZSKDK            PIC S9(13) COMP-3.
                    05 WKC-ZNKDK            PIC S9(13) COMP-3.
                    05 WKC-ZSYNB            PIC S9(09) COMP-3.
                    05 WKC-ZKRDK            PIC S9(13) COMP-3.
                 04 WKC-TOGTU.
                    05 WKC-TKIDK            PIC S9(13) COMP-3.
                    05 WKC-TSKDK            PIC S9(13) COMP-3.
                    05 WKC-MSKSK            PIC S9(13) COMP-3.
                    05 WKC-TURZN            PIC S9(13) COMP-3.
                 04 WKC-ZANKB               PIC  X(01).
                 04 WKC-SHZEI               PIC S9(13) COMP-3.
                 04 WKC-TKAIG               PIC S9(13) COMP-3.
                 04 WKC-SHZSK               PIC  X(01).
                 04 FILLER                  PIC  X(145).
           02 WKC-PGMSU                     PIC  X(02).
           02 WKC-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKE-R.
           02 WKE-SESCD                     PIC  X(07).
           02 WKE-SQNCD                     PIC  X(02).
           02 WKE-TORIT             OCCURS  15.
              03 WKE-TRIYU                  PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SESCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-SESCD                 PIC  X(07).
           02 WSV2-YSYBN                    PIC  X(07).
           02 WSV2-TJYSO.
              03 WSV2-TJYSA                 PIC  X(40).
              03 WSV2-TJYSB                 PIC  X(30).
              03 WSV2-BILME                 PIC  X(30).
           02 WSV2-SEYBN.
              03 WSV2-SEYB1                 PIC  X(03).
              03 WSV2-SEYB2                 PIC  X(04).
       01  FLAG-AREA.
           02 WFL1-OUTFL                    PIC  X(01).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL  WIN1-KEY = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  TJMFILE
                       WKBFILE
                OUTPUT WKAFILE
                       WKCFILE
                       WKEFILE.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-TJM THRU M12-EXIT.
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
           MOVE WKB-SESCD TO WIN1-SESCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ TJMFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-TJM.
           MOVE 'Z '    TO   S-CODE.
           READ TJMFILE INTO TJM-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKA1610 TJMFILE DATA 0 KEN ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN2-KEY
              GO TO M12-EXIT.
           MOVE TJM-TOKCD TO WIN2-TOKCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-WKBONL THRU M22-EXIT
              ELSE
                 PERFORM M23-TJMONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1  TO WSV1-KEY1.
           MOVE SPACE      TO WFL1-OUTFL.
           PERFORM M211-OUT01  THRU M211-EXIT
                UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
           PERFORM M12-READ-TJM THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : OUT01 (ÏÂÁ ¼À Ä·É ¼ÕÂØÖ¸)                          *
      *----------------------------------------------------------------*
       M211-OUT01.
           MOVE TJM-YUBNO  TO WSV2-YSYBN.
           MOVE TJM-ADKJ1  TO WSV2-TJYSA.
           MOVE TJM-ADKJ2  TO WSV2-TJYSB.
           MOVE TJM-ADKJ3  TO WSV2-BILME.
           MOVE TJM-YUBNO  TO WSV2-SEYBN.
           IF WKB-RKB60    <  '60'
              PERFORM M2111-EDIT-SKG  THRU M2111-EXIT
           ELSE
              PERFORM M2112-EDIT-SKM  THRU M2112-EXIT
           END-IF.
           IF TJM-ADRES  = SPACE AND
              WFL1-OUTFL = SPACE
              PERFORM M2113-EDIT-WKE  THRU M2113-EXIT
           END-IF.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1: EDIT SKG                                          *
      *----------------------------------------------------------------*
       M2111-EDIT-SKG.
           MOVE SPACE      TO WKC-R.
           INITIALIZE         WKC-R.
           MOVE WKB-CTLKEY TO WKC-CTLKEY.
           MOVE WSV2-YSYBN TO WKC-YSYBN.
           MOVE ZERO       TO WKC-PGMSU.
           MOVE SPACE      TO WKC-YTKBN.
           MOVE SPACE      TO WKC-SORTJ.
           IF WKB-RKB20 NOT = '20' GO TO M2111-L05.
       M2111-L01.
           MOVE SPACE      TO WKC-DATA10.
           MOVE ZERO       TO WKC-GNO10.
           MOVE '10'       TO WKC-RKB10.
           MOVE WSV2-TJYSO TO WKC-TJYSO.
           MOVE WSV2-SEYBN TO WKC-SEYBN.
           WRITE WKC-REC FROM WKC-R.
       M2111-L02.
           MOVE WKB-DATA20 TO WKC-DATA20.
           WRITE WKC-REC FROM WKC-R.
       M2111-L03.
           MOVE SPACE      TO WKC-DATA30.
           MOVE ZERO       TO WKC-GNO30.
           MOVE '30'       TO WKC-RKB30.
           MOVE ZERO       TO WKC-BNKSU.
           WRITE WKC-REC FROM WKC-R.
       M2111-L04.
           MOVE SPACE      TO WKC-DATA40.
           MOVE ZERO       TO WKC-GNO40.
           MOVE '40'       TO WKC-RKB40.
           WRITE WKC-REC FROM WKC-R.
       M2111-L05.
           IF WKB-RKB50  NOT =  '50'
              GO TO M2111-EXIT.
           MOVE WKB-DATA50 TO WKC-DATA50.
           WRITE WKC-REC FROM WKC-R.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.2: EDIT SKM                                          *
      *----------------------------------------------------------------*
       M2112-EDIT-SKM.
           MOVE WKB-R      TO WKA-R.
           MOVE WSV2-YSYBN TO WKA-YSYBN.
           MOVE SPACE      TO WKA-YTKBN.
           MOVE SPACE      TO WKA-SORTJ.
           MOVE ZERO       TO WKA-PGMSU.
           WRITE WKA-REC FROM WKA-R.
       M2112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.3: EDIT WKE                                          *
      *----------------------------------------------------------------*
       M2113-EDIT-WKE.
           MOVE '1'        TO WFL1-OUTFL.
           MOVE SPACE      TO WKE-R.
           MOVE WKB-SESCD  TO WKE-SESCD.
           MOVE WKB-SQNCD  TO WKE-SQNCD.
           MOVE '*'        TO WKE-TORIT(11).
           WRITE WKE-REC FROM WKE-R.
       M2113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKBONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           MOVE SPACE     TO WFL1-OUTFL.
           PERFORM M221-OUT02   THRU M221-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : OUT02                                              *
      *----------------------------------------------------------------*
       M221-OUT02.
           MOVE SPACE    TO WSV2-YSYBN.
           MOVE SPACE    TO WSV2-TJYSO.
           MOVE SPACE    TO WSV2-SEYBN.
           IF WKB-RKB60    <  '60'
              PERFORM M2111-EDIT-SKG  THRU M2111-EXIT
           ELSE
              PERFORM M2112-EDIT-SKM  THRU M2112-EXIT
           END-IF.
           IF WFL1-OUTFL  =  SPACE
              PERFORM M2113-EDIT-WKE  THRU M2113-EXIT
           END-IF.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : TJMFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-TJMONL.
           PERFORM M12-READ-TJM THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE TJMFILE
                 WKAFILE
                 WKBFILE
                 WKCFILE
                 WKEFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA1610 END                                           *
      *----------------------------------------------------------------*
