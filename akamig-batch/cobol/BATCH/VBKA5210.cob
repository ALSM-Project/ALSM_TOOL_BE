./ ADD NAME=VBKA5210  01.03 20031211 20031212 115059 00490 00490 MCSK065
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5210'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : KA                            *
      *              <> PG.OUTLINE     : SEIKYUUSYO DATA               *
      *                                  (SEIKYUU KAGAMI) SAKUSEI      *
      *                                  YUUSOUSAKI YUUBIN BANGOU FUKA *
      *                 * INPUT FILE   : SEIKYUU GOUKEIHYOU DATA       *
      *                                                         (DWKA) *
      *                 * INPUT FILE   : SEIKYUUSYO DATA               *
      *                                  (SEIKYUU KAGAMI)       (DWKB) *
      *                 * OUTPUT FILE  : SEIKYUUSYO DATA               *
      *                                  (SEIKYUU KAGAMI)       (DWKC) *
      *                                                                *
      *              <> AUTHOR         : CSK-M.YOSHIDA                 *
      *              <> DATE           : 03/12/11                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
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
           02 WKA-DATA03  REDEFINES  WKA-LPDAT.
              03 WKA-GNO03                  PIC  9(02).
              03 WKA-RKB03                  PIC  X(02).
              03 WKA-MESAI3.
                 04 WKA-TJYSO.
                    05 WKA-TJYSA            PIC  X(40).
                    05 WKA-TJYSB            PIC  X(30).
                    05 WKA-TKBLMJ           PIC  X(30).
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
                 04 FILLER                  PIC  X(147).
           02 WKA-DATA08  REDEFINES  WKA-LPDAT.
              03 WKA-GNO08                  PIC  9(02).
              03 WKA-RKB08                  PIC  X(02).
              03 WKA-MESAI8.
                 04 WKA-ZENGT8.
                    05 WKA-ZSKDK8           PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK8           PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB8           PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK8           PIC S9(13)  COMP-3.
                 04 WKA-TOGTU8.
                    05 WKA-TKIDK8           PIC S9(13)  COMP-3.
                    05 WKA-TSKDK8           PIC S9(13)  COMP-3.
                    05 WKA-MSKSK8           PIC S9(13)  COMP-3.
                    05 WKA-TURZN8           PIC S9(13)  COMP-3.
                 04 WKA-SHZEI8              PIC S9(13)  COMP-3.
                 04 WKA-TKAIG8              PIC S9(13)  COMP-3.
                 04 WKA-TNMJO8              PIC  X(40).
                 04 WKA-TNMGE8              PIC  X(40).
                 04 WKA-NOSKA8              PIC  X(30).
                 04 WKA-HIFLG8              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
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
           02 WKB-DATA10  REDEFINES  WKB-LPDAT.
              03 WKB-GNO10                  PIC  9(02).
              03 WKB-RKB10                  PIC  X(02).
              03 WKB-MESAI1.
                 04 WKB-TJYSO.
                    05 WKB-TJYSA            PIC  X(40).
                    05 WKB-TJYSB            PIC  X(30).
                    05 WKB-BILME            PIC  X(30).
                 04 WKB-SEYBN.
                    05 WKB-SEYB1            PIC  X(03).
                    05 WKB-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
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
           02 WKB-DATA30  REDEFINES  WKB-LPDAT.
              03 WKB-GNO30                  PIC  9(02).
              03 WKB-RKB30                  PIC  X(02).
              03 WKB-MESAI3.
                 04 WKB-HJPTN1              PIC  X(01).
                 04 WKB-MIDAS               PIC  X(08).
                 04 WKB-TRBNK  OCCURS  3.
                    05 WKB-TGSME            PIC  X(30).
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
                 04 FILLER                  PIC  X(146).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
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
                 04 FILLER                  PIC  X(105).
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
                 04 FILLER                  PIC  X(146).
           02 WKC-PGMSU                     PIC  X(02).
           02 WKC-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              04 WIN1-YUSCD                 PIC  X(07).
              04 WIN1-YUNCD                 PIC  X(02).
           02 WIN2-KEY.
              04 WIN2-YUSCD                 PIC  X(07).
              04 WIN2-YUNCD                 PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-YUSCD                 PIC  X(07).
              03 WSV1-YUNCD                 PIC  X(02).
           02 WSV2-KEY.
              03 WSV2-YUSCD                 PIC  X(07).
              03 WSV2-YUNCD                 PIC  X(02).
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
           MOVE WKA-YUSCD TO WIN1-YUSCD.
           MOVE WKA-YUNCD TO WIN1-YUNCD.
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
           MOVE WKB-YUSCD TO WIN2-YUSCD.
           MOVE WKB-YUNCD TO WIN2-YUNCD.
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
           MOVE  WIN2-KEY TO WSV2-KEY.
           PERFORM M211-EDIT-WKC THRU M211-EXIT
             UNTIL WIN2-KEY NOT = WSV2-KEY.
       M21-L01.
           MOVE  WIN1-KEY TO WSV1-KEY.
           PERFORM M11-READ-WKA THRU M11-EXIT
             UNTIL WIN1-KEY NOT = WSV1-KEY.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT WKCFILE                                       *
      *----------------------------------------------------------------*
       M211-EDIT-WKC.
           MOVE WKB-R      TO WKC-R.
           MOVE WKA-YSYBN  TO WKC-YSYBN.
           WRITE WKC-REC   FROM WKC-R.
       M211-L01.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           DISPLAY 'R VBKA5210 SEIKYU-KAGAMI NASI !=' WIN1-KEY
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           MOVE WKB-R       TO WKC-R.
           WRITE WKC-REC  FROM WKC-R.
       M23-L01.
           PERFORM M12-READ-WKB THRU M12-EXIT.
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
      *         VBKA5210 END                                           *
      *----------------------------------------------------------------*
