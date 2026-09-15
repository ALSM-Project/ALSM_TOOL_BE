./ ADD NAME=VBKA5040  01.03 20030731 20030731 134648 00344 00344 MCSK073
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5040'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : KA                            *
      *              <> PG.OUTLINE     : SEIKKYU GOUKEI HYO            *
      *                                  PAGENO KAMIKETA SETTEI        *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEI HYO DATA (DWKA) *
      *                 * INPUT FILE   : SEIKYU GOUKEI HYO PAGE DATA   *
      *                                                         (DWKC) *
      *                 * OUTPUT FILE  : SEIKYU GOUKEI HYO DATA (DWKB) *
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/07/31                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(20).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
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
       01  WKC-R.
           02 WKC-YUSCD                     PIC  X(07).
           02 WKC-YUNCD                     PIC  X(02).
           02 WKC-PAGEU                     PIC  9(04).
           02 FILLER                        PIC  X(07).
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
           02 WKB-DATA03  REDEFINES  WKB-LPDAT.
              03 WKB-GNO03                  PIC  9(02).
              03 WKB-RKB03                  PIC  X(02).
              03 WKB-MESAI3.
                 04 WKB-TJYSO.
                    05 WKB-TJYSA            PIC  X(40).
                    05 WKB-TJYSB            PIC  X(30).
                    05 WKB-TKBLMJ           PIC  X(30).
                 04 FILLER                  PIC  X(115).
           02 WKB-DATA04  REDEFINES  WKB-LPDAT.
              03 WKB-GNO04                  PIC  9(02).
              03 WKB-RKB04                  PIC  X(02).
              03 WKB-MESAI4.
                 04 WKB-TNMJO4              PIC  X(40).
                 04 WKB-TNMGE4              PIC  X(40).
                 04 WKB-NOSKA4              PIC  X(40).
                 04 WKB-YSBN04              PIC  X(03).
                 04 WKB-YSSN04              PIC  X(03).
                 04 WKB-YSCK04              PIC  X(05).
                 04 WKB-SMEYMD.
                    05 WKB-SIMYY            PIC  9(04).
                    05 WKB-SIMMM            PIC  9(02).
                    05 WKB-SIMDD            PIC  9(02).
                 04 FILLER                  PIC  X(76).
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
                 04 FILLER                  PIC  X(147).
           02 WKB-DATA08  REDEFINES  WKB-LPDAT.
              03 WKB-GNO08                  PIC  9(02).
              03 WKB-RKB08                  PIC  X(02).
              03 WKB-MESAI8.
                 04 WKB-ZENGT8.
                    05 WKB-ZSKDK8           PIC S9(13)  COMP-3.
                    05 WKB-ZNKDK8           PIC S9(13)  COMP-3.
                    05 WKB-ZSYNB8           PIC S9(09)  COMP-3.
                    05 WKB-ZKRDK8           PIC S9(13)  COMP-3.
                 04 WKB-TOGTU8.
                    05 WKB-TKIDK8           PIC S9(13)  COMP-3.
                    05 WKB-TSKDK8           PIC S9(13)  COMP-3.
                    05 WKB-MSKSK8           PIC S9(13)  COMP-3.
                    05 WKB-TURZN8           PIC S9(13)  COMP-3.
                 04 WKB-SHZEI8              PIC S9(13)  COMP-3.
                 04 WKB-TKAIG8              PIC S9(13)  COMP-3.
                 04 WKB-TNMJO8              PIC  X(40).
                 04 WKB-TNMGE8              PIC  X(40).
                 04 WKB-NOSKA8              PIC  X(30).
                 04 WKB-HIFLG8              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-YUSCD              PIC  X(07).
                 04 WIN1-YUNCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-YUSCD              PIC  X(07).
                 04 WIN2-YUNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-YUSCD                 PIC  X(07).
              03 WSV1-YUNCD                 PIC  X(02).
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
           OPEN INPUT  WKAFILE
                       WKCFILE
                OUTPUT WKBFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
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
      *    M1.2 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
       M12-L01.
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
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKCONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M211-EDIT-WKB THRU M211-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT WKBFILE                                       *
      *----------------------------------------------------------------*
       M211-EDIT-WKB.
           MOVE SPACE     TO WKB-R.
           INITIALIZE        WKB-R.
           MOVE WKA-R     TO WKB-R.
           MOVE WKC-PAGEU TO WKB-PAGE1.
           WRITE WKB-REC  FROM WKB-R.
       M211-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M11-READ-WKA THRU M11-EXIT
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
           CLOSE WKAFILE
                 WKCFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5040 END                                           *
      *----------------------------------------------------------------*
