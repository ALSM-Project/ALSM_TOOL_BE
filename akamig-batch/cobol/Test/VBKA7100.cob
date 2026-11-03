./ ADD NAME=VBKA7100  01.25 20190627 20190729 104502 00213 00299 MCSK013
       ID  DIVISION.
       PROGRAM-ID. 'VBKA7100'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN SYSTEM                *
      *              <> SUBSYSTEM      : KA                            *
      *              <> PG.OUTLINE     : SEIKYU GOUKEIHYOU SAKUSEI     *
      *                                  TAISYOU SAKI CYUSYUTU         *
      *                 * INPUT FILE   : SEIKYU SYO DATA               *
      *                                  (SEIKYU MEISAI)         (DWKA)*
      *                 * INPUT FILE   : SEIKYU GOUKEI HYO             *
      *                                  SAKUSEI TAISYOU SAKI    (DWKC)*
      *                 * OUTPUT FILE  : ZEIRITU BETU GOUKEI DATA      *
      *                                                          (DWKD)*
      *              <> AUTHOR         : SCSK-A.ISIKAWA                *
      *              <> DATE           : 19/06/28                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD CSK-X.XXXXXX         *
      * (02) XXXXXX XXXXXX      Â²¶    : YY/MM/DD CSK-X.XXXXXX         *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(020).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
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
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKC-R.
           02 WKC-YUSCD                     PIC  X(07).
           02 WKC-YUNCD                     PIC  X(02).
           02 WKC-SESCD                     PIC  X(07).
           02 WKC-SQNCD                     PIC  X(02).
           02 FILLER                        PIC  X(02).
       01  WKD-R.
           02 WKD-CTLKEY.
              03 WKD-KAINO                  PIC  X(02).
              03 WKD-YTKBN                  PIC  X(01).
              03 WKD-YSYBN.
                 04 WKD-YSYB1               PIC  X(03).
                 04 WKD-YSYB2               PIC  X(04).
              03 WKD-YSBNO                  PIC  X(03).
              03 WKD-YSSTN                  PIC  X(03).
              03 WKD-YSCKG                  PIC  X(05).
              03 WKD-YUSCD                  PIC  X(07).
              03 WKD-YUNCD                  PIC  X(02).
              03 WKD-YSALL                  PIC  9(04).
              03 WKD-INFLG                  PIC  X(01).
              03 WKD-YSTPG                  PIC  9(04).
              03 WKD-PAGEN.
                 04 WKD-PAGE1               PIC  9(04).
                 04 WKD-PAGE2               PIC  9(04).
              03 WKD-SESCD                  PIC  X(07).
              03 WKD-SQNCD                  PIC  X(02).
           02 WKD-LPDAT                     PIC  X(219).
           02 WKD-PGMSU                     PIC  X(02).
           02 WKD-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SESCD              PIC  X(07).
                 04 WIN1-SQNCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SESCD              PIC  X(07).
                 04 WIN2-SQNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-SESCD                 PIC  X(07).
              03 WSV1-SQNCD                 PIC  X(02).
      *    02 WSV2-KEY1.
      *       03 WSV2-SESCD                 PIC  X(07).
      *       03 WSV2-SQNCD                 PIC  X(02).
      *01  FLAG-AREA.
      *    02 WFL1-OUTFL                    PIC  X(01).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ((WIN1-KEY = HIGH-VALUE)
                          OR
                          (WIN2-KEY = HIGH-VALUE)).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                       WKCFILE
                OUTPUT WKDFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *---------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKA-SESCD TO WIN1-SESCD.
           MOVE WKA-SQNCD TO WIN1-SQNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKCFILE                                         *
      *---------------------------------------------------------------*
       M12-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKC-SESCD TO WIN2-SESCD.
           MOVE WKC-SQNCD TO WIN2-SQNCD.
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
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKCONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1  TO WSV1-KEY1.
           MOVE WKA-R      TO WKD-R.
           MOVE WKC-YUSCD TO WKD-YUSCD.
           MOVE WKC-YUNCD TO WKD-YUNCD.
           WRITE WKD-REC FROM WKD-R.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M11-READ-WKA THRU M11-EXIT.
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
                 WKDFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA7100 END                                           *
      *----------------------------------------------------------------*
