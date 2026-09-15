./ ADD NAME=VBKA5000  01.00 20081030 20081030 155105 00158 00158 MCSK028
       ID  DIVISION.
       PROGRAM-ID. 'VBKA5000'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : SEIKYU GOUKEIHYOU SAKUSEI     *
      *                                  TAISYOUSAKI TYUUSYUTU         *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEIHYOU SAKUSEIYOU  *
      *                                                    DATA  (DWKA)*
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYOU SAKUSEI     *
      *                                        TAISYOUSAKI       (DWKB)*
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/07/25                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1106             ÍÝº³   : 08/10/30  CSK-T.KINOSHITA     *
      * (02) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(30).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(20).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       01  WKA-R.
           02 WKA-SESCD                     PIC  X(07).
           02 WKA-SQNCD                     PIC  X(02).
           02 WKA-YUSCD                     PIC  X(07).
           02 WKA-YUNCD                     PIC  X(02).
           02 WKA-SEYHNB                    PIC  X(01).
           02 FILLER                        PIC  X(11).
       01  WKB-R.
           02 WKB-YUSCD                     PIC  X(07).
           02 WKB-YUNCD                     PIC  X(02).
           02 WKB-SESCD                     PIC  X(07).
           02 WKB-SQNCD                     PIC  X(02).
           02 FILLER                        PIC  X(02).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-YUSCD              PIC  X(07).
                 04 WIN1-YUNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-YUSCD              PIC  X(07).
                 04 WSV1-YUNCD              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-WKA.
              03 WED1-SESCD                 PIC  X(07).
              03 WED1-SQNCD                 PIC  X(02).
              03 WED1-YUSCD                 PIC  X(07).
              03 WED1-YUNCD                 PIC  X(02).
              03 WED1-SEYHNB                PIC  X(01).
              03 FILLER                     PIC  X(11).
       01  COUNT-AREA.
           02 WCT1-COUNT                    PIC S9(08) COMP.
       PROCEDURE DIVISION.
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
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE.
       M1-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
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
           MOVE WKA-YUSCD   TO WIN1-YUSCD.
           MOVE WKA-YUNCD   TO WIN1-YUNCD.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-KEY TO WSV1-KEY.
           MOVE 1        TO WCT1-COUNT.
           PERFORM M21-KEY-BREAK THRU M21-EXIT
             UNTIL WIN1-KEY NOT = WSV1-KEY.
           IF WCT1-COUNT NOT = 1
              MOVE SPACE      TO WKB-R
              INITIALIZE         WKB-R
              MOVE WED1-YUSCD TO WKB-YUSCD
              MOVE WED1-YUNCD TO WKB-YUNCD
              MOVE WED1-SESCD TO WKB-SESCD
              MOVE WED1-SQNCD TO WKB-SQNCD
              WRITE WKB-REC FROM WKB-R
           ELSE
              IF WED1-YUSCD NOT = WED1-SESCD
                 MOVE SPACE      TO WKB-R
                 INITIALIZE         WKB-R
                 MOVE WED1-YUSCD TO WKB-YUSCD
                 MOVE WED1-YUNCD TO WKB-YUNCD
                 MOVE WED1-SESCD TO WKB-SESCD
                 MOVE WED1-SQNCD TO WKB-SQNCD
                 WRITE WKB-REC FROM WKB-R.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1 : KEY-BREAK                                         *
      *-------------------------------------------------------------*
       M21-KEY-BREAK.
           MOVE WKA-R TO WED1-WKA.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           IF WIN1-KEY1 = WSV1-KEY1
              MOVE SPACE      TO WKB-R
              INITIALIZE         WKB-R
              MOVE WED1-YUSCD TO WKB-YUSCD
              MOVE WED1-YUNCD TO WKB-YUNCD
              MOVE WED1-SESCD TO WKB-SESCD
              MOVE WED1-SQNCD TO WKB-SQNCD
              WRITE WKB-REC FROM WKB-R
              COMPUTE WCT1-COUNT = WCT1-COUNT + 1.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA5000  END                                          *
      *-------------------------------------------------------------*
