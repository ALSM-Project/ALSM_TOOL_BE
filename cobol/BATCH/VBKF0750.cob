./ ADD NAME=VBKF0750  10.06 19990416 20100629 155229 00182 00318 MCSK020
       ID  DIVISION.
       PROGRAM-ID.     'VBKF0750'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : TOKUISAKI DATA SAKUSEI        *
      *                                                                *
      *                 * INPUT FILE   : SYSIN                         *
      *                                : TOKUISAKI MASTER   (TMSFILE)  *
      *                 * OUTPUT FILE  : TOKUISAKI DATA SEIKYU BUNSATU *
      *                                                        (DWKA)  *
      *                                : TOKUISAKI DATA      (DWKB)    *
      *                                                                *
      *              <> AUTHOR         : CSK-N.UEMURA                  *
      *              <> DATE           : 99/03/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1228             ÍÝº³   : 10/06/29  CSK-M.KODAMA        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TMSFILE ASSIGN    TO TMSFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TMS
                          FILE  STATUS S-CODE.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  TMSFILE
           LABEL RECORD STANDARD.
       01  TMS-REC.
           02 KEY-TMS                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(20).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(20).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  TMS-R.
           COPY VFILTMS SUPPRESS.
       01  WKA-R.
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-STKCD                     PIC  X(07).
           02 WKA-SHIME                     PIC  9(02).
           02 FILLER                        PIC  X(02).
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-STKCD                     PIC  X(07).
           02 WKB-SHIME                     PIC  9(02).
           02 FILLER                        PIC  X(02).
       01  SYSIN-AREA.
           02 SYS-CARD1.
              03 SYS-SHIME.
                 04 SYS-SHIMER              PIC  9(02).
              03 FILLER                     PIC  X(78).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (TMS-R = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M11-SYSIN1   THRU M11-EXIT.
       M1-L01.
           OPEN INPUT  TMSFILE
                OUTPUT WKAFILE
                       WKBFILE.
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : SYSIN1                                               *
      *----------------------------------------------------------------*
       M11-SYSIN1.
           MOVE LOW-VALUE    TO SYS-CARD1.
           ACCEPT SYS-CARD1.
       M11-L01.
           IF SYS-CARD1 = LOW-VALUE
              DISPLAY 'R VBKF0750 SYSIN NASI ! '
              UPON CONSOLE
              CALL 'VSBABEND'.
       M11-L02.
           IF SYS-SHIME NOT = '15' AND '20' AND '31'
              DISPLAY 'R VBKF0750 SYSIN ERROR = ' SYS-SHIME
              UPON CONSOLE
              CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ TMSFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-TMS.
           MOVE 'Z '    TO   S-CODE.
           READ TMSFILE INTO TMS-R
                AT END
                   MOVE HIGH-VALUE TO TMS-R
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKF0750 TMSFILE READ ERROR !!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO TMS-R
              GO TO M12-EXIT.
           IF TMS-SIMEB = SYS-SHIME AND
              TMS-SEYOS = '0'
              NEXT SENTENCE
           ELSE
              GO TO M12-READ-TMS.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF TMS-SEBUS = '1'
              PERFORM M21-WRITE-WKA THRU M21-EXIT
           ELSE
              PERFORM M22-WRITE-WKB THRU M22-EXIT.
       M2-L01.
           PERFORM M12-READ-TMS     THRU M12-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : WRITE WKAFILE                                        *
      *----------------------------------------------------------------*
       M21-WRITE-WKA.
           MOVE SPACE        TO WKA-R.
           INITIALIZE WKA-R.
           MOVE TMS-TOKCD    TO WKA-TOKCD.
           MOVE SPACE        TO WKA-NOSCD.
           MOVE TMS-SKUCD    TO WKA-STKCD.
           MOVE SYS-SHIMER   TO WKA-SHIME.
           WRITE WKA-REC FROM WKA-R.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WRITE WKBFILE                                        *
      *----------------------------------------------------------------*
       M22-WRITE-WKB.
           MOVE SPACE        TO WKB-R.
           INITIALIZE WKB-R.
           MOVE TMS-TOKCD    TO WKB-TOKCD.
           MOVE SPACE        TO WKB-NOSCD.
           MOVE TMS-SKUCD    TO WKB-STKCD.
           MOVE SYS-SHIMER   TO WKB-SHIME.
           WRITE WKB-REC FROM WKB-R.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE TMSFILE
                 WKAFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF0750 END                                           *
      *----------------------------------------------------------------*
