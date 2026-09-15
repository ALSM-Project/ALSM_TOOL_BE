./ ADD NAME=VBKF1320  01.03 20101117 20101117 154023 00193 00364 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKF1320'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYU DAIKOU (OYA)           *
      *                                  KASOU KOUZA NUMBER HANEI      *
      *                                                                *
      *                 * INPUT FILE   : KAISYU JYOHOU MASTER          *
      *                                  (OYA)           (DWKD)        *
      *                                : KAISYU JYOHOU MASTER          *
      *                                  (KO)            (DWKE)        *
      *                 * OUTPUT FILE  : KAISYU JYOHOU DATA            *
      *                                  (KASOU KOUZA NUMBER HANEI GO) *
      *                                                  (DWKF)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/11/17                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
           SELECT WKFFILE ASSIGN    TO S-DWKF.
       DATA DIVISION.
       FILE SECTION.
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(40).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(40).
       FD  WKFFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKF-REC.
           02 FILLER                        PIC  X(40).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKD-R.
           02 WKD-KDTKCD                    PIC  X(07).
           02 WKD-TOKCD                     PIC  X(07).
           02 WKD-NOSCD                     PIC  X(02).
           02 WKD-KKZNO                     PIC  X(10).
           02 WKD-GHBCD                     PIC  X(04).
           02 FILLER                        PIC  X(10).
       01  WKE-R.
           02 WKE-KDTKCD                    PIC  X(07).
           02 WKE-TOKCD                     PIC  X(07).
           02 WKE-NOSCD                     PIC  X(02).
           02 WKE-KKZNO                     PIC  X(10).
           02 WKE-GHBCD                     PIC  X(04).
           02 FILLER                        PIC  X(10).
       01  WKF-R.
           02 WKF-KDTKCD                    PIC  X(07).
           02 WKF-TOKCD                     PIC  X(07).
           02 WKF-NOSCD                     PIC  X(02).
           02 WKF-KKZNO                     PIC  X(10).
           02 WKF-GHBCD                     PIC  X(04).
           02 FILLER                        PIC  X(10).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-KDTKCD             PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-KDTKCD             PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-KDTKCD             PIC  X(07).
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
           OPEN INPUT  WKDFILE
                       WKEFILE
                OUTPUT WKFFILE.
           PERFORM M11-READ-WKD THRU M11-EXIT.
           PERFORM M12-READ-WKE THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKDFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKD.
           READ WKDFILE INTO WKD-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKD-KDTKCD TO WIN1-KDTKCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKEFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKE.
           READ WKEFILE INTO WKE-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKE-KDTKCD TO WIN2-KDTKCD.
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
                 PERFORM M22-WKDONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKEONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WKD-R      TO WKF-R.
           WRITE WKF-REC FROM WKF-R.
           MOVE WIN2-KEY1  TO WSV2-KEY1.
           PERFORM M211-WRITE-WKF THRU M211-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           PERFORM M11-READ-WKD THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKF                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKF.
           MOVE SPACE      TO WKF-R.
           MOVE WKE-KDTKCD TO WKF-KDTKCD.
           MOVE WKE-TOKCD  TO WKF-TOKCD.
           MOVE WKE-NOSCD  TO WKF-NOSCD.
           MOVE WKD-KKZNO  TO WKF-KKZNO.
           MOVE WKD-GHBCD  TO WKF-GHBCD.
           WRITE WKF-REC FROM WKF-R.
       M211-L01.
           PERFORM M12-READ-WKE THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKDFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKDONL.
           MOVE WKD-R      TO WKF-R.
           WRITE WKF-REC FROM WKF-R.
       M22-L01.
           PERFORM M11-READ-WKD THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKEFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKEONL.
           DISPLAY 'R VBKF1320 DWKE ONLY KEY=' WIN2-KEY1
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKDFILE
                 WKEFILE
                 WKFFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF1320 END                                           *
      *----------------------------------------------------------------*
