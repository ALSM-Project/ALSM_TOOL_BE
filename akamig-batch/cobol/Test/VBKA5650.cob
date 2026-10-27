./ ADD NAME=VBKA5650  01.09 20100629 20100928 193208 00192 00182 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5650'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : NOUSEI DETA HANEI             *
      *                                                                *
      *                 * INPUT FILE   : NOUSEI MASTER    (NSMFILE)    *
      *                                : TOKUISAKI DATA SEIKYU BUNSATU *
      *                                                       (DWKA)   *
      *                 * OUTPUT FILE  : TOKUISAKI DATA SEIKYU BUNSATU *
      *                                                       (DWKC)   *
      *              <> AUTHOR         : CSK-M.KODAMA                  *
      *              <> DATE           : 10/06/29                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) YYYYYY YYYYYY      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NSMFILE ASSIGN    TO NSMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-NSM
                          FILE  STATUS S-CODE.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  NSMFILE
           LABEL RECORD STANDARD.
       01  NSM-REC.
           02 KEY-NSM.
              03 KEY-NSM1                   PIC  X(07).
              03 KEY-NSM2                   PIC  X(02).
           02 FILLER                        PIC  X(91).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(20).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(20).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  NSM-R.
           COPY VFILNSM SUPPRESS.
       01  WKA-R.
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-STKCD                     PIC  X(07).
           02 WKA-SHIME                     PIC  9(02).
           02 FILLER                        PIC  X(02).
       01  WKC-R.
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-STKCD                     PIC  X(07).
           02 WKC-SHIME                     PIC  9(02).
           02 FILLER                        PIC  X(02).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD               PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD               PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY1.
              03 WSV2-TOKCD                  PIC  X(07).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE OR
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  NSMFILE
                       WKAFILE
                OUTPUT WKCFILE.
           PERFORM M11-READ-WKA  THRU M11-EXIT.
           PERFORM M12-READ-NSM  THRU M12-EXIT.
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
           MOVE WKA-TOKCD TO WIN1-TOKCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ NSMFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-NSM.
           MOVE 'Z '    TO   S-CODE.
           READ NSMFILE INTO NSM-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKA5650 NSMFILE READ ERROR !!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN2-KEY
              GO TO M12-EXIT.
           MOVE NSM-TOKCD TO WIN2-TOKCD.
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
                 PERFORM M23-NSMONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M211-WRITE-WKC THRU M211-EXIT
                   UNTIL ( WIN2-KEY1 NOT = WSV2-KEY1 ).
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKC                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKC.
           MOVE SPACE       TO WKC-R.
           INITIALIZE          WKC-R.
           MOVE WKA-TOKCD   TO WKC-TOKCD.
           MOVE NSM-NOSCD   TO WKC-NOSCD.
           MOVE WKA-STKCD   TO WKC-STKCD.
           MOVE WKA-SHIME   TO WKC-SHIME.
           WRITE WKC-REC  FROM WKC-R.
           PERFORM M12-READ-NSM THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : NSMFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-NSMONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M12-READ-NSM THRU M12-EXIT
                   UNTIL ( WIN2-KEY1 NOT = WSV2-KEY1 ).
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE NSMFILE
                 WKAFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5650 END                                           *
      *----------------------------------------------------------------*
