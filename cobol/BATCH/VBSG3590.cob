./ ADD NAME=VBSG3590  01.10 20021202 20030416 191957 00281 00268 MCSK044
       ID  DIVISION.
       PROGRAM-ID.     'VBSG3590'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : S                             *
      *              <> PG.OUTLINE     : HANBAI KAKAKU FILE(KANRIBUN)  *
      *                                  SAKUSEI                       *
      *                 * INPUT FILE   : KAKAKU KANRI SYOUHIN FILE     *
      *                                                      (SI.KAS)  *
      *                 * INPUT FILE   : HANBAI KAKAKU FILE(ZENRYOU)   *
      *                                                      (DWKA)    *
      *                 * OUTPUT FILE  : HANBAI KAKAKU FILE(KANRIBUN)  *
      *                                                      (DWKB)    *
      *              <> AUTHOR         : CSK-T.SUMI                    *
      *              <> DATE           : 02/12/02                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
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
           02 FILLER                        PIC  X(51).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(51).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKAS END-EXEC.
       01  KAS REDEFINES DCLKAS.
           COPY VTABKAS SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  WKA-R.
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-SYOCD                     PIC  X(09).
           02 WKA-TKYBI                     PIC  X(08).
           02 WKA-KETKB                     PIC  X(01).
           02 WKA-HTANK                     PIC S9(09)  COMP-3.
           02 WKA-SMHBK                     PIC S9(09)  COMP-3.
           02 WKA-KEIKB                     PIC  X(01).
           02 WKA-KEINO                     PIC  X(10).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-SYOKB                     PIC  X(01).
           02 WKA-KNSKB                     PIC  X(01).
           02 WKA-KKHKB                     PIC  X(01).
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-SYOCD                     PIC  X(09).
           02 WKB-TKYBI                     PIC  X(08).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-HTANK                     PIC S9(09)  COMP-3.
           02 WKB-SMHBK                     PIC S9(09)  COMP-3.
           02 WKB-KEIKB                     PIC  X(01).
           02 WKB-KEINO                     PIC  X(10).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-SYOKB                     PIC  X(01).
           02 WKB-KNSKB                     PIC  X(01).
           02 WKB-KKHKB                     PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-TOKCD                 PIC  X(07).
              03 WIN1-SYOCD                 PIC  X(09).
           02 WIN2-KEY.
              03 WIN2-TOKCD                 PIC  X(07).
              03 WIN2-SYOCD                 PIC  X(09).
       01  SAVE-AREA.
           02 WSV1-KEIKB                    PIC  X(01).
           02 WSV2-KEY.
              03 WSV2-TOKCD                 PIC  X(07).
              03 WSV2-SYOCD                 PIC  X(09).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WIN2-KEY = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-KAS THRU M10-EXIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE.
           PERFORM M11-OPEN-KAS THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-KAS                                             *
      *----------------------------------------------------------------*
       M10-LOCK-KAS.
           MOVE '   KAS'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : OPEN-KAS                                             *
      *----------------------------------------------------------------*
       M11-OPEN-KAS.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR     END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR     END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1
              FROM    KAS
              ORDER BY A0,A1
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M111-FETCH-KAS THRU M111-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M11-L01.
           GO TO M11-EXIT.
       M11-L01.
           DISPLAY 'M VBSG3590 TABLE(SI.KAS) DATA 0 KEN?!'
           UPON CONSOLE.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VBSG3590 TABLE(SI.KAS) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1 : FETCH-KAS                                          *
      *----------------------------------------------------------------*
       M111-FETCH-KAS.
           EXEC SQL WHENEVER NOT FOUND  GO TO M111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M111-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLKAS
           END-EXEC.
           MOVE KIKSK OF KAS TO WIN1-TOKCD.
           MOVE SYOCD OF KAS TO WIN1-SYOCD.
           GO TO M111-EXIT.
       M111-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M111-EXIT.
       M111-ERR.
           DISPLAY 'R VBSG3590 TABLE(SI.KAS) FETCH  ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKA-TOKCD TO WIN2-TOKCD.
           MOVE WKA-SYOCD TO WIN2-SYOCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY < WIN2-KEY
                 PERFORM M22-KASONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY TO WSV2-KEY.
           MOVE WKA-KEIKB TO WSV1-KEIKB.
           PERFORM M211-WRITE-WKB THRU M211-EXIT
                        UNTIL WIN2-KEY NOT = WSV2-KEY.
           PERFORM M111-FETCH-KAS THRU M111-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKBFILE                                      *
      *----------------------------------------------------------------*
       M211-WRITE-WKB.
           PERFORM M2111-WRITE THRU M2111-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : WRITE                                            *
      *----------------------------------------------------------------*
       M2111-WRITE.
           MOVE WKA-R TO WKB-R.
           IF WKA-KEIKB = SPACE
              MOVE WSV1-KEIKB TO WKB-KEIKB.
           IF WKA-KNSKB = SPACE
              MOVE '1'        TO WKB-KNSKB.
           WRITE WKB-REC FROM WKB-R.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : KASFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-KASONL.
           PERFORM M111-FETCH-KAS THRU M111-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
           MOVE WIN2-KEY TO WSV2-KEY.
           MOVE WKA-KEIKB TO WSV1-KEIKB.
           PERFORM M231-WRITE-WKB THRU M231-EXIT
                        UNTIL WIN2-KEY NOT = WSV2-KEY.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : WRITE WKBFILE                                      *
      *----------------------------------------------------------------*
       M231-WRITE-WKB.
           IF WKA-KNSKB = '1'
              PERFORM M2111-WRITE THRU M2111-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE.
           PERFORM M31-CLOSE-KAS THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-KAS                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-KAS.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBSG3590 TABLE(SI.KAS) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBSG3590 END                                         *
      *----------------------------------------------------------------*
