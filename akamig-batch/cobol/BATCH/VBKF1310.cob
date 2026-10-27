./ ADD NAME=VBKF1310  01.02 20101116 20101117 085102 00247 00279 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKF1310'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYU JYOHOU MASTER NUKIDASI *
      *                                  (KASOU KOUZA)                 *
      *                                                                *
      *                 * INPUT FILE   : KAISYU JYOHOU MASTER          *
      *                                                  (SI.KJH)      *
      *                 * OUTPUT FILE  : KAISYU JYOHOU MASTER          *
      *                                  (TANDOKUSAKI)   (DWKA)        *
      *                                : KAISYU JYOHOU MASTER          *
      *                                  (OYA)           (DWKB)        *
      *                                : KAISYU JYOHOU MASTER          *
      *                                  (KO)            (DWKC)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/11/16                      *
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
           02 FILLER                        PIC  X(40).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(40).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(40).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKJH END-EXEC.
       01  KJH REDEFINES DCLKJH.
           COPY VTABKJH SUPPRESS.
       01  WKA-R.
           02 WKA-KDTKCD                    PIC  X(07).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-KKZNO                     PIC  X(10).
           02 WKA-GHBCD                     PIC  X(04).
           02 FILLER                        PIC  X(10).
       01  WKB-R.
           02 WKB-KDTKCD                    PIC  X(07).
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-KKZNO                     PIC  X(10).
           02 WKB-GHBCD                     PIC  X(04).
           02 FILLER                        PIC  X(10).
       01  WKC-R.
           02 WKC-KDTKCD                    PIC  X(07).
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-KKZNO                     PIC  X(10).
           02 WKC-GHBCD                     PIC  X(04).
           02 FILLER                        PIC  X(10).
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( KJH = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN OUTPUT WKAFILE
                       WKBFILE
                       WKCFILE.
           PERFORM M11-OPEN-KJH THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   KJH'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : OPEN-KJH                                             *
      *----------------------------------------------------------------*
       M11-OPEN-KJH.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1,A3,B3,C7
              FROM    KJH
              ORDER BY A0,A1
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M111-FETCH-KJH THRU M111-EXIT.
           IF KJH = HIGH-VALUE
              GO TO M11-L01.
           GO TO M11-EXIT.
       M11-L01.
           DISPLAY 'M VBKF1310 TABLE(SI.KJH) DATA 0 KEN !?'
           UPON CONSOLE.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VBKF1310 TABLE(SI.KJH) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1 : FETCH-KJH                                          *
      *----------------------------------------------------------------*
       M111-FETCH-KJH.
           EXEC SQL WHENEVER NOT FOUND  GO TO M111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M111-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLKJH.A0,:DCLKJH.A1,:DCLKJH.A3,
                   :DCLKJH.B3,:DCLKJH.C7
           END-EXEC.
           GO TO M111-EXIT.
       M111-L01.
           MOVE HIGH-VALUE TO KJH.
           GO TO M111-EXIT.
       M111-ERR.
           DISPLAY 'R VBKF1310 TABLE(SI.KJH) FETCH ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF KDTKCD OF KJH = SPACE
              PERFORM M21-WRITE-WKA THRU M21-EXIT
           ELSE
              IF ( KDTKCD OF KJH = TOKCD OF KJH )  AND
                 ( NOSCD  OF KJH = SPACE )
                 PERFORM M22-WRITE-WKB THRU M22-EXIT
              ELSE
                 PERFORM M23-WRITE-WKC THRU M23-EXIT.
           PERFORM M111-FETCH-KJH THRU M111-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : WRITE WKA                                            *
      *----------------------------------------------------------------*
       M21-WRITE-WKA.
           MOVE SPACE         TO WKA-R.
           MOVE KDTKCD OF KJH TO WKA-KDTKCD.
           MOVE TOKCD  OF KJH TO WKA-TOKCD.
           MOVE NOSCD  OF KJH TO WKA-NOSCD.
           MOVE FKMICD OF KJH TO WKA-KKZNO.
           IF FKMICD OF KJH NOT = SPACE
              MOVE FGHCD1 OF KJH TO WKA-GHBCD.
           WRITE WKA-REC FROM WKA-R.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WRITE WKB                                            *
      *----------------------------------------------------------------*
       M22-WRITE-WKB.
           MOVE SPACE         TO WKB-R.
           MOVE KDTKCD OF KJH TO WKB-KDTKCD.
           MOVE TOKCD  OF KJH TO WKB-TOKCD.
           MOVE NOSCD  OF KJH TO WKB-NOSCD.
           MOVE FKMICD OF KJH TO WKB-KKZNO.
           IF FKMICD OF KJH NOT = SPACE
              MOVE FGHCD1 OF KJH TO WKB-GHBCD.
           WRITE WKB-REC   FROM WKB-R.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WRITE WKC                                            *
      *----------------------------------------------------------------*
       M23-WRITE-WKC.
           MOVE SPACE         TO WKC-R.
           MOVE KDTKCD OF KJH TO WKC-KDTKCD.
           MOVE TOKCD  OF KJH TO WKC-TOKCD.
           MOVE NOSCD  OF KJH TO WKC-NOSCD.
           WRITE WKC-REC   FROM WKC-R.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE.
           PERFORM M31-CLOSE-KJH THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-KJH                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-KJH.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKF1310 TABLE(SI.KJH) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF1310 END                                           *
      *----------------------------------------------------------------*
