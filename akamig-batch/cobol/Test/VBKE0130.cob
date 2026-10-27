./ ADD NAME=VBKE0130  10.00 20000809 20000809 145400 00246 00246 TSO220
       ID  DIVISION.
       PROGRAM-ID.     'VBKE0130'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KOUZA FURIKAE SYOKAI          *
      *                                        SIKIBETU KOUSIN         *
      *                                                                *
      *                 * INPUT FILE   : SYOKAI SIKIBETU DATA (DWKB)   *
      *                 * I/O   FILE   : KAISYU JYOUHOU MASTER(SI.KJH) *
      *                                                                *
      *              <> AUTHOR         : CSK-R.TAKAYANAGI              *
      *              <> DATE           : 98/11/11                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-443            ØºÝÊß²Ù  : 00/08/09  CSK-K.ITO           *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(09).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKJH END-EXEC.
       01  KJH REDEFINES DCLKJH.
           COPY VTABKJH SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
                 04 WIN1-NOSCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
                 04 WIN2-NOSCD              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY  =  HIGH-VALUE )
                      OR ( WIN2-KEY  =  HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-KJH THRU M10-EXIT.
       M1-L01.
           OPEN    INPUT  WKBFILE.
           PERFORM M11-OPEN-KJH THRU M11-EXIT.
           PERFORM M12-READ-WKB THRU M12-EXIT.
           CALL   'VSBA2280'   USING P228-PARM.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-KJH                                             *
      *----------------------------------------------------------------*
       M10-LOCK-KJH.
           MOVE '   KJH'   TO P89-TBLID(1).
           MOVE 'X'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : OPEN-KJH                                             *
      *----------------------------------------------------------------*
       M11-OPEN-KJH.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR END-EXEC.
           MOVE ZERO     TO KFSSB OF KJH.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1
              FROM     KJH
              WHERE D1ý=:DCLKJH.D1
              ORDER BY A0,A1
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M111-FETCH-KJH THRU M111-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M11-L01.
           GO TO M11-EXIT.
       M11-L01.
           DISPLAY 'M VBKE0190 TABLE(SI.KJH) DATA 0 KEN !?'
           UPON CONSOLE.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VBKE0190 TABLE(SI.KJH) OPEN ERROR !'
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
              INTO :DCLKJH.A0,:DCLKJH.A1
           END-EXEC.
           MOVE TOKCD OF KJH TO WIN1-TOKCD.
           MOVE NOSCD OF KJH TO WIN1-NOSCD.
           GO TO M111-EXIT.
       M111-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M111-EXIT.
       M111-ERR.
           DISPLAY 'R VBKE0190 TABLE(SI.KJH) FETCH ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKB                                             *
      *----------------------------------------------------------------*
       M12-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKB-TOKCD TO WIN2-TOKCD.
           MOVE WKB-NOSCD TO WIN2-NOSCD.
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
                 PERFORM M22-KJHONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKBONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           PERFORM M211-UPDATE-KJH THRU M211-EXIT.
       M21-L01.
           PERFORM M111-FETCH-KJH  THRU M111-EXIT.
           PERFORM M12-READ-WKB    THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : UPDATE-KJH                                         *
      *----------------------------------------------------------------*
       M211-UPDATE-KJH.
           EXEC SQL WHENEVER SQLERROR   GO TO M211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M211-ERR END-EXEC.
           MOVE ZERO       TO KFSSB  OF KJH.
           MOVE P228-DATEX TO KFSHDT OF KJH.
           EXEC SQL UPDATE    KJH
              SET   D1=:DCLKJH.D1,D2=:DCLKJH.D2
              WHERE A0=:DCLKJH.A0
                AND A1=:DCLKJH.A1
           END-EXEC.
           GO TO M211-EXIT.
       M211-ERR.
           DISPLAY 'R VBKE0130 TABLE(SI.KJH) UPDATE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : KJHFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-KJHONL.
           PERFORM M111-FETCH-KJH THRU M111-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKB ONLY                                             *
      *----------------------------------------------------------------*
       M23-WKBONL.
           PERFORM M12-READ-WKB   THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE   WKBFILE.
           PERFORM M31-CLOSE-KJH THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-KJH                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-KJH.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKE0190 TABLE(SI.KJH) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKE0190 END                                           *
      *----------------------------------------------------------------*
