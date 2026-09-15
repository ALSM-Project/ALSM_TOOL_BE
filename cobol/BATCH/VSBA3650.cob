./ ADD NAME=VSBA3650  01.42 20101119 20181106 091239 00228 00318 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VSBA3650'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : KASOU KOUZAYOU GINKOU HOJYOBO *
      *                                  HENKAN                        *
      *                                                                *
      *                 * INPUT.PARM   : P365-PARM                     *
      *                 * OUTPUT.PARM  : P365-PARM                     *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/11/19                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) H-010  Q-6826      ﾍﾝｺｳ   : 13/06/18  CSK-N.EGUCHI        *
      * (02) H-010  O-9042      ﾍﾝｺｳ   : 14/03/12  CSK-N.EGUCHI        *
      * (03) H-010  T-7347      ﾍﾝｺｳ   : 16/06/27  CSK-N.EGUCHI        *
      * (04) H-010  0-10414     ﾍﾝｺｳ   : 18/03/05  CSK-T.TAKADA        *
      * (05) H-010  O-10636     ﾍﾝｺｳ   : 18/11/06  CSK-N.EGUCHI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLTBK END-EXEC.
       01  TBK REDEFINES DCLTBK.
           COPY VTABTBK SUPPRESS.
       01  TABLE-AREA.
           02 WTB1-CODEX.
              03 FILLER                PIC X(41)
                 VALUE '80001800　　　　　　　　　　　　　　　0'.
              03 FILLER                PIC X(41)
                 VALUE '80102800　　　　　　　　　　　　　　　0'.
              03 FILLER                PIC X(41)
                 VALUE '80203020　　　　　　　　　　　　　　　0'.
              03 FILLER                PIC X(41)
                 VALUE '80011800　　　　　　　　　　　　　　　0'.
              03 FILLER                PIC X(41)
                 VALUE '80112800　　　　　　　　　　　　　　　0'.
              03 FILLER                PIC X(41)
                 VALUE '80122800　　　　　　　　　　　　　　　0'.
              03 FILLER                PIC X(41)
                 VALUE '80132800　　　　　　　　　　　　　　　0'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
              03 FILLER                PIC X(41)
                 VALUE '*****************************************'.
           02 WTB1-REDE   REDEFINES WTB1-CODEX.
              03 WTB1-CODE OCCURS 50 TIMES INDEXED BY I.
                 04 WTB1-GHCKA         PIC  X(04).
                 04 WTB1-GHCJI         PIC  X(04).
                 04 FILLER             PIC  X(01).
                 04 WTB1-SEIGN         PIC  X(30).
                 04 FILLER             PIC  X(01).
                 04 WTB1-GISTH         PIC  X(01).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  P365-PARM.
           COPY VPARB365 SUPPRESS.
       PROCEDURE DIVISION USING P365-PARM.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           SET I TO 1.
           SEARCH WTB1-CODE VARYING I
                  AT END
                     MOVE SPACE          TO P365-GHCJI
                     MOVE SPACE          TO P365-BUNO
                     MOVE SPACE          TO P365-STNNO
                     MOVE SPACE          TO P365-SEIGN
                     MOVE SPACE          TO P365-GISTH
                     MOVE '1'            TO P365-RTNCD
                  WHEN WTB1-GHCKA(I) = ALL '*'
                     MOVE SPACE          TO P365-GHCJI
                     MOVE SPACE          TO P365-BUNO
                     MOVE SPACE          TO P365-STNNO
                     MOVE SPACE          TO P365-SEIGN
                     MOVE SPACE          TO P365-GISTH
                     MOVE '1'            TO P365-RTNCD
                  WHEN WTB1-GHCKA(I) = P365-GHCKA
                     MOVE '0'            TO P365-RTNCD
           END-SEARCH.
           PERFORM M10-MAIN THRU M10-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1.0 : MAIN                                                 *
      *----------------------------------------------------------------*
       M10-MAIN.
           IF P365-RTNCD = '0'
              PERFORM M11-SELECT-TBK THRU M11-EXIT
              IF TBK = HIGH-VALUE
                 MOVE SPACE          TO P365-GHCJI
                 MOVE SPACE          TO P365-BUNO
                 MOVE SPACE          TO P365-STNNO
                 MOVE SPACE          TO P365-SEIGN
                 MOVE SPACE          TO P365-GISTH
                 MOVE '1'            TO P365-RTNCD
              ELSE
                 MOVE WTB1-GHCJI(I)  TO P365-GHCJI
                 MOVE BUNO  OF TBK   TO P365-BUNO
                 MOVE STNNO OF TBK   TO P365-STNNO
                 MOVE WTB1-SEIGN(I)  TO P365-SEIGN
                 MOVE WTB1-GISTH(I)  TO P365-GISTH
                 MOVE '0'            TO P365-RTNCD
              END-IF
           END-IF.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : SELECT-TBK                                           *
      *----------------------------------------------------------------*
       M11-SELECT-TBK.
           EXEC SQL WHENEVER NOT FOUND  GO TO M11-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR END-EXEC.
           MOVE WTB1-GHCJI(I) TO GHBCD OF TBK.
           EXEC SQL SELECT C1,C2
              INTO :DCLTBK.C1,:DCLTBK.C2
              FROM    TBK
              WHERE A0=:DCLTBK.A0
           END-EXEC.
           GO TO M11-EXIT.
       M11-L01.
           MOVE HIGH-VALUE TO TBK.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VSBA3650 TABLE(SI.TBK) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VSBA3650 END                                           *
      *----------------------------------------------------------------*
