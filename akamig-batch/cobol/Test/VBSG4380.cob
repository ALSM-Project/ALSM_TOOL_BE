./ ADD NAME=VBSG4380  01.01 20180912 20180912 141222 00185 00186 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBSG4380'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : S                             *
      *              <> PG.OUTLINE     : HANBAI KAKAKU FILE(ZENRYO) TO *
      *                                   KAKAKU IKKATU KANRISAKI NO   *
      *                                   HIMODUKE                     *
      *                                                                *
      *                 * INPUT FILE   : HANBAI KAKAKU FILE      (DWKA)*
      *                                    (ZENRYO)                    *
      *                                : KAKAKU KANRI GROUP FILE       *
      *                                                        (SI.KAG)*
      *                 * OUTPUT FILE  : HANBAI KAKAKU(ZENRYO)(KAKAKU  *
      *                                    IKKATU KANRISAKI TUKI)(DWKC)*
      *                                                                *
      *              <> AUTHOR         : CSK-N.EGUCHI                  *
      *              <> DATE           : 18/09/12                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(51).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(58).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKAG END-EXEC.
       01  KAG REDEFINES DCLKAG.
           COPY VTABKAG SUPPRESS.
       01  P89-PARM.
           COPY VPARB89  SUPPRESS.
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
       01  WKC-R.
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-TKYBI                     PIC  X(08).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-HTANK                     PIC S9(09)  COMP-3.
           02 WKC-SMHBK                     PIC S9(09)  COMP-3.
           02 WKC-KEIKB                     PIC  X(01).
           02 WKC-KEINO                     PIC  X(10).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-SYOKB                     PIC  X(01).
           02 WKC-KNSKB                     PIC  X(01).
           02 WKC-KKHKB                     PIC  X(01).
           02 WKC-KIKSK                     PIC  X(07).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-TOKCD                 PIC  X(07).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WIN1-KEY = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKCFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   KAG'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKA-TOKCD          TO WIN1-TOKCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE WKA-TOKCD  TO SIRKN OF KAG.
           PERFORM M21-SELECT-KAG THRU M21-EXIT.
       M2-L01.
           MOVE WIN1-KEY1    TO WSV1-KEY1.
           PERFORM M22-WRITE-WKC  THRU M22-EXIT
              UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : SELECT-KAG                                           *
      *----------------------------------------------------------------*
       M21-SELECT-KAG.
           EXEC SQL WHENEVER NOT FOUND  GO TO M21-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M21-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M21-ERR END-EXEC.
           EXEC SQL SELECT A0
              INTO :DCLKAG.A0
              FROM    KAG
              WHERE A1=:DCLKAG.A1
           END-EXEC.
           GO TO M21-EXIT.
       M21-L01.
           MOVE HIGH-VALUE   TO KAG.
           GO TO M21-EXIT.
       M21-ERR.
           DISPLAY 'R VBSG4380 TABLE(SI.KAG) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WRITE-WKC                                            *
      *----------------------------------------------------------------*
       M22-WRITE-WKC.
           IF KAG = HIGH-VALUE
              GO TO M22-L01.
           MOVE WKA-R             TO WKC-R.
           MOVE KIKSK OF KAG      TO WKC-KIKSK.
           WRITE WKC-REC FROM WKC-R.
       M22-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBSG4380 END                                         *
      *----------------------------------------------------------------*
