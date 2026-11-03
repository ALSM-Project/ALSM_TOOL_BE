./ ADD NAME=VSBA0890  01.00 19920417 19920417 213300 00200 00200 TSO3
       ID  DIVISION.
       PROGRAM-ID.   'VSBA0890'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN SHITEN SYSTEM            *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : DB2 TABLE LOCK ROUTINE           *
      *              <> AUTHOR      : CSK-M.TAKAHASHI                  *
      *              <> DATE        : 89/06/12                         *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) @@@@@@@@@@@@@@@@  ÍÝº³ : @@/@@/@@  CSK-@.@@@@@@@@@        *
      * (02) @@@@@@@@@@@@@@@@   Â²¶ : @@/@@/@@  CSK-@.@@@@@@@@@        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
       01  P90-PARM.
           COPY VPARB90 SUPPRESS.
       01  SQLTEXT.
           49 SQLTEXTL                 PIC S9(04) COMP VALUE +35.
           49 SQLTEXTC                 PIC  X(255).
       01  TABLE-AREA.
           02 WTB1-PARM.
              03 WTB1-TABLE OCCURS 20 TIMES.
                 04 WTB1-TBLID         PIC  X(06).
                 04 WTB1-LTYPE         PIC  X(01).
       01  SAVE-AREA.
           02 WSV1-TABLE.
              03 WSV1-TBLID            PIC  X(06).
              03 FILLER                PIC  X(01).
       01  FLAG-AREA.
           02 WFL1-PROC                PIC  X(01).
       01  EDIT-AREA.
           02 WED1-SQLCD               PIC  -(04).
           02 WED1-INTVL               PIC  ZZZ9.
           02 WED1-TEXT.
              03 FILLER                PIC  X(11) VALUE 'LOCK TABLE '.
              03 WED1-TBLID            PIC  X(07).
              03 FILLER                PIC  X(03) VALUE 'IN '.
              03 WED1-LTYPE            PIC  X(10).
              03 FILLER                PIC  X(04) VALUE 'MODE'.
       01  COUNT-AREA.
           02 WCT1-INTVL               PIC S9(04) COMP.
           02 I                        PIC S9(04) COMP.
           02 J                        PIC S9(04) COMP.
           02 K                        PIC S9(04) COMP.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARM.
           COPY VPARB89 SUPPRESS.
      /
       PROCEDURE DIVISION USING PARM.
      *----------------------------------------------------------------*
      *    MAIN-CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           MOVE PARM TO WTB1-PARM.
           PERFORM M11-SORT THRU M11-EXIT
              VARYING I FROM 1 BY 1
              UNTIL ( I > 19 )
              OR    ( WTB1-TBLID(I) = ALL '*' ).
       M1-L01.
           MOVE 1         TO P90-INTVL.
           MOVE LOW-VALUE TO WSV1-TBLID.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : SORT                                                 *
      *----------------------------------------------------------------*
       M11-SORT.
           ADD 1 I GIVING K.
           PERFORM M11-L01
              VARYING J FROM K BY 1
              UNTIL ( J > 20 )
              OR    ( WTB1-TBLID(J) = ALL '*' ).
           GO TO M11-EXIT.
       M11-L01.
           IF WTB1-TBLID(J) < WTB1-TBLID(I)
              MOVE WTB1-TABLE(J) TO WSV1-TABLE
              MOVE WTB1-TABLE(I) TO WTB1-TABLE(J)
              MOVE WSV1-TABLE    TO WTB1-TABLE(I).
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE ZERO TO WFL1-PROC.
           PERFORM M21-LOCK  THRU M21-EXIT
              VARYING I FROM 1 BY 1
              UNTIL ( I > 20 )
              OR    ( WTB1-TBLID(I) = ALL '*' )
              OR    ( WFL1-PROC NOT = ZERO ).
           IF WFL1-PROC = ZERO
              GO TO M2-EXIT.
           PERFORM M22-ERROR THRU M22-EXIT.
           GO TO M2-MAIN.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1: LOCK                                                  *
      *----------------------------------------------------------------*
       M21-LOCK.
           EXEC SQL WHENEVER SQLERROR   GO TO M21-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M21-ERR END-EXEC.
           MOVE WTB1-TBLID(I) TO WED1-TBLID.
           IF WTB1-LTYPE(I) = 'X'
              MOVE 'EXCLUSIVE' TO WED1-LTYPE.
           IF WTB1-LTYPE(I) = 'S'
              GO TO M21-EXIT.
      *       MOVE 'SHARE    ' TO WED1-LTYPE.
           MOVE WED1-TEXT TO SQLTEXTC.
           EXEC SQL EXECUTE IMMEDIATE :SQLTEXT
           END-EXEC.
           GO TO M21-EXIT.
       M21-ERR.
           MOVE '1'       TO WFL1-PROC.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : ERROR                                                *
      *----------------------------------------------------------------*
       M22-ERROR.
           SUBTRACT 1 FROM I.
           IF SQLCODE = -911 OR -913
              PERFORM M221-COMMIT   THRU M221-EXIT
              PERFORM M222-TIMEOVER THRU M222-EXIT
           ELSE
              PERFORM M223-SQLERROR THRU M223-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : COMMIT                                             *
      *----------------------------------------------------------------*
       M221-COMMIT.
           EXEC SQL WHENEVER SQLERROR   GO TO M221-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M221-ERR END-EXEC.
           EXEC SQL COMMIT END-EXEC.
           GO TO M221-EXIT.
       M221-ERR.
           DISPLAY 'R VSBA0890 SQL COMMIT ERROR ! '
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.2 : TIMEOVER                                           *
      *----------------------------------------------------------------*
       M222-TIMEOVER.
           IF WTB1-TBLID(I) NOT = WSV1-TBLID
              MOVE ZERO          TO WCT1-INTVL
              MOVE WTB1-TBLID(I) TO WSV1-TBLID.
           DIVIDE WCT1-INTVL BY 5 GIVING J REMAINDER K.
           IF K > ZERO
              GO TO M222-L01.
           DISPLAY 'M VSBA0890 LOCK TABLE (' WTB1-TBLID(I) ')'
                   ' HAS FAILED ! '
           UPON CONSOLE.
           IF J = ZERO
              GO TO M222-L01.
           MOVE WCT1-INTVL TO WED1-INTVL.
           DISPLAY '  ... WAITED' WED1-INTVL ' MINUTES ...'
           UPON CONSOLE.
       M222-L01.
           ADD 1 TO WCT1-INTVL.
           CALL 'VSBA0900' USING P90-PARM.
       M222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.3 : SQLERROR                                           *
      *----------------------------------------------------------------*
       M223-SQLERROR.
           DISPLAY 'R VSBA0890 LOCK TABLE (' WTB1-TBLID(I) ')'
                   ' HAS FAILED ! '
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M223-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    END : VSBA0890                                              *
      *----------------------------------------------------------------*
