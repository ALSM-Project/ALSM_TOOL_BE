./ ADD NAME=VSBA2710  10.15 19981021 19981104 083700 00490 00422 TSO67
       ID  DIVISION.
       PROGRAM-ID.     'VSBA2710'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : KOUZA HIKIOTOSHI              *
      *                                  KANREN HIZUKE SYUTOKU         *
      *                                                                *
      *                 * INPUT  PARM  : P271-SYMDO                    *
      *                                : P271-SKDCD                    *
      *                 * OUTPUT PARM  : P271-KZHBI                    *
      *                                : P271-IDDBI                    *
      *                                : P271-KJSBI                    *
      *                                                                *
      *              <> AUTHOR         : CSK-T.NAKAMURA                *
      *              <> DATE           : 98/10/21                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLSKC END-EXEC.
       01  SKC REDEFINES DCLSKC.
           COPY VTABSKC SUPPRESS.
           EXEC SQL INCLUDE VDCLSKD END-EXEC.
       01  SKD REDEFINES DCLSKD.
           COPY VTABSKD SUPPRESS.
       01  P89-PARM.
           COPY VPARB89  SUPPRESS.
       01  P227-PARM.
           COPY VPARB227 SUPPRESS.
       01  P229-PARM.
           COPY VPARB229 SUPPRESS.
       01  P232-PARM.
           COPY VPARB232 SUPPRESS.
       01  P272-PARM.
           COPY VPARB272 SUPPRESS.
       01  EDIT-AREA.
           02 WED1-SYMDO                    PIC  9(06).
           02 WED1-SYMDOX  REDEFINES  WED1-SYMDO.
              03 WED1-SSYY                  PIC  9(04).
              03 WED1-MM                    PIC  9(02).
           02 WED1-MKDBI                    PIC  9(08).
           02 WED1-YKDBI                    PIC  9(08).
           02 WED1-SQLCD                    PIC  ---9.
           02 WED2-SYMDO                    PIC  9(06).
           02 WED2-SYMDOX  REDEFINES  WED2-SYMDO.
              03 WED2-SSYY                  PIC  9(04).
              03 WED2-MM                    PIC  9(02).
           02 WED3-SYMD                     PIC  9(08).
           02 WED3-SYMDX  REDEFINES  WED3-SYMD.
              03 WED3-SYM                   PIC  9(06).
              03 WED3-DD                    PIC  9(02).
       01  COUNT-AREA.
           02 I                             PIC S9(04) COMP.
           02 J                             PIC S9(04) COMP.
           02 K                             PIC S9(04) COMP.
           02 L                             PIC S9(04) COMP.
           02 WCT1-CALST                    PIC S9(04) COMP.
           02 WCT1-DD                       PIC S9(04) COMP.
           02 WCT1-MK                       PIC S9(04) COMP.
           02 WCT1-YK                       PIC S9(04) COMP.
           02 WCT1-I                        PIC S9(04) COMP.
           02 WCT1-J                        PIC S9(04) COMP.
           02 WCT2-MK                       PIC S9(04) COMP.
           02 WCT2-YK                       PIC S9(04) COMP.
       01  FLAG-AREA.
           02 WFL1-KZHBI                    PIC  X(01).
           02 WFL1-MKDBI                    PIC  X(01).
           02 WFL1-YKDBI                    PIC  X(01).
           02 WFL1-IDDBI                    PIC  X(01).
           02 WFL1-KJSBI                    PIC  X(01).
           02 WFL1-IDDCK                    PIC  X(01).
           02 WFL1-KJSCK                    PIC  X(01).
           02 WFL1-CALTB                    PIC  X(01).
       01  TABLE-AREA.
           02 WTB1-CLEND         OCCURS  93.
              03 WTB1-SYMD.
                 04 WTB1-SYM                PIC  9(06).
                 04 WTB1-DD                 PIC  9(02).
              03 WTB1-KDBKB                 PIC  X(01).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB271.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK THRU M10-EXIT.
           MOVE LOW-VALUE  TO TABLE-AREA.
           MOVE P271-SYMDO TO WED1-SYMDO.
           MOVE ZERO TO P271-KZHBI.
           MOVE ZERO TO P271-IDDBI.
           MOVE ZERO TO P271-KJSBI.
           MOVE ZERO TO P227-LOADS.
           MOVE ZERO TO P232-LOADS.
           MOVE ZERO TO P229-LOADS.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK                                                 *
      *----------------------------------------------------------------*
       M10-LOCK.
           MOVE '   SKC'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE '   SKD'   TO P89-TBLID(2).
           MOVE 'S'        TO P89-LTYPE(2).
           MOVE ALL '*'    TO P89-TABLE(3).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           PERFORM M21-SELECT-SKD   THRU M21-EXIT.
           IF DCLSKD = HIGH-VALUE
              GO TO M2-EXIT
           END-IF.
           MOVE P271-SYMDO TO WED2-SYMDO.
           PERFORM M22-CALTBL-EDIT  THRU M22-EXIT.
           IF TABLE-AREA = LOW-VALUE
              GO TO M2-EXIT
           END-IF.
           PERFORM M23-KZHBI-SEARCH THRU M23-EXIT.
           IF WFL1-KZHBI = ZERO
              GO TO M2-EXIT
           END-IF.
           PERFORM M24-IDDBI-SEARCH THRU M24-EXIT.
           PERFORM M25-KJSBI-SEARCH THRU M25-EXIT.
           PERFORM M26-SUZCAL-CHECK THRU M26-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : SELECT SKD                                           *
      *----------------------------------------------------------------*
       M21-SELECT-SKD.
           EXEC SQL WHENEVER NOT FOUND  GO TO M21-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M21-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M21-ERR END-EXEC.
           MOVE P271-SKDCD TO SKDKCD OF SKD.
           EXEC SQL SELECT A4,A5,A6,A7
              INTO :DCLSKD.A4,:DCLSKD.A5,:DCLSKD.A6,:DCLSKD.A7
              FROM    SKD
              WHERE A0=:DCLSKD.A0
           END-EXEC.
           GO TO M21-EXIT.
       M21-L01.
           MOVE HIGH-VALUE TO DCLSKD.
           GO TO M21-EXIT.
       M21-ERR.
           DISPLAY 'R VSBA2710 TABLE(SI.SKD) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : CALENDER TABLE EDIT                                  *
      *----------------------------------------------------------------*
       M22-CALTBL-EDIT.
           PERFORM M221-SELECT-SKC THRU M221-EXIT.
           IF DCLSKC = HIGH-VALUE
              GO TO M22-EXIT
           END-IF.
           MOVE 32 TO WCT1-CALST.
           PERFORM M222-CALTBL-SET THRU M222-EXIT.
           IF WED1-MM = 1
              COMPUTE WED2-SSYY = WED2-SSYY - 1
              MOVE 12      TO WED2-MM
           ELSE
              MOVE WED1-SYMDO TO WED2-SYMDO
              COMPUTE WED2-MM = WED2-MM - 1
           END-IF.
           PERFORM M221-SELECT-SKC THRU M221-EXIT.
           IF DCLSKC NOT = HIGH-VALUE
              MOVE 1 TO WCT1-CALST
              PERFORM M222-CALTBL-SET THRU M222-EXIT
           END-IF.
           IF WED1-MM = 12
              COMPUTE WED2-SSYY = WED2-SSYY + 1
              MOVE 1       TO WED2-MM
           ELSE
              MOVE WED1-SYMDO TO WED2-SYMDO
              COMPUTE WED2-MM = WED2-MM + 1
           END-IF.
           PERFORM M221-SELECT-SKC THRU M221-EXIT.
           IF DCLSKC NOT = HIGH-VALUE
              MOVE 63 TO WCT1-CALST
              PERFORM M222-CALTBL-SET THRU M222-EXIT
           END-IF.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : SELECT SKC                                        *
      *----------------------------------------------------------------*
       M221-SELECT-SKC.
           EXEC SQL WHENEVER NOT FOUND  GO TO M221-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M221-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M221-ERR END-EXEC.
           MOVE P271-SKDCD TO SKDKCD OF SKC.
           MOVE WED2-SYMDO TO NESYM  OF SKC.
           EXEC SQL SELECT A2
              INTO :DCLSKC.A2
              FROM    SKC
              WHERE A0=:DCLSKC.A0
                AND A1=:DCLSKC.A1
           END-EXEC.
           GO TO M221-EXIT.
       M221-L01.
           MOVE HIGH-VALUE TO DCLSKC.
           GO TO M221-EXIT.
       M221-ERR.
           DISPLAY 'R VSBA2710 TABLE(SI.SKC) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.2 : CALTBL SET                                         *
      *----------------------------------------------------------------*
       M222-CALTBL-SET.
           MOVE 1 TO WCT1-DD.
           PERFORM VARYING I FROM WCT1-CALST BY 1
              UNTIL I > WCT1-CALST + 30
              MOVE NESYM OF SKC           TO WTB1-SYM(I)
              MOVE WCT1-DD                TO WTB1-DD(I)
              MOVE CLENDX OF SKC(WCT1-DD) TO WTB1-KDBKB(I)
              ADD 1 TO WCT1-DD
           END-PERFORM.
       M222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : KZHBI SEARCH                                         *
      *----------------------------------------------------------------*
       M23-KZHBI-SEARCH.
           MOVE ZERO TO WFL1-MKDBI.
           MOVE ZERO TO WFL1-YKDBI.
           MOVE ZERO TO WFL1-KZHBI.
           IF HKOBI OF SKD = '29' OR '30' OR '31'
              MOVE WED1-SYMDO   TO WED3-SYM
              MOVE HKOBI OF SKD TO WED3-DD
              MOVE WED3-SYMD    TO P272-SYMD
              CALL 'VSBA2720' USING P272-PARM
              COMPUTE J = P272-DD + 31
           ELSE
              COMPUTE J = HKOBI OF SKD + 31
           END-IF.
           IF J > 93
              GO TO M23-EXIT
           END-IF.
       M23-L01.
           IF WTB1-KDBKB(J) = ZERO
              MOVE WTB1-SYMD(J) TO P271-KZHBI
              MOVE '1' TO WFL1-KZHBI
              MOVE J   TO WCT1-I
           ELSE
              IF KYTRF OF SKD = '1'
                 COMPUTE WCT1-MK = J - 1
                 PERFORM M231-MKDBI-SEARCH THRU M231-EXIT
                 IF WFL1-MKDBI NOT = ZERO
                    MOVE WCT1-MK TO WCT1-I
                    MOVE WED1-MKDBI TO P271-KZHBI
                    MOVE '1' TO WFL1-KZHBI
                 END-IF
              ELSE
                 IF KYTRF OF SKD = '2'
                    COMPUTE WCT1-YK = J + 1
                    PERFORM M232-YKDBI-SEARCH THRU M232-EXIT
                    IF WFL1-YKDBI NOT = ZERO
                       MOVE WCT1-YK TO WCT1-I
                       MOVE WED1-YKDBI TO P271-KZHBI
                       MOVE '1' TO WFL1-KZHBI
                    END-IF
                 END-IF
              END-IF
           END-IF.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : MKDBI SEARCH                                       *
      *----------------------------------------------------------------*
       M231-MKDBI-SEARCH.
           PERFORM UNTIL WCT1-MK NOT > ZERO
                      OR WFL1-MKDBI NOT = ZERO
              IF WTB1-KDBKB(WCT1-MK) = ZERO
                 MOVE '1'                TO WFL1-MKDBI
                 MOVE WTB1-SYMD(WCT1-MK) TO WED1-MKDBI
              ELSE
                 COMPUTE WCT1-MK = WCT1-MK - 1
              END-IF
           END-PERFORM.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.2 : YKDBI SEARCH                                       *
      *----------------------------------------------------------------*
       M232-YKDBI-SEARCH.
           PERFORM UNTIL WCT1-YK > 93 OR
                         WFL1-YKDBI NOT = ZERO
              IF WTB1-KDBKB(WCT1-YK) = ZERO
                 MOVE '1' TO WFL1-YKDBI
                 MOVE WTB1-SYMD(WCT1-YK) TO WED1-YKDBI
              ELSE
                 COMPUTE WCT1-YK = WCT1-YK + 1
              END-IF
           END-PERFORM.
       M232-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.4 : IDDBI SEARCH                                         *
      *----------------------------------------------------------------*
       M24-IDDBI-SEARCH.
           MOVE ZERO TO WFL1-IDDBI.
           IF IRDBI OF SKD = ZERO
              MOVE WTB1-SYMD(WCT1-I) TO P271-IDDBI
              MOVE '1'               TO WFL1-IDDBI
              GO TO M24-EXIT
           END-IF.
           COMPUTE WCT1-J = WCT1-I - 1.
           MOVE 1 TO WCT2-MK.
           PERFORM VARYING K FROM WCT1-J BY -1
              UNTIL K NOT > ZERO
                 OR WFL1-IDDBI NOT = ZERO
                 IF WTB1-KDBKB(K) = ZERO
                    IF WCT2-MK = IRDBI OF SKD
                       MOVE '1' TO WFL1-IDDBI
                       MOVE WTB1-SYMD(K) TO P271-IDDBI
                    ELSE
                       ADD 1 TO WCT2-MK
                    END-IF
                 END-IF
           END-PERFORM.
       M24-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.5 : KJSBI SEARCH                                         *
      *----------------------------------------------------------------*
       M25-KJSBI-SEARCH.
           MOVE ZERO TO WFL1-KJSBI.
           IF KJSBI OF SKD = ZERO
              MOVE WTB1-SYMD(WCT1-I) TO P271-KJSBI
              MOVE '1'               TO WFL1-KJSBI
              GO TO M25-EXIT
           END-IF.
           COMPUTE WCT1-J = WCT1-I + 1.
           MOVE 1 TO WCT2-YK.
           PERFORM VARYING K FROM WCT1-J BY 1
              UNTIL K > 93
                 OR WFL1-KJSBI NOT = ZERO
                 IF WTB1-KDBKB(K) = ZERO
                    IF WCT2-YK = KJSBI OF SKD
                       MOVE '1' TO WFL1-KJSBI
                       MOVE WTB1-SYMD(K) TO P271-KJSBI
                    ELSE
                       ADD 1 TO WCT2-YK
                    END-IF
                 END-IF
           END-PERFORM.
       M25-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.6 : SUZCAL CHECK                                         *
      *----------------------------------------------------------------*
       M26-SUZCAL-CHECK.
           IF WFL1-IDDBI NOT = ZERO
              MOVE ZERO TO WFL1-IDDCK
              PERFORM M261-IDDBI-CHECK THRU M261-EXIT
                 UNTIL WFL1-IDDCK NOT = ZERO
           END-IF.
           IF WFL1-KJSBI NOT = ZERO
              MOVE ZERO TO WFL1-KJSCK
              PERFORM M262-KJSBI-CHECK THRU M262-EXIT
                 UNTIL WFL1-KJSCK NOT = ZERO
           END-IF.
       M26-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.6.1 : IDDBI CHECK                                        *
      *----------------------------------------------------------------*
       M261-IDDBI-CHECK.
           MOVE P271-IDDBI TO P227-DATE.
           CALL 'VSBA2270' USING P227-PARM.
           IF P227-SIGN = ZERO
              MOVE '1' TO WFL1-IDDCK
              GO TO M261-EXIT
           END-IF.
       M261-L01.
           MOVE P271-IDDBI TO P232-DATE.
           CALL 'VSBA2320' USING P232-PARM.
           MOVE ZERO TO WFL1-CALTB.
           PERFORM VARYING L FROM 1 BY 1
              UNTIL L > 93
                 OR WFL1-CALTB NOT = ZERO
              IF P232-DATE = WTB1-SYMD(L)
                 IF WTB1-KDBKB(L) = ZERO
                    MOVE P232-DATE TO P271-IDDBI
                    MOVE '1'       TO WFL1-IDDCK
                    MOVE '1'       TO WFL1-CALTB
                 ELSE
                    MOVE ZERO      TO WFL1-MKDBI
                    COMPUTE WCT1-MK = L - 1
                    PERFORM M231-MKDBI-SEARCH THRU M231-EXIT
                    IF WFL1-MKDBI = ZERO
                       MOVE ZERO       TO P271-IDDBI
                       MOVE '1'        TO WFL1-IDDCK
                       MOVE '1'        TO WFL1-CALTB
                    ELSE
                       MOVE WED1-MKDBI TO P271-IDDBI
                       MOVE '1'        TO WFL1-CALTB
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.
           IF WFL1-CALTB = ZERO
              MOVE ZERO TO P271-IDDBI
              MOVE '1'  TO WFL1-IDDCK
           END-IF.
       M261-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.6.2 : KJSBI CHECK                                        *
      *----------------------------------------------------------------*
       M262-KJSBI-CHECK.
           MOVE P271-KJSBI TO P227-DATE.
           CALL 'VSBA2270' USING P227-PARM.
           IF P227-SIGN = ZERO
              MOVE '1' TO WFL1-KJSCK
              GO TO M262-EXIT
           END-IF.
       M262-L01.
           MOVE P271-KJSBI TO P229-DATE.
           MOVE 1          TO P229-YOKU.
           CALL 'VSBA2290' USING P229-PARM.
           MOVE ZERO TO WFL1-CALTB.
           PERFORM VARYING L FROM 1 BY 1
              UNTIL L > 93
                 OR WFL1-CALTB NOT = ZERO
              IF P229-DATE = WTB1-SYMD(L)
                 IF WTB1-KDBKB(L) = ZERO
                    MOVE P229-DATE TO P271-KJSBI
                    MOVE '1'       TO WFL1-KJSCK
                    MOVE '1'       TO WFL1-CALTB
                 ELSE
                    MOVE ZERO TO WFL1-YKDBI
                    COMPUTE WCT1-YK = L + 1
                    PERFORM M232-YKDBI-SEARCH THRU M232-EXIT
                    IF WFL1-YKDBI = ZERO
                       MOVE ZERO       TO P271-KJSBI
                       MOVE '1'        TO WFL1-KJSCK
                       MOVE '1'        TO WFL1-CALTB
                    ELSE
                       MOVE WED1-YKDBI TO P271-KJSBI
                       MOVE '1'        TO WFL1-CALTB
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.
           IF WFL1-CALTB = ZERO
              MOVE ZERO TO P271-KJSBI
              MOVE '1'  TO WFL1-KJSCK
           END-IF.
       M262-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VSBA2710 END                                           *
      *----------------------------------------------------------------*
