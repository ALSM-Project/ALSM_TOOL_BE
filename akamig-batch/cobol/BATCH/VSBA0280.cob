./ ADD NAME=VSBA0280  01.03 20020822 20180511 100607 00252 00432 MCSK004
       ID  DIVISION.
       PROGRAM-ID.      'VSBA0280'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : SYOHIN MASTER READ               *
      *                                                                *
      *                 *  INPUT.PARM  : READ TYPE / KEY               *
      *                                                                *
      *                 *  OUTPUT.PARM : SYOHIN MASTER DATA            *
      *                                                                *
      *              <> AUTHOR      : CSK-A.ONITSUKA                   *
      *              <> DATE        : 84/09/13                         *
      *----------------------------------------------------------------*
      * (01) K-02066            ÍÝº³   : 18/05/11  CSK-N.EGUCHI        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  SM1FILE ASSIGN         SM1FILE
                           ORGANIZATION INDEXED
                           ACCESS MODE  DYNAMIC
                           RECORD KEY   KEY-SM1
                           FILE STATUS  S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  SM1FILE
           LABEL RECORD STANDARD.
       01  SM1-REC.
           02  KEY-SM1         PIC X(09).
           02  FILLER          PIC X(251).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       77  S-CODE              PIC  X(2).
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLSM1 END-EXEC.
       01  SM1 REDEFINES DCLSM1.
           COPY VTABSM1 SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  EDIT-AREA.
           02 WED1-SQLCD       PIC  ---9.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB28  SUPPRESS.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    1.0 : MAIN CONTROL                                          *
      *----------------------------------------------------------------*
       M10-MAIN-CONTROL.
           MOVE  ZERO  TO  P28-RTNCD.
           IF  P28-OPCLK  =  'O'
               PERFORM  M11-OPEN-PROC   THRU  M11-EXIT
           ELSE  IF  P28-OPCLK  =  ' '
                     PERFORM  M12-READ-PROC   THRU  M12-EXIT
           ELSE  IF  P28-OPCLK  =  'C'
                     PERFORM  M13-CLOSE-PROC  THRU  M13-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    1.1 : OPEN  SYORI                                           *
      *----------------------------------------------------------------*
       M11-OPEN-PROC.
           IF  P28-TYPAC  =  'S'
               MOVE  LOW-VALUE  TO  P28-KEY
               MOVE  'B'        TO  P28-TYPAC.
           IF  (P28-TYPRD  =  '1') AND
               (P28-TYPKY  =  '0')
               OPEN  INPUT  SM1FILE
               IF S-CODE NOT = ZERO
                  PERFORM M111-ABEND1 THRU M111-EXIT.
           IF  P28-TYPE  =  '10B'
               MOVE  P28-SYOCD  TO  KEY-SM1
               START  SM1FILE
                      KEY  NOT  <  KEY-SM1
               IF S-CODE NOT = ZERO
                  PERFORM M113-ABEND3 THRU M113-EXIT.
           IF  P28-TYPE  =  '11B'
               CONTINUE.
           MOVE  LOW-VALUE  TO  SMS-R.
           MOVE  SPACE      TO  P28-OPCLK.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    OPEN  ABEND  RTN                                            *
      *----------------------------------------------------------------*
       M111-ABEND1.
           DISPLAY 'R VSBA0280 SM1FILE OPEN ERROR ? S-CODE=' S-CODE
              UPON CONSOLE.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       M113-ABEND3.
           DISPLAY 'R VSBA0280 SM1FILE START ERROR ? S-CODE=' S-CODE
              ' TYPE=' P28-TYPE ' KEY=' P28-SYOCD
              UPON CONSOLE.
           IF (P28-TYPRD = '1') AND (P28-TYPKY  =  '0')
              CLOSE SM1FILE.
           CALL 'VSBABEND'.
       M113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.2 : READ  SYORI                                           *
      *----------------------------------------------------------------*
       M12-READ-PROC.
           IF  P28-TYPE  =  '10R'
               PERFORM  M121-SM1-READ  THRU  M121-EXIT.
           IF  P28-TYPE  =  '11R'
               PERFORM  M122-ID1-READ  THRU  M122-EXIT.
           IF  P28-TYPE  =  '10B'
               PERFORM  M123-SM1-NEXT  THRU  M123-EXIT.
           IF  P28-TYPE  =  '11B'
               CONTINUE.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.2.1 : SM1FILE READ                                        *
      *----------------------------------------------------------------*
       M121-SM1-READ.
           MOVE  P28-SYOCD  TO  KEY-SM1.
           READ  SM1FILE  INTO  P28-SM1
                 INVALID  KEY   MOVE  '1'  TO  P28-RTNCD.
       M121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.2.2 : ID1FILE READ                                        *
      *----------------------------------------------------------------*
       M122-ID1-READ.
           PERFORM  M1220-LOCK-SM1    THRU  M1220-EXIT.
           MOVE  P28-SYOCD  TO  TSYCD OF SM1.
           PERFORM  M1221-SELECT-SM1  THRU  M1221-EXIT.
           IF P28-RTNCD NOT = ZERO
              GO TO M122-EXIT.
           PERFORM  M1222-MOVE-SM1    THRU  M1222-EXIT.
       M122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.2.0 : LOCK-SM1                                         *
      *----------------------------------------------------------------*
       M1220-LOCK-SM1.
           MOVE '   SM1'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M1220-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.2.1 : SELECT-SM1                                       *
      *----------------------------------------------------------------*
       M1221-SELECT-SM1.
           EXEC SQL WHENEVER NOT FOUND  GO TO M1221-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M1221-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M1221-ERR END-EXEC.
           EXEC SQL SELECT A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,
                           B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,
                           C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,
                           D0,D1,D2,D3,D4,D7,D8,D9,
                           E0,E1,E2,E3,E4,E5,E6,E7
              INTO :DCLSM1.A0,:DCLSM1.A1,:DCLSM1.A2,:DCLSM1.A3,
                   :DCLSM1.A4,:DCLSM1.A5,:DCLSM1.A6,:DCLSM1.A7,
                   :DCLSM1.A8,:DCLSM1.A9,:DCLSM1.B0,:DCLSM1.B1,
                   :DCLSM1.B2,:DCLSM1.B3,:DCLSM1.B4,:DCLSM1.B5,
                   :DCLSM1.B6,:DCLSM1.B7,:DCLSM1.B8,:DCLSM1.B9,
                   :DCLSM1.C0,:DCLSM1.C1,:DCLSM1.C2,:DCLSM1.C3,
                   :DCLSM1.C4,:DCLSM1.C5,:DCLSM1.C6,:DCLSM1.C7,
                   :DCLSM1.C8,:DCLSM1.C9,:DCLSM1.D0,:DCLSM1.D1,
                   :DCLSM1.D2,:DCLSM1.D3,:DCLSM1.D4,:DCLSM1.D7,
                   :DCLSM1.D8,:DCLSM1.D9,:DCLSM1.E0,:DCLSM1.E1,
                   :DCLSM1.E2,:DCLSM1.E3,:DCLSM1.E4,:DCLSM1.E5,
                   :DCLSM1.E6,:DCLSM1.E7
              FROM    SM1
              WHERE A1=:DCLSM1.A1
           END-EXEC.
           GO TO M1221-EXIT.
       M1221-L01.
           MOVE  '1'  TO  P28-RTNCD.
           GO TO M1221-EXIT.
       M1221-ERR.
           DISPLAY 'R VSBA0280 TABLE(SI.SM1) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM  M13-CLOSE-PROC  THRU  M13-EXIT.
           CALL 'VSBABEND'.
       M1221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.2.2.2 : MOVE-SM1                                          *
      *----------------------------------------------------------------*
       M1222-MOVE-SM1.
           MOVE SPACE  TO P28-SM1.
           INITIALIZE     P28-SM1.
           MOVE SYOCD  OF SM1  TO  SMS-HSYCD.
           MOVE TSYCD  OF SM1  TO  SMS-HTSCD.
           MOVE SYOMH  OF SM1  TO  SMS-SYOMH.
           MOVE SYOMK  OF SM1  TO  SMS-SYOMK.
           MOVE SYOMY  OF SM1  TO  SMS-SYOMY.
           MOVE MAKCD  OF SM1  TO  SMS-MAKCD.
           MOVE GOJUN  OF SM1  TO  SMS-GOJUN.
           MOVE SBMMK  OF SM1  TO  SMS-SBMMK.
           MOVE HOSOT  OF SM1  TO  SMS-HOSO.
           MOVE KANSN  OF SM1  TO  SMS-KANSAN.
           MOVE YAYMD  OF SM1  TO  SMS-YAYMD.
           MOVE TYAKN  OF SM1  TO  SMS-TYAKN.
           MOVE TYAKO  OF SM1  TO  SMS-TYAKO.
           MOVE KCYGM  OF SM1  TO  SMS-KCYGM.
           MOVE HOUKB  OF SM1  TO  SMS-HOUKB.
           MOVE TORIA  OF SM1  TO  SMS-TORIA.
           MOVE GIGYO  OF SM1  TO  SMS-GIGYO.
           MOVE SMKUH  OF SM1  TO  SMS-SMKUH.
           MOVE KKSCD  OF SM1  TO  SMS-KKSCD.
           MOVE KONPO  OF SM1  TO  SMS-KONPO.
           MOVE TOYMD  OF SM1  TO  SMS-AYMD.
           MOVE HEYMD  OF SM1  TO  SMS-RYMD.
           MOVE HAYMD  OF SM1  TO  SMS-DYMD.
           MOVE SHMKK  OF SM1  TO  SMS-SYHKK.
           MOVE SKYKK  OF SM1  TO  SMS-SYKYKK.
           MOVE HTYMD  OF SM1  TO  SMS-HTYMD.
           MOVE KSSCD  OF SM1  TO  SMS-KSSCD.
           MOVE ANAYK  OF SM1  TO  SMS-ANYAKO.
           MOVE SYAKO  OF SM1  TO  SMS-SYAKO.
           MOVE JANCD  OF SM1  TO  SMS-JANCD.
       M1222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.2.3 : SM1FILE READ NEXT                                   *
      *----------------------------------------------------------------*
       M123-SM1-NEXT.
           READ  SM1FILE  NEXT  RECORD  INTO  P28-SM1
                 AT  END  MOVE  '2'  TO  P28-RTNCD
                          GO  TO  M123-EXIT.
           MOVE  SMS-KEY1  TO  P28-SYOCD.
       M123-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.3 : CLOSE  SYORI                                          *
      *----------------------------------------------------------------*
       M13-CLOSE-PROC.
           IF  (P28-TYPRD = '1') AND (P28-TYPKY  =  '0')
               CLOSE SM1FILE.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VSBA0280 END                                         *
      *----------------------------------------------------------------*
