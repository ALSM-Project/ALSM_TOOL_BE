./ ADD NAME=VSBA1260  01.02 19941019 19941019 140000 00214 00214 TSO207
       ID  DIVISION.
       PROGRAM-ID.      'VSBA1260'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN SITEN SYSTEM             *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : GINKOU NISSU GO(MAE) NO          *
      *                                EIGYOU BI SANSYUTSU             *
      *                                                                *
      *                 *  INPUT.PARM      : PC6-KUBUN                 *
      *                                    : PC6-EGYNS                 *
      *                 *  IN/OUTPUT.PARM  : PC6-HZUK1                 *
      *                                    : PC6-LDFLG                 *
      *                 *  OUTPUT.PARM     : PC6-CDTBL                 *
      *                                                                *
      *                                                                *
      *              <> AUTHOR      : SUZ-T.KOGISO                     *
      *              <> DATE        : 91/10/10                         *
      *----------------------------------------------------------------*
      *   01 H-010            ÍÝº³  : 94/10/19  : CSK-K.INUKAI         *
      *   02 XXXXXXXXXXXXXXX  ÍÝº³  : YY/MM/DD  : CSK-X.XXXXXXXXXXX    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
      *   INPUT-OUTPUT SECTION                                         *
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TBLFILE ASSIGN        TBLFILE
                          ORGANIZATION INDEXED
                          ACCESS MODE SEQUENTIAL
                          RECORD KEY BCL-KEY
                          FILE STATUS S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  TBLFILE
           LABEL RECORD STANDARD.
       01  BCL-R.
           COPY VFILBCL.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  S-CODE                 PIC  9(02).
       01  COUNT-AREA.
           02  J                  PIC S9(04) COMP.
           02  K                  PIC S9(04) COMP.
           02  L                  PIC S9(04) COMP.
           02  M                  PIC S9(04) COMP.
       01  WK-AREA.
           02  WK-I               PIC S9(04)  COMP VALUE +0.
           02  WK-FND             PIC  9(01).
       01  TABLE-AREA.
           02 WTB1-DTS.
              03 WTB1-KYJFLG      PIC  X(31) OCCURS 24.
           02 WTB1-DDX       REDEFINES WTB1-DTS.
              03 WTB1-DD          PIC  X(01) OCCURS 744.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARBC6.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    1.0 : MAIN CONTROL                                          *
      *----------------------------------------------------------------*
       M10-MAIN-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT.
           PERFORM M3-FINL THRU M3-EXIT.
       M10-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *   M.1  : INITIAL                                               *
      *----------------------------------------------------------------*
       M1-INIT.
           MOVE ZERO TO WK-I.
           IF PC6-LDFLG = ZERO
              MOVE  1   TO PC6-LDFLG
              PERFORM M11-TBL-LOAD THRU M11-EXIT
           ELSE
              PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > 24
                 MOVE PC6-CTDDX(WK-I) TO WTB1-KYJFLG(WK-I)
              END-PERFORM.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.1 : CALENDER TABLE LOAD                                   *
      *----------------------------------------------------------------*
       M11-TBL-LOAD.
           OPEN INPUT TBLFILE.
           MOVE LOW-VALUE TO BCL-KEY.
           MOVE 'BCL'     TO BCL-RECID.
           START TBLFILE KEY > BCL-KEY.
           IF S-CODE NOT = ZERO
               DISPLAY 'VSBA1260 START S-CODE' S-CODE.
       M11-L01.
           READ TBLFILE
                AT END
                   GO TO M11-L02.
           IF  S-CODE  NOT = ZERO
               DISPLAY 'VSBA1260 READ S-CODE' S-CODE.
           IF  BCL-RECID NOT = 'BCL'
               GO TO M11-L02.
           ADD 1 TO WK-I.
           IF  WK-I > 24
               GO TO M11-L02.
           MOVE BCL-CLNDA         TO PC6-CALEN (WK-I).
           MOVE PC6-CTDDX (WK-I)  TO WTB1-KYJFLG (WK-I).
           GO TO M11-L01.
       M11-L02.
           CLOSE TBLFILE.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF PC6-EGYNS = ZERO
              GO TO M2-EXIT.
           PERFORM M21-SRCH-SYM THRU M21-EXIT.
           IF  WK-FND NOT = 1
               DISPLAY 'R VSBA1260 CALENDER NI GAITO DATE NASI '
               PC6-HZUK1 UPON CONSOLE
               CALL 'VSBABEND'.
           PERFORM M22-SRCH-DD  THRU M22-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M21 : SYM SEARCH                                            *
      *----------------------------------------------------------------*
       M21-SRCH-SYM.
           MOVE  ZERO TO WK-FND.
           MOVE  1    TO WK-I.
       M21-L01.
           IF  WK-FND = 1 OR WK-I > 24
               GO TO M21-EXIT.
           IF  PC6-HZSYM  = PC6-CTSYM (WK-I)
               MOVE 1 TO WK-FND
           ELSE
               ADD 1 TO WK-I
               GO TO M21-L01.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : SEARCH DATE                                          *
      *----------------------------------------------------------------*
       M22-SRCH-DD.
           COMPUTE J = ( WK-I - 1 ) * 31 + PC6-HZDD.
           IF PC6-KUBUN = '0'
              PERFORM M221-SRCH1 THRU M221-EXIT
           ELSE
              IF PC6-KUBUN = '1'
                 PERFORM M222-SRCH2 THRU M222-EXIT
              ELSE
                 DISPLAY 'R VSBA1260 KUBUN ERROR ' PC6-KUBUN
                 UPON CONSOLE
                 CALL 'VSBABEND'.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : SEARCH 1                                           *
      *----------------------------------------------------------------*
       M221-SRCH1.
           MOVE ZERO TO K.
       M221-L01.
           ADD 1 TO J.
           IF J > 744
               DISPLAY 'R VSBA1260 CALENDER NI GAITO NISSU ATO NASI ! '
               PC6-HZUK1 UPON CONSOLE
               CALL 'VSBABEND'.
           IF WTB1-DD( J ) = '0'
              ADD 1 TO K.
           IF K = PC6-EGYNS
              GO TO M221-EXIT
           ELSE
              GO TO M221-L01.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.2 : SEARCH 2                                           *
      *----------------------------------------------------------------*
       M222-SRCH2.
           MOVE ZERO TO K.
       M222-L01.
           COMPUTE J = J - 1.
           IF J NOT > 0
               DISPLAY 'R VSBA1260 CALENDER NI GAITO NISSU MAE NASI ! '
               PC6-HZUK1 UPON CONSOLE
               CALL 'VSBABEND'.
           IF WTB1-DD( J ) = '0'
              ADD 1 TO K.
           IF K = PC6-EGYNS
              GO TO M222-EXIT
           ELSE
              GO TO M222-L01.
       M222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINL                                                   *
      *----------------------------------------------------------------*
       M3-FINL.
           IF PC6-EGYNS = ZERO
              GO TO M3-EXIT.
           DIVIDE 31 INTO J GIVING L REMAINDER M.
           IF M = ZERO
              MOVE PC6-CTSYM( L ) TO PC6-HZSYM
              MOVE 31             TO PC6-HZDD
              GO TO M3-EXIT.
           ADD 1 TO L.
           MOVE PC6-CTSYM( L ) TO PC6-HZSYM.
           MOVE M              TO PC6-HZDD.
       M3-EXIT.
           EXIT.
