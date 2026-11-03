./ ADD NAME=VSBA2290  01.01 19980318 19980318 101715 00144 00150 TSO245
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA2290'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : GET YOKU KADOBI                  *VSB00070
      *                                                                *VSB00080
      *                 *  INPUT.PARM1 : YYYYMMDD                      *VSB02290
      *                         .PARM2 : YOKU JITU NISSU               *VSB00100
      *                         .PARM3 : LOAD SIGN                     *VSB00110
      *                                                                *VSB00120
      *                 *  OUTPUT.PARM1 : SIGN = 0 / WEEKDAY           *VSB00130
      *                                        = 1 / HOLIDAY           *VSB00140
      *                                        = 9 / ?                 *VSB00150
      *                           PARM2 : CALENDER TABLE               *VSB00160
      *                                                                *VSB00170
      *              <> AUTHOR      : CSK-S.MATSUDA                    *VSB00180
      *              <> DATE        : 98/03/18                         *VSB00190
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*VSB00200
       ENVIRONMENT DIVISION.                                            VSB00210
      *----------------------------------------------------------------*VSB00220
      *   INPUT-OUTPUT SECTION                                         *VSB00230
      *----------------------------------------------------------------*VSB00240
       INPUT-OUTPUT SECTION.                                            VSB00250
       FILE-CONTROL.                                                    VSB00260
           SELECT TBLFILE ASSIGN        TBLFILE                         VSB00270
                          ORGANIZATION INDEXED                          VSB00280
                          ACCESS MODE SEQUENTIAL                        VSB00290
                          RECORD KEY TCL-KEY                            VSB00300
                          FILE STATUS S-CODE.                           VSB00310
       DATA DIVISION.                                                   VSB00320
       FILE SECTION.                                                    VSB00330
       FD  TBLFILE                                                      VSB00340
           LABEL RECORD STANDARD.                                       VSB00350
       01  TCL-R.                                                       VSB00360
           COPY VFILTCL.                                                VSB00370
       WORKING-STORAGE SECTION.                                         VSB00380
      *----------------------------------------------------------------*VSB00390
      *   WORKING-STORAGE SECTION                                      *VSB00400
      *----------------------------------------------------------------*VSB00410
       01  WK-AREA.                                                     VSB00420
           02  WK-KEY          PIC  X(3).                               VSB00430
           02  WK-LEN          PIC S9(8)  COMP.                         VSB00440
           02  WK-I            PIC S9(4)  COMP VALUE +0.                VSB00450
           02  WK-FND          PIC  9.                                  VSB00460
           02  WK-YOKU         PIC 99.                                  VSB00470
       01  S-CODE              PIC 99.                                  VSB00480
       LINKAGE SECTION.                                                 VSB00490
      *----------------------------------------------------------------*VSB00500
      *   LINCAGE SECTION                                              *VSB00510
      *----------------------------------------------------------------*VSB00520
       01  PARAM.                                                       VSB00530
           COPY VPARB229.                                               VSB00540
       PROCEDURE DIVISION USING PARAM.                                  VSB00550
      *----------------------------------------------------------------*VSB00560
      *    1.0 : MAIN CONTROL                                          *VSB00570
      *----------------------------------------------------------------*VSB00580
       M10-MAIN-CONTROL.                                                VSB00590
      *                                                                 VSB00600
           IF  P229-LOADS = ZERO                                        VSB00610
               MOVE  1  TO  P229-LOADS                                  VSB00620
               MOVE ZERO  TO  WK-I                                      VSB00630
               PERFORM M11-TBL-LOAD THRU M11-EXIT.                      VSB00640
           MOVE P229-YOKU TO WK-YOKU.                                   VSB00650
           PERFORM M12-TBL-SEARCH THRU M12-EXIT UNTIL WK-YOKU = ZERO.   VSB00660
       M10-EXIT.                                                        VSB00670
           GOBACK.                                                      VSB00680
      *----------------------------------------------------------------*VSB00690
      *    1.1 : CALENDER TABLE LOAD                                   *VSB00700
      *----------------------------------------------------------------*VSB00710
       M11-TBL-LOAD.                                                    VSB00720
           OPEN INPUT TBLFILE.                                          VSB00730
           MOVE LOW-VALUE TO TCL-KEY.                                   VSB00740
           MOVE 'TCL' TO TCL-RECID.                                     VSB00750
           START TBLFILE KEY > TCL-KEY.                                 VSB00760
           IF  S-CODE  NOT = ZERO                                       VSB00770
               DISPLAY 'BSBA2290 START S-CODE' S-CODE.                  VSB00780
       L11-01.                                                          VSB00790
           READ TBLFILE                                                 VSB00800
                AT END                                                  VSB00810
                   GO TO L11-02.                                        VSB00820
           IF  S-CODE  NOT = ZERO                                       VSB00830
               DISPLAY 'BSBA2290 READ S-CODE' S-CODE.                   VSB00840
           IF  TCL-RECID NOT = 'TCL'                                    VSB00850
               GO TO L11-02.                                            VSB00860
           ADD 1 TO WK-I.                                               VSB00870
           IF  WK-I > 24                                                VSB00880
               GO TO L11-02.                                            VSB00890
           MOVE TCL-CALEN TO P229-CALEN (WK-I).                         VSB22900
           GO TO L11-01.                                                VSB00910
       L11-02.                                                          VSB00920
           CLOSE TBLFILE.                                               VSB00930
       M11-EXIT.                                                        VSB00940
           EXIT.                                                        VSB00950
      *----------------------------------------------------------------*VSB00960
      *    1.2 : YOKU KADOBI SEARCH                                    *VSB00970
      *----------------------------------------------------------------*VSB00980
       M12-TBL-SEARCH.                                                  VSB00990
           PERFORM M121-YYMM-SEARCH THRU M121-EXIT.                     VSB01000
           IF  WK-FND NOT = 1                                           VSB01010
               DISPLAY 'R BSBA2290 CALENDER NI GAITO DATE NASI '        VSB01020
                       P229-DATE UPON CONSOLE                           VSB01030
               CALL 'VSBABEND'                                          VSB01040
           ELSE                                                         VSB01050
               PERFORM M122-DD-SEARCH THRU M122-EXIT.                   VSB01060
       M12-EXIT.                                                        VSB01070
           EXIT.                                                        VSB01080
      *----------------------------------------------------------------*VSB01090
      *    1.2.1 : YYMM SEARCH                                         *VSB01100
      *----------------------------------------------------------------*VSB01110
       M121-YYMM-SEARCH.                                                VSB01120
           MOVE  ZERO TO WK-FND.                                        VSB01130
           MOVE  1    TO WK-I.                                          VSB01140
       L121-01.                                                         VSB01150
           IF  WK-FND = 1 OR WK-I > 24                                  VSB01160
               GO TO  M121-EXIT.                                        VSB01170
           IF  P229-YYMM = P229-TYYMM (WK-I)                            VSB01180
               MOVE  1 TO WK-FND                                        VSB01190
           ELSE                                                         VSB01200
               ADD  1 TO WK-I                                           VSB01210
               GO TO  L121-01.                                          VSB01220
       M121-EXIT.                                                       VSB01230
           EXIT.                                                        VSB01240
      *----------------------------------------------------------------*VSB01250
      *    1.2.2 : DD   SEARCH                                         *VSB01260
      *----------------------------------------------------------------*VSB01270
       M122-DD-SEARCH.                                                  VSB01280
           ADD 1 TO P229-DD.                                            VSB01290
           IF  P229-DD > 31                                             VSB01300
               ADD 1 TO P229-MM                                         VSB01310
               MOVE ZERO  TO P229-DD                                    VSB01320
               IF  P229-MM > 12                                         VSB01330
                   ADD  1 TO P229-YY                                    VSB01340
                   MOVE 1 TO P229-MM                                    VSB01350
               ELSE                                                     VSB01360
                   NEXT SENTENCE                                        VSB01370
           ELSE                                                         VSB01380
               IF  P229-TDD (WK-I P229-DD) = ZERO                       VSB01390
                   SUBTRACT 1 FROM WK-YOKU.                             VSB01400
       M122-EXIT.                                                       VSB01410
           EXIT.                                                        VSB01420
