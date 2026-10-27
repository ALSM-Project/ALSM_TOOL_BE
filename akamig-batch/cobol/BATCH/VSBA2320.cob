./ ADD NAME=VSBA2320  01.01 19980318 19980417 153128 00140 00140 TSO241
       ID  DIVISION.                                                    VSB00010
       PROGRAM-ID.      'VSBA2320'.                                     VSB00020
      *----------------------------------------------------------------*VSB00030
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00040
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00050
      *              <> PG.OUTLINE  : GET MAE  KADOBI                  *VSB00060
      *                                                                *VSB00070
      *                 *  INPUT.PARM1   : YYYYMMDD                    *VSB00080
      *                 *  INPUT.PARM2   : LOAD HYOSIKI                *VSB00090
      *                 *  OUTPUT.PARM1  : ZENKADOBI YYYYMMDD          *VSB00100
      *                                                                *VSB00110
      *                                                                *VSB00120
      *              <> AUTHOR      : CSK-S.MATSUDA                    *VSB00130
      *              <> DATE        : 98/03/18                         *VSB00140
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*VSB00150
       ENVIRONMENT DIVISION.                                            VSB00160
      *----------------------------------------------------------------*VSB00170
      *   INPUT-OUTPUT SECTION                                         *VSB02320
      *----------------------------------------------------------------*VSB00190
       INPUT-OUTPUT SECTION.                                            VSB00200
       FILE-CONTROL.                                                    VSB00210
           SELECT TBLFILE ASSIGN        TBLFILE                         VSB00220
                          ORGANIZATION INDEXED                          VSB00230
                          ACCESS MODE SEQUENTIAL                        VSB00240
                          RECORD KEY TCL-KEY                            VSB00250
                          FILE STATUS S-CODE.                           VSB00260
       DATA DIVISION.                                                   VSB00270
       FILE SECTION.                                                    VSB00280
       FD  TBLFILE                                                      VSB00290
           LABEL RECORD STANDARD.                                       VSB00300
       01  TCL-R.                                                       VSB00310
           COPY VFILTCL.                                                VSB00320
       WORKING-STORAGE SECTION.                                         VSB00330
      *----------------------------------------------------------------*VSB00340
      *   WORKING-STORAGE SECTION                                      *VSB00350
      *----------------------------------------------------------------*VSB00360
       01  WK-AREA.                                                     VSB00370
           02  WK-KEY          PIC  X(3).                               VSB00380
           02  WK-LEN          PIC S9(8)  COMP.                         VSB00390
           02  WK-I            PIC S9(4)  COMP VALUE +0.                VSB00400
           02  WK-MAE          PIC 99.                                  VSB00410
           02  WK-FND          PIC  9.                                  VSB00420
       01  S-CODE              PIC 99.                                  VSB00430
       LINKAGE SECTION.                                                 VSB00440
      *----------------------------------------------------------------*VSB00450
      *   LINKAGE SECTION                                              *VSB00460
      *----------------------------------------------------------------*VSB00470
       01  PARAM.                                                       VSB00480
           COPY VPARB232.                                               VSB00490
       PROCEDURE DIVISION USING PARAM.                                  VSB00500
      *----------------------------------------------------------------*VSB00510
      *    1.0 : MAIN CONTROL                                          *VSB00520
      *----------------------------------------------------------------*VSB00530
       M10-MAIN-CONTROL.                                                VSB00540
      *                                                                 VSB00550
           IF  P232-LOADS = ZERO                                        VSB00560
               MOVE  1  TO  P232-LOADS                                  VSB00570
               MOVE ZERO  TO  WK-I                                      VSB00580
               PERFORM M11-TBL-LOAD THRU M11-EXIT.                      VSB00590
           MOVE   1     TO WK-MAE.                                      VSB00600
           PERFORM M12-TBL-SEARCH THRU M12-EXIT UNTIL WK-MAE = ZERO.    VSB00610
       M10-EXIT.                                                        VSB00620
           GOBACK.                                                      VSB00630
      *----------------------------------------------------------------*VSB00640
      *    1.1 : CALENDER TABLE LOAD                                   *VSB00650
      *----------------------------------------------------------------*VSB00660
       M11-TBL-LOAD.                                                    VSB00670
           OPEN INPUT TBLFILE.                                          VSB00680
           MOVE LOW-VALUE TO TCL-KEY.                                   VSB00690
           MOVE 'TCL' TO TCL-RECID.                                     VSB00700
           START TBLFILE KEY > TCL-KEY.                                 VSB00710
           IF  S-CODE  NOT = ZERO                                       VSB00720
               DISPLAY 'VSBA2320 START S-CODE' S-CODE.                  VSB00730
       L11-01.                                                          VSB00740
           READ TBLFILE                                                 VSB00750
                AT END                                                  VSB00760
                   GO TO L11-02.                                        VSB00770
           IF  S-CODE  NOT = ZERO                                       VSB00780
               DISPLAY 'VSBA2320 READ S-CODE' S-CODE.                   VSB00790
           IF  TCL-RECID NOT = 'TCL'                                    VSB00800
               GO TO L11-02.                                            VSB00810
           ADD 1 TO WK-I.                                               VSB00820
           IF  WK-I > 24                                                VSB00830
               GO TO L11-02.                                            VSB00840
           MOVE TCL-CALEN TO P232-CALEN (WK-I).                         VSB00850
           GO TO L11-01.                                                VSB00860
       L11-02.                                                          VSB00870
           CLOSE TBLFILE.                                               VSB00880
       M11-EXIT.                                                        VSB00890
           EXIT.                                                        VSB00900
      *----------------------------------------------------------------*VSB00910
      *    1.2 : MAE KADOBI SEARCH                                     *VSB00920
      *----------------------------------------------------------------*VSB00930
       M12-TBL-SEARCH.                                                  VSB00940
           PERFORM M121-YYMM-SEARCH THRU M121-EXIT.                     VSB00950
           IF  WK-FND NOT = 1                                           VSB00960
               DISPLAY 'R VSBA2320 CALENDER NI GAITO DATE NASI '        VSB00970
                       P232-DATE UPON CONSOLE                           VSB00980
               CALL 'VSBABEND'                                          VSB00990
           ELSE                                                         VSB01000
               PERFORM M122-DD-SEARCH THRU M122-EXIT.                   VSB01010
       M12-EXIT.                                                        VSB01020
           EXIT.                                                        VSB01030
      *----------------------------------------------------------------*VSB01040
      *    1.2.1 : YYMM SEARCH                                         *VSB01050
      *----------------------------------------------------------------*VSB01060
       M121-YYMM-SEARCH.                                                VSB01070
           MOVE  ZERO TO WK-FND.                                        VSB01080
           MOVE  1    TO WK-I.                                          VSB01090
       L121-01.                                                         VSB01100
           IF  WK-FND = 1 OR WK-I > 24                                  VSB01110
               GO TO  M121-EXIT.                                        VSB01120
           IF  P232-YYMM = P232-TYYMM (WK-I)                            VSB01130
               MOVE  1 TO WK-FND                                        VSB01140
           ELSE                                                         VSB01150
               ADD  1 TO WK-I                                           VSB01160
               GO TO  L121-01.                                          VSB01170
       M121-EXIT.                                                       VSB01180
           EXIT.                                                        VSB01190
      *----------------------------------------------------------------*VSB01200
      *    1.2.2 : DD   SEARCH                                         *VSB01210
      *----------------------------------------------------------------*VSB01220
       M122-DD-SEARCH.                                                  VSB01230
           COMPUTE  P232-DD = P232-DD - 1.                              VSB01240
           IF  P232-DD = ZERO                                           VSB01250
               COMPUTE  P232-MM = P232-MM - 1                           VSB01260
               MOVE 32 TO P232-DD                                       VSB01270
               IF  P232-MM = ZERO                                       VSB01280
                   MOVE 12 TO P232-MM
                   COMPUTE P232-YY = P232-YY - 1
               ELSE                                                     VSB01310
                   NEXT SENTENCE                                        VSB01320
           ELSE                                                         VSB01330
               IF  P232-TDD (WK-I P232-DD) = ZERO                       VSB01340
                   SUBTRACT 1 FROM WK-MAE.                              VSB01350
       M122-EXIT.                                                       VSB01360
           EXIT.                                                        VSB01370
