./ ADD NAME=VSBA2270  01.00 19980317 19980317 165147 00109 00109 TSO245
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA2270'.                                     VSB02270
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : KADOBI CHECK                     *VSB00070
      *                                                                *VSB00080
      *                 *  INPUT.PARM : YYYYMMDD                       *VSB00090
      *                                                                *VSB00100
      *                 *  OUTPUT.PARM1 : SIGN = 0 / WEEKDAY           *VSB00110
      *                                        = 1 / HOLIDAY           *VSB00120
      *                                        = 9 / ?                 *VSB00130
      *                           PARM2 : CALENDER TABLE               *VSB00140
      *                                                                *VSB00150
      *              <> AUTHOR      : CSK-S.MATSUDA                    *VSB00160
      *              <> DATE        : 98/03/17                         *VSB00170
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (02) YYYYYY YYYYYY      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.                                            VSB00190
      *----------------------------------------------------------------*VSB00200
      *   INPUT-OUTPUT SECTION                                         *VSB00210
      *----------------------------------------------------------------*VSB00220
       INPUT-OUTPUT SECTION.                                            VSB00230
       FILE-CONTROL.                                                    VSB00240
           SELECT TBLFILE ASSIGN        TBLFILE                         VSB00250
                          ORGANIZATION INDEXED                          VSB00260
                          ACCESS MODE SEQUENTIAL                        VSB00270
                          RECORD KEY TCL-KEY                            VSB00280
                          FILE STATUS S-CODE.                           VSB00290
       DATA DIVISION.                                                   VSB22700
       FILE SECTION.                                                    VSB00310
       FD  TBLFILE                                                      VSB00320
           LABEL RECORD STANDARD.                                       VSB00330
       01  TCL-R.                                                       VSB00340
           COPY VFILTCL.                                                VSB00350
       WORKING-STORAGE SECTION.                                         VSB00360
      *----------------------------------------------------------------*VSB00370
      *   WORKING-STORAGE SECTION                                      *VSB00380
      *----------------------------------------------------------------*VSB00390
       01  WK-KEY              PIC  X(3).                               VSB00400
       01  WK-LEN              PIC S9(8)  COMP.                         VSB00410
       01  WK-I                PIC S9(4)  COMP.                         VSB00420
       01  WK-FND              PIC  9.                                  VSB00430
       01  S-CODE              PIC 99.                                  VSB00440
       LINKAGE SECTION.                                                 VSB00450
      *----------------------------------------------------------------*VSB00460
      *   LINCAGE SECTION                                              *VSB00470
      *----------------------------------------------------------------*VSB00480
       01  PARAM.                                                       VSB00490
           COPY VPARB227.                                               VSB00500
       PROCEDURE DIVISION USING PARAM.                                  VSB00510
      *----------------------------------------------------------------*VSB00520
      *    1.0 : MAIN CONTROL                                          *VSB00530
      *----------------------------------------------------------------*VSB00540
       M10-MAIN-CONTROL.                                                VSB00550
      *                                                                 VSB00560
           IF  P227-LOADS = 0                                           VSB00570
               MOVE 1 TO P227-LOADS                                     VSB00580
               MOVE 0 TO WK-I                                           VSB00590
               PERFORM M11-TBL-LOAD THRU M11-EXIT.                      VSB00600
           PERFORM M12-TBL-SEARCH THRU M12-EXIT.                        VSB00610
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
           IF  S-CODE NOT = ZERO                                        VSB00720
               DISPLAY 'VSBA2270 START S-CODE=' S-CODE.                 VSB00730
       L11-01.                                                          VSB00740
           READ TBLFILE                                                 VSB00750
                AT END                                                  VSB00760
                   GO TO L11-02.                                        VSB00770
           IF  S-CODE NOT = ZERO                                        VSB00780
               DISPLAY 'VSBA2270 READ S-CODE=' S-CODE.                  VSB00790
           ADD 1 TO WK-I.                                               VSB00800
           IF  WK-I > 24                                                VSB00810
               GO TO L11-02.                                            VSB00820
           MOVE TCL-CALEN TO P227-CALEN (WK-I).                         VSB00830
           GO TO L11-01.                                                VSB00840
       L11-02.                                                          VSB00850
           CLOSE TBLFILE.                                               VSB00860
       M11-EXIT.                                                        VSB00870
           EXIT.                                                        VSB00880
      *----------------------------------------------------------------*VSB00890
      *    1.2 : CALENDER TABLE SEARCH                                 *VSB00900
      *----------------------------------------------------------------*VSB00910
       M12-TBL-SEARCH.                                                  VSB00920
           MOVE  ZERO TO WK-FND.                                        VSB00930
           MOVE  1    TO WK-I.                                          VSB00940
       L12-01.                                                          VSB00950
           IF  WK-FND = 1                                               VSB00960
               GO TO  M12-EXIT.                                         VSB00970
           IF  WK-I   > 24                                              VSB00980
               MOVE  9 TO P227-SIGN                                     VSB00990
               GO TO  M12-EXIT.                                         VSB01000
           IF  P227-YYMM = P227-TYYMM (WK-I)                            VSB01010
               MOVE  1 TO WK-FND                                        VSB01020
               MOVE  P227-TDD (WK-I P227-DD) TO P227-SIGN.              VSB01030
           ADD  1 TO WK-I.                                              VSB01040
           GO TO  L12-01.                                               VSB01050
       M12-EXIT.                                                        VSB01060
           EXIT.                                                        VSB01070
