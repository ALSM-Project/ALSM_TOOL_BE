./ ADD NAME=VSBA1130  01.03 19911009 19911024 140300 00106 00109 TSO47
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA1130'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN SITEN SYSTEM             *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : GINKOU EIGYOUBI CHECK            *VSB00070
      *                                                                *VSB00080
      *                 *  INPUT.PARM : PB3-SYMD                       *VSB00090
      *                               : PB3-LDFLG                      *VSB00090
      *                                                                *VSB00100
      *                 *  OUTPUT.PARM1 : PB3-KYJHS                    *VSB00140
      *                                   SIGN = 0 / WEEKDAY           *VSB00110
      *                                        = 1 / HOLIDAY           *VSB00120
      *                           PARM2 : PB3-LDFLG                    *VSB00140
      *                           PARM3 : PB3-CDTBL                    *VSB00140
      *                                                                *VSB00150
      *              <> AUTHOR      : SUZ-T.KOGISO                     *VSB00160
      *              <> DATE        : 91/10/09                         *VSB00170
      *----------------------------------------------------------------*VSB00180
       ENVIRONMENT DIVISION.                                            VSB00190
      *----------------------------------------------------------------*VSB00200
      *   INPUT-OUTPUT SECTION                                         *VSB00210
      *----------------------------------------------------------------*VSB00220
       INPUT-OUTPUT SECTION.                                            VSB00230
       FILE-CONTROL.                                                    VSB00240
           SELECT TBLFILE ASSIGN        TBLFILE                         VSB00250
                          ORGANIZATION INDEXED                          VSB00260
                          ACCESS MODE SEQUENTIAL                        VSB00270
                          RECORD KEY BCL-KEY                            VSB00280
                          FILE STATUS S-CODE.                           VSB00290
       DATA DIVISION.                                                   VSB00300
       FILE SECTION.                                                    VSB00310
       FD  TBLFILE                                                      VSB00320
           LABEL RECORD STANDARD.                                       VSB00330
       01  BCL-R.                                                       VSB00340
           COPY VFILBCL.                                                VSB00350
       WORKING-STORAGE SECTION.                                         VSB00360
      *----------------------------------------------------------------*VSB00370
      *   WORKING-STORAGE SECTION                                      *VSB00380
      *----------------------------------------------------------------*VSB00390
       01  WK-I                PIC S9(04)  COMP.                        VSB00420
       01  WK-FND              PIC  9(01).                              VSB00430
       01  S-CODE              PIC  9(02).                              VSB00440
       LINKAGE SECTION.                                                 VSB00450
      *----------------------------------------------------------------*VSB00460
      *   LINCAGE SECTION                                              *VSB00470
      *----------------------------------------------------------------*VSB00480
       01  PARAM.                                                       VSB00490
           COPY VPARBB3.                                                VSB00500
       PROCEDURE DIVISION USING PARAM.                                  VSB00510
      *----------------------------------------------------------------*VSB00520
      *    1.0 : MAIN CONTROL                                          *VSB00530
      *----------------------------------------------------------------*VSB00540
       M10-MAIN-CONTROL.                                                VSB00550
           IF  PB3-LDFLG = 0                                            VSB00570
               MOVE 1 TO PB3-LDFLG                                      VSB00580
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
           MOVE LOW-VALUE TO BCL-KEY.                                   VSB00690
           MOVE 'BCL'     TO BCL-RECID.                                 VSB00700
           START TBLFILE KEY > BCL-KEY.                                 VSB00710
           IF  S-CODE NOT = ZERO                                        VSB00720
               DISPLAY 'VSBA1130 START S-CODE=' S-CODE.                 VSB00730
       M11-L01.                                                         VSB00740
           READ TBLFILE                                                 VSB00750
                AT END                                                  VSB00760
                   GO TO M11-L02.                                       VSB00770
           IF  S-CODE NOT = ZERO                                        VSB00780
               DISPLAY 'VSBA1130 READ S-CODE=' S-CODE.                  VSB00790
           ADD 1 TO WK-I.                                               VSB00800
           IF  WK-I > 24                                                VSB00810
               GO TO M11-L02.                                           VSB00820
           MOVE BCL-CLNDA TO PB3-CALEN (WK-I).                          VSB00830
           GO TO M11-L01.                                               VSB00840
       M11-L02.                                                         VSB00850
           CLOSE TBLFILE.                                               VSB00860
       M11-EXIT.                                                        VSB00870
           EXIT.                                                        VSB00880
      *----------------------------------------------------------------*VSB00890
      *    1.2 : CALENDER TABLE SEARCH                                 *VSB00900
      *----------------------------------------------------------------*VSB00910
       M12-TBL-SEARCH.                                                  VSB00920
           MOVE  ZERO TO WK-FND.                                        VSB00930
           MOVE  1    TO WK-I.                                          VSB00940
       M12-L01.                                                         VSB00950
           IF  WK-FND = 1                                               VSB00960
               GO TO  M12-EXIT.                                         VSB00970
           IF  WK-I   > 24                                              VSB00980
               DISPLAY 'R VSBA1130 CALENDER NI GAITO DATA NASI ! '      VSB00970
               PB3-SYMD UPON CONSOLE                                    VSB00980
               CALL 'VSBABEND'.                                         VSB00990
           IF  PB3-SYM = PB3-CTSYM (WK-I)                               VSB01010
               MOVE  1 TO WK-FND                                        VSB01020
               MOVE  PB3-CTDD (WK-I PB3-SYMDDD) TO PB3-KYJHS.           VSB01030
           ADD  1 TO WK-I.                                              VSB01040
           GO TO M12-L01.                                               VSB01050
       M12-EXIT.                                                        VSB01060
           EXIT.                                                        VSB01070
