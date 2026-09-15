./ ADD NAME=VSBA0410  01.04 19850614 20140925 103159 00127 00126 MCSK082
       ID  DIVISION.                                                    VSA00010
       PROGRAM-ID.      'VSBA0410'.                                     VSA00020
      *----------------------------------------------------------------*VSA00030
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSA00040
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSA00050
      *              <> PG.OUTLINE  : DATA FORMAT HENKAN               *VSA00060
      *                                                                *VSA00070
      *                 *  INPUT.PARM : DATA                           *VSA00080
      *                                                                *VSA00090
      *                 *  OUTPUT.PARM  : FORMAT GO DATA               *VSA00100
      *                                                                *VSA00120
      *              <> AUTHOR      : CSK-A.ONITSUKA                   *VSA00130
      *              <> DATE        : 85/06/14                         *VSA00140
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1616           ØºÝÊß²Ù  : 14/09/25  CSK-K.KOYAMA        *
      * (02) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*VSA00150
       ENVIRONMENT DIVISION.                                            VSA00160
       DATA DIVISION.                                                   VSA00170
       WORKING-STORAGE SECTION.                                         VSA00180
      *----------------------------------------------------------------*VSA00190
      *   WORKING-STORAGE SECTION                                      *VSA00200
      *----------------------------------------------------------------*VSA00210
       01  WK-DATA.                                                     VSA00220
           02  WK-DATA1        PIC  X(1)  OCCURS  200.                  VSA00220
       01  FILLER   REDEFINES  WK-DATA.                                 VSA00220
           02  FILLER          PIC  X(1).                               VSA00220
           02  WK-DATA199      PIC  X(199).                             VSA00220
       01  WK-SO-X             PIC S9(4)  COMP  VALUE  +14.             VSA00220
       01  FILLER   REDEFINES  WK-SO-X.                                 VSA00220
           02  FILLER          PIC  X(1).                               VSA00220
           02  WK-SO           PIC  X(1).                               VSA00220
       01  WK-SI-X             PIC S9(4)  COMP  VALUE  +15.             VSA00220
       01  FILLER   REDEFINES  WK-SI-X.                                 VSA00220
           02  FILLER          PIC  X(1).                               VSA00220
           02  WK-SI           PIC  X(1).                               VSA00220
       01  WK-I                PIC S9(4)  COMP.                         VSA00220
       01  WK-J                PIC S9(4)  COMP.                         VSA00220
       01  WK-A.                                                        VSA00220
           02  WK-A1-X.                                                 VSA00220
               03  WK-A1       PIC  9(1).                               VSA00220
           02  WK-A2-X.                                                 VSA00220
               03  WK-A2       PIC  9(1).                               VSA00220
       01  WK-B                PIC S9(4)  COMP.                         VSA00220
       01  FILLER   REDEFINES  WK-B.                                    VSA00220
           02  FILLER          PIC  X(1).                               VSA00220
           02  WK-B1           PIC  X(1).                               VSA00220
       LINKAGE SECTION.                                                 VSA01270
      *----------------------------------------------------------------*VSA01280
      *   LINKAGE SECTION                                              *VSA01290
      *----------------------------------------------------------------*VSA01300
       01  VPARB41.                                                     VSA01310
           COPY VPARB41  SUPPRESS.                                      VSA01320
       PROCEDURE DIVISION  USING VPARB41.                               VSA01330
      *----------------------------------------------------------------*VSA01340
      *    1.0 : MAIN CONTROL                                          *VSA01350
      *----------------------------------------------------------------*VSA01360
       M10-MAIN-CONTROL.                                                VSA01370
           PERFORM  M11-INIT     THRU  M11-EXIT.                        VSA01390
           IF  P41-HNBCD  =  '1'                                        VSA01420
               PERFORM  M12-SFT-ADD  THRU  M12-EXIT.                    VSA01390
           IF  P41-HNBCD  =  '2'                                        VSA01420
               PERFORM  M13-HEX-CHG  THRU  M13-EXIT                     VSA01390
               MOVE  WK-DATA  TO  P41-DATA                              VSA01390
               PERFORM  M12-SFT-ADD  THRU  M12-EXIT.                    VSA01390
           IF  P41-HNBCD  =  '3'                                        VSA01420
               PERFORM  M13-HEX-CHG  THRU  M13-EXIT.                    VSA01390
           MOVE  WK-DATA  TO  P41-DATA.                                 VSA01390
       M10-EXIT.                                                        VSA01530
           GOBACK.                                                      VSA01540
      *----------------------------------------------------------------*VSA01340
      *    1.1 : INITIAL                                               *VSA01350
      *----------------------------------------------------------------*VSA01360
       M11-INIT.                                                        VSA01370
           MOVE  SPACE  TO  WK-DATA.                                    VSA01390
           MOVE  201    TO  WK-I.                                       VSA01390
       M11-L01.                                                         VSA01390
           SUBTRACT  1  FROM  WK-I.                                     VSA01420
           IF  WK-I  NOT  >  ZERO  OR                                   VSA01420
               P41-DATA1 (WK-I)  NOT  =  SPACE                          VSA01390
               NEXT  SENTENCE                                           VSA01390
           ELSE                                                         VSA01420
               GO  TO  M11-L01.                                         VSA01390
       M11-EXIT.                                                        VSA01530
           EXIT.                                                        VSA01540
      *----------------------------------------------------------------*VSA01340
      *    1.2 : SHIFT CODE ADD                                        *VSA01350
      *----------------------------------------------------------------*VSA01360
       M12-SFT-ADD.                                                     VSA01370
           MOVE  WK-SO     TO  WK-DATA1 (1).                            VSA01390
           MOVE  P41-DATA  TO  WK-DATA199.                              VSA01390
           ADD  2   TO  WK-I.                                           VSA01390
           MOVE  WK-SI     TO  WK-DATA1 (WK-I).                         VSA01390
       M12-EXIT.                                                        VSA01530
           EXIT.                                                        VSA01540
      *----------------------------------------------------------------*VSA01340
      *    1.3 : HEX => CHARACTER  (PACK)                              *VSA01350
      *----------------------------------------------------------------*VSA01360
       M13-HEX-CHG.                                                     VSA01370
           COMPUTE  WK-I  =  WK-I / 2.                                  VSA01390
           MOVE  ZERO      TO  WK-J.                                    VSA01390
       M13-L01.                                                         VSA01530
           ADD  1   TO  WK-J.                                           VSA01390
           IF  WK-J  >  WK-I                                            VSA01390
               GO  TO  M13-EXIT.
           MOVE  P41-DATA2 (WK-J)  TO  WK-A.                            VSA01390
           IF  WK-A1-X  = 'A'  COMPUTE  WK-B = 16 * 10.
           IF  WK-A1-X  = 'B'  COMPUTE  WK-B = 16 * 11.
           IF  WK-A1-X  = 'C'  COMPUTE  WK-B = 16 * 12.
           IF  WK-A1-X  = 'D'  COMPUTE  WK-B = 16 * 13.
           IF  WK-A1-X  = 'E'  COMPUTE  WK-B = 16 * 14.
           IF  WK-A1-X  = 'F'  COMPUTE  WK-B = 16 * 15.
           IF  WK-A1-X  NOT  <  '0'
                               COMPUTE  WK-B = 16 * WK-A1.
           IF  WK-A2-X  = 'A'  ADD  10     TO  WK-B.
           IF  WK-A2-X  = 'B'  ADD  11     TO  WK-B.
           IF  WK-A2-X  = 'C'  ADD  12     TO  WK-B.
           IF  WK-A2-X  = 'D'  ADD  13     TO  WK-B.
           IF  WK-A2-X  = 'E'  ADD  14     TO  WK-B.
           IF  WK-A2-X  = 'F'  ADD  15     TO  WK-B.
           IF  WK-A2-X  NOT  <  '0'
                               ADD  WK-A2  TO  WK-B.
           MOVE  WK-B1   TO  WK-DATA1 (WK-J).
           GO  TO  M13-L01.
       M13-EXIT.                                                        VSA01530
           EXIT.                                                        VSA01540
