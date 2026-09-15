./ ADD NAME=VSBA2340  01.00 19980320 19980320 104652 00181 00181 TSO245
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA2340'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : SEIKYUU YYYYMM SYUTOKU           *VSB00070
      *                 *  INPUT.PARM : YYYYMMDD (SEIREKI )            *VSB00090
      *                 *  OUTPUT.PARM : YYYYMM   (GATUDO)             *VSB00110
      *              <> AUTHOR      : CSK-S.MATSUDA                    *VSB00130
      *              <> DATE        : 98/03/20                         *VSB00140
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) YYYYYY YYYYYY      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.                                            VSB00160
       DATA DIVISION.                                                   VSB00170
       WORKING-STORAGE SECTION.                                         VSB00180
      *----------------------------------------------------------------*VSB00190
      *   WORKING-STORAGE SECTION                                      *VSB00200
      *----------------------------------------------------------------*VSB00210
       01  WSV1-AREA.                                                   VSB00220
           02 WSV1-YMD.                                                 VSB00230
              03 WSV1-YY       PIC  9(04).
              03 WSV1-MM       PIC  9(02).
              03 WSV1-DD       PIC  9(02).
           02 WSV1-YMDO.                                                VSB00230
              03 WSV1-YYDO     PIC  9(04).
              03 WSV1-MMDO     PIC  9(02).
       LINKAGE SECTION.                                                 VSB00250
      *----------------------------------------------------------------*VSB00260
      *   LINCAGE SECTION                                              *VSB00270
      *----------------------------------------------------------------*VSB00280
       01  PARAM.                                                       VSB00290
           COPY VPARB234.                                               VSB00300
       PROCEDURE DIVISION USING PARAM.                                  VSB00310
      *----------------------------------------------------------------*VSB00320
      *    1.0 : MAIN CONTROL                                          *VSB00330
      *----------------------------------------------------------------*VSB00340
       M10-MAIN-CONTROL.                                                VSB00350
      *                                                                 VSB00400
           IF P234-SIMEB NOT = 31
              IF P234-SIMEB NOT = 20
                 IF P234-SIMEB NOT = 15
                    PERFORM M12-SIMEB20 THRU M12-EXIT
                 ELSE
                    PERFORM M11-SIMEB15 THRU M11-EXIT
              ELSE
                 PERFORM M12-SIMEB20 THRU M12-EXIT
           ELSE
              PERFORM M13-SIMEB31 THRU M13-EXIT.
      *                                                                 VSB00400
       M10-EXIT.                                                        VSB00600
           GOBACK.                                                      VSB00610
      *----------------------------------------------------------------*VSB00320
      *    1.1 : SIMEB = 15                                            *VSB00330
      *----------------------------------------------------------------*VSB00340
       M11-SIMEB15.                                                     VSB00350
           IF P234-SEIMM = ZERO                                         VSB00410
              IF P234-GYOMD NOT > P234-HYMD                             VSB00510
                 MOVE P234-HYMD  TO WSV1-YMD                            VSB00520
                 PERFORM M111-YYMM15 THRU M111-EXIT                     VSB00530
                 MOVE WSV1-YMDO TO P234-SEIYM                           VSB00540
              ELSE                                                      VSB00560
                 MOVE P234-GYOMD TO WSV1-YMD                            VSB00520
                 PERFORM M111-YYMM15 THRU M111-EXIT                     VSB00530
                 MOVE WSV1-YMDO TO P234-SEIYM                           VSB00540
           ELSE                                                         VSB00560
              IF P234-TORKB = '83'                                      VSB00420
                 IF P234-GYOMD NOT > P234-HYMD                          VSB00510
                    MOVE P234-HYMD  TO WSV1-YMD                         VSB00520
                    PERFORM M111-YYMM15 THRU M111-EXIT                  VSB00530
                    MOVE WSV1-YMDO TO P234-SEIYM                        VSB00540
                 ELSE                                                   VSB00560
                    MOVE P234-GYOMD TO WSV1-YMD                         VSB00520
                    PERFORM M111-YYMM15 THRU M111-EXIT                  VSB00530
                    MOVE WSV1-YMDO TO P234-SEIYM                        VSB00540
              ELSE
                 MOVE P234-HYMD TO WSV1-YMD
                 PERFORM M111-YYMM15 THRU M111-EXIT                     VSB00530
                 IF P234-SEIMM < WSV1-MMDO
                    COMPUTE P234-YY = P234-HYY + 01
                    MOVE P234-SEIMM TO P234-MM
                 ELSE
                    MOVE WSV1-YYDO TO P234-YY
                    MOVE P234-SEIMM TO P234-MM.
      *                                                                 VSB00590
       M11-EXIT.                                                        VSB00600
      *----------------------------------------------------------------*VSB00320
      *    1.1.1  ;  YY MM DO (15)                                     *VSB00330
      *----------------------------------------------------------------*VSB00340
       M111-YYMM15.                                                     VSB00350
           MOVE ZERO     TO WSV1-YMDO.
           IF WSV1-DD < 01 OR WSV1-DD > 31                              VSB00410
              GO TO M111-EXIT.
           IF WSV1-DD > 15                                              VSB00510
              IF WSV1-MM = 12
                 COMPUTE WSV1-YYDO = WSV1-YY + 01                       VSB00520
                 MOVE 01      TO WSV1-MMDO                              VSB00540
              ELSE                                                      VSB00560
                 MOVE WSV1-YY TO WSV1-YYDO                              VSB00570
                 COMPUTE WSV1-MMDO = WSV1-MM + 01                       VSB00520
           ELSE                                                         VSB00560
               MOVE WSV1-YY TO WSV1-YYDO                                VSB00420
               MOVE WSV1-MM TO WSV1-MMDO.                               VSB00420
      *                                                                 VSB00590
       M111-EXIT.                                                       VSB00600
      *----------------------------------------------------------------*VSB00320
      *    1.2 : SIMEB = 20 OR SIMEB = 15, 30 IGAI                     *VSB00330
      *----------------------------------------------------------------*VSB00340
       M12-SIMEB20.                                                     VSB00350
           IF P234-SEIMM = ZERO                                         VSB00410
              MOVE P234-HYMD  TO WSV1-YMD                               VSB00520
              PERFORM M121-YYMM20 THRU M121-EXIT                        VSB00530
              MOVE WSV1-YMDO TO P234-SEIYM                              VSB00540
           ELSE                                                         VSB00560
              IF P234-TORKB = '83'                                      VSB00420
                 MOVE P234-HYMD  TO WSV1-YMD                            VSB00520
                 PERFORM M121-YYMM20 THRU M121-EXIT                     VSB00530
                 MOVE WSV1-YMDO TO P234-SEIYM                           VSB00540
              ELSE
                 MOVE P234-HYMD TO WSV1-YMD
                 PERFORM M121-YYMM20 THRU M121-EXIT                     VSB00530
                 IF P234-SEIMM < WSV1-MMDO
                    COMPUTE P234-YY = P234-HYY + 01
                    MOVE P234-SEIMM TO P234-MM
                 ELSE
                    MOVE WSV1-YYDO TO P234-YY
                    MOVE P234-SEIMM TO P234-MM.
      *                                                                 VSB00590
       M12-EXIT.                                                        VSB00600
      *----------------------------------------------------------------*VSB00320
      *    1.2.1  ;  YY MM DO (20)                                     *VSB00330
      *----------------------------------------------------------------*VSB00340
       M121-YYMM20.                                                     VSB00350
           MOVE ZERO     TO WSV1-YMDO.
           IF WSV1-DD < 01 OR WSV1-DD > 31                              VSB00410
              GO TO M121-EXIT.
           IF WSV1-DD > 20                                              VSB00510
              IF WSV1-MM = 12
                 COMPUTE WSV1-YYDO = WSV1-YY + 01                       VSB00520
                 MOVE 01      TO WSV1-MMDO                              VSB00540
              ELSE                                                      VSB00560
                 MOVE WSV1-YY TO WSV1-YYDO                              VSB00570
                 COMPUTE WSV1-MMDO = WSV1-MM + 01                       VSB00520
           ELSE                                                         VSB00560
               MOVE WSV1-YY TO WSV1-YYDO                                VSB00420
               MOVE WSV1-MM TO WSV1-MMDO.                               VSB00420
      *                                                                 VSB00590
       M121-EXIT.                                                       VSB00600
      *----------------------------------------------------------------*VSB00320
      *    1.3 : SIMEB = 31                                            *VSB00330
      *----------------------------------------------------------------*VSB00340
       M13-SIMEB31.                                                     VSB00350
           IF P234-SEIMM = ZERO                                         VSB00410
              IF P234-GYOMD NOT > P234-HYMD                             VSB00510
                 MOVE P234-HYY TO P234-YY                               VSB00520
                 MOVE P234-HMM TO P234-MM                               VSB00540
              ELSE                                                      VSB00560
                 MOVE P234-GYY TO P234-YY                               VSB00520
                 MOVE P234-GMM TO P234-MM                               VSB00540
           ELSE                                                         VSB00560
              IF P234-TORKB = '83'                                      VSB00420
                 IF P234-GYOMD NOT > P234-HYMD                          VSB00510
                    MOVE P234-HYY TO P234-YY                            VSB00520
                    MOVE P234-HMM TO P234-MM                            VSB00540
                 ELSE                                                   VSB00560
                    MOVE P234-GYY TO P234-YY                            VSB00520
                    MOVE P234-GMM TO P234-MM                            VSB00540
              ELSE
                 MOVE P234-HMM TO WSV1-MMDO                             VSB00540
                 IF P234-SEIMM < WSV1-MMDO
                    COMPUTE P234-YY = P234-HYY + 01
                    MOVE P234-SEIMM TO P234-MM
                 ELSE
                    MOVE P234-HYY  TO P234-YY
                    MOVE P234-SEIMM TO P234-MM.
      *                                                                 VSB00590
       M13-EXIT.                                                        VSB00600
      *----------------------------------------------------------------*VSB00320
      *    VSBA2340   END                                              *VSB00330
      *----------------------------------------------------------------*VSB00340
