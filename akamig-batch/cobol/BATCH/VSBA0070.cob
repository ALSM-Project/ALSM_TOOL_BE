./ ADD NAME=VSBA0070  01.00 19980323 19980323 111117 00074 00074 TSO245
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA0070'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : REFORM CURRENT-DATE              *VSB00070
      *                                      MM/DD/YY ----> YYMMDD     *VSB00080
      *                                                                *VSB00090
      *                 *  INPUT.PARM  : NONE.                         *VSB00100
      *                                                                *VSB00110
      *                 *  OUTPUT.PARM : CURRENT DATE                  *VSB00120
      *                                     YYMMDD                     *VSB00130
      *                                                                *VSB00140
      *              <> AUTHOR      : CSK-Y.W                          *VSB00150
      *              <> DATE        : 82/07/28                         *VSB00160
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) YOUKEN NO = K-275  ÍÝº³   : 98/03/23  CSK-S.MATSUDA       *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.                                            VSB00180
       INPUT-OUTPUT SECTION.                                            VSB00190
       FILE-CONTROL.                                                    VSB00200
           SELECT  TBLFILE ASSIGN         TBLFILE                       VSB00210
                           ORGANIZATION INDEXED                         VSB00220
                           ACCESS MODE  RANDOM                          VSB00230
                           RECORD KEY   KEY-DAT                         VSB00240
                           FILE STATUS  S-CODE.                         VSB00250
       DATA DIVISION.                                                   VSB00260
       FILE SECTION.                                                    VSB00270
       FD  TBLFILE                                                      VSB00280
           LABEL RECORD STANDARD.                                       VSB00290
       01  DAT-REC.                                                     VSB00300
           02  KEY-DAT         PIC X(06).                               VSB00310
           02  FILLER          PIC X(74).                               VSB00320
                                                                        VSB00330
       WORKING-STORAGE SECTION.                                         VSB00340
      *----------------------------------------------------------------*VSB00350
      *   WORKING-STORAGE SECTION                                      *VSB00360
      *----------------------------------------------------------------*VSB00370
       77  S-CODE              PIC  X(2).                               VSB00380
       01  DAT-R.                                                       VSB00390
           02  DAT-KEYX        PIC  X(06).                              VSB00400
           02  DAT-YMD         PIC  X(06).                              VSB00410
           02  FILLER          PIC  X(68).                              VSB00420
       LINKAGE SECTION.                                                 VSB00490
      *----------------------------------------------------------------*VSB00500
      *   LINCAGE SECTION                                              *VSB00510
      *----------------------------------------------------------------*VSB00520
       01  PARAM.                                                       VSB00530
           COPY VPARB07.                                                VSB00540
       PROCEDURE DIVISION USING PARAM.                                  VSB00550
      *----------------------------------------------------------------*VSB00560
      *    1.0 : MAIN CONTROL                                          *VSB00570
      *----------------------------------------------------------------*VSB00580
       M10-MAIN-CONTROL.                                                VSB00590
      *                                                                 VSB00600
           OPEN  INPUT  TBLFILE.                                        VSB00610
           MOVE  'DAT001'  TO  KEY-DAT.                                 VSB00620
           MOVE  SPACE     TO  P07-DATEX.                               VSB00630
           READ  TBLFILE  INTO  DAT-R                                   VSB00640
                        INVALID  GO TO  ABEND.                          VSB00650
           IF  S-CODE NOT = ZERO
               GO  TO  ABEND.
           IF  DAT-YMD  =      ZERO                                     VSB00660
               ACCEPT P07-DATE FROM DATE                                VSB00670
           ELSE                                                         VSB00710
               MOVE  DAT-YMD  TO  P07-DATE.                             VSB00720
           GO  TO  OWARI.
       ABEND.
           CLOSE  TBLFILE.
           CALL 'VSBABEND'.
       OWARI.
           CLOSE  TBLFILE.
           GOBACK.                                                      VSB00750
