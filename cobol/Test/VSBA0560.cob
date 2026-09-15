./ ADD NAME=VSBA0560  01.01 19861018 20140925 103458 00089 00085 MCSK082
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA0560'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : SPACE ZUME HENSYUU               *VSB00070
      *                 *  INPUT.PARM : KANJI-A KANJI B                *VSB00090
      *                 *  OUTPUT.PARM : XXXXXXXXXXXXXXX               *VSB00110
      *              <> AUTHOR      : CSK-K.OTSUKA                     *VSB00130
      *              <> DATE        : 86/08/22                         *VSB00140
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1616           ØºÝÊß²Ù  : 14/09/25  CSK-K.KOYAMA        *
      * (02) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*VSB00150
       ENVIRONMENT DIVISION.                                            VSB00160
       DATA DIVISION.                                                   VSB00170
       WORKING-STORAGE SECTION.                                         VSB00180
      *----------------------------------------------------------------*VSB00190
      *   WORKING-STORAGE SECTION                                      *VSB00200
      *----------------------------------------------------------------*VSB00210
       01  SAVE-AREA.                                                   VSB00220
           02 WSV1-KUMKA                    PIC  X(40).
           02 WSV1-KUMKA-R REDEFINES WSV1-KUMKA.                        VSB00230
              03 WSV1-KOUA                  PIC  X(02) OCCURS 20.
           02 WSV1-KUMKB                    PIC  X(40).
           02 WSV1-KUMKB-R REDEFINES WSV1-KUMKB.                        VSB00230
              03 WSV1-KOUB                  PIC  X(02) OCCURS 20.
           02 WSV1-KUMKR                    PIC  X(100).
           02 WSV1-KUMKR-R REDEFINES WSV1-KUMKR.                        VSB00230
              03 WSV1-KOUR                  PIC  X(02) OCCURS 50.
       01  COUNT-AREA.                                                  VSB00220
           02 WCT1-I                        PIC S9(04)  COMP.
           02 WCT1-J                        PIC S9(04)  COMP.
       LINKAGE SECTION.                                                 VSB00250
      *----------------------------------------------------------------*VSB00260
      *   LINKAGE SECTION                                              *VSB00270
      *----------------------------------------------------------------*VSB00280
       01  PARAM.                                                       VSB00290
           COPY VPARB56 SUPPRESS.                                       VSB00300
       PROCEDURE DIVISION USING PARAM.                                  VSB00310
      *----------------------------------------------------------------*
      *    M0 : MAIN-CONTROL                                           *
      *----------------------------------------------------------------*
       M0-MAIN-CONTROL.                                                 VSB00350
           PERFORM M1-SEARCH1 THRU M1-EXIT.
           PERFORM M2-EDIT    THRU M2-EXIT.
       M0-EXIT.                                                         VSB00600
           GOBACK.                                                      VSB00610
      *----------------------------------------------------------------*
      *    M1 : SEARCH1                                                *
      *----------------------------------------------------------------*
       M1-SEARCH1.                                                      VSB00350
           MOVE P56-KUMKA TO WSV1-KUMKA.
           MOVE P56-KUMKB TO WSV1-KUMKB.
           MOVE SPACE     TO WSV1-KUMKR.
           IF WSV1-KUMKA = SPACE
              MOVE ZERO TO WCT1-I
              GO TO M1-EXIT.
           MOVE 0021      TO WCT1-I.
       M1-L01.
           COMPUTE WCT1-I = WCT1-I - 1.
           IF WCT1-I < 02
              GO TO M1-EXIT.
           IF WSV1-KOUA (WCT1-I) NOT = SPACE
              GO TO M1-EXIT.
           GO TO M1-L01.
       M1-EXIT.                                                         VSB00600
           EXIT.                                                        VSB00610
      *----------------------------------------------------------------*
      *    M2 :EDIT                                                    *
      *----------------------------------------------------------------*
       M2-EDIT.                                                         VSB00350
           MOVE P56-KUMKA TO WSV1-KUMKR.
           COMPUTE WCT1-J = WCT1-I + P56-SPACE.
           MOVE ZERO TO WCT1-I.
       M2-L01.
           ADD 1    TO WCT1-J.
           ADD 1    TO WCT1-I.
           IF WCT1-J > 50 OR WCT1-I > 20
              MOVE WSV1-KUMKR TO P56-KUMKR
              GO TO M2-EXIT.
           MOVE WSV1-KOUB (WCT1-I) TO WSV1-KOUR (WCT1-J).
           GO TO M2-L01.
       M2-EXIT.                                                         VSB00600
           EXIT.                                                        VSB00610
      *----------------------------------------------------------------*VSB00320
      *    VSBA0560   END                                              *VSB00330
      *----------------------------------------------------------------*VSB00340
