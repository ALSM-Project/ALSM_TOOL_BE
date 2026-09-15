./ ADD NAME=VSBA0480  01.02 19861013 20140925 103402 00066 00062 MCSK082
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA0480'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : EISUUJI KANJI HENKAN             *VSB00070
      *                 *  INPUT.PARM :                                *VSB00090
      *                 *  OUTPUT.PARM :                               *VSB00110
      *              <> AUTHOR      : CSK-T.HATANO                     *VSB00130
      *              <> DATE        : 86/04/30                         *VSB00140
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
       01  SAVE-AREA.
           02 WSV1-42X                      PIC S9(04) COMP  VALUE +66.
           02 FILLER              REDEFINES WSV1-42X.
              03 FILLER                     PIC  X(01).
              03 WSV1-42                    PIC  X(01).
       01  COUNT-AREA.
           02 WCT1-I                        PIC  9(02).
       LINKAGE SECTION.                                                 VSB00250
      *----------------------------------------------------------------*VSB00260
      *   LINKAGE SECTION                                              *VSB00270
      *----------------------------------------------------------------*VSB00280
       01  PARAM.                                                       VSB00290
           COPY VPARB48  SUPPRESS.                                      VSB00300
       PROCEDURE DIVISION USING PARAM.                                  VSB00310
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   VARYING WCT1-I FROM 1 BY 1
                   UNTIL (WCT1-I > P48-INLEN OR
                          WCT1-I > 20).
           GOBACK.                                                      00610000
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           MOVE SPACE TO P48-OTDAT-X.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        MOJI SEARCH                                *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF P48-INDAT (WCT1-I) = SPACE
              MOVE SPACE TO P48-OTDAT (WCT1-I)
           ELSE
              STRING WSV1-42 P48-INDAT (WCT1-I)
                 DELIMITED BY SIZE
                 INTO P48-OTDAT (WCT1-I).
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*00750000
      *    E N D   V S B A 0 4 8 0                                     *00760000
      *----------------------------------------------------------------*00770000
