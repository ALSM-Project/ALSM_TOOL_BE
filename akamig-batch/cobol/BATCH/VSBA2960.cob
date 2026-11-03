./ ADD NAME=VSBA2960  01.32 20010427 20010501 155037 00107 00088 TSO229
       ID  DIVISION.
       PROGRAM-ID.      'VSBA2960'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : KINGAKU NO MARUME             *
      *                                                                *
      *                 *  INPUT.PARM  : P296-KINGK                    *
      *                                  P296-KEISN1                   *
      *                                  P296-KEISN2                   *
      *                                                                *
      *                 *  OUTPUT.PARM : P296-KINGK                    *
      *                                : P296-RTNCD                    *
      *                                                                *
      *              <> AUTHOR         : CSK-H.ISHII                   *
      *              <> DATE           : 00/08/08                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) H-010              ÍÝº³   : 01/04/27  CSK-C.MIYAWAKI      *
      * (02) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (03) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  EDIT-AREA.
           02 WED1-TANI                     PIC  9(13) COMP-3.
           02 WED1-KINGK                    PIC S9(12) COMP-3.
           02 WED1-KETA                     PIC  9(01).
           02 WED1-KINGK1                   PIC  9(12)V9.
           02 WED1-KINGK2 REDEFINES WED1-KINGK1.
              03 WED1-KINGK9                PIC  9(01) OCCURS 13.
       01  FLAG-AREA.
           02 WFL1-UP                       PIC  X(01).
       01  COUNT-AREA.
           02 I                             PIC S9(02) COMP-3.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  P296-PARM.
           COPY VPARB296 SUPPRESS.
       PROCEDURE DIVISION USING P296-PARM.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM  M1-INIT THRU  M1-EXIT.
           PERFORM  M2-MAIN THRU  M2-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           MOVE SPACE TO P296-RTNCD.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF P296-KEISN1 < '0' OR P296-KEISN1 > '6' OR
              P296-KEISN2 < '1' OR P296-KEISN2 > '3'
              MOVE '1' TO P296-RTNCD
              GO TO M2-EXIT.
       M2-L01.
           EVALUATE P296-KEISN1
              WHEN '0' MOVE 1        TO WED1-TANI,
              WHEN '1' MOVE 10       TO WED1-TANI,
              WHEN '2' MOVE 100      TO WED1-TANI,
              WHEN '3' MOVE 1000     TO WED1-TANI,
              WHEN '4' MOVE 10000    TO WED1-TANI,
              WHEN '5' MOVE 100000   TO WED1-TANI,
              WHEN '6' MOVE 1000000  TO WED1-TANI,
           END-EVALUATE.
           IF P296-KINGK < WED1-TANI
              MOVE '2' TO P296-RTNCD
              GO TO M2-EXIT.
       M2-L02.
           EVALUATE P296-KEISN2
            WHEN '1' COMPUTE WED1-KINGK = P296-KINGK / WED1-TANI,
            WHEN '2' MOVE P296-KINGK  TO WED1-KINGK1
                     MOVE P296-KEISN1 TO WED1-KETA
                     INITIALIZE WFL1-UP
                     PERFORM VARYING I FROM 13 BY -1
                        UNTIL I = 13 - (WED1-KETA + 1) OR WFL1-UP = '*'
                        IF WED1-KINGK9(I) NOT = 0
                           MOVE '*' TO WFL1-UP
                        END-IF
                     END-PERFORM
                     IF WFL1-UP = '*'
                       COMPUTE WED1-KINGK = (P296-KINGK / WED1-TANI) + 1
                     ELSE
                       COMPUTE WED1-KINGK = P296-KINGK / WED1-TANI
                     END-IF,
            WHEN '3' COMPUTE WED1-KINGK ROUNDED = P296-KINGK /
                                                  WED1-TANI,
           END-EVALUATE.
           COMPUTE P296-KINGK = WED1-KINGK * WED1-TANI.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    END VSBA2960                                                *
      *----------------------------------------------------------------*
