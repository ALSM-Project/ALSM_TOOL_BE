./ ADD NAME=VBKA4190  10.11 19951020 20011225 140100 00220 00220 TSO62
       ID  DIVISION.
       PROGRAM-ID. 'VBKA4190'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : K.KAI                         *
      *              <> PG.OUTLINE     : URIKAKE ERROR LIST ( SIMEBI   *
      *                                  HENKOU SAKI ) SYUTURYOKU      *
      *                 * INPUT FILE   : URIKAKE IJYOU HASSEI FILE     *
      *                                                  (CUIFILE)     *
      *                 * OUTPUT FILE  : ERROR LIST      (PRT1)        *
      *              <> AUTHOR         : CSK-H.MIYATA                  *
      *              <> DATE           : 95/10/20                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXX   XXXXX                : XX/XX/XX  XXX-X.XXXXX      *
      * (02) XXXXX   XXXXX                : XX/XX/XX  XXX-X.XXXXX      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS SKPCH01.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUIFILE ASSIGN    TO AS-CUIFILE
                          ORGANIZATION SEQUENTIAL
                          FILE  STATUS S-CODE.
           SELECT LPFILE  ASSIGN    TO S-PRT1.
       DATA DIVISION.
       FILE SECTION.
       FD  CUIFILE
           LABEL RECORD STANDARD.
       01  CUI-REC.
           02 FILLER                        PIC  X(430).
       FD  LPFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD OMITTED.
       01  LP-REC.
           02 FILLER                        PIC  X(132).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  P02-PARM.
           COPY VPARB02 SUPPRESS.
       01  P07-PARM.
           COPY VPARB07 SUPPRESS.
       01  CUI-R.
           COPY VFILCUI SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-PRGNM                 PIC  X(08).
       01  COUNT-AREA.
           02 WCT1-PAGE                     PIC S9(04)  COMP.
           02 WCT1-LCNT                     PIC S9(04)  COMP.
       01  LPH-HEADER.
           02 LPH-HEAD0.
              03 FILLER        PIC  X(132) VALUE SPACE.
           02 LPH-HEAD1.
              03 FILLER        PIC  X(17) VALUE SPACE.
              03 FILLER        PIC  X(03) VALUE '** '.
              03 FILLER        PIC  X(20) VALUE '  ³ Ø ¶ ¹   ´ × - Ø '.
              03 FILLER        PIC  X(20) VALUE '½ Ä  ( ¼ Ò ËÞ Í Ý º '.
              03 FILLER        PIC  X(20) VALUE '³ » · )   **        '.
              03 FILLER        PIC  X(01) VALUE SPACE.
              03 LPH-DATE      PIC  99/99/99.
              03 FILLER        PIC  X(43) VALUE SPACE.
           02 LPH-HEAD2.
              03 FILLER        PIC  X(84) VALUE SPACE.
              03 FILLER        PIC  X(02) VALUE 'P.'.
              03 LPH-PAGE      PIC  ZZ9.
              03 FILLER        PIC  X(43) VALUE SPACE.
           02 LPH-HEAD3.
              03 FILLER        PIC  X(05) VALUE SPACE.
              03 FILLER        PIC  X(15) VALUE 'ÌßÛ¸Þ×ÑNO    K '.
              03 FILLER        PIC  X(20) VALUE '  E  Y            ´ '.
              03 FILLER        PIC  X(20) VALUE '× -   Ò Â ¾ - ¼ Þ   '.
              03 FILLER        PIC  X(72) VALUE SPACE.
       01  LPD-DETLE.
           02 LPD-DETL1.
              03 FILLER         PIC  X(05).
              03 LPD-PRGNM      PIC  X(08).
              03 FILLER         PIC  X(05).
              03 LPD-KEY        PIC  X(15).
              03 FILLER         PIC  X(05).
              03 LPD-MSG        PIC  X(25).
              03 FILLER         PIC  X(69).
       01  LPE-ERROR.
           02 LPE-ERR1.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****----------- V B'.
              03 FILLER       PIC  X(20)  VALUE ' K A 4 1 9 0 -------'.
              03 FILLER       PIC  X(20)  VALUE '-----*****          '.
              03 FILLER       PIC  X(52)  VALUE SPACE.
           02 LPE-ERR2.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****               '.
              03 FILLER       PIC  X(20)  VALUE '                    '.
              03 FILLER       PIC  X(20)  VALUE '     *****          '.
              03 FILLER       PIC  X(52)  VALUE SPACE.
           02 LPE-ERR3.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****       ´ × -   '.
              03 FILLER       PIC  X(20)  VALUE 'D A T A   Å ¼ !!    '.
              03 FILLER       PIC  X(20)  VALUE '     *****          '.
              03 FILLER       PIC  X(52)  VALUE SPACE.
           02 LPE-ERR4.
              03 FILLER       PIC  X(20)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****---------------'.
              03 FILLER       PIC  X(20)  VALUE '--------------------'.
              03 FILLER       PIC  X(20)  VALUE '-----*****          '.
              03 FILLER       PIC  X(52)  VALUE SPACE.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).
           PERFORM M4-ERR-NASI THRU M4-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  CUIFILE
                OUTPUT LPFILE.
           MOVE ZERO TO WCT1-PAGE.
           MOVE 99   TO WCT1-LCNT.
           CALL 'VSBA0070' USING P07-PARM.
           MOVE P07-DATE TO P02-DATE.
           CALL 'VSBA0020' USING P02-PARM.
           MOVE P02-DATE TO LPH-DATE.
       M1-L01.
           PERFORM M11-READ-CUI THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ CUIFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-CUI.
           MOVE 'Z ' TO S-CODE.
           READ CUIFILE INTO CUI-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA4190 CUIFILE READ ERROR ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
       M11-L01.
           MOVE CUI-PRGNM TO WIN1-PRGNM.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2   :      MAIN                                            *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WCT1-LCNT NOT < 60
              PERFORM M21-HEADER THRU M21-EXIT.
           MOVE SPACE      TO LPD-DETL1.
           MOVE WIN1-PRGNM TO LPD-PRGNM.
           MOVE CUI-KEY    TO LPD-KEY.
           MOVE CUI-MSG    TO LPD-MSG.
           WRITE LP-REC FROM LPD-DETL1 AFTER 1.
           ADD 1 TO WCT1-LCNT.
       M2-L01.
           PERFORM M11-READ-CUI THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 :   HEADER                                             *
      *----------------------------------------------------------------*
       M21-HEADER.
           ADD 1 TO WCT1-PAGE.
           MOVE WCT1-PAGE TO LPH-PAGE.
           WRITE LP-REC FROM LPH-HEAD0 AFTER SKPCH01.
           WRITE LP-REC FROM LPH-HEAD1 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD3 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD0 AFTER 1.
           MOVE 6 TO WCT1-LCNT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M4 : ERROR NASI                                             *
      *----------------------------------------------------------------*
       M4-ERR-NASI.
           IF WCT1-LCNT NOT = 99
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
       M4-L01.
           MOVE 1 TO LPH-PAGE.
           WRITE LP-REC FROM LPH-HEAD0 AFTER SKPCH01.
           WRITE LP-REC FROM LPH-HEAD1 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR1 AFTER 3.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR3 AFTER 1.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR4 AFTER 1.
           PERFORM M3-FINL THRU M3-EXIT.
       M4-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE CUIFILE
                 LPFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA4190 END                                           *
      *----------------------------------------------------------------*
