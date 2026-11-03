./ ADD NAME=VBMA2160  10.01 19980430 20011225 103100 00364 00364 TSO78
       ID  DIVISION.
       PROGRAM-ID. 'VBMA2160'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : SONOTA 134                    *
      *              <> PG.OUTLINE     : URIKAKEKIN FILE NO            *
      *                                          ZANDAKA CHECK         *
      *                 * INPUT FILE   : URIKAKEKIN FILE    (URIFILE)  *
      *                                : TOKUISAKI MASTER   (TMSFILE)  *
      *                 * OUTPUT FILE  : URIKAKEKIN FILE               *
      *                                  ZANDAKA ERROR LIST (PRT1)     *
      *              <> AUTHOR         : CSK-M.UENO                    *
      *              <> DATE           : 87/03/06                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) PRINT FORMAT       ÍÝº³   : 87/03/09  CSK-K.HAYASHI       *
      * (02) SONOTA  NO.182            : 87/08/04  CSK-K.OTSUKA        *
      * (03) KAI     NO.45             : 89/03/02  CSK-R.K             *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS SKPCH01.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT URIFILE ASSIGN    TO URIFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-URI
                          FILE  STATUS S-CODE.
           SELECT TMSFILE ASSIGN    TO TMSFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TMS
                          FILE  STATUS S-CODE.
           SELECT LPFILE  ASSIGN    TO S-PRT1.
       DATA DIVISION.
       FILE SECTION.
       FD  URIFILE
           LABEL RECORD STANDARD.
       01  URI-REC.
           02 KEY-URI                       PIC  X(09).
           02 FILLER                        PIC  X(841).
       FD  TMSFILE
           LABEL RECORD STANDARD.
       01  TMS-REC.
           02 KEY-TMS                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  LPFILE   BLOCK CONTAINS 0 RECORDS
           LABEL RECORD OMITTED.
       01  LP-REC.
           02 FILLER                        PIC  X(132).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  URI-R.
           COPY VFILURI SUPPRESS.
       01  TMS-R.
           COPY VFILTMS SUPPRESS.
       01  P02-PARM.
           COPY VPARB02 SUPPRESS.
       01  P07-PARM.
           COPY VPARB07 SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-TOKCD              PIC  X(07).
       01  COUNT-AREA.
           02 WCT1-PAGE                     PIC S9(03)  COMP.
           02 WCT1-LINE                     PIC S9(04)  COMP.
       01  TOTAL-AREA.
           02 WTT1-KINGK                    PIC S9(13)  COMP-3.
           02 WTT2-KINGK                    PIC S9(13)  COMP-3.
       01  LPH-HEADER.
           02 LPH-HEAD1.
              03 FILLER        PIC  X(132) VALUE SPACE.
           02 LPH-HEAD2.
              03 FILLER        PIC  X(30)  VALUE SPACE.
              03 FILLER        PIC  X(20)  VALUE '***  ³ Ø ¶ ¹ · Ý  Ì '.
              03 FILLER        PIC  X(20)  VALUE '± ² Ù  »Þ Ý ÀÞ ¶  ´ '.
              03 FILLER        PIC  X(20)  VALUE '× - Ø ½ Ä  ***      '.
              03 LPH-YYMMDD    PIC  99/99/99.
              03 FILLER        PIC  X(10)  VALUE ' ¹ÞÝ»Þ²   '.
              03 LPH-PAGE      PIC  ZZ9.
              03 FILLER        PIC  X(06)  VALUE ' Íß-¼Þ'.
              03 FILLER        PIC  X(15)  VALUE SPACE.
           02 LPH-HEAD3.
              03 FILLER        PIC  X(20)  VALUE '       ¼ÃÝ     Á ¸  '.
              03 FILLER        PIC  X(20)  VALUE '   Ä¸²»· º-ÄÞ       '.
              03 FILLER        PIC  X(20)  VALUE ' Ä¸²»· Ò²           '.
              03 FILLER        PIC  X(20)  VALUE '       ½½Þ¹Ý ¼ÞÒ    '.
              03 FILLER        PIC  X(20)  VALUE '   Ä¸²»· ¼ÞÒ        '.
              03 FILLER        PIC  X(20)  VALUE '¼ÒËÞ  Ö³¼·          '.
              03 FILLER        PIC  X(12)  VALUE SPACE.
       01  LPD-DETLE.
           02 LPD-DETL1.
              03 FILLER        PIC  X(07).
              03 LPD-STNCD     PIC  X(03).
              03 FILLER        PIC  X(04).
              03 LPD-CKUKG     PIC  X(05).
              03 FILLER        PIC  X(04).
              03 LPD-TOKCD     PIC  X(08).
              03 FILLER        PIC  X(01).
              03 LPD-NOSCD     PIC  X(02).
              03 FILLER        PIC  X(04).
              03 LPD-TOKME     PIC  X(25).
              03 FILLER        PIC  X(02).
              03 LPD-SUZSM     PIC  ZZZ,ZZZ,ZZZ,ZZ9-.
              03 LPD-TOKSM     PIC  ZZZ,ZZZ,ZZZ,ZZ9-.
              03 FILLER        PIC  X(04).
              03 LPD-SIMEB     PIC  X(02).
              03 FILLER        PIC  X(05).
              03 LPD-YOSIK     PIC  X(01).
              03 FILLER        PIC  X(23).
       01  LPE-ERROR.
           02 LPE-ERR1.
              03 FILLER       PIC  X(30)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****----------- V B'.
              03 FILLER       PIC  X(20)  VALUE ' M A 2 1 6 0 -------'.
              03 FILLER       PIC  X(20)  VALUE '-----*****          '.
              03 FILLER       PIC  X(42)  VALUE SPACE.
           02 LPE-ERR2.
              03 FILLER       PIC  X(30)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****               '.
              03 FILLER       PIC  X(20)  VALUE '                    '.
              03 FILLER       PIC  X(20)  VALUE '     *****          '.
              03 FILLER       PIC  X(42)  VALUE SPACE.
           02 LPE-ERR3.
              03 FILLER       PIC  X(30)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****       ´ × -   '.
              03 FILLER       PIC  X(20)  VALUE 'D A T A   Å ¼ !!    '.
              03 FILLER       PIC  X(20)  VALUE '     *****          '.
              03 FILLER       PIC  X(42)  VALUE SPACE.
           02 LPE-ERR4.
              03 FILLER       PIC  X(30)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****---------------'.
              03 FILLER       PIC  X(20)  VALUE '--------------------'.
              03 FILLER       PIC  X(20)  VALUE '-----*****          '.
              03 FILLER       PIC  X(42)  VALUE SPACE.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).
           PERFORM M4-ERROR-CHK THRU M4-EXIT.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  URIFILE
                       TMSFILE
                OUTPUT LPFILE.
           CALL 'VSBA0070' USING P07-PARM.
           MOVE P07-DATE TO P02-DATE.
           CALL 'VSBA0020' USING P02-PARM.
           MOVE P02-DATE TO LPH-YYMMDD.
           MOVE ZERO     TO WCT1-PAGE.
           MOVE 99       TO WCT1-LINE.
           PERFORM M11-READ-URI THRU M11-EXIT.
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ URIFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-URI.
           MOVE 'Z '    TO   S-CODE.
           READ URIFILE INTO URI-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBMA2160 URIFILE DATA 0 KEN ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN1-KEY
              GO TO M11-EXIT.
           MOVE URI-TOKCD TO WIN1-TOKCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ TMSFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-TMS.
           MOVE 'Z '    TO   S-CODE.
           READ TMSFILE INTO TMS-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBMA2160 TMSFILE DATA 0 KEN ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN2-KEY
              GO TO M12-EXIT.
           MOVE TMS-TOKCD TO WIN2-TOKCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2   :      MAIN                                            *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-URIONL THRU M22-EXIT
              ELSE
                 PERFORM M23-TMSONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 :   MATCH                                              *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
       M21-L01.
           PERFORM M211-LIST-PUT THRU M211-EXIT
                   UNTIL ( WIN1-KEY1 NOT = WSV1-KEY1 ).
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 :  IST EDIT & WRITE                                  *
      *----------------------------------------------------------------*
       M211-LIST-PUT.
           PERFORM M2111-KEISAN THRU M2111-EXIT.
           IF WTT1-KINGK NOT = WTT2-KINGK
              PERFORM M2112-DETAIL THRU M2112-EXIT.
           PERFORM M11-READ-URI THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : KEISAN                                           *
      *----------------------------------------------------------------*
       M2111-KEISAN.
           MOVE ZERO       TO WTT1-KINGK.
           MOVE ZERO       TO WTT2-KINGK.
           COMPUTE WTT1-KINGK = URI-ZMURI +
                                URI-ICYO  - ( URI-TOGEN + URI-TOKOG +
                                  URI-TOFKO + URI-TOFKA + URI-TOTEG +
                                  URI-TOSOS ) +
                                ( URI-TOET1 + URI-TONE1 ) +
                                ( URI-TOET2 + URI-TONE2 ) +
                                ( URI-STET1 + URI-STNE1 ) +
                                ( URI-STET2 + URI-STNE2 ).
           COMPUTE WTT2-KINGK = URI-CZURI +
                                URI-CICYO - ( URI-CTGEN + URI-CTKOG +
                                  URI-CTFKO + URI-CTFKA + URI-CTTEG +
                                  URI-CTSOS ) +
                                ( URI-CTET1 + URI-CTNE1 ) +
                                ( URI-CTET2 + URI-CTNE2 ) +
                                ( URI-SCET1 + URI-SCNE1 ) +
                                ( URI-SCET2 + URI-SCNE2 ).
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.2 :   DETAIL                                         *
      *----------------------------------------------------------------*
       M2112-DETAIL.
           IF WCT1-LINE NOT < 28
              PERFORM M21121-HEADER THRU M21121-EXIT.
           MOVE SPACE      TO LPD-DETL1.
           MOVE TMS-STNCD  TO LPD-STNCD.
           MOVE TMS-CKUKG  TO LPD-CKUKG.
           STRING URI-TKCD1 '-' URI-TKCD2
              DELIMITED BY SIZE INTO LPD-TOKCD.
           MOVE URI-NOSCD  TO LPD-NOSCD.
           MOVE TMS-TOKME  TO LPD-TOKME.
           MOVE WTT1-KINGK TO LPD-SUZSM.
           MOVE WTT2-KINGK TO LPD-TOKSM.
           MOVE TMS-SIMEB  TO LPD-SIMEB.
           MOVE TMS-SEYOS  TO LPD-YOSIK.
           WRITE LP-REC FROM LPD-DETL1 AFTER 2.
       M2112-L01.
           ADD 1           TO WCT1-LINE.
       M2112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.2.1 :   HEADER                                       *
      *----------------------------------------------------------------*
       M21121-HEADER.
           ADD 1          TO WCT1-PAGE.
           MOVE WCT1-PAGE TO LPH-PAGE.
           WRITE LP-REC FROM LPH-HEAD1 AFTER SKPCH01.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD3 AFTER 2.
           MOVE ZERO      TO WCT1-LINE.
       M21121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 :  RIFILE ONLY                                       *
      *----------------------------------------------------------------*
       M22-URIONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M11-READ-URI THRU M11-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 :   TMSFILE ONLY                                       *
      *----------------------------------------------------------------*
       M23-TMSONL.
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M4   : WRITE ERROR LIST 0 KEN                               *
      *----------------------------------------------------------------*
       M4-ERROR-CHK.
           IF WCT1-LINE NOT = 99
              GO TO M4-L05.
           ADD 1          TO WCT1-PAGE.
           MOVE WCT1-PAGE TO LPH-PAGE.
           WRITE LP-REC FROM LPH-HEAD1 AFTER SKPCH01.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD3 AFTER 2.
           WRITE LP-REC FROM LPE-ERR1 AFTER 3.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR3 AFTER 1.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR4 AFTER 1.
           GO TO M4-EXIT.
       M4-L05.
           DISPLAY '**------- V B M A  2 1 6 0 -------**' UPON CONSOLE.
           DISPLAY '**                                **' UPON CONSOLE.
           DISPLAY '**      URIKAKEKIN  ZANDAKA       **' UPON CONSOLE.
           DISPLAY '**                                **' UPON CONSOLE.
           DISPLAY '**          E R R O R !!          **' UPON CONSOLE.
           DISPLAY '**                                **' UPON CONSOLE.
           DISPLAY '**--------------------------------**' UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M4-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE URIFILE
                 TMSFILE
                 LPFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBMA2160 END                                           *
      *----------------------------------------------------------------*
