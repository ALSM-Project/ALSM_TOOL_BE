./ ADD NAME=VBKA6400  01.05 20161031 20161101 161615 00212 00190 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBKA6400'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : URIKAKE IJYOU HASSEI DUMP LIST*
      *                                                                *
      *                 * INPUT FILE   : (HONSYA) URIKAKE IJYOU        *
      *                                     HASSEI FILE          (DWKA)*
      *                 * OUTPUT FILE  : URIKAKE IJYOU HASSEI DUMP LIST*
      *                                                          (PRT1)*
      *                                                                *
      *              <> AUTHOR         : CSK-N.EGUCHI                  *
      *              <> DATE           : 16/10/31                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT LPFILE  ASSIGN    TO S-PRT1.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(260).
       FD  LPFILE
           LABEL RECORD STANDARD.
       01  LP-REC.
           02 FILLER                        PIC  X(132).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           COPY VFILHUI SUPPRESS.
       01  P02-PARM.
           COPY VPARB02 SUPPRESS.
       01  P07-PARM.
           COPY VPARB07 SUPPRESS.
       01  COUNT-AREA.
           02 WCT1-PAGE                     PIC S9(04)  COMP.
           02 WCT1-LCNT                     PIC S9(04)  COMP.
       01  SYSIN-AREA.
           02 SYS-CARD1.
              03 SYS-DATA                   PIC  X(02).
              03 FILLER                     PIC  X(78).
       01  LPH-HEADER.
           02 LPH-HEAD0.
              03 FILLER       PIC  X(132) VALUE SPACE.
           02 LPH-HEAD1.
              03 FILLER       PIC  X(10) VALUE SPACE.
              03 FILLER       PIC  X(50) VALUE
                 '**  ³ Ø ¶ ¹   ² ¼Þ Ö ³ Ê Â ¾ ²   ÀÞ Ý Ìß Ø ½ Ä  **'.
              03 FILLER       PIC  X(10) VALUE SPACE.
              03 LPH-DATE     PIC  99/99/99.
           02 LPH-HEAD2.
              03 FILLER       PIC  X(73) VALUE SPACE.
              03 FILLER       PIC  X(02) VALUE 'P '.
              03 LPH-PAGE     PIC  ZZ9.
           02 LPH-HEAD3.
              03 FILLER       PIC  X(05) VALUE SPACE.
              03 FILLER       PIC  X(15) VALUE 'ÌßÛ¸Þ×ÑÒ²    ·-'.
              03 FILLER       PIC  X(19) VALUE SPACE.
              03 FILLER       PIC  X(11) VALUE 'ÒÂ¾-¼Þ Å²Ö³'.
       01  LPD-DETLE.
           02 LPD-DETL1.
              03 FILLER       PIC  X(05).
              03 LPD-PRGME    PIC  X(08).
              03 FILLER       PIC  X(05).
              03 LPD-KEY      PIC  X(16).
              03 FILLER       PIC  X(05).
              03 LPD-MSG      PIC  X(25).
       01  LPE-NOERR.
           02 LPE-ERR1.
              03 FILLER       PIC  X(10)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****----------- V B'.
              03 FILLER       PIC  X(20)  VALUE ' K A 6 4 0 0 -------'.
              03 FILLER       PIC  X(10)  VALUE '-----*****'.
           02 LPE-ERR2.
              03 FILLER       PIC  X(10)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****               '.
              03 FILLER       PIC  X(20)  VALUE '                    '.
              03 FILLER       PIC  X(10)  VALUE '     *****'.
           02 LPE-ERR3.
              03 FILLER       PIC  X(10)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****       ´ × -   '.
              03 FILLER       PIC  X(20)  VALUE 'D A T A   Å ¼ !!    '.
              03 FILLER       PIC  X(10)  VALUE '     *****'.
           02 LPE-ERR4.
              03 FILLER       PIC  X(10)  VALUE SPACE.
              03 FILLER       PIC  X(20)  VALUE '*****---------------'.
              03 FILLER       PIC  X(20)  VALUE '--------------------'.
              03 FILLER       PIC  X(10)  VALUE '-----*****'.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WKA-R = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                OUTPUT LPFILE.
           MOVE ZERO TO WCT1-PAGE.
           MOVE 99   TO WCT1-LCNT.
           CALL 'VSBA0070' USING P07-PARM.
           MOVE P07-DATE TO P02-DATE.
           CALL 'VSBA0020' USING P02-PARM.
           MOVE P02-DATE TO LPH-DATE.
       M1-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WKA-R.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WCT1-LCNT NOT < 60
              PERFORM M21-HEADER THRU M21-EXIT.
           MOVE SPACE      TO LPD-DETL1.
           MOVE HUI-PRGME  TO LPD-PRGME.
           MOVE HUI-DKEY   TO LPD-KEY.
           MOVE HUI-MSGNI  TO LPD-MSG.
           WRITE LP-REC FROM LPD-DETL1 AFTER 1.
           ADD 1 TO WCT1-LCNT.
       M2-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 :  HEADER                                              *
      *----------------------------------------------------------------*
       M21-HEADER.
           ADD 1 TO WCT1-PAGE.
           MOVE WCT1-PAGE TO LPH-PAGE.
           WRITE LP-REC FROM LPH-HEAD0 AFTER PAGE.
           WRITE LP-REC FROM LPH-HEAD1 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD3 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD0 AFTER 1.
           MOVE 5 TO WCT1-LCNT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           IF WCT1-LCNT NOT = 99
              GO TO M3-L02.
       M3-L01.
           MOVE 1 TO LPH-PAGE.
           WRITE LP-REC FROM LPH-HEAD0 AFTER PAGE.
           WRITE LP-REC FROM LPH-HEAD1 AFTER 1.
           WRITE LP-REC FROM LPH-HEAD2 AFTER 1.
           WRITE LP-REC FROM LPE-ERR1  AFTER 1.
           WRITE LP-REC FROM LPE-ERR2  AFTER 1.
           WRITE LP-REC FROM LPE-ERR2  AFTER 1.
           WRITE LP-REC FROM LPE-ERR3  AFTER 1.
           WRITE LP-REC FROM LPE-ERR2  AFTER 1.
           WRITE LP-REC FROM LPE-ERR2  AFTER 1.
           WRITE LP-REC FROM LPE-ERR4  AFTER 1.
           PERFORM M31-CLOSE-PROC THRU M31-EXIT.
           GO TO M3-EXIT.
       M3-L02.
           DISPLAY '*-------- V B K A 6 4 0 0 ---------*' UPON CONSOLE.
           DISPLAY '*                                  *' UPON CONSOLE.
           DISPLAY '*   URIKAKE IJYOU DATA HASSEI !!   *' UPON CONSOLE.
           DISPLAY '*                                  *' UPON CONSOLE.
           DISPLAY '*        SE  CALL  ( WAIT )        *' UPON CONSOLE.
           DISPLAY '*                                  *' UPON CONSOLE.
           DISPLAY '*----------------------------------*' UPON CONSOLE.
       M3-L03.
           DISPLAY 'M VBKA6400 ACCEPT KEYIN = OK' UPON CONSOLE.
           ACCEPT SYS-CARD1 FROM CONSOLE.
           IF SYS-DATA = 'OK'
              DISPLAY 'R VBKA6400 IJYOU HASSEI NI YORI ABEND'
                       UPON CONSOLE
              PERFORM M31-CLOSE-PROC THRU M31-EXIT
              CALL 'VSBABEND'
           ELSE
              GO  TO  M3-L03.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-PROC                                           *
      *----------------------------------------------------------------*
       M31-CLOSE-PROC.
           CLOSE WKAFILE
                 LPFILE.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA6400 END                                           *
      *----------------------------------------------------------------*
