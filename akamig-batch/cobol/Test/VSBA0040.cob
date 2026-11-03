./ ADD NAME=VSBA0040  01.00 20150623 20150623 140043 00072 00072 MCSK013
       ID  DIVISION.
       PROGRAM-ID.      'VSBA0040'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : LOAD SOSIKI TABLE                *
      *                                                                *
      *                 *  OUTPUT.PARM  : SOSHIKI TABLLE ENTRY         *
      *                                                                *
      *                                                                *
      *              <> AUTHOR      : CSK-F.M                          *
      *              <> DATE        : 82/06/03                         *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-394              ÍÝº³   : 00/08/01  CSK-S.KOZAWA        *
      * (02) K-1758             ÍÝº³   : 15/06/23  CSK-Y.YOSHIGA       *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TBLFILE ASSIGN        TBLFILE
                          ORGANIZATION INDEXED
                          ACCESS MODE SEQUENTIAL
                          RECORD KEY TSZ-KEY
                          FILE STATUS S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  TBLFILE
           LABEL RECORD STANDARD.
       01  TSZ-R.
           COPY  VFILTSZ.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  WK-I                PIC S9(4)  COMP.
       01  S-CODE              PIC  9(2).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB04.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    1.0 : MAIN CONTROL                                          *
      *----------------------------------------------------------------*
       M10-MAIN-CONTROL.
           OPEN INPUT TBLFILE.
           MOVE ZERO TO WK-I.
           MOVE LOW-VALUE TO TSZ-KEY.
           MOVE 'TZ' TO TSZ-RECID.
           START TBLFILE KEY > TSZ-KEY.
       L10-01.
           ADD 1 TO WK-I.
           IF  WK-I  NOT <  3500
               GO TO L10-02.
       L10-011.
           READ  TBLFILE
                 AT END
                    GO TO L10-02.
           IF  TSZ-RECID NOT = 'TZ'
               GO TO L10-02.
           IF  TSZ-SOSIKI = SPACE
               GO TO L10-011.
           MOVE TSZ-SOSIKI TO P04-SOSIKI (WK-I).
           GO TO L10-01.
       L10-02.
           MOVE  ALL '*'  TO P04-SOSIKI (WK-I).
           CLOSE TBLFILE.
       M14-EXIT.
           GOBACK.
