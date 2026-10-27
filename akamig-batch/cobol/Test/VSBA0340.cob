./ ADD NAME=VSBA0340  01.00 19910312 19910312 100800 00072 00072 TSO45
       ID  DIVISION.
       PROGRAM-ID.      'VSBA0340'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : LOAD SOSIKI CODE HENKAN TABLE    *
      *                                                                *
      *                 *  OUTPUT.PARM  : SOSHIKI HENKAN TABLLE ENTRY  *
      *                                                                *
      *              <> AUTHOR      : CSK-T.T                          *
      *              <> DATE        : 85/03/14                         *
      *---------------- MAINTENANCE HISTORY ---------------------------*
      * (1) SOKO NO.            Â²¶           86/08/26   CSK-T.HATANO  *
      * (2) MAX 100 => 150     ÍÝº³           91/03/12   SUZ-Y.KODAIRA *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TBLFILE ASSIGN        TBLFILE
                          ORGANIZATION INDEXED
                          ACCESS MODE SEQUENTIAL
                          RECORD KEY TSC-KEY
                          FILE STATUS S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  TBLFILE
           LABEL RECORD STANDARD.
       01  TSC-R.
           COPY  VFILTSC.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  WK-AREA.
           02  WK-KEY          PIC  X(3)  VALUE 'TSC'.
       01  WK-I                PIC S9(4)  COMP.
       01  S-CODE              PIC  9(2).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB34.                                                VPA00010
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    1.0 : MAIN CONTROL                                          *
      *----------------------------------------------------------------*
       M10-MAIN-CONTROL.
           OPEN INPUT TBLFILE.
           MOVE ZERO TO WK-I.
           MOVE LOW-VALUE TO TSC-KEY.
           MOVE 'TSC' TO TSC-RECID.
           START TBLFILE KEY > TSC-KEY.
       L10-01.
           ADD 1 TO WK-I.
           IF  WK-I  NOT <  150
               GO TO L10-02.
       L10-011.
           READ  TBLFILE
                 AT END
                    GO TO L10-02.
           IF  TSC-RECID NOT = 'TSC'
               GO TO L10-02.
           IF  TSC-TABLE  = SPACE
               GO TO L10-011.
           MOVE TSC-TABLE  TO P34-TABLE  (WK-I).
           GO TO L10-01.
       L10-02.
           MOVE  ALL '*'  TO P34-TABLE  (WK-I).
           CLOSE TBLFILE.
       M14-EXIT.
           GOBACK.
