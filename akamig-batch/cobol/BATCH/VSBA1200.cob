./ ADD NAME=VSBA1200  01.04 19911002 19911007 110300 00067 00073 TSO82
       ID  DIVISION.
       PROGRAM-ID.     'VSBA1200'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : MATUJITU SYUTOKU(8KETA TAIOU) *
      *                                                                *
      *                 * INPUT  PARM  : PC0-YMDS8                     *
      *                 * OUTPUT PARM  : PC0-DDS8                      *
      *                                                                *
      *              <> AUTHOR         : CSK-K.KOYAMA                  *
      *              <> DATE           : 91/10/02                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WK-AREA.
           02 WK-YY                         PIC  9(02).
           02 WK-AMARI                      PIC  9(02).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARBC0.
       PROCEDURE DIVISION USING PARAM.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           IF PC0-MMS8 = 1 OR 3  OR 5  OR 7 OR
                         8 OR 10 OR 12
              MOVE 31 TO PC0-DDS8
           ELSE
              IF PC0-MMS8 = 2
                 PERFORM M11-MAIN THRU M11-EXIT
              ELSE
                 MOVE 30 TO PC0-DDS8.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : MAIN                                                 *
      *----------------------------------------------------------------*
       M11-MAIN.
           DIVIDE PC0-YYS8 BY 4 GIVING WK-YY REMAINDER WK-AMARI.
           IF WK-AMARI = ZERO
              MOVE 29 TO PC0-DDS8
           ELSE
              MOVE 28 TO PC0-DDS8.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VSBA1200 END                                           *
      *----------------------------------------------------------------*
