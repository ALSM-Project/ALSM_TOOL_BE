./ ADD NAME=VSBA2720  01.02 19981020 19981020 133929 00063 00061 TSO67
       ID  DIVISION.
       PROGRAM-ID.     'VSBA2720'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : SUBROUTINE                    *
      *              <> PG.OUTLINE     : KAISYUU YOTEIBI GETSUMATSU    *
      *                                  HENKAN                        *
      *                                                                *
      *                 * INPUT  PARM  : P272-SYMD                     *
      *                 * OUTPUT PARM  : P272-SYMD                     *
      *                                                                *
      *              <> AUTHOR         : CSK-T.NAKAMURA                *
      *              <> DATE           : 98/10/20                      *
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
       01  PC0-PARM.
           COPY VPARBC0 SUPPRESS.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARAM.
           COPY VPARB272.
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
           MOVE P272-SYMD TO PC0-YMDS8.
           CALL 'VSBA1200' USING PC0-PARM.
       M1-L01.
           EVALUATE P272-DD
              WHEN  31
                 MOVE PC0-YMDS8 TO P272-SYMD,
              WHEN  30
                 COMPUTE PC0-DDS8 = PC0-DDS8 - 1
                 MOVE PC0-YMDS8 TO P272-SYMD,
              WHEN  29
                 COMPUTE PC0-DDS8 = PC0-DDS8 - 2
                 MOVE PC0-YMDS8 TO P272-SYMD,
              WHEN  OTHER
                 CONTINUE
           END-EVALUATE.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VSBA2720 END                                           *
      *----------------------------------------------------------------*
