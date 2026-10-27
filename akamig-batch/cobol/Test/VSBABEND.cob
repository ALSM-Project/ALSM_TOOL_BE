./ ADD NAME=VSBABEND  01.00 20140925 20140925 103008 00030 00030 MCSK082
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBABEND'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : DISPALAY CANCEL                  *VSB00070
      *                                                                *VSB00080
      *                 *  INPUT PARM : NOTHING                        *VSB00090
      *                                                                *VSB00100
      *                 * OUTPUT PARM : NOTHING                        *VSB00110
      *                                                                *VSB00120
      *              <> AUTHOR      : CSK-N.T                          *VSB00130
      *              <> DATE        : 82/07/22                         *VSB00140
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1616           ØºÝÊß²Ù  : 14/09/25  CSK-K.KOYAMA        *
      * (02) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*VSB00150
       ENVIRONMENT DIVISION.                                            VSB00160
       DATA DIVISION.                                                   VSB00170
       PROCEDURE DIVISION.                                              VSB00180
      *----------------------------------------------------------------*VSB00190
      *    1.0 : DISPLAY 'CANCEL' ON CONSOLE FOR OPERATOR CANCEL       *VSB00200
      *----------------------------------------------------------------*VSB00210
       M10-MAIN.                                                        VSB00220
      *                                                                 VSB00230
           STOP  'CANCEL'.                                              VSB00240
           GO TO  M10-MAIN.                                             VSB00250
       M10-EXIT.                                                        VSB00260
           EXIT.                                                        VSB00270
