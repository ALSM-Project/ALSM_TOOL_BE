./ ADD NAME=VSBA0900  01.01 19890613 19890613 134500 00074 00059 SPG5
//VSBA0900 JOB MSGCLASS=H,CLASS=S                                       00000100
//ASM   EXEC  PGM=IEV90,                                                00000200
//            PARM='DECK,NOOBJECT,LIST,XREF,ALIGN'                      00000300
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR                                 00000400
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(400,400))                        00000500
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(400,400))                        00000600
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(400,400))                        00000700
//SYSPUNCH DD  DSN=&&LOADCET,                                           00000800
//             UNIT=SYSDA,DISP=(,PASS),                                 00000900
//             SPACE=(400,(100,100,1)),                                 00001000
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=400)                      00001100
//SYSPRINT DD  SYSOUT=*                                                 00001200
//SYSIN    DD  *                                                        00001300
*----------------------------------------------------------------*      00003000
*      PROGRAM-ID.      'VSBA0900'.                              *      00002000
*----------------------------------------------------------------*      00003000
*    SUMMARY   <> WAIT SUBROUTINE                                *      00004000
*                                                                *      00007000
*                                                                *      00009300
*              <> DATE        : 89/06/13                         *      00009400
*--- MAINTENANCE HISTORY ----------------------------------------*      00009500
*                                                                *      00009600
*                                                                *      00009600
*                                                                *      00009600
*                                                                *      00009600
*                                                                *      00009600
*                                                                *      00009600
*----------------------------------------------------------------*      00010200
         PRINT GEN,DATA                                                 00130000
         START
VSBA0900 CSECT
         SAVE    (14,12)
         LR      3,15
         USING   VSBA0900,3
         ST      13,SAVEAREA+4
         LR      12,13
         LA      13,SAVEAREA
         ST      13,8(12)
         ST      12,4(13)
*
BEGIN    EQU   *                                                        00140000
         L     2,0(1)
         MVC   PARM,0(2)
         PACK  PPARM,PARM
         MP    PPARM,=P'6000'
         CVB   4,PPARM
         ST    4,TIMEINT
*
TIME1    EQU  *
         STIMER WAIT,BINTVL=TIMEINT
*
         L 13,SAVEAREA+4
         RETURN (14,12),RC=0
*
SAVEAREA DS 18F
PARM     DS CL2
         DS 0D
PPARM    DS D
TIMEINT  DS CL8
         LTORG
         END                                                            00157900
/*                                                                      00886400
//LKED    EXEC PGM=IEWL, COND=(5,LT,ASM),                               00886500
//             PARM='XREF,LET,LIST,NCAL,AC=1',REGION=512K               00886600
//SYSUT1   DD  UNIT=SYSDA,DCB=BLKSIZE=1024,                             00886700
//             SPACE=(1024,(200,20))                                    00886800
//SYSLMOD  DD  DSN=TEST2.BATCH.LOAD,DISP=SHR                            00886900
//SYSPRINT DD  SYSOUT=*                                                 00887000
//SYSLIN   DD  DSN=&&LOADCET,DISP=(OLD,DELETE)                          00887100
//         DD  DDNAME=SYSIN                                             00887200
//SYSIN    DD  *                                                        00887300
   NAME VSBA0900(R)                                                     00887400
/*                                                                      00887500
//                                                                      00887600
