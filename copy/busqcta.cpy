       01  AREA-BUSQCTA.
           05  AREA-BUSQCTA-ENTRADA.
               10  BUSQCTA-E-ID                PIC 9(10).
               10  BUSQCTA-E-ID-X              REDEFINES BUSQCTA-E-ID
                                               PIC X(10).
               10  BUSQCTA-E-NUM               PIC 9(20).
               10  BUSQCTA-E-NUM-X             REDEFINES BUSQCTA-E-NUM
                                               PIC X(20).
               10  BUSQCTA-E-CRITERIO          PIC 9(01) VALUE 0.
                   88  BUSQCTA-CRIT-ID         VALUE 0. 
                   88  BUSQCTA-CRIT-NUM        VALUE 1.
                   
           05  AREA-BUSQCTA-SALIDA.
               10  BUSQCTA-CUENTA.
                   15  BUSQCTA-S-ID            PIC 9(10).
                   15  BUSQCTA-S-NUM           PIC 9(20).
                   15  BUSQCTA-S-SALDO         PIC S9(08)V99
                                               LEADING SEPARATE.

               10  BUSQCTA-SQLCODE             PIC S9(09) COMP-5.
               10  BUSQCTA-STAT                PIC S9(01) VALUE 0.
                   88  BUSQCTA-STAT-OK         VALUE 0.
                   88  BUSQCTA-STAT-ENC-NO     VALUE 1.
                   88  BUSQCTA-STAT-ERR-CRIT   VALUE -1.
                   88  BUSQCTA-STAT-ERR-ID     VALUE -2.
                   88  BUSQCTA-STAT-ERR-NUM    VALUE -3.
                   88  BUSQCTA-STAT-ERR-SQL    VALUE -4.
