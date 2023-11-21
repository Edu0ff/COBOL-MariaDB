       01  AREA-BUSQCLI.
           05  AREA-BUSQCLI-ENTRADA.
               10  BUSQCLI-E-ID                PIC 9(10).
               10  BUSQCLI-E-ID-X              REDEFINES BUSQCLI-E-ID
                                               PIC X(10).
               10  BUSQCLI-E-NIF               PIC X(10).
               10  BUSQCLI-E-CRITERIO          PIC 9(01) VALUE 0.
                   88  BUSQCLI-CRIT-ID         VALUE 0. 
                   88  BUSQCLI-CRIT-NIF        VALUE 1.
                   
           05  AREA-BUSQCLI-SALIDA.
               10  BUSQCLI-CLIENTE.
                   15  BUSQCLI-S-ID            PIC 9(10).
                   15  BUSQCLI-S-NIF           PIC X(10).
                   15  BUSQCLI-S-FEC-NAC       PIC 9(08).
                   15  BUSQCLI-S-ID-DOM        PIC 9(10).
               10  BUSQCLI-SQLCODE             PIC S9(09) COMP-5.
               10  BUSQCLI-STAT                PIC S9(01) VALUE 0.
                   88  BUSQCLI-STAT-OK         VALUE 0.
                   88  BUSQCLI-STAT-ENC-NO     VALUE 1.
                   88  BUSQCLI-STAT-ERR-CRIT   VALUE -1.
                   88  BUSQCLI-STAT-ERR-ID     VALUE -2.
                   88  BUSQCLI-STAT-ERR-NIF    VALUE -3.
                   88  BUSQCLI-STAT-ERR-SQL    VALUE -4.
