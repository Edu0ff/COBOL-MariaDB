       01  AREA-INSMOV.
           05  AREA-INSMOV-ENTRADA.
               10  INSMOV-E-ID                 PIC 9(10).
               10  INSMOV-E-FEC                PIC 9(20).
               10  INSMOV-E-CPT                PIC X(49).
               10  INSMOV-E-IMPT               PIC S9(08)V99
                                               LEADING SEPARATE.
           05  AREA-INSMOV-SALIDA.
               10  INSMOV-S-SQLCODE            PIC S9(09) COMP-5.
               10  INSMOV-STAT                 PIC S9(01) VALUE 0.
                   88  INSMOV-STAT-OK          VALUE 0.
                   88  INSMOV-STAT-ERR-ID      VALUE -1.
                   88  INSMOV-STAT-ERR-FEC     VALUE -2.
                   88  INSMOV-STAT-ERR-CPT     VALUE -3.
                   88  INSMOV-STAT-ERR-SQL     VALUE -4.
