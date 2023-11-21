       01  AREA-INSRLN.
           05  AREA-INSRLN-ENTRADA.
               10  INSRLN-E-CLI-ID             PIC 9(10).
               10  INSRLN-E-CTA-ID             PIC 9(10).
               10  INSRLN-E-RLN                PIC X(05).
                   88  INSRLN-E-TIT            VALUE 'T'.
                   88  INSRLN-E-COTIT          VALUE 'C'.
                   88  INSRLN-E-AUT            VALUE 'A'.  
                        
           05  AREA-INSRLN-SALIDA.
               10  INSRLN-SQLCODE              PIC S9(09) COMP-5.
               10  INSRLN-STAT                 PIC S9(01) VALUE 0.
                   88  INSRLN-STAT-OK          VALUE 0.
                   88  INSRLN-STAT-ERR-CTA-ID  VALUE -1.
                   88  INSRLN-STAT-ERR-CLI-ID  VALUE -2.
                   88  INSRLN-STAT-ERR-RLN     VALUE -3.
                   88  INSRLN-STAT-ERR-SQL     VALUE -4.
