       01  AREA-INSCTA.
           05  AREA-INSCTA-ENTRADA.
               10  INSCTA-E-NUM                PIC X(20).
               10  INSCTA-E-SALDO              PIC S9(08)V99.    
                                  
           05  AREA-INSCTA-SALIDA.
               10  INSCTA-S-CLIENTE.
                   15  INSCTA-S-CTA-ID         PIC 9(10).
               10  INSCTA-S-SQLCODE            PIC S9(09) COMP-5.
               10  INSCTA-STAT                 PIC S9(01) VALUE 0.
                   88  INSCTA-STAT-OK          VALUE 0.
                   88  INSCTA-STAT-ERR-NUM     VALUE -1.
                   88  INSCTA-STAT-ERR-SQL     VALUE -2.

