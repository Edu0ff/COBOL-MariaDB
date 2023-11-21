       01  AREA-ALTACLIENTE.
           05  ALTACLIENTE-ENTRADA.
               10  ALTACLI-CLIENTE.
                   15  ALTACLI-CLI-NOMBRE        PIC X(57).
                   15  ALTACLI-CLI-FEC-NAC       PIC 9(08).
                   15  ALTACLI-CLI-NIF           PIC X(10).

               10  ALTACLI-DOMICILIO.
                   15  ALTACLI-DOM-CALLE         PIC X(35).
                   15  ALTACLI-DOM-NUMERO        PIC X(03).
                   15  ALTACLI-DOM-CODPOS        PIC 9(05).
                   15  ALTACLI-DOM-PROV          PIC X(16).
                   15  ALTACLI-DOM-POBL          PIC X(16).

               10  ALTACLI-CUENTAS-TARJETAS.
                   15  ALTACLI-CUE-TAR-CONTADOR  PIC 9(01) VALUE 0.
                   15  ALTACLI-CUE-TAR           OCCURS 5 TIMES.
                       20  ALTACLI-NUM-CTA       PIC 9(20).
                       20  ALTACLI-SALDO-CTA     PIC S9(08)V99 
                                                 LEADING SEPARATE.
                       20  ALTACLI-RLN-CTA       PIC X(01).
                           88  A-CLI-CUE-TIT     VALUE 'T'.
                           88  A-CLI-CUE-CO      VALUE 'C'.
                           88  A-CLI-CUE-AU      VALUE 'A'.
                                     

                       20  ALTACLI-NUM-TAR       PIC 9(16).
                       20  ALTACLI-CRE-TAR       PIC S9(08)V99 
                                                 LEADING SEPARATE.
                       20  ALTACLI-FEC-TAR       PIC 9(04).
                       20  ALTACLI-CCV-TAR       PIC 9(03).

               10  ALTACLI-MOVIMIENTOS.
                   15  ALTACLI-MOV-CONTADOR              PIC 9(01) 
                                                         VALUE 0.
                   15  ALTACLI-MOVIMIENTO                OCCURS 5 TIMES.
                       20  ALTACLI-MOV-CPT               PIC X(23).
                       20  ALTACLI-MOV-IMPORTE           PIC S9(08)V99 
                                                         LEADING 
                                                         SEPARATE.
                       20  ALTACLI-MOV-NUM-MEDIO-CTA     PIC 9(20).
                       20  ALTACLI-MOV-NUM-MEDIO-TAR     PIC 9(16).    
                       20  ALTACLI-MOV-TIPO-MEDIO        PIC X(01).
                           88  ALTACLI-MOV-TIPO-MEDIO-C  VALUE 'C'.
                           88  ALTACLI-MOV-TIPO-MEDIO-T  VALUE 'T'.
                       20  ALTACLI-MOV-FEC               PIC 9(20).

           05  ALTACLIENTE-SALIDA.
               10  ALTACLI-STAT                      PIC S9(02) VALUE 0.
                   88  ALTACLI-STAT-OK               VALUE 0.
                   88  ALTACLI-STAT-ERR-COD-POS      VALUE -1.
                   88  ALTACLI-STAT-ERR-CALLE        VALUE -2.
                   88  ALTACLI-STAT-ERR-POBL         VALUE -3.
                   88  ALTACLI-STAT-ERR-PROV         VALUE -4.
                   88  ALTACLI-STAT-ERR-CLI-ENC      VALUE -5.
                   88  ALTACLI-STAT-ERR-NIF          VALUE -6.
                   88  ALTACLI-STAT-ERR-NOM          VALUE -7.
                   88  ALTACLI-STAT-ERR-FEC-NAC      VALUE -8.
                   88  ALTACLI-STAT-ERR-CTA-NUM      VALUE -9.
                   88  ALTACLI-STAT-CTA-ENC-NO       VALUE -10.
                   88  ALTACLI-STAT-ERR-RLN-ENC      VALUE -11.
                   88  ALTACLI-STAT-ERR-RLN-TIP      VALUE -12.
                   88  ALTACLI-STAT-ERR-TAR-ENC      VALUE -13.
                   88  ALTACLI-STAT-TAR-ENC-NO       VALUE -14.
                   88  ALTACLI-STAT-ERR-TAR-NUM      VALUE -15.
                   88  ALTACLI-STAT-ERR-TAR-FEC      VALUE -16.
                   88  ALTACLI-STAT-ERR-TAR-CCV      VALUE -17.
                   88  ALTACLI-STAT-ERR-MOV-ENC      VALUE -18.
                   88  ALTACLI-STAT-ERR-MOV-ID       VALUE -19.
                   88  ALTACLI-STAT-ERR-MOV-CPT      VALUE -20.
                   88  ALTACLI-STAT-ERR-MOV-FEC      VALUE -21.
                   88  ALTACLI-STAT-ERR-SQL          VALUE -22.
               10  ALTACLI-SQLCODE                   PIC S9(09) COMP-5.

