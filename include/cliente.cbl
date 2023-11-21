      *    Tabla CLIENTE.
      *    EXEC SQL
      *         DECLARE CLIENTE TABLE
      *         (
      *             ID_CLIENTE               INT         NOT NULL,
      *             NOM_CLIENTE              VARCHAR(45) NOT NULL,
      *             FEC_NAC_CLIENTE          DATE        NOT NULL,
      *             NIF_CLIENTE              VARCHAR(10) NOT NULL,
      *             ID_DOM                   INT
      *         )
      *    END-EXEC.
      
      * El archivo como la tabla(mismo nombre) y un area para los datos      
      * y otra para los nulls.
      * Variables host (de COBOL) que van a recibir mis variables guest
      * (de SQL).
       01  REG-CLIENTE.
           05  CLIENTE-ID                PIC 9(10).
           05  CLIENTE-NIF               PIC X(10).
           05  CLIENTE-NOM               PIC X(57).
           05  CLIENTE-FEC-NAC           PIC X(08).
           05  CLIENTE-ID-DOM            PIC 9(10).
      * La nulidad no es un valor de nuestra celda, si no un atributo de
      * la misma. En nuestros atributos que no puedan ser nulos no hay
      * problema pero en los que sí activaríamos una bandera de NULL,
      * cuando es cero, no es nulo. Si es menos uno, es NULL, sea lo que
      * sea que hayamos grabado en nuestra celda.
       01  REG-CLIENTE-NULL.
           05  CLIENTE-ID-NULL           PIC S9(04) COMP-5.
           05  CLIENTE-NIF-NULL          PIC S9(04) COMP-5.
           05  CLIENTE-NOM-NULL          PIC S9(04) COMP-5.
           05  CLIENTE-FEC-NAC-NULL      PIC S9(04) COMP-5.
           05  CLIENTE-ID-DOM-NULL       PIC S9(04) COMP-5.