# COBOL-BBDD-RUTINAS
Proyecto ficticio basado en un banco.  

Escrito con especificaciones dadas por un profesional senior en el sector financiero. Este proyecto se ha realizado para asentar conocimientos en COBOL.  
Se ha utlizado GNUCobol en el sistema operativo Fedora 39. Y como preprocesador y entorno de ejecución SQL: [ESQL for ODBC for GnuCobol/OpenCOBOL](http://www.kiska.net/opencobol/esql/).  

Este programa maneja ficheros .dat e inserta los datos en una BBDD (MariaDB), escrito en COBOL.  
En el directorio "Documentación" se encuentran las especificaciones más detallada del funcionamiento contenidas en los análisis funcional y orgánico, así como el modelado de la BBDD.

El código está enormemente comentado dado que quería detallar al máximo el proceso de escritura del mismo, con el objetivo de aprender y poder repasarlo posteriormente si fuera necesario.  
La convención de nomenclatura usada ha sido la utilizada en una entidad bancaria concreta.  
Para mayor comodidad de compilado y ejecución, se dispone de un makefile y un script bash para leer distintos ficheros en el programa principal de forma dinámica.  

Próximamente me gustaría añadir un informe también escrito en COBOL que se imprima al finalizar las transacciones con éxito, y contenedorizar el programa con Docker.
