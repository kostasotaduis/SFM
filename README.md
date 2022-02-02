# SFM
Modelo de fuerza social

¡Hola! Mi nombre es Karlos y te doy la bienvenida a la sala de máquinas de mi trabajo acerca del modelo de fuerza social.
Al igual que con las neveras estamos en el lado feo, pero sin él no tendríamos todos esos gráficos y fotos que tan bonitas se ven.

Si estás aquí es por dos motivos:

A) Desconfías de la veracidad de lo que observas en el pdf y quieres destapar el fraude que es todo este trabajo.
En ese caso, persiona Alt+F4 en tu teclado y estás listo para comenzar! ;)

B) No te consideras igual al resto de la gente. Quieres más. Al leer el pdf sientes que solamente se te está mostrando una parte de lo que realmente
existe. Como Platón, quieres salir de la cueva, acceder al mundo de las ideas donde está el origen de lo que ves con tus sentidos. Quieres, incluso, 
crear tus propios sistemas, un pasillo con elefantes cada 8m a lo largo, un cruce de 17 caminos o la evacuación de una sala con la forma de mozambique.
En ese caso, lo primero es que es posible, que al igual que el hombre que escapa de la caverna, te cieguen en este caso las interminables líneas de
código y las infinitas variables con nombres que te hacen dudar de si el autor quizá sea polaco. Lo único que puedo decir es que mejor que esto no hay.
Dentro de cada programa se intenta clarificar todo lo que se hace pero, el monstruo que he creado es demasiado grande como para controlarlo. Hay, sin
embargo, algunos consejos que te pueden ser útiles en tu travesía. 

  1. Deberías saber hablar Fortran. Quiero decir, ya va a ser complicado para un hablante nativo, así que no quiero imaginar si no sabes...
  
  2. Deberías saber también Gnuplot. No tan importante, pero saberte alguna expresión no vendrá mal.
  
  3. Los archivos que ves en la carpeta raíz se repiten en cada una de las carpetas que contiene. Estos archivos son el núcleo del programa y sin ellos
  no vamos a ningún lado (y están escritos en Fortran, por si aún seguías aquí). Todos están en cada subcarpeta con el mismo nombre excepto por el que
  se llama "pasillo.f95", que es el programa principal y que se llamará en cada carpeta "[NombreDeLaSubcarpeta].f95" (excepto el caso de la evacuación,
  en cuyo caso se llama "puertap.f95"). Aquí una pequeña descripción de lo que hace cada uno
    -util.f95: algunas constantes y funciones que, como bien dice el nombre, son útiles
    -vor.f95: sirve para calcular la densidad mediante las celdas de Voronoi
    -cv.f95: controla características del peatón como por ejemplo su campo visual
    -fobs.f95: controla la interacción persona-objeto
    -fij.f95: controla la interacción persona-persona
    -fluc.f95: controla las fluctuaciones
    -pasillo.f95: PROGRAMA PRINCIPAL
  Para que el programa funcione, se deben compilar el programa principal junto a sus modulos de la siguiente manera:
    g95 -std=F (-o Nombrequelequierasdar) util.f95 vor.f95 cv.f95 fobs.f95 fij.f95 fluc.f95 pasillo.f95
