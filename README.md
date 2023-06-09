# PySharp

Este proyecto tiene como objetivo la creación de un parser y un mini lenguaje usando **F#** y la biblioteca <a href="https://github.com/stephan-tolksdorf/fparsec">FParsec</a>.

### Dependencias

El proyecto se encuentra desarrollado en **.Net 7** y depende de la biblioteca <a href="https://github.com/stephan-tolksdorf/fparsec">FParsec</a>, para instalarla debe contar con el siguiente paquete <a href="https://www.nuget.org/packages/FParsec/">nuget</a>

### Ejecutando el Proyecto

Para ejecutar el proyecto debe contar en sus sistema operativo con las especificaciones antes mencionadas, una vez hecho esto puede ejecutar en su terminal:

```bash
make file=<file>
```

donde `<file>` es la ruta del archivo que contiene el código a ejecutar. Otra forma de ejecutar el proyecto es la siguiente si no tiene **make** instalado en su sistema:

```bash
dotnet run --project Execute <file>
```

también cuenta con una consola interactiva a la cual se puede acceder mediante el comando:

```bash
make
```

o

```bash
dotnet run --project Execute
```

una vez en la consola interactiva puede escribir su programa. Para ejecutar bloques de código que requieran varias líneas de
código puede usar el comando `init` y para finalizar un bloque `end`. Para finalizar el programa puede usar los comandos
`q`, `exit` o `quit`.
