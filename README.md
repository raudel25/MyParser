# MyParser

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
dotnet run --project Compiler <file>
```

también cuenta con una consola interactiva a la cual se puede acceder mediante el comando:

```bash
make
```

o

```bash
dotnet run --project Compiler
```

una vez en la consola interactiva puede escribir su programa y finalizar el mismo mediante los comandos `q`, `exit` o `quit`.

## Mini Lenguaje

### Tipos

El lenguaje es de tipado dinámico y soporta los siguientes tipos: **int**, **double**, **string**, **char** y **array**. Los tipos personalizados se pueden definir usando **struct**.

```
a = 2 + 3;
b = "hola mundo";
c = [ 1 , 2 , 3 ];
d = [ 'a' ; 2 ];
e = [ d , 2 + 1 , [ 2 , 3 ] ];
```

La expresión de tipo **array** `[ 'a' ; 2 ]` representa un **array** dinámico que contiene 2 elementos con valor `'a'` cada uno.

### Bucles

El lenguaje cuenta con los tradicionales bucles **while** y **for**.

```
for i in 0, 10
{
    printLn(i);
}

for i in 2, 20, 2
{
    printLn(i);
}

i = 10;
while (i > -1)
{
    printLn(i);
    i -= 1;
}
```

Los ejemplos anteriores imprimen los números del 0 al 10 (sin incluir el 10), los números pares desde 2 hasta 20 (sin incluir el 20) y los números del 0 al 10 en orden inverso.

### Condicionales

El lenguaje cuenta con las tradicionales instrucciones **if**, **else** y **elif**.

```
if (2 > 3)
{
    printLn("2 es mayor que 3");
}
elif (4 > 3)
{
    printLn("2 no es mayor que 3, pero 4 es mayor que 3");
}
else
{
    printLn("2 no es mayor que 3, ni 4 es mayor que 3");
}
```

### Funciones

Las funciones se declaran con la palabra reservada **func** y luego se especifican el nombre y los parámetros de la función. Además las funciones pueden ser tratadas al mismo nivel que las variables lo que permite que se puedan asignar o pasar como parámetros a otra función.

```
func gcd( x , y )
{
    while ( y != 0 )
    {
        r = x % y;
        x = y;
        y = r;
    }

    return x;
}

a = gcd;
b = [ a , gcd ];
```

### Structs 

Para definir tipos personalizados se pueden usar los structs, los cuales permiten crear una estructura con varias propiedades.

```
struct point{ x , y }

p = point{ 1 + 2 , 2 };

printLn(p.x);
```

### Funciones Predefinidas

- `printLn` y `printL`: Imprime en la terminal.
- `input`: Retorna un **string**, que es introducido por el usuario en la terminal.
- `int`: Convierte un tipo a **int**.
- `double`: Convierte un tipo a **double**.
- `str`: Convierte un tipo a **string**.
