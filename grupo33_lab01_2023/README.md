# <span style="color:purple"> Laboratorio 1</span>
---
## <span style="color:purple"> Grupo</span>

todo_por_dos_pesos*

### <span style="color:purple"> Integrantes </span>

**- Hubmann, Tomas Alejandro**

**- Ramirez, Ignacio Tomas**

**- Strasorier, Marcos Emanuel**

---
### <span style="color:purple"> Preguntas:</span>
#### **(1) ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.**
Las funcionalidades están separadas en módulos para poder tener un código más ordenado y modularizado. De esta manera, cada módulo tiene una responsabilidad específica.
En particular, en los módulos de <span style="color:purple"> Dibujo.hs</span> y <span style="color:purple"> Pred.hs</span> , se definen los tipos de datos que va a tener nuestro lenguaje y las funciones relacionadas a cada uno de ellos, mas específicamente, la sintaxis. En el módulo de <span style="color:purple"> Interp.hs</span>, se definen las funciones que van a ser utilizadas para interpretar el código que haremos en nuestro archivo de dibujo como tal (grilla, escher, etc), es decir implementamos la semántica. Por último, en el módulo main, se definen las funciones que van a ser utilizadas para leer el archivo de entrada y luego interpretarlo. 


- **<span style="color:purple"> Dibujo.hs</span>**: En este módulo definimos el tipo de dato que va a tener nuestro lenguaje, *Dibujo a*. Este es un tipo polimórfico ya que 'a' puede ser algo de cualquier tipo. En este archivo tambien están definias las funciones que llaman a los constructores del tipo. Nuestra funcion más basica, es decir, que transforma algo de tipo *a* en algo de tipo *Dibujo a*, es la funcion figura.

- **<span style="color:purple"> Pred.hs</span>**: Este módulo se encarga de implementar funciones para el manejo de figuras básicas. Definimos el tipo pred, como una función que toma un un elemento de tipo *a* y devuelve un elemento *booleano*. Este tipo *pred*, abstrae el hecho de comprobar, a partir de una condición que nosotros pasamos, que una figura básica sea la que justamente estamos buscando al momento de recorrer un dibujo. A esa condición dada la aplicamos, por ejemplo, por primera vez en la función *cambiar*, implementada con el uso de *foldDib*. *Cambiar*, utiliza el predicado sobre figuras básicas que definimos anteriormente, es decir, una condición. Cambia todas las que satisfacen el predicado, por la figura básica indicada en el segundo argumento (a -> Dibujo a). Falta terminar

- <span style="color:purple"> FloatingPic.hs</span>: Las definiciones mas importantes que tenemos en este archivo son Output a y Floating.  
    - Output a: toma algo de tipo a, esto implica que es polimorfico, y devuelve algo FloatingPic.
    - FloatingPic: Toma tres vectores y devuelve algo de tipo Picture, que es el tipo traido de Gloss. Se utiliza para convertir algo de tipo a, en una Picture que se puede imprimir por pantalla.  

    Los tres vectores que toma FloatingPic son los que toman protagonizmo en <span style="color:purple"> Interp.hs</span> y se van a utilizar para pasarles las coordenadas necesarias para las funciones de Gloss.

- **<span style="color:purple"> Interp.hs</span>**: 
Acá es donde las operaciones definidas en <span style="color:purple"> *Dibujo.hs*</span> dejan de ser algo abstracto y les damos, como bien indica el nombre del archivo, una interpretacion. Para ésto, utilizamos la libreria Gloss. Usando el tipo **Output a** definido en <span style="color:purple"> *FloatingPic.hs*</span> y llamando a los constructores de Data.Picture de Gloss es que pasamos de algo de tipo 'a' a una imagen. Para llevar esto a cabo, se usa la función **interp**, que lo único que hace es llamar a foldDib con el Output de a y todas las funciones "auxiliares" que definimos para interpretar cada operación de Dibujo.hs.  
Esas funciones auxiliares toman una FloatingPic y devuelven algo del mismo tipo pero con los vectores modificados para que realize lo que le pide la operacion.
En este modulo tambien esta definido el tipo Conf que es un par (String, Floating Pic) donde la String va a ser el nombre asociado a la imagen que va a aparecer al ejecutar ./Main nombre. La funcion interpConf toma una configuracion y dos flotantes, que representan el tamano de la imagen. La ultima funcion, initial, toma una configuracion, un flotante y devuelve algo de tipo input-output, es decir, lo que se va a imprimir en pantalla.

- **<span style="color:purple"> Main.hs</span>**: Es a este modulo que llegan las configuraciones de los dibujos de la carpeta homonima y se los acomoda en una lista de configuraciones. La funcion initial' toma esa lista de configuraciones y el string con el nombre del dibujo a ejecutar. Recorre la lista y cuando encuentra una configuracion con el mismo nombre que la string que le pasaron, llama a la funcion initial de <span style="color:purple"> Interp.hs</span>. Es esta funcion que va a ser llamada en la funcion main para que haga el dibujo.



#### **(2) ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y envez es un parámetro del tipo?**

Las figuras no estan incluidas en la definicion del lenguaje porque la idea es que independientemente de la implemetacion que haga cada usuario de sus figuras, las funciones que operan sobre esos dibujos sea la misma.

#### **(3) ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?**  
La ventaja de usar fold sobre patt es que nos permite mantener la implementacion opaca para el resto de los modulos ya que no necesitamos exportar los constructores definidos en Dibujo y solamente operamos con las funciones especificas de cada modulo. A su vez, como a fold el usuario le pasa las funciones que haya definido en su implementacion, sin importar que haga, el resultado deberia ser el mismo por como definimos foldDib.