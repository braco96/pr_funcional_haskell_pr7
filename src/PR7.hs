 
---------------------------------------------------------SINONIMOS


type DatosPersona = (String,String,Int) ---------------primer elemento es nombre y apellidos , segundo dni y 3º numerode telefono
type Vector = (Integer,Integer) ---------1º primera coordenada 2º segunda coordenada
type Usuario =(String,String)---------Usuario y contraseña

---------------------------------------------------------SINONIMOS
---------------------------------------TIpos
implicacion ::Bool->Bool->Bool
implicacion a b | (a==False && b==True )= False
                |otherwise = True 

mayorQuee :: Char->Char
mayorQuee = (max 'e')

contadorPalabras :: String->Char->String
contadorPalabras hola c =c:hola


espacio :: Int->Int->Int-----------Funcion usada anteriormente
espacio v t = v*t

dineroFinanciar ::Integer->Integer->Integer
dineroFinanciar tengo precio = precio-tengo

moduloPlano::Double->Double->Double
moduloPlano v1 v2= sqrt((v1*v1)+(v1*v1))

---------------------------------------------------------TIPOS

---------------------------------------------------------LISTAS
cuadrado x = x*x

moduloLista lista= sqrt(sum(map (cuadrado) lista))
 

cantidad lista e1 = length([x|x<-lista,x==e1])


cantidad1 lista e1 = sum [1|x<-lista,x==e1]

cantidad2 lista e1 = length(filter e1 lista)


cantidadrecursiva :: (Eq a)=>[a]->a->[a]
cantidadrecursiva [] e1 =[]
cantidadrecursiva (x:xx) e1=if e1==x then [x]++ cantidadrecursiva xx e1
                            else cantidadrecursiva xx e1

------------------------------------------------------------

sumartodos lista e1 = foldl (+) 0 lista

sumartodos1 lista e1 = sum [x|x<-lista,x==e1]

sumartodosrecursiva :: (Num a)=> [a]->a
sumartodosrecursiva [] =0
sumartodosrecursiva (x:xx)=x+sumartodosrecursiva xx
--------------------------------------------------------------

divisores :: Int ->[Int]
divisores n = [x| x <- [1..n],( mod n x) ==0]

interseccion:: (Ord a, Eq a)=>([a],[a]) -> [a]
interseccion (xs,ys)
  |null xs  || null ys = []
  |otherwise                   = if x<y then interseccion (restoXs, ys) 
                                 else if x>y then interseccion (xs, restoYs)
                                 else x:(interseccion (restoXs,restoYs))
                                 where
                                   (x:restoXs) = xs
                                   (y:restoYs) = ys

mcd::Int->Int->[Int]
mcd a b =interseccion(divisores(a),divisores(b))
 
sumarUno ::[Int]->[Int] -----Suma uno a todos los elementos de una lista
sumarUno [] = []------Caso base : ya hemos recorrido la lista
sumarUno (x:xx) = (x+1):sumarUno xx ------sumamos uno al elemento y recorremos de forma
----------recursiva la lista hasta que no tengamos ningun elemento

colaLista :: [a]->[a]
colaLista (_:xx) =xx

moduloCinco ::Int->Bool
moduloCinco num = if ((mod num 5)==0)then True
                else False



-------------------------------------------------------------TUPLAS

numeroElementos :: ([Int],Int)->Int ---------Toma una tupla compuesta en el primer elemento por una lista de naturales
                                    ---------En el segundo elemento toma el numero que queremos contar
numeroElementos (lista,num) =sum([1|x<-lista,num==x])------------------sumando la lista resultante calculada a base de 
                                                    ------meter 1 si el elemento está en la lista
 

numeroCeros =  flip (curry numeroElementos) 0 -------------curry elimina la tupla y deja la funcion con una signatura
                                            --------------[Int]->Int->Int de forma que flip la invierte Int->[Int]->Int
                                            --------------numeroCeros lista contará el numero de 0s
 


tuplasValor :: [([Int],Int)]->Int ->[([Int],Int)] --------------Dado una lista de tuplas( donde una lista tiene asociado un valor)
                                                    -----------y un valor ,devolveremos una lista de tuplas con la lista asociada
                                                    ------------a los valores
tuplasValor [] _ =[] --------------caso base
tuplasValor (x:xx) val = if (snd(x)==val)  then (fst(x),val): (tuplasValor xx val) --------si el segundo elemento es el valor lo
                                                                ---------------metemos y llamamos recursivamente
                        else (tuplasValor xx val)           ---------en caso contrario solo llamaramos recursivamente                   
 
 -------------------------------------------------------------Tipos algebraicos: 

