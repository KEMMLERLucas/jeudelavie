type Grille = List[(Int,Int)]
val test =List("X X",
    " X ",
    "  X")
val g=List((-1,1), (0,1), (1,2), (2,0), (2,1))
val l = List(" XX",
    "  X",
    "XXX")
def chainesToGrille(l:List[String]): Grille={
    def aux(posX:Int,posY:Int): Grille={
        if(l.length<posX+1){
            //Lors de la prochaine itération, on va sortir du tableau
            Nil//On retourne la grille, normalement remplie
        }else{//On est dans le tableau
            val chaineActuelle=l(posX);
            val res=(posX,posY);
            if(chaineActuelle.charAt(posY).compare('X')==0){//Lors que l'ont prend l'élément de la chaine actuelle et que l'on regarde sa valeur à la pos X, c'est égal à 0 donc c'est vrai
                //On doit vérifier que, lors de la prochaine itération, posY ne dépasse pas le tableau
                if(posY+1>=chaineActuelle.length){
                    //Lors de la prochaine itération, on sera en dehors de la chaîne. On l'ajoute tout de même à la grille

                    //On appelle aux sur la prochaine colone
                    res::aux(posX+1,0);
                }else{//Lors de la prochaine itération, c'est toujours bon.
                    //Lors de la prochaine itération, on ne sera pas en dehors de la chaîne.

                    res::aux(posX,posY+1)
                }
            }else{//Ici, on a pas de X mais bien un 0
                //On doit vérifier que, lors de la prochaine itération, posY ne dépasse pas le tableau
                if(posY+1>=chaineActuelle.length){
                    //Lors de la prochaine itération, on sera en dehors de la chaîne.
                    //On appelle aux sur la prochaine colone
                    res::aux(posX+1,0);
                }else{//Lors de la prochaine itération, c'est toujours bon.
                    //Lors de la prochaine itération, on ne sera pas en dehors de la chaîne.
                    res::aux(posX,posY+1)
                }
            }
        }
    }
    aux(0,0)
}
chainesToGrille(l)
assert(chainesToGrille(l) == List((0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)))
print(5);

val (x,y)=(1,5)
print(x)
print(y)
//1.2
//Récupérer le min et le max x et y
def maxX(g:Grille, max:Int): Int = g match{
    case Nil => max
    case t::q =>
        val (x,y) = t
        if(x>max){
            maxX(q,x)
        } else {
            maxX(q,max)
        }
}
def maxY(g:Grille, max:Int): Int = g match{
    case Nil => max
    case t::q =>
        val (x,y) = t
        if(y>max){
            maxY(q,y)
        } else {
            maxY(q,max)
        }
}
def minX(g: Grille,min:Int):Int=g match {
    case Nil=>min
    case t::q=>
        val(x,y)=t
        if(x<min){
            minX(q,x)
        } else {
            minX(q, min)
        }
}
def minY(g: Grille,min:Int):Int=g match {
    case Nil=>min
    case t::q=>
        val(x,y)=t
        if(y<min){
            minY(q,y)
        } else {
            minY(q, min)
        }
}
//Test des méthodes min et max
maxX(chainesToGrille(l),0);//C'est ok ça marche
maxY(chainesToGrille(l),0)//Boucle peut-être ?

minX(chainesToGrille(l),10)//Boucle

minY(chainesToGrille(l),10)
//Faire le parcours et regarder si les valeurs sont présentes dans la grille.
def afficherGrille(g:Grille): Unit ={
    val MaxX : Int = maxX(g,-10);
    val MaxY = maxY(g,-10);
    val MinX = minX(g,20);
    val MinY = minY(g,20);
    def aux(gr:Grille,posX:Int,posY:Int):Unit={
        if(gr==Nil){
            Nil //Fin de la grille mais c'est pas forcément finis
        }else{
            val (x, y)=gr.head;//On récupère la valeur du premier tuple de la grille
            if(MaxY<posY){
                if(x==posX && y ==posY){
                    print(posY)
                    print("X")
                    Nil
                }else{
                    print(MaxY + " : " + posY+1)
                    Nil
                }
            }else{//On est dans le tableau
                if(MaxX<posX+1){
                    if(x==posX && y ==posY){
                        print("X");
                        print("\n");
                        aux(gr.tail,0,posY+1)
                    }else{
                        print("_")
                        print("\n");
                        aux(gr.tail,0,posY+1)
                    }
                }else{ //On est dans la ligne la
                    if(x==posX && y ==posY){
                        print("X");
                        aux(gr.tail,posX+1,posY)
                    }else{
                        print("_")

                        aux(gr.tail,posX+1,posY)
                    }
                }
            }
        }
    }
    aux(g,MinX,MinY);
}
print("\n")
println("Pose chaine de base")
print(chainesToGrille(l))
println("Affichage chaine")
afficherGrille(chainesToGrille(l))

//question3
def voisine8(x:Int,y:Int):List[(Int, Int)]={
    List((x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1))
}
//question 4
def estDedans(g:Grille,v:(Int,Int)): Boolean = g match {
    case Nil => false;
    case t::q =>
        if(t==v){
            true
        } else {
            estDedans(q,v);
        }
}

def combientSontDedans(g:Grille,test:Grille, acc:Int): Int = test match{
    case Nil => acc
    case t::q =>
        if(estDedans(g,t)){
            combientSontDedans(g,q,acc+1)
        } else {
            combientSontDedans(g,q,acc)
        }
}

def surviante(gDeTouteLesCellulesDuJeu: Grille,gDeTouteLesCellueVivante:Grille): Grille= gDeTouteLesCellueVivante match{
    case Nil => Nil
    case t::q =>
        val (x,y) = t
        // on regarde combien il y a de casse a coté de celle ci dans la grille
        val c = combientSontDedans(gDeTouteLesCellulesDuJeu,voisine8(x,y),0)
        //regarde dans g si 2 ou 3 voisin sont present
        if(c==2 || c==3) {
            t::surviante(gDeTouteLesCellulesDuJeu,q)
        } else {
            surviante(gDeTouteLesCellulesDuJeu,q)
        }
}