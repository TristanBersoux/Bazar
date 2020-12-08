//Auteurs : BERSOUX Tristan et COMPERE Nicolas
//Gr. 581, année 2020

import java.awt.Color;
import java.awt.Point;
import java.util.LinkedList;
import java.util.Stack;
public class QuadTree {
	protected Color rCouleur; //Couleur representée par le pixel (null par défaut, seules les feuilles en ont une)
	protected Point rCoords; //Coordonnées du coin haut gauche du carré représenté par ce noeud
	protected int nbPixelCote; //Longueur d'un côté (Par exemple, 2 pour un quadtree/Une feuille représentant un carré de 4*4 pixels)
	protected QuadTree filsNO;
	protected QuadTree filsNE;
	protected QuadTree filsSE;
	protected QuadTree filsSO;
	//Sauvegarder les images prend de la place, mais on en a besoin pour toPNG et pour compareEQM
	//Idée pour prendre moins de place : créer une classe mère de QuadTree qui sauvegarde les images et un pointeur vers la racine + bouger des méthodes
	//Mais pas le temps pour le faire (pour juste un gain complexité spatial, pas très important ici)
	protected ImagePNG imageOriginale; //On sauvegarde l'image initiale
	protected ImagePNG imageClone; //On fait une copie de l'image pour travailler dessus



	//Constructeur d'un QuadTree vide
	public QuadTree() {

	}

	//Constructeur d'un QuadTree représentant seulement un pixel
	//Entrée : Color pixel : le pixel représenté
	public QuadTree(Color pixel) {
		this.rCouleur = pixel;
	}

	//Constructeur plus pratique que celui demandé par le sujet
	//Entrée : Image PNG image : l'image dont on construit l'arbre
	//			Point coords : coordonnées du noeud actuellement construit
	//			int tCote : longueur d'un côté du carré représenté par ce noeud
	public QuadTree(ImagePNG image,Point coords,int tCote) {
		this.rCoords = coords;
		this.nbPixelCote = tCote;
		//Construction du noeud sans compresser
		if (tCote>1) {
			int tCoteFils = tCote/2; //Marcherai aussi avec height
			//Appels récursifs comme vu CM
			filsNO = new QuadTree(image,new Point(coords.x,coords.y),tCoteFils);
			filsNE = new QuadTree(image,new Point(coords.x+tCoteFils,coords.y),tCoteFils);
			filsSE = new QuadTree(image,new Point(coords.x+tCoteFils,coords.y+tCoteFils),tCoteFils);
			filsSO = new QuadTree(image,new Point(coords.x,coords.y+tCoteFils),tCoteFils);
		} else {
			this.rCouleur = (image.getPixel(coords.x,coords.y));
		}

		//Compression sans perte
		if (this.rCouleur == null){ //Si le noeud n'est pas une feuille
			if (this.filsNO != null && this.filsNE != null && this.filsSE != null && this.filsSO != null) { //Si il a bien 4 fils
				if((this.filsNO.rCouleur != null) && (this.filsNE.rCouleur != null) && (this.filsSE.rCouleur != null) && (this.filsSO.rCouleur != null)) { //Si les 4 fils ont une couleur
					if ((this.filsNO.rCouleur != null) && this.filsNE.rCouleur.equals(filsNO.rCouleur) && this.filsSO.rCouleur.equals(filsNO.rCouleur) && this.filsSE.rCouleur.equals(filsNO.rCouleur)) {//Si la couleur des 4 fils est la même (et pas null)
						//On compresse
						this.rCouleur = this.filsNO.rCouleur;
						this.filsNO = null;
						this.filsNE = null;
						this.filsNO = null;
						this.filsSO = null;
					}
				}
			}
		}
	}

	//Constructeur du Quadtree selon l'image PNG (tel que demandé par le sujet)
	//Entrée : ImagePNG image : l'image dont on veut construire l'arbre
	public QuadTree(ImagePNG image) {
		this(image,new Point(0,0),image.width());
		this.imageOriginale = image;
		this.imageClone = image.clone();
	}

	//Calcul l'écart colorimétrique entre 4 feuilles d'un noeud
	//Note : On ne considère que la partie entière, pour gagner beaucoup de place et simplifier le programme lors de la compression phi.
	//		De toute manière, cela ne change quasi rien, car une différence de moins de 1 entre deux écarts colorimétriques n'est que très peu significatif.
	//Préconditions : les 4 fils du noeuds ont bien une couleur
	//Sortie : int : l'écart colorimétrique du noeud
	public int ecartColor() {
		int ecartMax = 0;
		int ecart;
		int Rm = 0; int Vm = 0; int Bm = 0;
		Color[] colorArray = new Color[]{this.filsNO.rCouleur,this.filsNE.rCouleur,this.filsSE.rCouleur,this.filsSO.rCouleur};

		for(Color couleur : colorArray) {
			Rm = Rm + couleur.getRed()/4;
			Vm = Vm + couleur.getGreen()/4;
			Bm = Bm + couleur.getBlue()/4;
		}
		for(Color couleur : colorArray) {
			ecart = (int) java.lang.Math.sqrt((Math.pow(couleur.getRed()-Rm, 2) + Math.pow(couleur.getBlue()-Bm,2) + Math.pow(couleur.getGreen()-Vm, 2))/3);
			if (ecart > ecartMax) {
				ecartMax = ecart;
			}
		}
		if(ecartMax<0 || ecartMax>255) {
			System.out.println(ecartMax);
		}
		return ecartMax;
	}

	//Renvoie la couleur moyenne des 4 fils d'un noeud
	//Préconditions : Le noeud a bien 4 fils qui sont des feuilles
	//Entrée : QuadTree noeud : le noeud dont on veut calculer la couleur moyenne des fils
	//Sortie : Color : la couleur moyenne des 4 fils
	public static Color couleurMoyenne(QuadTree noeud) {
		int Rm = (noeud.filsNE.rCouleur.getRed() + noeud.filsNO.rCouleur.getRed() + noeud.filsSE.rCouleur.getRed() + noeud.filsSO.rCouleur.getRed())/4;
		int Vm = (noeud.filsNE.rCouleur.getGreen() + noeud.filsNO.rCouleur.getGreen() + noeud.filsSE.rCouleur.getGreen() + noeud.filsSO.rCouleur.getGreen())/4;
		int Bm = (noeud.filsNE.rCouleur.getBlue() + noeud.filsNO.rCouleur.getBlue() + noeud.filsSE.rCouleur.getBlue() + noeud.filsSO.rCouleur.getBlue())/4;
		return new Color(Rm,Vm,Bm);
	}
	
	//Retourne le nombre de feuille
	//Sortie : int : le nombre de feuille de l'arbre
	public int nbFeuilles() {
		if(this.rCouleur == null) { //Si le noeud n'est pas une feuille
			return this.filsNE.nbFeuilles() + this.filsNO.nbFeuilles() + this.filsSE.nbFeuilles() + this.filsSO.nbFeuilles();
		} else {
			return 1;
		}
	}
	
	//Renvoie le père d'un noeud passé en paramètre
	//Entrée : racine : racine de l'arbre ou du sous-arbre dans lequel on cherche
	//			recherche : l'élément recherché
	//Sortie : le pere de du noeud recherché
	public QuadTree pereDe(QuadTree racine,QuadTree recherche) {
		if((racine.filsNE.equals(recherche))||(racine.filsNO.equals(recherche))||(racine.filsSE.equals(recherche))||(racine.filsSO.equals(recherche))) { //Si l'un des fils est notre recherche
			return racine;
		}else {
			if(recherche.rCoords.x < racine.filsSE.rCoords.x) {//Si l'élement recherché est dans l'un des fils ouest
				if(recherche.rCoords.y<racine.filsSE.rCoords.y) { //S'il est au nord
					return pereDe(racine.filsNO,recherche); 
				}else {//S'il est au sud
					return pereDe(racine.filsSO,recherche);
				}
			}else { //S'il est dans l'un des fils est
				if(recherche.rCoords.y<racine.filsSE.rCoords.y) { //S'il est au nord
					return pereDe(racine.filsNE, recherche);
				}else { //S'il est au sud
					return pereDe(racine.filsSE, recherche);
				}
			}
		}
	}
	
	//Compresse l'image selon la méthode delta
	//Entrée : int delta 
	public void compressDelta(int delta) {
		if ((delta < 0) || (delta > 255)) {
			System.out.println("ERROR : Delta not in 0..255");
		} else {
			if(this.rCouleur == null) { // Si le noeud n'est pas une feuille
				if((this.filsNE.rCouleur != null) && (this.filsNO.rCouleur != null) && (this.filsSE.rCouleur != null) && (this.filsSO.rCouleur != null)) { //Si les fils sont des feuilles
					if (this.ecartColor() <= delta) {
						this.rCouleur = couleurMoyenne(this);
						this.filsNE = null;
						this.filsNO = null;
						this.filsSE = null;
						this.filsSO = null;
					}
				} else { //Si les fils ne sont pas des feuilles
					this.filsNE.compressDelta(delta);
					this.filsNO.compressDelta(delta);
					this.filsSE.compressDelta(delta);
					this.filsSO.compressDelta(delta);
				}
			} 
		}
	}

	//Initialise une liste de 256 piles de QTEcartColor (une pour chaque écart colorimétrique possible)
	//Sortie : liste de 256 piles de QTEcartColor
	public LinkedList<Stack<QTEcartColor>> initPile() {
		LinkedList<Stack<QTEcartColor>> liste = new LinkedList<Stack<QTEcartColor>>();
		for (int i = 0;i<256;i++) {
			liste.add(new Stack<QTEcartColor>());
		}
		return liste;
	}

	//Ajoute un QtEcartColor à pile qui lui correspond dans la liste de pile définie précédemment
	//Entrée : liste : la liste des piles dans laquelle ajouter
	//		aAjouter : l'élément à ajouter
	public void addListePile(LinkedList<Stack<QTEcartColor>> liste,QTEcartColor aAjouter){
		liste.get(aAjouter.ecartColor).push(aAjouter);
	}

	//Dépile et renvoie le plus le QTEcartColor ayant le plus petit écart colorimétrique
	//Si il y a plusieurs "plus petits", c'est celui empilé le dernier qui est dépilé 
	//Précondition : il y a au moins une pile non-vide dans la liste
	//Entrée : liste : la liste des piles dans laquelle ajouter
	//Sortie : le plus petit QTEcartColor de toute la liste
	public QTEcartColor depilerPlusPetit(LinkedList<Stack<QTEcartColor>> liste) {
		int nPile = 0;
		boolean trouve = false;
		while(!trouve) {
			if(!liste.get(nPile).isEmpty()) {
				trouve = true;
			}else {
				nPile++;
			}
		}
		return liste.get(nPile).pop();
	}

	//Remplit le tableau des noeuds triés selon leur écarts colorimétriques (s'ils en ont un)
	//Entrée : liste : la liste des piles dans laquelle ajouter
	public void remplirListe(LinkedList<Stack<QTEcartColor>> liste){
		if(this.rCouleur == null) { //Si le noeud n'est pas une feuille
			if((this.filsNE.rCouleur != null) &&(this.filsNO.rCouleur != null)&&(this.filsSE.rCouleur != null)&&(this.filsSO.rCouleur != null)) { //S'il a des feuilles comme fils
				//On crée un QTEcartColor à partir de ce noeud
				QTEcartColor aPlacer = new QTEcartColor(this);
				//On le place dans la pile qui lui correspond
				this.addListePile(liste,aPlacer);
			} else { //Si le noeud a des noeuds internes comme fils
				this.filsNO.remplirListe(liste);
				this.filsNE.remplirListe(liste);
				this.filsSE.remplirListe(liste);
				this.filsSO.remplirListe(liste);

			}
		} 
	}


	//Compresse l'image selon la méthode phi
	//Entree : int phi
	public void compressPhi(int phi) {
		if (phi <= 0)
		{
			System.out.println("ERROR : Phi not > 0");
		} else {
			int nbFeuillesASuppr = this.nbFeuilles()-phi;

			//Remplir le tableau trié de colorimétries
			LinkedList<Stack<QTEcartColor>> laListe = this.initPile();
			this.remplirListe(laListe);
			//Tant que il reste trop de feuilles par rapport à phi,
			while(nbFeuillesASuppr >0) {
				//Supprimer de la liste le noeud ayant le plus petit écart colorimétrique en lui attribuant la couleur moyenne de ses fils
				QTEcartColor removed = this.depilerPlusPetit(laListe);
				removed.noeud.rCouleur = couleurMoyenne(removed.noeud);
				//On supprime ses fils
				removed.noeud.filsNO = null;
				removed.noeud.filsNE = null;
				removed.noeud.filsSE = null;
				removed.noeud.filsSO = null;
				nbFeuillesASuppr -= 3;
				//Vérifier son père et mettre à jour la liste si besoin
				//C'est à dire ajouter en place le père à la liste si tous ses fils sont des feuilles
				if(!removed.noeud.equals(this)) {
					QuadTree pere = pereDe(this,removed.noeud);
					if((pere.filsNE.rCouleur != null)&&(pere.filsNO.rCouleur != null)&&(pere.filsSE.rCouleur != null)&&(pere.filsSO.rCouleur != null)){
						QTEcartColor aPlacer = new QTEcartColor(pere);
						addListePile(laListe, aPlacer);
					}
				}
			}
		}
	}

	//Fonction en deux parties
	//Première donne a la seconde l'image sur laquelle faire les modifications
	//Retourne une ImagePNG à partir du Quadtree
	//Sortie : L'imagePNG correspondant à cet arbre
	public ImagePNG toPNG() {

		//Décommenter ce bloc pour tester la fonction, puisqu'il ré-initialise l'image toute blanche on s'assure de ne pas tricher
		/*for(int x = 0; x < imageClone.width() ; x++) {
			for(int y = 0; y < imageClone.height();y++) {
				this.imageClone.setPixel(x, y, Color.WHITE);
			}
		}*/
		
		this.toPNG(imageClone);
		return imageClone;
	}
	
	//On utilise un clone de l'image de base comme support
	//Entrée : ImagePNG aModifier : l'image de base clonée
	public void toPNG(ImagePNG aModifier) {
		if(this.rCouleur != null) { // Si le noeud est une feuille
			for (int x = this.rCoords.x ; x < this.nbPixelCote+this.rCoords.x ; x++) {
				for (int y = this.rCoords.y ; y < this.nbPixelCote+this.rCoords.y ; y++) {
					aModifier.setPixel(x, y, rCouleur);
				}
			}
		}
		if(this.filsNE != null) {
			filsNE.toPNG(aModifier);
		}
		if(this.filsNO != null) {
			filsNO.toPNG(aModifier);
		}
		if(this.filsSE != null) {
			filsSE.toPNG(aModifier);
		}
		if(this.filsSO != null) {
			filsSO.toPNG(aModifier);
		}
	}

	//Retourne la représentation textuelle du QuadTree
	//Sortie : String : Quadtree sous forme textuelle
	public String toString() {
		if(this.rCouleur != null) { //Si le noeud est une feuille
			return ImagePNG.colorToHex(this.rCouleur);
		} else { //Si le noeud est interne
			return "("+this.filsNO.toString()+" "+this.filsNE.toString()+" "+this.filsSE.toString()+" "+this.filsSO.toString()+")";
		}
	}


}
