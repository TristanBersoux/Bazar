import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

//Auteurs : BERSOUX Tristan et COMPERE Nicolas
//Gr. 581, année 2020



public class Main {

	//Fonctions qui sert à choisir les options
	public static int options(String[] args,Scanner scanner) {
		int choix = 0;
		System.out.println();
		System.out.println("Choisissez une option en entrant son chiffre associé : ");
		System.out.println("1. Charger une image PNG en mémoire");
		System.out.println("2. Appliquer une compression Delta pour un Delta donné");
		System.out.println("3. Appliquer une compression Phi pour un Phi donné");
		System.out.println("4. Sauvegarder le quadtree dans un fichier PNG");
		System.out.println("5. Sauvegarder la représentation textuelle du quadtree dans un fichier txt");
		System.out.println("6. Donner les mesures comparatives des fichiers images PNG");
		System.out.println("7. Quitter le programme");
		System.out.println();
		while(choix<1 || choix>7) {

			String choixString = scanner.nextLine();
			try {
				choix = Integer.parseInt(choixString);
				if(choix >7 || choix<1) {
					System.out.println("L'option entrée est incorrecte ! Ré-essayez !");
				}
			} catch(NumberFormatException e) {
				System.out.println("L'option entrée est incorrecte ! Ré-essayez !");
			}
		}
		return choix;
	}

	//Charger une image dans un QuadTree à partir de son chemin absolu donnée en paramètre
	public static QuadTree chargerImage(String path) {
		QuadTree retour = new QuadTree();
		try {
			System.out.println("Lecture de " + path);
			ImagePNG image = new ImagePNG(path);
			retour = new QuadTree(image);
			System.out.println("L'image a bien été chargée");
		}catch(IOException e){
			System.out.println("Problème d'accès à l'image ! ");
			e.printStackTrace();
		}

		return retour;
	}

	//Appliquer la méthode de compression Delta avec le delta en paramètre, sur l'arbre en paramètre
	public static void appliquerDelta(int delta,QuadTree arbre) {
		arbre.compressDelta(delta);
		System.out.println("Compression terminée");

	}

	//Appliquer la méthode de compression Phi avec le phi en paramètre, sur l'arbre en paramètre
	public static void appliquerPhi(int phi,QuadTree arbre) {
		arbre.compressPhi(phi);
		System.out.println("Compression terminée");
	}

	//Sauvegarder l'Arbre sous forme d'image dont le chemin de sauvegarde et le nom sont données dans deux paramètres distincts
	public static void saveQuadTreePNG(QuadTree arbre,String path,String nomFichier) {
		try {
			arbre.toPNG().save(path+"/"+nomFichier+".png");
			if(!(nomFichier.equals("temp"))) {
				System.out.println("Image sauvegardée : "+path + "/"+nomFichier+".png");
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	//Sauvegarder l'Arbre sous forme de texte dans un fichier txt dont le chemin de sauvegarde et le nom sont données dans deux paramètres distincts
	public static void saveQuadTreeString(QuadTree arbre,String path,String nomFichier) {
		String save = arbre.toString();
		try {
			File fichier = new File(nomFichier+".txt");
			FileWriter ecrivain = new FileWriter(fichier.getName());
			ecrivain.write(save);
			ecrivain.close();
			System.out.println("Arbre sauvegardé : " + path + "/" + nomFichier + ".txt");
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	//Comparer l'image originale et celle produit par le Quadtree selon le facteur de compression (ratio de taille) et l'EQM
	//On sauvegarde puis supprime à la volée l'image produite par l'Arbre
	public static void comparePNG(QuadTree arbre, String path,String nomFichier) {
		long tailleOrigine = 1;
		long tailleCompression = 1;
		String eqm = Double.toString(ImagePNG.computeEQM(arbre.toPNG(),arbre.imageOriginale));
		saveQuadTreePNG(arbre,path,"temp");
		try {
			tailleOrigine = Files.size(Paths.get(path+nomFichier));
			tailleCompression = Files.size(Paths.get(path+"/temp.png"));
			Files.delete(Paths.get(path+"/temp.png"));
			System.out.println("EQM : " + eqm + ", Compressé " + (float) tailleOrigine/tailleCompression + "x ("+ (float) tailleCompression*100/tailleOrigine+"% taille de l'original)") ;

		} catch (IOException e) {
			System.out.println("Erreur dans le path du fichier original / dans l'accès à la sauvegarde");
			e.printStackTrace();
		}

	}


	public static void main(String[] args) {

		String path ="";
		QuadTree arbre = new QuadTree();
		Scanner myScanner = new Scanner(System.in);

		//Récupérer le path du programme
		try {
			path = new File(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI()).getPath();
			//Pour que ça marche depuis eclipse mais aussi avec le code exporté en .jar :

			if(path.substring(path.length()-9,path.length()).equals("/Main.jar")) {
				path = path.substring(0,path.length()-9);
			}
		} catch (URISyntaxException e1) {
			e1.printStackTrace();
		}


		int choix = 0;

		//Options manuelles
		if(args.length <1) {
			Boolean programmeEnCours  = true;
			String nomImage = "";
			while(programmeEnCours){
				choix = options(args,myScanner);
				switch(choix) {
				case 1: //Charger l'image
					System.out.println("Donner le nom de l'image à charger");
					System.out.println("Exemple : monImage.png");
					nomImage = myScanner.nextLine();
					arbre = chargerImage(path+"/pngs/"+nomImage);
					break;

				case 2: //Appliquer la compression Delta
					if(nomImage.isEmpty()) {
						System.out.println("Chargez une image d'abord !");
					}else {
						System.out.println("Donnez le delta pour la compression : ");
						int delta = Integer.parseInt(myScanner.nextLine());
						appliquerDelta(delta,arbre);
					}

					break;

				case 3: //Appliquer la compression Phi
					if(nomImage.isEmpty()) {
						System.out.println("Chargez une image d'abord !");
					}else {
						System.out.println("Donnez le phi pour la compression : ");
						int phi = Integer.parseInt(myScanner.nextLine());
						appliquerPhi(phi,arbre);
					}
					break;

				case 4: //Sauvegarder l'image forme PNG
					if(nomImage.isEmpty()) {
						System.out.println("Chargez une image d'abord !");
					}else {
						System.out.println("Donner le nom de l'image");
						String nomImageSave = myScanner.nextLine();
						saveQuadTreePNG(arbre,path,nomImageSave);
					}
					break;

				case 5: //Sauvegarder l'image forme txt
					if(nomImage.isEmpty()) {
						System.out.println("Chargez une image d'abord !");
					}else {
						System.out.println("Donner le nom du fichier txt");
						String nomTXT = myScanner.nextLine();
						saveQuadTreeString(arbre,path,nomTXT);
					}
					break;

				case 6: //Comparer l'image originale et l'image produite
					if(nomImage.isEmpty()) {
						System.out.println("Chargez une image d'abord !");
					}else {
						comparePNG(arbre, path+"/pngs/", nomImage);
					}
					break;

				case 7: //Quitter le programme
					programmeEnCours = false;
					System.out.println("Fermeture du programme.");
					break;
				}

				//Pour voir les messages d'erreurs ou de confirmation avant de poursuivre (confort utilisateur)
				try {
					TimeUnit.MILLISECONDS.sleep(1300);
				} catch (InterruptedException e1) {
					e1.printStackTrace();
				}
			}	
		}

		//Appel avec arguments
		else {
			if(args.length == 3) { //nomFichier delta phi
				String nomImage = args[0].substring(0,args[0].length()-4);
				
				QuadTree arbreDelta = chargerImage(path+"/pngs/" + args[0]);
				QuadTree arbrePhi = chargerImage(path+"/pngs/" + args[0]);
				System.out.println();
				appliquerDelta(Integer.parseInt(args[1]),arbreDelta);
				appliquerPhi(Integer.parseInt(args[2]),arbrePhi);
				System.out.println();
				saveQuadTreePNG(arbreDelta,path,nomImage+"-delta"+args[1]);
				saveQuadTreePNG(arbrePhi,path,nomImage+"-phi"+args[2]);
				saveQuadTreeString(arbreDelta,path,nomImage+"-delta"+args[1]);
				saveQuadTreeString(arbrePhi,path,nomImage+"-phi"+args[2]);
				
				System.out.println();
				System.out.println("Comparaison delta/originale");
				comparePNG(arbreDelta, path+"/pngs/", args[0]);
				System.out.println("Comparaison phi/originale");
				comparePNG(arbrePhi, path+"/pngs/", args[0]);
			}
		}
		myScanner.close();
	}
}