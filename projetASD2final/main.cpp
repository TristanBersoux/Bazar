#include <cstdlib>
#include <iostream>
#include <string>
#include "parser.hpp"
#include "strucBloc.hpp"

using namespace std;

//------------Fonctions------------

	//Rôle : indente l'affichage de l'interpréteur, à la fois pour les entrées et les sorties (Pour les sorties, appeler avec "ouput" à 1, 0 sinon)
	//Retour : Un string contenant uniquement les espaces d'indentation
  string indenter(bool output,int nbBlocs, int lineNb){
  	string indentation="";
  	if (output){
  		indentation = "                              ";
  	}else{
			for (int i=1;i<=nbBlocs;i++){
				indentation += "  ";
			}
  	
  	//Prise en compte de l'affichage du numéro de ligne pour que cela reste aligné, limité à 999 lignes
  		if (lineNb < 100){
  			if (lineNb < 10){
  				indentation += " ";
  				}
  			indentation +=" ";
  		}
  	}
  	return indentation;
  }
   
   
   
//------------Main------------
int main(void)
{

/*
	//------------Tests des objets------------

  //Test de la classe bloc et de ses fonctions. Si bloc fonctionne correctement, on doit observer "3 10 4" en sortie (Pour valeur de x, true/false, pour valeur y)
  bloc monbloc;
  monbloc.declarer("x",3);
  cout << monbloc.lecture("x") << endl;
  cout << monbloc.est_dans("x") << monbloc.est_dans("y") << endl;
  monbloc.modifier("x",4);
  cout << monbloc.lecture("x") << endl;
  

  //Test de la classe strucBloc et de ses fonctions. Si strucBloc fonctionne correctement, on doit observer "1 2 1" en sortie
  strucBloc a_structure;
  a_structure.ajouterBloc();
  a_structure.declarer("x",1);
  a_structure.afficher("x");
  a_structure.ajouterBloc();
  a_structure.declarer("x",2);
  a_structure.afficher("x");
  a_structure.retirerBloc();
  a_structure.afficher("x");
  
  
 */

  //------------Programme principal------------
  
  parser a_parser;
  string line;
  TOKEN tok;
  strucBloc a_structure;
  int lineNumber = 2;
  int blocsOuverts = 1;
  
  cout << "------------------------------------------------------" << endl;
  cout << "--         interpreter of the asd2 language         --" << endl;
  cout << "------------------------------------------------------" << endl;
	cout << "INPUT                         | OUTPUT         " <<endl;
	cout << "------------------------------------------------------" <<endl;
	// Par défaut, on ouvre un bloc. En effet, le programme se termine quand il n'y a pas de bloc ouvert.
	cout << "1  {" << endl;
	a_structure.ajouterBloc();
	
	while (blocsOuverts > 0){
		cout << lineNumber;
		cout << indenter(0,blocsOuverts,lineNumber) ;
		
		
		lineNumber = lineNumber+1;
		getline(cin,line);
		tok = a_parser.parse(line);
		
		
		switch(tok)
		{
		  case TOKEN_OPEN:
		  {
		    a_structure.ajouterBloc();
		    blocsOuverts++;
		    break;
		  }
		  case TOKEN_CLOSE:
		  {
		 		cout << indenter(1,blocsOuverts,lineNumber);
		 	 	a_structure.listeVariables();
		    a_structure.retirerBloc();
		    blocsOuverts--;
		    
		    break;
		  }
		  case TOKEN_VAR:
		  {
		  	int erreur;
		 		erreur = a_structure.declarer(a_parser.var(),a_parser.value());
		  	if (erreur == 1){
		  		cout << indenter(1,blocsOuverts,lineNumber) << "| error : Variable already declared !" << endl;
		  	}
		    break;
		  }
		  case TOKEN_PRINT:
		  {
		  	cout << indenter(1,blocsOuverts,lineNumber);
		    a_structure.afficher(a_parser.var());
		    break;
		  }
		  case TOKEN_ERROR:
		  {
		  	cout << indenter(1,blocsOuverts,lineNumber);
		  	cout << "| Line " << lineNumber-1 << " : " << a_parser.error() << endl;
		    break;
		  }
		  case TOKEN_ADD:
		  {
		  	int erreur;
		  	erreur =  a_structure.incrementer(a_parser.var());
		  	if (erreur == 1){
		  		cout << indenter(1,blocsOuverts,lineNumber) << "| error: " << a_parser.var() << " not found !" <<endl;
		  	}
		   
		    break;
		  }
		}
		
	}
	
	cout << "------------------------------------------------------" <<endl;


  return EXIT_SUCCESS;
}
