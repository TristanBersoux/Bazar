#include <iostream>
#include <unordered_map>
#include <queue>
typedef std::string string;
typedef std::unordered_map<string,int> t_table;
typedef std::pair<string,int> t_entree;
typedef std::queue<string> queue;


class bloc{
public:
	//constructeur d'un bloc vide
	bloc();
	
	//destructeur
	~bloc();
	
	//ajoute la variable nommée c, ayant pour valeur V
	//retourne 0 si la déclaration s'est bien passée, 1 sinon
	int declarer(string c, int v);
	
	//renvoie vraie si la variable c est dans le bloc
	bool est_dans(string c);
	
	//renvoie la valeur numérique associée à la variable c
	//Pré-condition : c est présente dans le bloc
	int lecture(string c);
	
	//modifie en v la valeur de la variable c
	//Pré-condition : c est présente dans le bloc
	void modifier(string c, int v);

	//liste les variables du bloc par ordre chronologique d'ajout
	void listeVariables();

	
private:
	queue file;
	t_table table;	

};
