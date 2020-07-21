/*
  parsing.hpp

  Fonctionnalités de parsing d'une ligne de commande.

  μsh --- un micro-shell
  Mini-projet de travaux pratiques pour le module "Systèmes d'exploitations" (X22I030)
  2018/2019, Université de Nantes.
  
 */
#ifndef __parsing_hpp__
#define __parsing_hpp__

#include <vector>
#include <list>
#include <string>

// Une commande est composée d'un nom de programme et de paramètres.
// On utilise une liste chaînée pour pouvoir facilement insérer le
// résultat d'une expansion de globbing
using command_t = std::list<std::string>;

// Une commande composée est une commande simple ou un vecteur
// de commandes simples à exécuter en parallèle.
using compound_command_t = std::vector<command_t>;

/*
  Parsing d'une ligne de commandes. Chaque commande composée est stockée dans
  un tableau dans l'ordre d'exécution. Une commande composée peut contenir une
  commande simple seule ou plusieurs commandes simples à exécuter en parallèle.
  Chaque commande simple est un tableau de chaînes de caractères (la commande 
  à exécuter en première position et ses paramètres dans les positions suivantes). 

  Grammaire d'une ligne de commandes:
  ===================================
  
  commande_composée: commande_composée ';' commande_simple
                   | commande_simple
						 ;
  commande_simple: commande_simple '//' commande
                 | commande
                 ;
  commande: nom_commande liste_paramètres
          ;
  liste_paramètres: liste_paramètres paramètre
                  | paramètre
                  ;
  nom_commande: STRING
              ;
  paramètre: STRING
           ;

  Exemple d'utilisation:
  ======================
  vector<compound_command_t> commands;
  parse_line("ls -l; rm * // touch toto; mkdir titi",commands);

  commands == 
  [ [ ["ls","-l"] ],
    [ ["rm", "*"], ["touch", "toto"] ],
    [ ["mkdir","titi"] ] ]

 */
void parse_line(const std::string& line, std::vector<compound_command_t>& commands);

#endif // __parsing_hpp__
