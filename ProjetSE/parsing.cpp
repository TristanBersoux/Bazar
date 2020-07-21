/*
  parsing.cpp

  Fonctionnalités de parsing d'une ligne de commande.

  μsh --- un micro-shell
  Mini-projet de travaux pratiques pour le module "Systèmes d'exploitations" (X22I030)
  2018/2019, Université de Nantes.
  
 */

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include "parsing.hpp"

using namespace std;

void parse_line(const std::string& line, std::vector<compound_command_t>& commands)
{
  // Découpage suivant les ";" 
  regex seq_re("\\s*;\\s*");
  // Découpage suivant les "//"
  regex par_re("\\s*//\\s*");
  // Découpage à l'interieur d'une commande suivant les espaces
  regex cmd_re("\\s+");
  sregex_token_iterator seqiter(line.begin(), line.end(),seq_re,-1);
  sregex_token_iterator reg_end;

  // Itération sur chaque commmande composée
  for (;seqiter != reg_end; ++seqiter) {
	 const string& compound_cmd{seqiter->str()};
	 sregex_token_iterator pariter(compound_cmd.begin(), compound_cmd.end(),par_re,-1);
	 compound_command_t compcmd;
	 // On itère sur chaque commande simple
	 for (;pariter != reg_end; ++pariter) {
		const string& cmdstr{pariter->str()};
		sregex_token_iterator cmditer(cmdstr.begin(), cmdstr.end(),cmd_re,-1);
		command_t cmd;
		for (; cmditer != reg_end; ++cmditer) {
		  cmd.emplace_back(cmditer->str());
		}
		compcmd.push_back(cmd);
	 }
	 commands.push_back(compcmd);
  }
}
