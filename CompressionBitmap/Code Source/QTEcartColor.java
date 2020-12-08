//Un QuadTree qui en plus sauvegarde l'écart colorimétrique de sa racine
public class QTEcartColor{
	protected QuadTree noeud;
	protected int ecartColor;
	
	QTEcartColor(QuadTree noeud){
		this.noeud = noeud;
		ecartColor = this.noeud.ecartColor();
	}

}
