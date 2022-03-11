#include <stdlib.h>

typedef struct noeud noeud;
struct noeud
{
	int n;
	noeud* parent;
	noeud* gauche;
	noeud* droit;
};


void rotation_gauche(noeud* x){
	assert(x != NULL && x->droit != NULL);
	noeud* y = x->droit;
	// Le fils gauche de y devient le fils droit de x
	x->droit = y->gauche;
	if (x->droit != NULL) x->droit->parent = x;
	// On fait remonter le noeud y en mettant à jour son parent
	y->parent = x->parent;
	if (y->parent != NULL) {
		if (y->parent->gauche == x) {
			y->parent->gauche = y;
		} else {
			assert(y->parent->droit == x);
			y->parent->droit = y;
		}
	}
	// Le fils gauche de y est maintenant x
	y->gauche = x;
	x->parent = y;
}