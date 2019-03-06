#include "lispcanvas.h"

int main(void)
{
	int canvas = canvas_init(800, 600);
	while(1) {
		sleep(1000);
	}
	canvas_fini(canvas);
	return 0;
}
