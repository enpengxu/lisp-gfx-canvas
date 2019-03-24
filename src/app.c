#include <unistd.h>
#include "canvas.h"

int main(void)
{
	int canvas;

	canvas = canvas_init(800, 600);
	canvas_active(canvas);
	
	canvas_point_color(1.0f, 0, 0);
	canvas_point_size(5.0f);

	canvas_draw_begin(DRAW_POINT);
	for (int y=-400; y<400; y++) {
		for (int x=-400; x<400; x++) {
			canvas_point_color((float)(x)/400.0f, 0, 0);
			canvas_draw_point(x, y);
		}
	}
	canvas_draw_end();

	while(1) {
		sleep(1000);
	} 
	canvas_fini();

	return 0;
}
