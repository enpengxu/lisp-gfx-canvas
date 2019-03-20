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
	for (int i=0; i<100; i++) {
		canvas_point_color((float)(i+1)/99.0f, 0, 0);
		canvas_draw_point(i, 0);
	}
	canvas_draw_end();

	while(1) {
		sleep(1000);
	} 
	canvas_fini();

	return 0;
}
