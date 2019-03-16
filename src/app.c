#include <unistd.h>
#include "canvas.h"

int main(void)
{
	int canvas;

	canvas = canvas_init(800, 600);
	canvas_active(canvas);
	
	canvas_draw_begin(DRAW_POINT);
	canvas_draw_color(1.0f, 0, 0);
	canvas_draw_size(5.0f);
	canvas_draw_point(0, 0);
	canvas_draw_end();

	while(1) {
		sleep(1000);
	}
	canvas_fini(canvas);
	return 0;
}
