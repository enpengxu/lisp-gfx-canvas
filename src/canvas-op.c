#include <limits.h>
#include <assert.h>
#include "canvas.h"

static inline void
canvas_post_update()
{
	// update
	glfwPostEmptyEvent();
}


int
canvas_draw_begin(int primitive)
{
	GET_CTX();

	int rc = 0;
	canvas_lock(ctx);

	if (ctx->cur_state.primitive != -1) {
		rc = -1;
	} else {
		switch(primitive) {
		case DRAW_POINT:
		case DRAW_LINE:
		case DRAW_TRIANGLE:
			ctx->cur_state.primitive = primitive;
			break;
		default:
			rc = -1;
		}
	}
	canvas_unlock(ctx);
	return rc;
}

int
canvas_point_color(float r, float g, float b)
{
	GET_CTX();

	canvas_lock(ctx);

	ctx->cur_state.color[0] = r;
	ctx->cur_state.color[1] = g;
	ctx->cur_state.color[2] = b;

	canvas_unlock(ctx);

	canvas_post_update();
	return 0;
}

int canvas_point_size(float s)
{
	GET_CTX();
	canvas_lock(ctx);

	ctx->cur_state.point_size = s;
	ctx->cur_state.flag |= MOD_VERTEX_SIZE;

	canvas_unlock(ctx);
	return 0;
}

int
canvas_draw_point(float x, float y)
{
	GET_CTX();
	canvas_lock(ctx);

	int rc = 0;
	if (ctx->cur_state.primitive == -1) {
		rc = -1;
	} else {
		struct drawitem * draw = &ctx->draws[ctx->cur_state.primitive];
		verpool_add(&draw->vpool, x, y, ctx->cur_state.color);
	}
	ctx->cur_state.flag |= MOD_VERTEX_NUM;
	canvas_unlock(ctx);
	return rc;
}

int
canvas_draw_end(void)
{
	GET_CTX();
	canvas_lock(ctx);

	int rc = 0;
	if (ctx->cur_state.primitive == -1) {
		rc = -1;
	} else {
		ctx->cur_state.primitive = -1;
	}
	canvas_unlock(ctx);
	// update
	canvas_post_update();

	return rc;
}

int
canvas_remove_points(int primitive, int num)
{
	GET_CTX();
	int rc = 0;

	canvas_lock(ctx);
	if (ctx->cur_state.primitive != -1) {
		rc = -1;
	} else {
		struct drawitem * draw = &ctx->draws[primitive];
		verpool_remove(&draw->vpool, num);
	}
	ctx->cur_state.flag |= MOD_VERTEX_NUM;
	canvas_unlock(ctx);
	canvas_post_update();
	return rc;
}

int
canvas_clear(void)
{
	int rc = 0;
	for (int i=0; i<DRAW_LAST; i++) {
		rc |= canvas_remove_points(i, INT_MAX);
	}
	return rc;
}
