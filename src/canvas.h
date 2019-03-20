#ifndef __LISP_CANVAS_H
#define __LISP_CANVAS_H

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include <assert.h>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include "linmath.h"

#include "vpool.h"

enum DRAW_PRIMITIVE {
	DRAW_POINT = 0,
	DRAW_LINE,
	DRAW_TRIANGLE,
	DRAW_LAST,
};

enum STATUS {
	INIT = 0,
	RUNNING,
	STOP,
};

struct drawitem {
	enum DRAW_PRIMITIVE primitive;
	unsigned vbuf;
	struct verpool vpool;
};

struct glsl_shader {
	GLint mvp_location;
	GLint vpos_location;
	GLint vcol_location;
	GLuint program;
};

struct canvas_ctx {
	struct {
		float center[2];
		unsigned win_size[2];
	} settings;

	struct {
		float canvas_size[2];
		int   win_size[2];
		float rot;
		float point_size;
		float color[3];
		float bk_color[4];

		struct glsl_shader * shader;

		enum DRAW_PRIMITIVE primitive;
	} cur_state;

    GLFWwindow* win;

	struct drawitem draws[DRAW_LAST];

	enum STATUS status;
	pthread_t tid;
	pthread_mutex_t mutex;
	pthread_cond_t cond;

	struct canvas_ctx * next;
};

struct canvas_ctx_list {
	int cur;
	int num_ctx;
	struct canvas_ctx * ctx_list[16];
	pthread_mutex_t mutex;
};

extern struct canvas_ctx_list __ctx_list;

#define GET_CTX()  \
	assert(__ctx_list.cur >= 0 && __ctx_list.cur < 16);			   \
	struct canvas_ctx * ctx = __ctx_list.ctx_list[__ctx_list.cur]; \
	if (!ctx)													   \
		return -1


#define canvas_log  printf

#define canvas_lock(ctx) {							\
		int rc = pthread_mutex_lock(&(ctx)->mutex);	\
		assert(rc == 0);							\
		(void)rc;									\
	}

#define canvas_unlock(ctx) {							\
		int rc = pthread_mutex_unlock(&(ctx)->mutex);	\
		assert(rc == 0);							\
		(void)rc;									\
	}


int  canvas_init(int w, int h);
int  canvas_fini(void);
void canvas_active(int canvas);

int canvas_draw_begin(int primitive);

int canvas_point_color(float r, float g, float b);
int canvas_point_size(float s);
int canvas_draw_point(float x, float y);
int canvas_remove_points(int num);
int canvas_draw_end(void);


//int canvas_draw_line(float x, float y);
 

#endif
