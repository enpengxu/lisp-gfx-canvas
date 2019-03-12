#inndef __LISP_CANVAS_H
#define __LISP_CANVAS_H

enum DRAW_TYPE {
	DRAW_POINT = 0,
	DRAW_LINE,
	DRAW_TRIANGLE,
	DRAW_LAST,
};

struct vertex {
	float x, y;
	float r, g, b;
};

struct ver_pool {
	unsigned size;
	unsigned num_ver;
	struct vertex * ver;
};

struct drawitem {
	DRAW_TYPE type;
	struct ver_pool vpool;
	GLuint vbuf;
	int dirty;
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
	} cur_states;

    GLFWwindow* win;

	struct drawitem draws[DRAW_LAST];

	int status;
	pthread_t tid;
	pthread_mutex_t mutex;
	pthread_cond_t cond;

	struct canvas_ctx * next;
};

struct canvas_ctx_list {
	int cur;
	struct canvas_ctx * ctx_list[16];
};

extern struct canvas_ctx_list __ctx_list;

#define GET_CTX()  \
	assert(__ctx_list.cur >= 0 && __ctx_list.cur < 16);			   \
	struct canvas_ctx * ctx = __ctx_list.ctx_list[__ctx_list.cur]; \
	if (!ctx)													   \
		return -1


#define canvas_log  printf

#define canvas_lock(ctx) {							\
		int rc = pthread_mutex_lock((ctx)->mutex);	\
		assert(rc == 0);							\
		(void)rc;									\
	}

#define canvas_unlock(ctx) {							\
		int rc = pthread_mutex_unlock((ctx)->mutex);	\
		assert(rc == 0);							\
		(void)rc;									\
	}

int canvas_draw_begin(int type);

int canvas_draw_color(float r, float g, floar b);
int canvas_draw_size(float s);
int canvas_draw_point(float x, float y);

int canvas_draw_end();

int canvas_undo();

//int canvas_draw_line(float x, float y);
 

#endif