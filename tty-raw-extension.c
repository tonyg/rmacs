/* PLT Racket extension for selecting "raw" TTY mode */

#include "escheme.h"

#include <stdio.h>
#include <signal.h>
#include <termios.h>
#include <stdlib.h>
#include <unistd.h>

#define STDIN_FD 0

static int is_raw = 0;
static struct termios saved;

static int ttyraw(void) {
  /* Based on the settings given in http://www.minek.com/files/unix_examples/raw.html */
  struct termios t;

  if (is_raw) return 0;

  if (tcgetattr(STDIN_FD, &saved) < 0) return -1;
  t = saved;

  t.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  t.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  t.c_cflag &= ~(CSIZE | PARENB);
  t.c_cflag |= CS8;
  t.c_oflag &= ~(OPOST);
  t.c_cc[VMIN] = 1;
  t.c_cc[VTIME] = 0;

  if (tcsetattr(STDIN_FD, TCSAFLUSH, &t) < 0) return -1;

  is_raw = 1;
  return 0;
}
	
static int ttyrestore(void) {
  if (!is_raw) return 0;

  if (tcsetattr(STDIN_FD, TCSAFLUSH, &saved) < 0) return -1;

  is_raw = 0;
  return 0;
}

static Scheme_Object *sch_ttyraw(int argc, Scheme_Object **argv) {
  return ttyraw() == 0 ? scheme_true : scheme_false;
}

static Scheme_Object *sch_ttyrestore(int argc, Scheme_Object **argv) {
  return ttyrestore() == 0 ? scheme_true : scheme_false;
}

Scheme_Object *scheme_reload(Scheme_Env *env) {
  Scheme_Env *mod_env;
  mod_env = scheme_primitive_module(scheme_intern_symbol("tty-raw-extension"), env);
  scheme_add_global("tty-raw!",
		    scheme_make_prim_w_arity(sch_ttyraw, "tty-raw!", 0, 0),
		    mod_env);
  scheme_add_global("tty-restore!",
		    scheme_make_prim_w_arity(sch_ttyrestore, "tty-restore!", 0, 0),
		    mod_env);
  scheme_finish_primitive_module(mod_env);
  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  atexit(ttyrestore);
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name(void) {
  return scheme_intern_symbol("tty-raw-extension");
}
