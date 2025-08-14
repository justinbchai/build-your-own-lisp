#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "editline/history.h"
#include "editline/readline.h"
#include "mpc.h"

typedef struct lval {
  int type;
  long num;
  char *err;
  char *sym;
  int count;
  struct lval **cell;

} lval;

// type enums
enum { TYPE_NUM, TYPE_ERR, TYPE_SYM, TYPE_SEXPR };

// type errors
enum { ERR_DIV_ZERO, ERR_UNKNOWN_OP, ERR_BAD_NUM };

void lval_print(lval *v);
lval *lval_eval(lval *v);

lval *lval_num(long x) {
  lval *v = malloc(sizeof(lval));
  v->type = TYPE_NUM;
  v->num = x;
  return v;
}

lval *lval_err(char *e) {
  lval *v = malloc(sizeof(lval));
  v->type = TYPE_ERR;
  v->err = malloc(strlen(e) + 1);
  strcpy(v->err, e);
  return v;
}

lval *lval_sym(char *s) {
  lval *v = malloc(sizeof(lval));
  v->type = TYPE_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

lval *lval_sexpr() {
  lval *v = malloc(sizeof(lval));
  v->type = TYPE_SEXPR;
  v->cell = NULL;
  v->count = 0;
  return v;
}

void lval_free(lval *v) {
  switch (v->type) {
  case TYPE_NUM:
    break;
  case TYPE_ERR:
    free(v->err);
    break;
  case TYPE_SYM:
    free(v->sym);
    break;
  case TYPE_SEXPR:
    for (int i = 0; i < v->count; i++) {
      lval_free(v->cell[i]);
    }
    free(v->cell);
    break;
  }
  free(v);
}

lval *lval_read_num(mpc_ast_t *t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE ? lval_num(x) : lval_err("invalid number");
}

lval *lval_add(lval *root, lval *child) {
  root->count++;
  root->cell = realloc(root->cell, sizeof(lval) * root->count);
  root->cell[root->count - 1] = child;
  return root;
}

lval *lval_read(mpc_ast_t *t) {
  if (strstr(t->tag, "number")) {
    return lval_read_num(t);
  }
  if (strstr(t->tag, "symbol")) {
    return lval_sym(t->contents);
  }

  lval *v = NULL;
  if (strcmp(t->tag, ">") == 0) {
    v = lval_sexpr();
  }
  if (strstr(t->tag, "sexpr")) {
    v = lval_sexpr();
  }

  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) {
      continue;
    }
    if (strcmp(t->children[i]->contents, ")") == 0) {
      continue;
    }
    if (strcmp(t->children[i]->tag, "regex") == 0) {
      continue;
    }
    v = lval_add(v, lval_read(t->children[i]));
  }
  return v;
}

void lval_expr_print(lval *v, char open, char close) {
  putchar(open);
  for (int i = 0; i < v->count; i++) {
    lval_print(v->cell[i]);
    if (i != (v->count - 1)) {
      putchar(' ');
    }
  }
  putchar(close);
}

void lval_print(lval *v) {
  switch (v->type) {
  case TYPE_NUM:
    printf("%li", v->num);
    break;
  case TYPE_ERR:
    printf("Error: %s", v->err);
    break;
  case TYPE_SYM:
    printf("%s", v->sym);
    break;
  case TYPE_SEXPR:
    lval_expr_print(v, '(', ')');
    break;
  }
}

void lval_println(lval *v) {
  lval_print(v);
  putchar('\n');
}

int num_leaves(mpc_ast_t *t) {
  if (strstr(t->tag, "number")) {
    return 1;
  }
  int sum = 0;
  for (int i = 0; i < t->children_num; i++) {
    if (strstr(t->children[i]->tag, "expr")) {
      sum = sum + num_leaves(t->children[i]);
    }
  }
  return sum;
}

lval *lval_pop(lval *v, int i) {
  lval *x = v->cell[i];

  memmove(&v->cell[i], &v->cell[i + 1], sizeof(lval *) * (v->count - i - 1));

  v->count--;

  v->cell = realloc(v->cell, sizeof(lval *) * v->count);
  return x;
}

lval *lval_take(lval *v, int i) {
  lval *x = lval_pop(v, i);
  lval_free(v);
  return x;
}

lval *builtin_op(lval *a, char *op) {
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type != TYPE_NUM) {
      lval_free(a);
      return lval_err("Error: argument is not a number");
    }
  }

  lval *x = lval_pop(a, 0);

  if ((strcmp(op, "-") == 0) && a->count == 0) {
    x->num = -x->num;
  }

  while (a->count > 0) {
    lval *y = lval_pop(a, 0);

    if (strcmp(op, "+") == 0) {
      x->num += y->num;
    }
    if (strcmp(op, "-") == 0) {
      x->num -= y->num;
    }
    if (strcmp(op, "*") == 0) {
      x->num *= y->num;
    }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_free(x);
        lval_free(y);
        x = lval_err("Error: division by zero");
        break;
      }
      x->num /= y->num;
    }
    if (strcmp(op, "%") == 0) {
      if (y->num == 0) {
        lval_free(x);
        lval_free(y);
        x = lval_err("Error: division by zero");
        break;
      }
      x->num %= y->num;
    }
    lval_free(y);
  }
  lval_free(a);
  return x;
}

lval *lval_eval_sexpr(lval *v) {
  for (int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(v->cell[i]);
  }

  for (int i = 0; i < v->count; i++) {
    if (v->cell[i]->type == TYPE_ERR) {
      return lval_take(v, i);
    }
  }

  if (v->count == 0) {
    return v;
  }

  if (v->count == 1) {
    return lval_take(v, 0);
  }

  lval *f = lval_pop(v, 0);
  if (f->type != TYPE_SYM) {
    lval_free(f);
    lval_free(v);
    return lval_err("S-expression does not start with a symbol");
  }

  lval *result = builtin_op(v, f->sym);
  lval_free(f);
  return result;
}

lval *lval_eval(lval *v) {
  if (v->type == TYPE_SEXPR) {
    return lval_eval_sexpr(v);
  }
  return v;
}

// lval eval_op(lval x, char* op, lval y) {
//     if (x.type == TYPE_ERR) {
//         return x;
//     }
//     if (y.type == TYPE_ERR) {
//         return y;
//     }
//
//	if (strcmp(op, "+") == 0) {return lval_num(x.num + y.num); }
//	if (strcmp(op, "-") == 0) {return lval_num(x.num - y.num); }
//	if (strcmp(op, "*") == 0) {return lval_num(x.num * y.num); }
//	if (strcmp(op, "/") == 0) {return y.num == 0 ? lval_err(ERR_DIV_ZERO) :
// lval_num(x.num / y.num); } 	if (strcmp(op, "%") == 0) {return lval_num(x.num
// % y.num); } 	return lval_err(ERR_UNKNOWN_OP);
// }

// lval eval(mpc_ast_t* t) {
//	if (strstr(t->tag, "number")) {
//         errno = 0;
//         long x = strtol (t->contents, NULL, 10);
//		return errno != ERANGE ? lval_num(x) : lval_err(ERR_BAD_NUM);
//	}
//
//	char* op = t->children[1]->contents;
//
//	lval x = eval(t->children[2]);
//
//	int i = 3;
//	while (strstr(t->children[i]->tag, "expr")) {
//		x = eval_op(x, op, eval(t->children[i]));
//		i++;
//	}
//
//	return x;
// }

int main(int argc, char **argv) {
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Symbol = mpc_new("symbol");
  mpc_parser_t *Sexpr = mpc_new("sexpr");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "                                                   \
      number	 : /-?[0-9]+/;                              \
      symbol   : '+' | '-' | '*' | '/' | '%' ;            \
      sexpr    : '(' <expr>* ')' ;                        \
      expr     : <number> | <symbol> | <sexpr>;           \
      lispy    : /^/ <expr>* /$/;              \
		",
            Number, Symbol, Sexpr, Expr, Lispy);

  puts("Lispy Version 0.0.3");
  puts("Press Ctrl+c to Exit\n");

  while (1) {

    char *input = readline("lispy> ");

    add_history(input);

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      lval *x = lval_eval(lval_read(r.output));
      lval_println(x);
      lval_free(x);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
    free(input);
  }
  mpc_cleanup(5, Number, Symbol, Sexpr, Expr, Lispy);
  return 0;
}
