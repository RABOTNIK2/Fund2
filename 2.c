#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <limits.h>

enum Status {
    ST_OK = 0,
    ST_ERR_ARGS,
    ST_ERR_FLAG,
    ST_ERR_OPEN_IN,
    ST_ERR_OPEN_OUT,
    ST_ERR_ALLOC,
    ST_ERR_PARSE_LINE,
    ST_ERR_EMPTY_FILE,
    ST_ERR_WRITE,
    ST_ERR_READ,
    ST_ERR_BAD_EPS
};

typedef struct {
    unsigned int id;
    char *name;
    char *surname;
    double salary;
    double eps;
    int ascending;
} Employee;

static int is_alpha_str(const char *s) {
    if (!s || !*s) return 0;
    for (const char *p = s; *p; ++p)
        if (!((*p >= 'A' && *p <= 'Z') || (*p >= 'a' && *p <= 'z')))
            return 0;
    return 1;
}

static int is_uint_str(const char *s) {
    if (!s || !*s) return 0;
    for (const char *p = s; *p; ++p)
        if (!isdigit((unsigned char)*p)) return 0;
    return 1;
}

static int is_nonneg_double_str(const char *s) {
    if (!s || !*s) return 0;
    char *end = NULL;
    double v = strtod(s, &end);
    if (end == s || *end != '\0') return 0;
    if (isnan(v) || isinf(v)) return 0;
    if (v < 0.0) return 0;
    return 1;
}

static int split_ws(char *line, char *tokens[], int max_tokens) {
    int n = 0;
    char *p = line;
    while (*p) {
        while (*p && isspace((unsigned char)*p)) ++p;
        if (!*p) break;
        if (n == max_tokens) return -1;
        tokens[n++] = p;
        while (*p && !isspace((unsigned char)*p)) ++p;
        if (*p) { *p = '\0'; ++p; }
    }
    return n;
}

static enum Status parse_line(char *line, Employee *out) {
    char *tok[5];
    int n = split_ws(line, tok, 4);
    if (n < 0) return ST_ERR_PARSE_LINE;
    if (n != 4) return ST_ERR_PARSE_LINE;

    const char *t_id = tok[0];
    const char *t_name = tok[1];
    const char *t_surname = tok[2];
    const char *t_salary = tok[3];

    if (!is_uint_str(t_id)) return ST_ERR_PARSE_LINE;
    if (!is_alpha_str(t_name)) return ST_ERR_PARSE_LINE;
    if (!is_alpha_str(t_surname)) return ST_ERR_PARSE_LINE;
    if (!is_nonneg_double_str(t_salary)) return ST_ERR_PARSE_LINE;

    size_t id_len = strlen(t_id);
    if (id_len > 10) return ST_ERR_PARSE_LINE;
    unsigned long idv = strtoul(t_id, NULL, 10);
    if (idv > 0xFFFFFFFFUL) return ST_ERR_PARSE_LINE;

    double sal = strtod(t_salary, NULL);

    char *nm = malloc(strlen(t_name) + 1);
    if (!nm) return ST_ERR_ALLOC;
    char *sn = malloc(strlen(t_surname) + 1);
    if (!sn) { free(nm); return ST_ERR_ALLOC; }
    strcpy(nm, t_name);
    strcpy(sn, t_surname);

    out->id = (unsigned int)idv;
    out->name = nm;
    out->surname = sn;
    out->salary = sal;
    out->eps = 0.0;
    out->ascending = 1;
    return ST_OK;
}

static int read_line(FILE *f, char **buf, size_t *cap) {
    if (!*buf || *cap == 0) {
        *cap = 128;
        *buf = malloc(*cap);
        if (!*buf) return -1;
    }
    (*buf)[0] = '\0';
    size_t len = 0;
    int got_any = 0;

    for (;;) {
        if (len + 1 >= *cap) {
            size_t ncap = *cap * 2;
            char *tmp = realloc(*buf, ncap);
            if (!tmp) return -1;
            *buf = tmp; *cap = ncap;
        }
        if (!fgets(*buf + len, (int)(*cap - len), f)) {
            if (ferror(f)) return -2;
            break;
        }
        got_any = 1;
        len += strlen(*buf + len);
        if (len > 0 && (*buf)[len - 1] == '\n') {
            (*buf)[len - 1] = '\0';
            break;
        }
    }
    return got_any ? 1 : 0;
}

static enum Status read_employees(const char *path, Employee **arr_out, size_t *count_out) {
    FILE *f = fopen(path, "r");
    if (!f) return ST_ERR_OPEN_IN;

    size_t cap = 8, cnt = 0;
    Employee *arr = malloc(cap * sizeof(*arr));
    if (!arr) { fclose(f); return ST_ERR_ALLOC; }

    char *line = NULL;
    size_t line_cap = 0;
    enum Status st = ST_OK;
    int rc;

    while ((rc = read_line(f, &line, &line_cap)) == 1) {
        int only_ws = 1;
        for (const char *p = line; *p; ++p)
            if (!isspace((unsigned char)*p)) { only_ws = 0; break; }
        if (only_ws) continue;

        if (cnt == cap) {
            size_t ncap = cap * 2;
            Employee *tmp = realloc(arr, ncap * sizeof(*arr));
            if (!tmp) { st = ST_ERR_ALLOC; break; }
            arr = tmp; cap = ncap;
        }
        Employee e;
        memset(&e, 0, sizeof(e));
        st = parse_line(line, &e);
        if (st != ST_OK) break;
        arr[cnt++] = e;
    }
    if (rc == -1 && st == ST_OK) st = ST_ERR_ALLOC;
    if (rc == -2 && st == ST_OK) st = ST_ERR_READ;

    free(line);
    fclose(f);

    if (st != ST_OK) {
        for (size_t i = 0; i < cnt; ++i) { free(arr[i].name); free(arr[i].surname); }
        free(arr);
        return st;
    }
    if (cnt == 0) { free(arr); return ST_ERR_EMPTY_FILE; }
    *arr_out = arr;
    *count_out = cnt;
    return ST_OK;
}

static int cmp_employees(const void *pa, const void *pb) {
    const Employee *a = pa;
    const Employee *b = pb;
    const double eps = a->eps;
    const int sign = a->ascending;

    double ds = a->salary - b->salary;
    if (fabs(ds) > eps) return (ds < 0.0 ? -1 : 1) * sign;

    int c = strcmp(a->surname, b->surname);
    if (c != 0) return (c < 0 ? -1 : 1) * sign;

    c = strcmp(a->name, b->name);
    if (c != 0) return (c < 0 ? -1 : 1) * sign;

    if (a->id < b->id) return -1 * sign;
    if (a->id > b->id) return  1 * sign;
    return 0;
}

static enum Status sort_employees(Employee *arr, size_t n, double epsilon, int ascending) {
    if (!arr || n == 0) return ST_OK;
    if (!(epsilon > 0.0) || isnan(epsilon) || isinf(epsilon))
        return ST_ERR_BAD_EPS;

    for (size_t i = 0; i < n; ++i) {
        arr[i].eps = epsilon;
        arr[i].ascending = ascending ? 1 : -1;
    }
    qsort(arr, n, sizeof(*arr), cmp_employees);
    return ST_OK;
}

static enum Status write_employees(const char *path, const Employee *arr, size_t n) {
    FILE *f = fopen(path, "w");
    if (!f) return ST_ERR_OPEN_OUT;
    enum Status st = ST_OK;
    for (size_t i = 0; i < n; ++i) {
        if (fprintf(f, "%u %s %s %.10g\n", arr[i].id, arr[i].name, arr[i].surname, arr[i].salary) < 0) {
            st = ST_ERR_WRITE; break;
        }
    }
    if (fclose(f) != 0 && st == ST_OK) st = ST_ERR_WRITE;
    return st;
}

static void free_employees(Employee *arr, size_t n) {
    if (!arr) return;
    for (size_t i = 0; i < n; ++i) { free(arr[i].name); free(arr[i].surname); }
    free(arr);
}

static enum Status parse_flag(const char *flag, int *ascending_out) {
    if (!flag || strlen(flag) != 2) return ST_ERR_FLAG;
    if (flag[0] != '-' && flag[0] != '/') return ST_ERR_FLAG;
    if (flag[1] == 'a') { *ascending_out = 1; return ST_OK; }
    if (flag[1] == 'd') { *ascending_out = 0; return ST_OK; }
    return ST_ERR_FLAG;
}

int main(int argc, char **argv) {
    if (argc != 4) {
        fprintf(stderr, "Usage: %s <input> <-a|-d|/a|/d> <output>\n", argv[0]);
        return ST_ERR_ARGS;
    }
    const char *in_path = argv[1];
    const char *flag = argv[2];
    const char *out_path = argv[3];

    int ascending = 1;
    enum Status st = parse_flag(flag, &ascending);
    if (st != ST_OK) {
        fprintf(stderr, "Invalid flag: %s\n", flag);
        return st;
    }

    Employee *arr = NULL;
    size_t n = 0;
    st = read_employees(in_path, &arr, &n);
    if (st != ST_OK) {
        switch (st) {
            case ST_ERR_OPEN_IN:
                fprintf(stderr, "Cannot open input\n"); break;
            case ST_ERR_ALLOC:
                fprintf(stderr, "Memory allocation error\n"); break;
            case ST_ERR_PARSE_LINE:
                fprintf(stderr, "Malformed line in input\n"); break;
            case ST_ERR_EMPTY_FILE:
                fprintf(stderr, "Input file is empty\n"); break;
            case ST_ERR_READ:
                fprintf(stderr, "Read error\n"); break;
            default:
                fprintf(stderr, "Unknown error\n"); break;
        }
        return st;
    }

    const double EPS = 1e-9;
    st = sort_employees(arr, n, EPS, ascending);
    if (st != ST_OK) {
        free_employees(arr, n);
        fprintf(stderr, "Sort error\n");
        return st;
    }

    st = write_employees(out_path, arr, n);
    if (st != ST_OK) {
        free_employees(arr, n);
        switch (st) {
            case ST_ERR_OPEN_OUT: 
                fprintf(stderr, "Cannot open output\n"); break;
            case ST_ERR_WRITE: 
                fprintf(stderr, "Write error\n"); break;
            default: 
                fprintf(stderr, "Unknown error\n"); break;
        }
        return st;
    }

    free_employees(arr, n);
    return ST_OK;
}