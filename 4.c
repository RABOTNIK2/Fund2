#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

enum status {
    ST_OK = 0,
    ST_ERR_ALLOC,
    ST_ERR_INPUT,
    ST_ERR_PARAM,
    ST_ERR_EOF
};

struct file_info {
    char *name;
    long size;
    long importance;
};

struct backup_plan {
    struct file_info **selected;
    size_t count;
    long total_size;
    long total_importance;
};

static int skip_ws(void)
{
    int c;
    do {
        c = getchar();
    } while (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' || c == '\f');
    return c;
}

static enum status read_token_dyn(char **out_name)
{
    *out_name = NULL;

    int c = skip_ws();
    if (c == EOF) {
        return ST_ERR_EOF;
    }

    size_t cap = 16;
    size_t len = 0;
    char *buf = malloc(cap);
    if (buf == NULL) {
        return ST_ERR_ALLOC;
    }

    while (c != EOF && c != ' ' && c != '\t' &&
           c != '\n' && c != '\r' && c != '\v' && c != '\f') {
        if (len + 1 >= cap) {
            size_t new_cap = cap * 2;
            if (new_cap < cap) {
                free(buf);
                return ST_ERR_ALLOC;
            }
            char *tmp = realloc(buf, new_cap);
            if (tmp == NULL) {
                free(buf);
                return ST_ERR_ALLOC;
            }
            buf = tmp;
            cap = new_cap;
        }
        buf[len++] = (char)c;
        c = getchar();
    }
    buf[len] = '\0';

    *out_name = buf;
    return ST_OK;
}

static enum status read_long(long *out_value)
{
    int c = skip_ws();
    if (c == EOF) {
        return ST_ERR_EOF;
    }

    int neg = 0;
    if (c == '+' || c == '-') {
        neg = (c == '-');
        c = getchar();
    }
    if (c < '0' || c > '9') {
        return ST_ERR_INPUT;
    }

    long val = 0;
    while (c >= '0' && c <= '9') {
        int digit = c - '0';
        if (val > (LONG_MAX - digit) / 10) {
            return ST_ERR_PARAM;
        }
        val = val * 10 + digit;
        c = getchar();
    }
    if (c != EOF && c != ' ' && c != '\t' && c != '\n' &&
        c != '\r' && c != '\v' && c != '\f') {
        return ST_ERR_INPUT;
    }

    *out_value = neg ? -val : val;
    return ST_OK;
}

static enum status read_files(struct file_info **out_files, size_t *out_n, long *out_capacity)
{
    long n_long = 0;
    long capacity = 0;

    enum status st = read_long(&n_long);
    if (st != ST_OK) return st;
    st = read_long(&capacity);
    if (st != ST_OK) return st;

    if (n_long <= 0) return ST_ERR_PARAM;
    if (capacity < 0) return ST_ERR_PARAM;
    if (n_long > 1000000L) return ST_ERR_PARAM;

    size_t n = (size_t)n_long;
    struct file_info *files = calloc(n, sizeof(*files));
    if (files == NULL) return ST_ERR_ALLOC;

    for (size_t i = 0; i < n; i++) {
        char *name = NULL;
        long size = 0, importance = 0;

        st = read_token_dyn(&name);
        if (st != ST_OK) {
            for (size_t j = 0; j < i; j++) free(files[j].name);
            free(files);
            return st;
        }
        st = read_long(&size);
        if (st != ST_OK) {
            free(name);
            for (size_t j = 0; j < i; j++) free(files[j].name);
            free(files);
            return st;
        }
        st = read_long(&importance);
        if (st != ST_OK) {
            free(name);
            for (size_t j = 0; j < i; j++) free(files[j].name);
            free(files);
            return st;
        }
        if (size <= 0 || importance < 0) {
            free(name);
            for (size_t j = 0; j < i; j++) free(files[j].name);
            free(files);
            return ST_ERR_PARAM;
        }

        files[i].name = name;
        files[i].size = size;
        files[i].importance = importance;
    }

    *out_files = files;
    *out_n = n;
    *out_capacity = capacity;
    return ST_OK;
}

static enum status solve_backup(const struct file_info *files, size_t n, long capacity, struct backup_plan *out_plan)
{
    if (files == NULL || out_plan == NULL) {
        return ST_ERR_PARAM;
    }
    if (capacity < 0) {
        return ST_ERR_PARAM;
    }

    out_plan->selected = NULL;
    out_plan->count = 0;
    out_plan->total_size = 0;
    out_plan->total_importance = 0;

    if (n == 0 || capacity == 0) {
        return ST_OK;
    }

    size_t cap = (size_t)capacity + 1;

    long *dp = calloc(cap, sizeof(*dp));
    if (dp == NULL) {
        return ST_ERR_ALLOC;
    }

    unsigned char *take = NULL;
    if (cap <= SIZE_MAX / n) {
        take = calloc(n * cap, 1);
    }
    if (take == NULL) {
        free(dp);
        return ST_ERR_ALLOC;
    }

    for (size_t i = 0; i < n; i++) {
        long w = files[i].size;
        long v = files[i].importance;
        unsigned char *take_row = take + i * cap;

        for (size_t j = cap; j-- > 0; ) {
            if ((size_t)w <= j) {
                size_t prev = j - (size_t)w;
                long cand = dp[prev] + v;
                if (cand > dp[j]) {
                    dp[j] = cand;
                    take_row[j] = 1;
                }
            }
        }
    }

    struct file_info **sel = malloc(n * sizeof(*sel));
    if (sel == NULL) {
        free(take);
        free(dp);
        return ST_ERR_ALLOC;
    }

    size_t count = 0;
    size_t j = (size_t)capacity;
    long total_size = 0, total_imp = 0;

    for (size_t i = n; i-- > 0; ) {
        unsigned char *take_row = take + i * cap;
        if (take_row[j]) {
            sel[count++] = (struct file_info *)&files[i];
            total_size += files[i].size;
            total_imp  += files[i].importance;
            j -= (size_t)files[i].size;
        }
    }

    for (size_t a = 0, b = count; a + 1 < b; a++, b--) {
        struct file_info *tmp = sel[a];
        sel[a] = sel[b - 1];
        sel[b - 1] = tmp;
    }

    out_plan->selected = sel;
    out_plan->count = count;
    out_plan->total_size = total_size;
    out_plan->total_importance = total_imp;

    free(take);
    free(dp);
    return ST_OK;
}

static void print_plan(const struct backup_plan *plan)
{
    printf("TOTAL_SIZE %ld\n", plan->total_size);
    printf("TOTAL_IMPORTANCE %ld\n", plan->total_importance);
    printf("FILES %zu\n", plan->count);
    for (size_t i = 0; i < plan->count; i++) {
        printf("%s\n", plan->selected[i]->name);
    }
}

int main(void)
{
    struct file_info *files = NULL;
    size_t n = 0;
    long capacity = 0;

    enum status st = read_files(&files, &n, &capacity);
    switch (st) {
    case ST_OK:
        break;
    case ST_ERR_INPUT:
        fprintf(stderr, "Error: invalid input format\n");
        return 1;
    case ST_ERR_PARAM:
        fprintf(stderr, "Error: invalid parameter value\n");
        return 1;
    case ST_ERR_ALLOC:
        fprintf(stderr, "Error: memory allocation failed\n");
        return 1;
    case ST_ERR_EOF:
        fprintf(stderr, "Error: unexpected end of input\n");
        return 1;
    default:
        fprintf(stderr, "Error: unknown status\n");
        return 1;
    }

    struct backup_plan plan;
    st = solve_backup(files, n, capacity, &plan);
    switch (st) {
    case ST_OK:
        break;
    case ST_ERR_ALLOC:
        fprintf(stderr, "Error: memory allocation failed\n");
        for (size_t i = 0; i < n; i++) free(files[i].name);
        free(files);
        return 1;
    case ST_ERR_PARAM:
        fprintf(stderr, "Error: invalid algorithm parameters\n");
        for (size_t i = 0; i < n; i++) free(files[i].name);
        free(files);
        return 1;
    default:
        fprintf(stderr, "Error: unknown status\n");
        for (size_t i = 0; i < n; i++) free(files[i].name);
        free(files);
        return 1;
    }

    print_plan(&plan);

    free(plan.selected);
    for (size_t i = 0; i < n; i++) free(files[i].name);
    free(files);

    return 0;
}