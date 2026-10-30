#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_NULL_ARG,
    STATUS_ALLOC_ERR,
    STATUS_INVALID_FLAG,
    STATUS_INVALID_COUNT,
    STATUS_INVALID_SEED,
    STATUS_TOO_FEW_ARGS,
    STATUS_TOO_MANY_ARGS,
    STATUS_EMPTY_STRING
} Status;

static size_t my_strlen(const char *s)
{
    size_t n = 0;
    if (s == NULL) return 0;
    while (s[n] != '\0') n++;
    return n;
}

static int is_all_digits(const char *s)
{
    size_t i;
    if (s == NULL || s[0] == '\0') return 0;
    for (i = 0; s[i] != '\0'; i++) {
        if (!isdigit((unsigned char)s[i])) return 0;
    }
    return 1;
}

static Status parse_uint(const char *s, unsigned int *out)
{
    unsigned long long acc = 0;
    size_t i;
    if (s == NULL || out == NULL) return STATUS_NULL_ARG;
    if (!is_all_digits(s)) return STATUS_INVALID_SEED;
    for (i = 0; s[i] != '\0'; i++) {
        acc = acc * 10ULL + (unsigned long long)(s[i] - '0');
        if (acc > 0xFFFFFFFFULL) return STATUS_INVALID_SEED;
    }
    *out = (unsigned int)acc;
    return STATUS_OK;
}

static Status str_length(const char *s, size_t *out_len)
{
    if (s == NULL || out_len == NULL) return STATUS_NULL_ARG;
    *out_len = my_strlen(s);
    return STATUS_OK;
}

static Status str_reverse(const char *s, char **out)
{
    size_t n, i;
    char *res;
    if (s == NULL || out == NULL) return STATUS_NULL_ARG;
    n = my_strlen(s);
    res = (char *)malloc(n + 1);
    if (res == NULL) return STATUS_ALLOC_ERR;
    for (i = 0; i < n; i++) {
        res[i] = s[n - 1 - i];
    }
    res[n] = '\0';
    *out = res;
    return STATUS_OK;
}

static Status str_upper_odd(const char *s, char **out)
{
    size_t n, i;
    char *res;
    if (s == NULL || out == NULL) return STATUS_NULL_ARG;
    n = my_strlen(s);
    res = (char *)malloc(n + 1);
    if (res == NULL) return STATUS_ALLOC_ERR;
    for (i = 0; i < n; i++) {
        if ((i % 2) == 1) {
            res[i] = (char)toupper((unsigned char)s[i]);
        } else {
            res[i] = s[i];
        }
    }
    res[n] = '\0';
    *out = res;
    return STATUS_OK;
}

static Status str_sort_classes(const char *s, char **out)
{
    size_t n, i, pos = 0;
    char *res;
    if (s == NULL || out == NULL) return STATUS_NULL_ARG;
    n = my_strlen(s);
    res = (char *)malloc(n + 1);
    if (res == NULL) return STATUS_ALLOC_ERR;

    for (i = 0; i < n; i++) {
        if (isdigit((unsigned char)s[i])) res[pos++] = s[i];
    }
    for (i = 0; i < n; i++) {
        if (isalpha((unsigned char)s[i])) res[pos++] = s[i];
    }
    for (i = 0; i < n; i++) {
        if (!isdigit((unsigned char)s[i]) && !isalpha((unsigned char)s[i])) {
            res[pos++] = s[i];
        }
    }
    res[n] = '\0';
    *out = res;
    return STATUS_OK;
}

static Status str_concat_random(char **arr, size_t cnt, unsigned int seed, char **out)
{
    size_t total = 0, i, pos = 0;
    size_t *order;
    char *res;

    if (arr == NULL || out == NULL) return STATUS_NULL_ARG;
    if (cnt == 0) return STATUS_EMPTY_STRING;

    for (i = 0; i < cnt; i++) total += my_strlen(arr[i]);

    res = (char *)malloc(total + 1);
    if (res == NULL) return STATUS_ALLOC_ERR;

    order = (size_t *)malloc(cnt * sizeof(size_t));
    if (order == NULL) { free(res); return STATUS_ALLOC_ERR; }

    for (i = 0; i < cnt; i++) order[i] = i;

    srand(seed);
    for (i = cnt; i > 1; i--) {
        size_t j = (size_t)(rand() % (int)i);
        size_t tmp = order[i - 1];
        order[i - 1] = order[j];
        order[j] = tmp;
    }

    for (i = 0; i < cnt; i++) {
        const char *src = arr[order[i]];
        size_t k = 0;
        while (src[k] != '\0') { res[pos++] = src[k++]; }
    }
    res[pos] = '\0';
    free(order);
    *out = res;
    return STATUS_OK;
}

static void print_status(Status st)
{
    switch (st) {
        case STATUS_OK:
            break;
        case STATUS_NULL_ARG: 
            fprintf(stderr, "Error: NULL argument\n"); break;
        case STATUS_ALLOC_ERR:
            fprintf(stderr, "Error: memory allocation failed\n"); break;
        case STATUS_INVALID_FLAG:
            fprintf(stderr, "Error: invalid flag\n"); break;
        case STATUS_INVALID_COUNT:
            fprintf(stderr, "Error: invalid count\n"); break;
        case STATUS_INVALID_SEED:
            fprintf(stderr, "Error: invalid seed\n"); break;
        case STATUS_TOO_FEW_ARGS:
            fprintf(stderr, "Error: too few arguments\n"); break;
        case STATUS_TOO_MANY_ARGS:
            fprintf(stderr, "Error: too many arguments\n"); break;
        case STATUS_EMPTY_STRING:
            fprintf(stderr, "Error: empty string set\n"); break;
    }
}

int main(int argc, char **argv)
{
    Status st = STATUS_OK;
    const char *flag;
    const char *s;

    if (argc < 3) {
        fprintf(stderr, "Usage:\n  %s -l <string>\n  %s -r <string>\n  %s -u <string>\n  %s -n <string>\n  %s -c <string> <seed(uint)> <string1> [string2 ...]\n", argv[0], argv[0], argv[0], argv[0], argv[0]);
        return 1;
    }

    flag = argv[1];
    s = argv[2];

    if (flag[0] == '-' && flag[1] == 'l' && flag[2] == '\0') {
        size_t len = 0;
        if (argc != 3) { print_status(STATUS_TOO_MANY_ARGS); return 1; }
        st = str_length(s, &len);
        if (st != STATUS_OK) { print_status(st); return 1; }
        printf("Length of \"%s\": %zu\n", s, len);
    }
    else if (flag[0] == '-' && flag[1] == 'r' && flag[2] == '\0') {
        char *res = NULL;
        if (argc != 3) { print_status(STATUS_TOO_MANY_ARGS); return 1; }
        st = str_reverse(s, &res);
        if (st != STATUS_OK) { print_status(st); return 1; }
        printf("Reversed: \"%s\"\n", res);
        free(res);
    }
    else if (flag[0] == '-' && flag[1] == 'u' && flag[2] == '\0') {
        char *res = NULL;
        if (argc != 3) { print_status(STATUS_TOO_MANY_ARGS); return 1; }
        st = str_upper_odd(s, &res);
        if (st != STATUS_OK) { print_status(st); return 1; }
        printf("Uppercase on odd positions: \"%s\"\n", res);
        free(res);
    }
    else if (flag[0] == '-' && flag[1] == 'n' && flag[2] == '\0') {
        char *res = NULL;
        if (argc != 3) { print_status(STATUS_TOO_MANY_ARGS); return 1; }
        st = str_sort_classes(s, &res);
        if (st != STATUS_OK) { print_status(st); return 1; }
        printf("Digits|letters|other: \"%s\"\n", res);
        free(res);
    }
    else if (flag[0] == '-' && flag[1] == 'c' && flag[2] == '\0') {
        unsigned int seed = 0;
        char *res = NULL;
        size_t cnt;

        if (argc < 5) { print_status(STATUS_TOO_FEW_ARGS); return 1; }

        st = parse_uint(argv[3], &seed);
        if (st != STATUS_OK) { print_status(st); return 1; }

        {
            size_t total = (size_t)argc;
            char **selected = (char **)malloc(total * sizeof(char *));
            size_t idx = 0;
            int i;
            if (selected == NULL) { print_status(STATUS_ALLOC_ERR); return 1; }

            selected[idx++] = argv[2];
            for (i = 4; i < argc; i++) selected[idx++] = argv[i];

            cnt = idx;
            st = str_concat_random(selected, cnt, seed, &res);
            free(selected);
        }

        if (st != STATUS_OK) { print_status(st); return 1; }
        printf("Concatenation (seed=%u): \"%s\"\n", seed, res);
        free(res);
    }
    else {
        print_status(STATUS_INVALID_FLAG);
        return 1;
    }

    return 0;
}