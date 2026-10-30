#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <limits.h>

typedef enum {
    OFS_OK = 0,
    OFS_EOF,
    OFS_MATCH_FAIL,
    OFS_ALLOC_ERR,
    OFS_INVALID_ARG,
    OFS_DOMAIN_ERR
} ofs_status;

typedef struct {
    int (*get)(void *ctx);
    void (*unget)(void *ctx, int c);
    void *ctx;
} char_src;

static int file_get(void *ctx) { return fgetc((FILE *)ctx); }
static void file_unget(void *ctx, int c) { ungetc(c, (FILE *)ctx); }

typedef struct {
    const char *s;
    size_t pos;
} str_ctx;

static int str_get(void *ctx) {
    str_ctx *c = (str_ctx *)ctx;
    int ch = (unsigned char)c->s[c->pos];
    if (ch == '\0') return EOF;
    c->pos++;
    return ch;
}
static void str_unget(void *ctx, int ch) {
    str_ctx *c = (str_ctx *)ctx;
    if (ch != EOF && c->pos > 0) c->pos--;
}

static void skip_ws(char_src *src) {
    int c;
    while ((c = src->get(src->ctx)) != EOF && isspace((unsigned char)c)) { }
    if (c != EOF) src->unget(src->ctx, c);
}

static ofs_status read_token(char_src *src, char *buf, size_t bufsz, size_t *out_len) {
    if (!src || !buf || bufsz == 0) return OFS_INVALID_ARG;
    skip_ws(src);
    size_t n = 0;
    int c;
    while ((c = src->get(src->ctx)) != EOF && !isspace((unsigned char)c)) {
        if (n + 1 >= bufsz) return OFS_MATCH_FAIL;
        buf[n++] = (char)c;
    }
    if (c != EOF) src->unget(src->ctx, c);
    if (n == 0) return OFS_EOF;
    buf[n] = '\0';
    if (out_len) *out_len = n;
    return OFS_OK;
}

static int roman_digit_value(char c) {
    switch (c) {
        case 'I': return 1;
        case 'V': return 5;
        case 'X': return 10;
        case 'L': return 50;
        case 'C': return 100;
        case 'D': return 500;
        case 'M': return 1000;
        default:  return -1;
    }
}

static ofs_status parse_roman(const char *s, int *out) {
    if (!s || !out) return OFS_INVALID_ARG;
    if (*s == '\0') return OFS_MATCH_FAIL;
    long total = 0;
    int prev = 0;
    for (const char *p = s; *p; ++p) {
        int v = roman_digit_value(*p);
        if (v < 0) return OFS_MATCH_FAIL;
        if (v > prev && prev != 0) total -= 2L * prev;
        total += v;
        prev = v;
    }
    if (total <= 0 || total > INT_MAX) return OFS_DOMAIN_ERR;
    *out = (int)total;
    return OFS_OK;
}

static ofs_status parse_zeckendorf(const char *s, unsigned int *out) {
    if (!s || !out) return OFS_INVALID_ARG;
    size_t n = strlen(s);
    if (n < 2) return OFS_MATCH_FAIL;
    if (s[n - 1] != '1') return OFS_MATCH_FAIL;

    unsigned long long total = 0;
    unsigned long long fib[64];
    fib[0] = 1; fib[1] = 2;
    size_t maxbits = n;
    if (maxbits > 63) return OFS_DOMAIN_ERR;
    for (size_t i = 2; i <= maxbits; ++i) fib[i] = fib[i-1] + fib[i-2];

    for (size_t i = 0; i + 1 < n; ++i) {
        if (s[i] != '0' && s[i] != '1') return OFS_MATCH_FAIL;
        if (s[i] == '1') total += fib[i];
    }
    if (total > UINT_MAX) return OFS_DOMAIN_ERR;
    *out = (unsigned int)total;
    return OFS_OK;
}

static ofs_status parse_base(const char *s, int base, int require_lower, int require_upper, int *out) {
    if (!s || !out) return OFS_INVALID_ARG;
    if (base < 2 || base > 36) base = 10;
    if (*s == '\0') return OFS_MATCH_FAIL;

    long long acc = 0;
    int neg = 0;
    const char *p = s;
    if (*p == '-' || *p == '+') { neg = (*p == '-'); ++p; }
    if (*p == '\0') return OFS_MATCH_FAIL;

    for (; *p; ++p) {
        int d;
        if (*p >= '0' && *p <= '9') d = *p - '0';
        else if (*p >= 'a' && *p <= 'z') {
            if (require_upper) return OFS_MATCH_FAIL;
            d = *p - 'a' + 10;
        } else if (*p >= 'A' && *p <= 'Z') {
            if (require_lower) return OFS_MATCH_FAIL;
            d = *p - 'A' + 10;
        } else return OFS_MATCH_FAIL;

        if (d >= base) return OFS_MATCH_FAIL;
        acc = acc * base + d;
        if (acc > (long long)INT_MAX + (neg ? 1 : 0)) return OFS_DOMAIN_ERR;
    }
    if (neg) acc = -acc;
    if (acc < INT_MIN || acc > INT_MAX) return OFS_DOMAIN_ERR;
    *out = (int)acc;
    return OFS_OK;
}

static ofs_status ofs_core(char_src *src, const char *format, va_list ap, int *matched) {
    if (!src || !format || !matched) return OFS_INVALID_ARG;
    *matched = 0;
    int assigned = 0;
    const char *p = format;

    while (*p) {
        if (isspace((unsigned char)*p)) {
            skip_ws(src);
            ++p;
            continue;
        }
        if (*p != '%') {
            int c = src->get(src->ctx);
            if (c == EOF) return assigned ? OFS_OK : OFS_EOF;
            if (c != (unsigned char)*p) { src->unget(src->ctx, c); return OFS_MATCH_FAIL; }
            ++p;
            continue;
        }
        ++p;
        if (*p == '%') {
            int c = src->get(src->ctx);
            if (c == EOF) return assigned ? OFS_OK : OFS_EOF;
            if (c != '%') { src->unget(src->ctx, c); return OFS_MATCH_FAIL; }
            ++p;
            continue;
        }

        if (p[0] == 'R' && p[1] == 'o') {
            p += 2;
            char buf[256];
            ofs_status st = read_token(src, buf, sizeof buf, NULL);
            if (st == OFS_EOF) return assigned ? OFS_OK : OFS_EOF;
            if (st != OFS_OK) return st;
            int *dst = va_arg(ap, int *);
            if (!dst) return OFS_INVALID_ARG;
            int v;
            st = parse_roman(buf, &v);
            if (st != OFS_OK) return st;
            *dst = v;
            ++assigned; ++*matched;
            continue;
        }
        if (p[0] == 'Z' && p[1] == 'r') {
            p += 2;
            char buf[256];
            ofs_status st = read_token(src, buf, sizeof buf, NULL);
            if (st == OFS_EOF) return assigned ? OFS_OK : OFS_EOF;
            if (st != OFS_OK) return st;
            unsigned int *dst = va_arg(ap, unsigned int *);
            if (!dst) return OFS_INVALID_ARG;
            unsigned int v;
            st = parse_zeckendorf(buf, &v);
            if (st != OFS_OK) return st;
            *dst = v;
            ++assigned; ++*matched;
            continue;
        }
        if (p[0] == 'C' && (p[1] == 'v' || p[1] == 'V')) {
            int upper = (p[1] == 'V');
            p += 2;
            char buf[256];
            ofs_status st = read_token(src, buf, sizeof buf, NULL);
            if (st == OFS_EOF) return assigned ? OFS_OK : OFS_EOF;
            if (st != OFS_OK) return st;
            int *dst = va_arg(ap, int *);
            int base = va_arg(ap, int);
            if (!dst) return OFS_INVALID_ARG;
            int v;
            st = parse_base(buf, base, !upper, upper, &v);
            if (st != OFS_OK) return st;
            *dst = v;
            ++assigned; ++*matched;
            continue;
        }
        if (*p == 'd') {
            ++p;
            char buf[256];
            ofs_status st = read_token(src, buf, sizeof buf, NULL);
            if (st == OFS_EOF) return assigned ? OFS_OK : OFS_EOF;
            if (st != OFS_OK) return st;
            int *dst = va_arg(ap, int *);
            if (!dst) return OFS_INVALID_ARG;
            char *end = NULL;
            long v = strtol(buf, &end, 10);
            if (end == buf || *end != '\0') return OFS_MATCH_FAIL;
            if (v < INT_MIN || v > INT_MAX) return OFS_DOMAIN_ERR;
            *dst = (int)v;
            ++assigned; ++*matched;
            continue;
        }
        if (*p == 'u') {
            ++p;
            char buf[256];
            ofs_status st = read_token(src, buf, sizeof buf, NULL);
            if (st == OFS_EOF) return assigned ? OFS_OK : OFS_EOF;
            if (st != OFS_OK) return st;
            unsigned int *dst = va_arg(ap, unsigned int *);
            if (!dst) return OFS_INVALID_ARG;
            char *end = NULL;
            unsigned long v = strtoul(buf, &end, 10);
            if (end == buf || *end != '\0') return OFS_MATCH_FAIL;
            if (v > UINT_MAX) return OFS_DOMAIN_ERR;
            *dst = (unsigned int)v;
            ++assigned; ++*matched;
            continue;
        }
        if (*p == 's') {
            ++p;
            char *dst = va_arg(ap, char *);
            if (!dst) return OFS_INVALID_ARG;
            char buf[1024];
            ofs_status st = read_token(src, buf, sizeof buf, NULL);
            if (st == OFS_EOF) return assigned ? OFS_OK : OFS_EOF;
            if (st != OFS_OK) return st;
            strcpy(dst, buf);
            ++assigned; ++*matched;
            continue;
        }
        if (*p == 'c') {
            ++p;
            char *dst = va_arg(ap, char *);
            if (!dst) return OFS_INVALID_ARG;
            int c = src->get(src->ctx);
            if (c == EOF) return assigned ? OFS_OK : OFS_EOF;
            *dst = (char)c;
            ++assigned; ++*matched;
            continue;
        }
        return OFS_MATCH_FAIL;
    }
    return OFS_OK;
}

int overfscanf(FILE *stream, const char *format, ...) {
    if (!stream || !format) return EOF;
    char_src src = { file_get, file_unget, stream };
    va_list ap;
    va_start(ap, format);
    int matched = 0;
    ofs_status st = ofs_core(&src, format, ap, &matched);
    va_end(ap);
    if (st == OFS_EOF) return EOF;
    return matched;
}

int oversscanf(const char *str, const char *format, ...) {
    if (!str || !format) return EOF;
    str_ctx ctx = { str, 0 };
    char_src src = { str_get, str_unget, &ctx };
    va_list ap;
    va_start(ap, format);
    int matched = 0;
    ofs_status st = ofs_core(&src, format, ap, &matched);
    va_end(ap);
    if (st == OFS_EOF) return EOF;
    return matched;
}

int main(void) {
    int r = 0, c1 = 0, c2 = 0;
    unsigned int z = 0;
    int n = oversscanf("XIV 1011 ff FF", "%Ro %Zr %Cv %CV", &r, &z, &c1, 16, &c2, 16);
    printf("oversscanf matched=%d\n", n);
    printf("  roman XIV  = %d (expected 14)\n", r);
    printf("  zeck 1011  = %u (expected 4)\n", z);
    printf("  Cv  ff b16 = %d (expected 255)\n", c1);
    printf("  CV  FF b16 = %d (expected 255)\n", c2);
    printf("  [zeckendorf 1011 => 1 + 3 = 4]\n");

    FILE *f = tmpfile();
    if (!f) { perror("tmpfile"); return 1; }
    fputs("MMXXV 10011 abc ABC\n", f);
    rewind(f);

    int year = 0, base_v = 0, base_V = 0;
    unsigned int zz = 0;
    n = overfscanf(f, "%Ro %Zr %Cv %CV", &year, &zz, &base_v, 16, &base_V, 16);
    printf("\noverfscanf matched=%d\n", n);
    printf("  MMXXV = %d (expected 2025)\n", year);
    printf("  zeck 10011 = %u\n", zz);
    printf("  Cv abc b16 = %d\n", base_v);
    printf("  CV ABC b16 = %d\n", base_V);

    fclose(f);

    int x = 0;
    n = oversscanf("abc", "%Ro", &x);
    printf("\nerror case: oversscanf(\"abc\",\"%%Ro\") => %d (expected 0)\n", n);

    n = oversscanf("XYZ", "%Cv", &x, 36);
    printf("error case: uppercase input for %%Cv => %d (expected 0)\n", n);

    n = oversscanf("123", "%Cv", &x, 99);
    printf("%%Cv base=99 -> treated as 10, matched=%d, x=%d\n", n, x);

    return 0;
}