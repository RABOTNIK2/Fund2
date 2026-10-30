#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include <ctype.h>
#include <math.h>
#include <float.h>

typedef enum {
    FMT_OK = 0,
    FMT_ERR_NULL = -1,
    FMT_ERR_NOMEM = -2,
    FMT_ERR_OVERFLOW = -3,
    FMT_ERR_WRITE = -4,
    FMT_ERR_BAD_ARG = -5,
    FMT_ERR_INTERNAL = -6
} FmtStatus;

static int dbl_cmp_eps(double a, double b, double eps)
{
    if (eps <= 0.0) eps = DBL_EPSILON;

    int a_nan = (a != a), b_nan = (b != b);
    if (a_nan || b_nan) {
        if (a_nan && b_nan) return 0;
        return a_nan ? 1 : -1;
    }

    double d = a - b;
    if (fabs(d) <= eps) return 0;
    return (d < 0.0) ? -1 : 1;
}

static int int_to_roman(int value, char *buf, size_t bufsz)
{
    if (!buf || bufsz < 2) return -1;

    static const int vals[]   = {1000,900,500,400,100,90,50,40,10,9,5,4,1};
    static const char *syms[] = {"M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"};
    const int n = (int)(sizeof(vals)/sizeof(vals[0]));

    size_t pos = 0;
    unsigned int mag;

    if (value < 0) {
        if (pos + 1 >= bufsz) return -1;
        buf[pos++] = '-';
        mag = (unsigned int)(-(long long)value);
    } else {
        mag = (unsigned int)value;
    }

    for (int i = 0; i < n; ++i) {
        while (mag >= (unsigned)vals[i]) {
            size_t len = strlen(syms[i]);
            if (pos + len + 1 > bufsz) return -1;
            memcpy(buf + pos, syms[i], len);
            pos += len;
            mag -= (unsigned)vals[i];
        }
    }
    buf[pos] = '\0';
    return (int)pos;
}

static int to_zeckendorf(unsigned int value, char *buf, size_t bufsz)
{
    if (!buf || bufsz < 2) return -1;
    if (value == 0) { buf[0] = '1'; buf[1] = '\0'; return 1; }

    unsigned int fib[64];
    int n = 0;
    fib[n++] = 1;
    fib[n++] = 2;
    while (n < (int)(sizeof(fib)/sizeof(fib[0])) &&
           fib[n-1] + fib[n-2] <= value) {
        fib[n] = fib[n-1] + fib[n-2];
        ++n;
    }

    unsigned char coeff[64] = {0};
    unsigned int rem = value;
    for (int i = n - 1; i >= 0; --i) {
        if (fib[i] <= rem) { coeff[i] = 1; rem -= fib[i]; }
    }
    if (rem != 0) return -1;

    size_t pos = 0;
    for (int i = 0; i < n; ++i) {
        if (pos + 1 >= bufsz) return -1;
        buf[pos++] = coeff[i] ? '1' : '0';
    }
    if (pos + 1 >= bufsz) return -1;
    buf[pos++] = '1';
    buf[pos] = '\0';
    return (int)pos;
}

static int int_to_base(int value, int base, int upper, char *buf, size_t bufsz)
{
    if (!buf || bufsz < 3) return -1;
    if (base < 2 || base > 36) base = 10;

    const char *digits = upper ? "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                               : "0123456789abcdefghijklmnopqrstuvwxyz";

    char tmp[40];
    int  tpos = 0;
    unsigned int mag;
    int neg = 0;

    if (value < 0) { neg = 1; mag = (unsigned int)(-(long long)value); }
    else           { mag = (unsigned int)value; }

    if (mag == 0) tmp[tpos++] = '0';
    while (mag > 0) {
        tmp[tpos++] = digits[mag % (unsigned)base];
        mag /= (unsigned)base;
    }

    size_t pos = 0;
    if (neg) {
        if (pos + 1 >= bufsz) return -1;
        buf[pos++] = '-';
    }
    while (tpos > 0) {
        if (pos + 1 >= bufsz) return -1;
        buf[pos++] = tmp[--tpos];
    }
    buf[pos] = '\0';
    return (int)pos;
}

static FmtStatus str_base_to_dec(const char *s, int base, int upper, int *out)
{
    if (!s || !out) return FMT_ERR_NULL;
    if (base < 2 || base > 36) base = 10;

    while (*s == ' ' || *s == '\t') ++s;
    if (*s == '\0') return FMT_ERR_BAD_ARG;

    int neg = 0;
    if (*s == '+' || *s == '-') { neg = (*s == '-'); ++s; }
    if (*s == '\0') return FMT_ERR_BAD_ARG;

    long long acc = 0;
    int any = 0;

    for (; *s; ++s) {
        int c = (unsigned char)*s;
        if (isspace(c)) break;

        int d;
        if (c >= '0' && c <= '9') d = c - '0';
        else if (upper && c >= 'A' && c <= 'Z') d = c - 'A' + 10;
        else if (!upper && c >= 'a' && c <= 'z') d = c - 'a' + 10;
        else return FMT_ERR_BAD_ARG;

        if (d >= base) return FMT_ERR_BAD_ARG;

        acc = acc * base + d;
        if (acc > (long long)INT_MAX + 1) return FMT_ERR_OVERFLOW;
        any = 1;
    }
    if (!any) return FMT_ERR_BAD_ARG;
    if (neg) acc = -acc;
    if (acc < INT_MIN || acc > INT_MAX) return FMT_ERR_OVERFLOW;

    *out = (int)acc;
    return FMT_OK;
}

static int make_byte_dump(const void *data, size_t size, char *buf, size_t bufsz)
{
    if (!data || !buf) return -1;
    size_t need = size * 8 + (size > 0 ? size - 1 : 0) + 1;
    if (bufsz < need) return -1;

    const unsigned char *p = (const unsigned char *)data;
    size_t pos = 0;
    for (size_t i = 0; i < size; ++i) {
        if (i) buf[pos++] = ' ';
        for (int b = 7; b >= 0; --b) {
            buf[pos++] = ((p[i] >> b) & 1u) ? '1' : '0';
        }
    }
    buf[pos] = '\0';
    return (int)pos;
}

typedef FmtStatus (*WriterFn)(void *ctx, const char *data, size_t len);

typedef struct {
    WriterFn write;
    void    *ctx;
} Sink;

static FmtStatus emit(Sink *s, const char *data, size_t len)
{
    if (!s || !s->write) return FMT_ERR_NULL;
    if (len == 0) return FMT_OK;
    return s->write(s->ctx, data, len);
}

static int our_spec_len(const char *p)
{
    if (p[0] == 'R' && p[1] == 'o')                  return 2;
    if (p[0] == 'Z' && p[1] == 'r')                  return 2;
    if (p[0] == 'C' && (p[1] == 'v' || p[1] == 'V')) return 2;
    if ((p[0] == 't' || p[0] == 'T') &&
        (p[1] == 'o' || p[1] == 'O'))                return 2;
    if (p[0] == 'm' && (p[1] == 'i' || p[1] == 'u' ||
                        p[1] == 'd' || p[1] == 'f')) return 2;
    return 0;
}

static FmtStatus handle_our_spec_ex(char c1, char c2,
                                    va_list *ap, Sink *sink,
                                    double eps, int *consumed)
{
    char buf[512];
    int  n = 0;

    *consumed = 2;

    switch (c1) {
    case 'R':
        if (c2 != 'o') return FMT_ERR_INTERNAL;
        {
            int v = va_arg(*ap, int);
            n = int_to_roman(v, buf, sizeof buf);
            if (n < 0) return FMT_ERR_INTERNAL;
            return emit(sink, buf, (size_t)n);
        }
    case 'Z':
        if (c2 != 'r') return FMT_ERR_INTERNAL;
        {
            unsigned int v = va_arg(*ap, unsigned int);
            n = to_zeckendorf(v, buf, sizeof buf);
            if (n < 0) return FMT_ERR_INTERNAL;
            return emit(sink, buf, (size_t)n);
        }
    case 'C':
        if (c2 != 'v' && c2 != 'V') return FMT_ERR_INTERNAL;
        {
            int v     = va_arg(*ap, int);
            int base  = va_arg(*ap, int);
            int upper = (c2 == 'V');
            n = int_to_base(v, base, upper, buf, sizeof buf);
            if (n < 0) return FMT_ERR_INTERNAL;
            return emit(sink, buf, (size_t)n);
        }
    case 't':
    case 'T':
        if (c2 != 'o' && c2 != 'O') return FMT_ERR_INTERNAL;
        {
            const char *str = va_arg(*ap, const char *);
            int base        = va_arg(*ap, int);
            int upper       = (c1 == 'T') || (c2 == 'O');
            int val = 0;
            FmtStatus st = str_base_to_dec(str, base, upper, &val);
            if (st != FMT_OK) return st;
            n = snprintf(buf, sizeof buf, "%d", val);
            if (n < 0 || (size_t)n >= sizeof buf) return FMT_ERR_INTERNAL;
            return emit(sink, buf, (size_t)n);
        }
    case 'm':
        {
            const void *ptr = NULL;
            size_t sz = 0;
            if (c2 == 'i') { int v = va_arg(*ap, int); ptr = &v; sz = sizeof v; }
            else if (c2 == 'u') { unsigned int v = va_arg(*ap, unsigned int); ptr = &v; sz = sizeof v; }
            else if (c2 == 'd') { double v = va_arg(*ap, double); (void)dbl_cmp_eps(v, v, eps); ptr = &v; sz = sizeof v; }
            else if (c2 == 'f') { double vd = va_arg(*ap, double); float vf = (float)vd; ptr = &vf; sz = sizeof vf; }
            else return FMT_ERR_INTERNAL;

            n = make_byte_dump(ptr, sz, buf, sizeof buf);
            if (n < 0) return FMT_ERR_INTERNAL;
            return emit(sink, buf, (size_t)n);
        }
    default:
        return FMT_ERR_INTERNAL;
    }
}

typedef union {
    int i;
    unsigned int u;
    double d;
    void *p;
    const char *s;
} ArgVal;

static FmtStatus fetch_arg(char conv, va_list *ap, ArgVal *out)
{
    switch (conv) {
    case 'd': case 'i': case 'c': out->i = va_arg(*ap, int); return FMT_OK;
    case 'o': case 'u': case 'x': case 'X': out->u = va_arg(*ap, unsigned int); return FMT_OK;
    case 's': out->s = va_arg(*ap, const char *); if (!out->s) out->s = "(null)"; return FMT_OK;
    case 'p': out->p = va_arg(*ap, void *); return FMT_OK;
    case 'f': case 'F': case 'e': case 'E':
    case 'g': case 'G': case 'a': case 'A': out->d = va_arg(*ap, double); return FMT_OK;
    default:  return FMT_ERR_BAD_ARG;
    }
}

static int render_arg(char *buf, size_t bufsz, const char *spec,
                      char conv, const ArgVal *a)
{
    switch (conv) {
    case 'd': case 'i': case 'c': return snprintf(buf, bufsz, spec, a->i);
    case 'o': case 'u': case 'x': case 'X': return snprintf(buf, bufsz, spec, a->u);
    case 's': return snprintf(buf, bufsz, spec, a->s);
    case 'p': return snprintf(buf, bufsz, spec, a->p);
    case 'f': case 'F': case 'e': case 'E':
    case 'g': case 'G': case 'a': case 'A': return snprintf(buf, bufsz, spec, a->d);
    default: return -1;
    }
}

static FmtStatus format_core(Sink *sink, const char *fmt, va_list ap_in, double eps)
{
    if (!sink || !fmt) return FMT_ERR_NULL;

    va_list ap;
    va_copy(ap, ap_in);

    const char *p = fmt;
    FmtStatus st = FMT_OK;

    while (*p) {
        if (*p != '%') {
            const char *start = p;
            while (*p && *p != '%') ++p;
            st = emit(sink, start, (size_t)(p - start));
            if (st != FMT_OK) break;
            continue;
        }

        if (p[1] == '%') {
            st = emit(sink, "%", 1);
            if (st != FMT_OK) break;
            p += 2;
            continue;
        }

        if (our_spec_len(p + 1) > 0) {
            int consumed = 0;
            st = handle_our_spec_ex(p[1], p[2], &ap, sink, eps, &consumed);
            if (st != FMT_OK) break;
            p += 1 + consumed;
            continue;
        }

        const char *q = p + 1;
        while (*q && strchr("-+ #0'", *q)) ++q;
        if (*q == '*') { (void)va_arg(ap, int); ++q; }
        else while (*q && isdigit((unsigned char)*q)) ++q;
        if (*q == '.') {
            ++q;
            if (*q == '*') { (void)va_arg(ap, int); ++q; }
            else while (*q && isdigit((unsigned char)*q)) ++q;
        }
        while (*q && strchr("hljztL", *q)) ++q;
        if (!*q) { st = FMT_ERR_BAD_ARG; break; }

        size_t spec_len_full = (size_t)(q - p) + 1;
        char spec[32];
        if (spec_len_full >= sizeof spec) { st = FMT_ERR_BAD_ARG; break; }
        memcpy(spec, p, spec_len_full);
        spec[spec_len_full] = '\0';

        char conv = *q;
        ArgVal av;
        st = fetch_arg(conv, &ap, &av);
        if (st != FMT_OK) break;

        char tmpbuf[1024];
        int need = render_arg(tmpbuf, sizeof tmpbuf, spec, conv, &av);
        if (need < 0) { st = FMT_ERR_BAD_ARG; break; }

        if ((size_t)need < sizeof tmpbuf) {
            st = emit(sink, tmpbuf, (size_t)need);
            if (st != FMT_OK) break;
        } else {
            char *big = (char *)malloc((size_t)need + 1);
            if (!big) { st = FMT_ERR_NOMEM; break; }
            int need2 = render_arg(big, (size_t)need + 1, spec, conv, &av);
            if (need2 < 0 || need2 != need) { free(big); st = FMT_ERR_INTERNAL; break; }
            st = emit(sink, big, (size_t)need2);
            free(big);
            if (st != FMT_OK) break;
        }

        p += spec_len_full;
    }

    va_end(ap);
    return st;
}

typedef struct {
    FILE *fp;
    int   err;
} FileCtx;

static FmtStatus file_writer(void *ctx, const char *data, size_t len)
{
    FileCtx *fc = (FileCtx *)ctx;
    if (!fc || !fc->fp) return FMT_ERR_WRITE;
    size_t w = fwrite(data, 1, len, fc->fp);
    if (w != len) { fc->err = 1; return FMT_ERR_WRITE; }
    return FMT_OK;
}

typedef struct {
    char  *dst;
    size_t cap;
    size_t used;
} StrCtx;

static FmtStatus str_writer(void *ctx, const char *data, size_t len)
{
    StrCtx *sc = (StrCtx *)ctx;
    if (!sc) return FMT_ERR_NULL;
    if (len == 0) return FMT_OK;
    if (sc->used + len + 1 > sc->cap) return FMT_ERR_OVERFLOW;
    memcpy(sc->dst + sc->used, data, len);
    sc->used += len;
    sc->dst[sc->used] = '\0';
    return FMT_OK;
}

int overfprintf(FILE *stream, const char *format, ...)
{
    if (!stream || !format) return FMT_ERR_NULL;

    FileCtx fc = { stream, 0 };
    Sink    sink = { file_writer, &fc };

    va_list ap;
    va_start(ap, format);
    FmtStatus st = format_core(&sink, format, ap, DBL_EPSILON);
    va_end(ap);

    if (st != FMT_OK) return (int)st;
    if (fc.err)       return FMT_ERR_WRITE;
    return 0;
}

int oversprintf(char *str, const char *format, ...)
{
    if (!str || !format) return FMT_ERR_NULL;

    StrCtx sc = { str, SIZE_MAX, 0 };
    str[0] = '\0';
    Sink sink = { str_writer, &sc };

    va_list ap;
    va_start(ap, format);
    FmtStatus st = format_core(&sink, format, ap, DBL_EPSILON);
    va_end(ap);

    return (int)st;
}

int oversnprintf(char *str, size_t cap, const char *format, ...)
{
    if (!str || !format || cap == 0) return FMT_ERR_NULL;

    StrCtx sc = { str, cap, 0 };
    str[0] = '\0';
    Sink sink = { str_writer, &sc };

    va_list ap;
    va_start(ap, format);
    FmtStatus st = format_core(&sink, format, ap, DBL_EPSILON);
    va_end(ap);

    return (int)st;
}

static void demo_overfprintf(FILE *out)
{
    fprintf(out, "=== overfprintf ===\n");

    overfprintf(out, "%%Ro  int 1987 = %Ro\n", 1987);
    overfprintf(out, "%%Ro  int -42  = %Ro\n", -42);
    overfprintf(out, "%%Ro  int 0    = %Ro\n", 0);

    overfprintf(out, "%%Zr  10u = %Zr\n", 10u);
    overfprintf(out, "%%Zr  100u= %Zr\n", 100u);
    overfprintf(out, "%%Zr  0u  = %Zr\n", 0u);

    overfprintf(out, "%%Cv  255 base16 = %Cv\n", 255, 16);
    overfprintf(out, "%%CV  255 base16 = %CV\n", 255, 16);
    overfprintf(out, "%%Cv  -100 base2 = %Cv\n", -100, 2);
    overfprintf(out, "%%Cv  1234 base99(->10) = %Cv\n", 1234, 99);

    overfprintf(out, "%%to  \"ff\" base16 = %to\n", "ff", 16);
    overfprintf(out, "%%TO  \"FF\" base16 = %TO\n", "FF", 16);
    overfprintf(out, "%%to  \"-1010\" base2 = %to\n", "-1010", 2);
    overfprintf(out, "%%to  \"zz\" base36 = %to\n", "zz", 36);

    int iv = 0x01020304;
    unsigned uv = 0xDEADBEEFu;
    double dv = 1.0;
    float fv = 1.0f;
    overfprintf(out, "%%mi  int    0x%08x = %mi\n", (unsigned)iv, iv);
    overfprintf(out, "%%mu  uint   0x%08x = %mu\n", uv, uv);
    overfprintf(out, "%%md  double 1.0     = %md\n", dv);
    overfprintf(out, "%%mf  float  1.0f    = %mf\n", (double)fv);

    overfprintf(out, "mix: %d + %Ro = %s\n", 10, 5, "ok");

    int rc = overfprintf(out, "bad %%to: %to\n", "GG", 16);
    fprintf(out, "  (rc for bad %%to = %d)\n", rc);

    rc = overfprintf(NULL, "x");
    fprintf(out, "  (rc for NULL stream = %d)\n", rc);
}

static void demo_oversprintf(FILE *out)
{
    fprintf(out, "=== oversprintf ===\n");

    char buf[512];

    int rc = oversprintf(buf, "roman=%Ro zeck=%Zr base=%Cv", 2024, 100u, 255, 16);
    fprintf(out, "  rc=%d, \"%s\"\n", rc, buf);

    rc = oversprintf(buf, "to=%to TO=%TO mem=%mi", "ff", 16, "FF", 16, 0x01020304);
    fprintf(out, "  rc=%d, \"%s\"\n", rc, buf);

    char small[8];
    rc = oversnprintf(small, sizeof small, "very long string with %Ro", 3999);
    fprintf(out, "  oversnprintf rc=%d (overflow=%d), buf=\"%s\"\n", rc, FMT_ERR_OVERFLOW, small);

    int st = oversprintf(buf, "%to", "zz", 16);
    switch (st) {
        case FMT_OK:
            fprintf(out, "  FMT_OK\n");           break;
        case FMT_ERR_BAD_ARG:
            fprintf(out, "  FMT_ERR_BAD_ARG\n");  break;
        case FMT_ERR_NULL:
            fprintf(out, "  FMT_ERR_NULL\n");     break;
        case FMT_ERR_NOMEM:
            fprintf(out, "  FMT_ERR_NOMEM\n");    break;
        case FMT_ERR_OVERFLOW:
            fprintf(out, "  FMT_ERR_OVERFLOW\n"); break;
        case FMT_ERR_WRITE:
            fprintf(out, "  FMT_ERR_WRITE\n");    break;
        default:
            fprintf(out, "  FMT_ERR_INTERNAL\n"); break;
    }
}

static int parse_args(int argc, char **argv, const char **out_path)
{
    if (!out_path) return -1;
    if (argc == 1) { *out_path = "out_lab5.txt"; return 0; }
    if (argc == 2) {
        if (!argv[1] || argv[1][0] == '\0') return -1;
        *out_path = argv[1];
        return 0;
    }
    return -1;
}

int main(int argc, char **argv)
{
    const char *path = NULL;
    if (parse_args(argc, argv, &path) != 0) {
        fprintf(stderr, "Usage: %s [output-file]\n", argv[0] ? argv[0] : "lab5");
        return 2;
    }

    demo_overfprintf(stdout);
    demo_oversprintf(stdout);

    FILE *f = fopen(path, "w");
    if (!f) {
        fprintf(stderr, "Cannot open %s for writing\n", path);
        return 1;
    }
    int rc = overfprintf(f, "file: %Ro %Zr %Cv %CV %to %TO %mi %mu %md %mf\n", 42, 100u, 255, 16, 255, 16, "ff", 16, "FF", 16, 0x11223344, 0xDEADBEEFu, 3.14, (double)2.5f);
    if (fclose(f) != 0) {
        fprintf(stderr, "Error closing %s\n", path);
        return 1;
    }
    printf("\noverfprintf(file) rc=%d, see %s\n", rc, path);
    return 0;
}