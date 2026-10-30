#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_EMPTY_SUBSTR,
    STATUS_ERR_BAD_COUNT,
    STATUS_ERR_FILE_OPEN,
    STATUS_ERR_FILE_READ,
    STATUS_ERR_MEMORY,
    STATUS_ERR_WRITE
} Status;

typedef struct {
    long line;
    long column;
    size_t file_index;
} MatchInfo;

typedef struct {
    MatchInfo *items;
    size_t count;
    size_t capacity;
} MatchList;

static void match_list_init(MatchList *list) {
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static void match_list_free(MatchList *list) {
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static Status match_list_push(MatchList *list, MatchInfo info) {
    if (list->count == list->capacity) {
        size_t new_cap = (list->capacity == 0) ? 16 : list->capacity * 2;
        MatchInfo *tmp = realloc(list->items, new_cap * sizeof(MatchInfo));
        if (!tmp) {
            return STATUS_ERR_MEMORY;
        }
        list->items = tmp;
        list->capacity = new_cap;
    }
    list->items[list->count++] = info;
    return STATUS_OK;
}

static size_t find_substring(const char *haystack, size_t hay_len, const char *needle, size_t needle_len, size_t start) {
    if (needle_len == 0 || needle_len > hay_len) {
        return (size_t)-1;
    }
    if (start > hay_len - needle_len) {
        return (size_t)-1;
    }
    for (size_t i = start; i + needle_len <= hay_len; ++i) {
        size_t j = 0;
        while (j < needle_len && haystack[i + j] == needle[j]) {
            ++j;
        }
        if (j == needle_len) {
            return i;
        }
    }
    return (size_t)-1;
}

static Status read_file(const char *path, char **out_data, size_t *out_len) {
    if (!path || !out_data || !out_len) {
        return STATUS_ERR_NULL_ARG;
    }
    *out_data = NULL;
    *out_len = 0;

    FILE *f = fopen(path, "rb");
    if (!f) {
        return STATUS_ERR_FILE_OPEN;
    }

    size_t cap = 4096;
    size_t len = 0;
    char *buf = malloc(cap);
    if (!buf) {
        fclose(f);
        return STATUS_ERR_MEMORY;
    }

    for (;;) {
        if (len == cap) {
            size_t new_cap = cap * 2;
            char *tmp = realloc(buf, new_cap);
            if (!tmp) {
                free(buf);
                fclose(f);
                return STATUS_ERR_MEMORY;
            }
            buf = tmp;
            cap = new_cap;
        }
        size_t n = fread(buf + len, 1, cap - len, f);
        len += n;
        if (n == 0) {
            if (ferror(f)) {
                free(buf);
                fclose(f);
                return STATUS_ERR_FILE_READ;
            }
            if (feof(f)) {
                break;
            }
        }
    }
    fclose(f);

    *out_data = buf;
    *out_len = len;
    return STATUS_OK;
}

static Status search_in_buffer(const char *data, size_t len, const char *needle, size_t needle_len, size_t file_index, MatchList *list) {
    if (!data || !needle || !list) {
        return STATUS_ERR_NULL_ARG;
    }
    if (needle_len == 0) {
        return STATUS_ERR_EMPTY_SUBSTR;
    }
    if (needle_len > len) {
        return STATUS_OK;
    }

    long line = 1;
    long col = 1;
    size_t pos = 0;
    size_t start = 0;

    for (;;) {
        size_t idx = find_substring(data, len, needle, needle_len, start);
        if (idx == (size_t)-1) {
            break;
        }

        while (pos < idx) {
            if (data[pos] == '\n') {
                line++;
                col = 1;
            } else {
                col++;
            }
            pos++;
        }

        MatchInfo info;
        info.line = line;
        info.column = col;
        info.file_index = file_index;

        Status st = match_list_push(list, info);
        if (st != STATUS_OK) {
            return st;
        }

        start = idx + 1;
    }
    return STATUS_OK;
}

Status search_substring_in_files(const char *substr, int file_count, MatchList *results, const char **file_names, ...) {
    if (!substr || !results || !file_names) {
        return STATUS_ERR_NULL_ARG;
    }
    if (file_count <= 0) {
        return STATUS_ERR_BAD_COUNT;
    }
    if (substr[0] == '\0') {
        return STATUS_ERR_EMPTY_SUBSTR;
    }

    match_list_init(results);
    size_t needle_len = strlen(substr);

    va_list ap;
    va_start(ap, file_names);

    Status final_status = STATUS_OK;

    for (int i = 0; i < file_count; ++i) {
        const char *path = va_arg(ap, const char *);
        if (!path) {
            final_status = STATUS_ERR_NULL_ARG;
            break;
        }
        file_names[i] = path;

        char *data = NULL;
        size_t len = 0;
        Status st = read_file(path, &data, &len);
        if (st != STATUS_OK) {
            final_status = st;
            break;
        }

        st = search_in_buffer(data, len, substr, needle_len, (size_t)i, results);
        free(data);

        if (st != STATUS_OK) {
            final_status = st;
            break;
        }
    }

    va_end(ap);

    if (final_status != STATUS_OK) {
        match_list_free(results);
    }
    return final_status;
}

static Status print_results(const MatchList *results, const char **file_names, size_t file_count, FILE *out) {
    if (!results || !file_names || !out) {
        return STATUS_ERR_NULL_ARG;
    }
    for (size_t i = 0; i < file_count; ++i) {
        int found = 0;
        for (size_t k = 0; k < results->count; ++k) {
            if (results->items[k].file_index == i) {
                if (!found) {
                    if (fprintf(out, "File: %s\n", file_names[i]) < 0) {
                        return STATUS_ERR_WRITE;
                    }
                    found = 1;
                }
                if (fprintf(out, "  line %ld, char %ld\n", results->items[k].line, results->items[k].column) < 0) {
                    return STATUS_ERR_WRITE;
                }
            }
        }
        if (!found) {
            if (fprintf(out, "File: %s\n  no matches found\n", file_names[i]) < 0) {
                return STATUS_ERR_WRITE;
            }
        }
    }
    return STATUS_OK;
}

static Status create_test_file(const char *path, const char *content) {
    if (!path || !content) {
        return STATUS_ERR_NULL_ARG;
    }
    FILE *f = fopen(path, "wb");
    if (!f) {
        return STATUS_ERR_FILE_OPEN;
    }
    size_t len = strlen(content);
    size_t written = fwrite(content, 1, len, f);
    int close_res = fclose(f);
    if (written != len || close_res != 0) {
        return STATUS_ERR_WRITE;
    }
    return STATUS_OK;
}

int main(void) {
    const char *f1 = "test1.txt";
    const char *f2 = "test2.txt";
    const char *f3 = "test3.txt";

    Status st = create_test_file(f1,
        "hello world\nhello again\nno match here\nhello hello\n");
    if (st != STATUS_OK) {
        fprintf(stderr, "Failed to create %s (code %d)\n", f1, st);
        return 1;
    }
    st = create_test_file(f2,
        "hello\n  hello with indent\n\ttab hello\n");
    if (st != STATUS_OK) {
        fprintf(stderr, "Failed to create %s (code %d)\n", f2, st);
        return 1;
    }
    st = create_test_file(f3, "nothing to see here\n");
    if (st != STATUS_OK) {
        fprintf(stderr, "Failed to create %s (code %d)\n", f3, st);
        return 1;
    }

    MatchList results;
    const char *names[3];

    st = search_substring_in_files("hello", 3, &results, names, f1, f2, f3);
    switch (st) {
        case STATUS_OK: {
            Status pst = print_results(&results, names, 3, stdout);
            if (pst != STATUS_OK) {
                fprintf(stderr, "Output error (code %d)\n", pst);
                match_list_free(&results);
                return 1;
            }
            match_list_free(&results);
            break;
        }
        case STATUS_ERR_NULL_ARG:
            fprintf(stderr, "Error: NULL argument passed\n");
            return 1;
        case STATUS_ERR_EMPTY_SUBSTR:
            fprintf(stderr, "Error: empty substring\n");
            return 1;
        case STATUS_ERR_BAD_COUNT:
            fprintf(stderr, "Error: invalid file count\n");
            return 1;
        case STATUS_ERR_FILE_OPEN:
            fprintf(stderr, "Error: failed to open file\n");
            return 1;
        case STATUS_ERR_FILE_READ:
            fprintf(stderr, "Error: failed to read file\n");
            return 1;
        case STATUS_ERR_MEMORY:
            fprintf(stderr, "Error: memory allocation failed\n");
            return 1;
        default:
            fprintf(stderr, "Unknown error (code %d)\n", st);
            return 1;
    }

    printf("\n--- Searching for substring \"\\thello\" ---\n");
    MatchList r2;
    const char *n2[3];
    st = search_substring_in_files("\thello", 3, &r2, n2, f1, f2, f3);
    if (st == STATUS_OK) {
        Status pst = print_results(&r2, n2, 3, stdout);
        match_list_free(&r2);
        if (pst != STATUS_OK) {
            fprintf(stderr, "Output error (code %d)\n", pst);
            return 1;
        }
    } else {
        fprintf(stderr, "Search error (code %d)\n", st);
        return 1;
    }

    remove(f1);
    remove(f2);
    remove(f3);

    return 0;
}