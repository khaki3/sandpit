#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <math.h>
#include <git2.h>
#include <openssl/md5.h>

typedef struct date {
    unsigned short year;
    unsigned short month;
    unsigned short day;
} Date;

Date make_date(time_t time) {
    Date ret;
    struct tm *tmp;

    tmp = localtime(&time);
    ret.year  = tmp->tm_year + 1900;
    ret.month = tmp->tm_mon + 1;
    ret.day   = tmp->tm_mday;
    return ret;
}

Date now() {
    return make_date(time(NULL));
}

int chartoint(char c) {
    if (c <= '9' && c >= '0') {
        return c - '0';
    }
    return -1;
}

Date strtodate_absolute(char *str) {
    Date d;

    d.year  = 0;
    d.month = 0;
    d.day   = 0;

    for (int i = 1; i <= 8; i++) {
        int tmp = chartoint(str[i-1]);
        if (tmp < 0)
            goto fail;
        else if (i <= 4)
            d.year  += tmp * (int)pow(10, (4 - i));
        else if (i <= 6)
            d.month += tmp * (int)pow(10, (6 - i));
        else
            d.day   += tmp * (int)pow(10, (8 - i));
    }
    return d;

  fail:
    return now();
}

Date strtodate_relative(char *str) {
    return make_date(time(NULL) + atoi(str) * 24 * 60 * 60);
}

Date strtodate(char *str) {
    if (strlen(str) == 8) {
        return strtodate_absolute(str);
    }
    return strtodate_relative(str);
}

long fdsize(int fd) {
    struct stat stbuf;

    if(fstat(fd, &stbuf) == 0) {
        return stbuf.st_size;
    }
    return -1;
}

int fdcopy(int fd_src, int fd_dest) {
    char *buf = NULL;
    long size;
    int ret = -1;

    if ((size = fdsize(fd_src)) >= 0
        && lseek(fd_src, 0, SEEK_SET) == 0
        && (buf = (char*)malloc(size)) != NULL
        && read (fd_src,  buf, size) == size
        && write(fd_dest, buf, size) == size
        && (ftruncate(fd_dest, size) == 0 || errno == EINVAL)) {
        ret = 0;
    }
    free(buf);
    return ret;
}

int file_exist(char *path) {
    return (access(path, F_OK) == 0);
}

int md5file(char *path, unsigned char out[MD5_DIGEST_LENGTH]) {
    MD5_CTX c;
    int fd, ret = -1;
    char *buf = NULL;
    long size;

    MD5_Init(&c);

    if ((fd = open(path, O_RDONLY)) >= 0
        && (size = fdsize(fd)) >= 0
        && lseek(fd, 0, SEEK_SET) == 0
        && (buf = (char*)malloc(size)) != NULL
        && read(fd, buf, size) == size
        && MD5_Update(&c, buf, size)
        && MD5_Final(out, &c)) {
        ret = 0;
    }
    free(buf);
    return ret;
}

int md5cmp(unsigned char hash1[MD5_DIGEST_LENGTH], unsigned char hash2[MD5_DIGEST_LENGTH]) {
    for (int i = 0; i < MD5_DIGEST_LENGTH; i++) {
        if (hash1[i] != hash2[i]) {
            return 1;
        }
    }
    return 0;
}

int streq(char *str1, char *str2) {
    return (strcmp(str1, str2) == 0);
}

char *strjoin(char *str1, char *str2) {
    char *ret;

    ret = (char*)malloc(strlen(str1) + strlen(str2) + 1);
    strcpy(ret, str1);
    strcat(ret, str2);
    return ret;
}

void usage() {
    fputs("Usage: jog [--help] [COMMAND] [DATE]" "\n", stderr);

    fputs("\n" "COMMAND:\n", stderr);
    fputs("\t" "open" "\t" "open the journal" "\n", stderr);
    fputs("\t" "show" "\t" "show the journal" "\n", stderr);
    fputs("\t" "help" "\t" "show this usage"  "\n", stderr);

    fputs("\n" "EXAMPLE:\n", stderr);
    fputs("\t" "% jog"
          "\t" "(open today's journal)" "\n", stderr);
    fputs("\t" "% jog 1"
          "\t" "(open tomorrow's journal)" "\n", stderr);
    fputs("\t" "% jog show -1"
          "\t" "(show yesterday's journal)" "\n", stderr);
    fputs("\t" "% jog open 1"
          "\t" "(open tomorrow's journal)" "\n", stderr);
    fputs("\t" "% jog open 20141227"
          "\t" "(open 2014/12/27's journal)" "\n", stderr);
}

int decide_jog_directory(char **out_jogdir, git_repository **out_repo) {
    char *home;

    if ((home = getenv("HOME")) == NULL) {
        perror("Please set HOME environment variable");
        goto fail;
    }
    if (git_repository_init(out_repo, (*out_jogdir = strjoin(home, "/.jog.d")), false) != 0) {
        perror(NULL);
        goto fail;
    }
    return 0;

  fail:
    return -1;
}

char *construct_journal_path(char *jogdir, Date d) {
    char *ret;
    ret = (char*)malloc(strlen(jogdir) + 12); // sizeof("/..../../..\0") = 12
    sprintf(ret, "%s/%04d/%02d/%02d", jogdir, d.year, d.month, d.day);
    return ret;
}

char *construct_journal_path_relative(char *jogdir, Date d) {
    char *ret;
    ret = (char*)malloc(11); // sizeof("..../../..\0") = 11
    sprintf(ret, "%04d/%02d/%02d", d.year, d.month, d.day);
    return ret;
}

char *construct_temp_path(Date d) {
    char *ret;
    ret = (char*)malloc(25);
    sprintf(ret, "/tmp/jog_%04d%02d%02d.XXXXXX", d.year, d.month, d.day);
    return ret;
}

char *summary(char *jourpath, int size) {
    char* ret;
    FILE *fp;
    int i = 0;
    bool spaced = true;

    if ((fp = fopen(jourpath, "r")) == NULL)
        return "";

    ret = (char*)malloc(size);
    size -= 4;

    for (; i < size; i++) {
        ret[i] = fgetc(fp);
        if (ret[i] == EOF) goto end;
        if (isspace(ret[i]) && spaced) {
            i--;
            continue;
        }
        if (isspace(ret[i])) {
            ret[i] = ' ';
            spaced = true;
        } else {
            spaced = false;
        }
    }

    ret[i++] = '.';
    ret[i++] = '.';
    ret[i++] = '.';

  end:
    ret[i] = '\0';
    fclose(fp);
    return ret;
}

char *construct_commit_message(char *jogdir, Date d) {
    char *ret;

    ret = (char*)malloc(100);
    sprintf(ret, "(%04d/%02d/%02d) %s",
            d.year, d.month, d.day, summary(construct_journal_path(jogdir, d), 85));
    return ret;
}

int jog_commit(char *jogdir, git_repository *repo, Date d) {
    git_index *idx;
    git_signature *sig;
    git_oid tree_id, commit_id;
    git_tree *tree;
    git_commit *last;
    git_oid oid_last;
    char *jourpath;
    char *commitmsg;

    jourpath  = construct_journal_path_relative(jogdir, d);
    commitmsg = construct_commit_message(jogdir, d);

    if (git_repository_index(&idx, repo) == 0
        // git add
        && git_index_add_bypath(idx, jourpath) == 0
        && git_index_write(idx) == 0
        // get current tree
        && git_index_write_tree(&tree_id, idx) >= 0
        && git_tree_lookup(&tree, repo, &tree_id) >= 0) {

        if(git_signature_default(&sig, repo) < 0) {
            fputs("Can't get your git-signature\n", stderr);
            fputs("Please run \"git config --global user.name ... && git config --global user.email ...\"\n", stderr);
            goto fail;
        }
        if (git_reference_name_to_id(&oid_last, repo, "HEAD") == 0
            && git_commit_lookup(&last, repo, &oid_last) == 0
            && git_commit_create_v(&commit_id, repo, "HEAD", sig, sig,
                                   NULL, commitmsg, tree, 1, last) == 0) {
            return 0;
        }
        // first commit
        if (git_commit_create_v(
                &commit_id, repo, "HEAD", sig, sig,
                NULL, commitmsg, tree, 0, NULL) == 0) {
            return 0;
        }
    }

  fail:
    return -1;
}

int jog_open(char *jogdir, git_repository *repo, Date d) {
    char *jourpath;
    char *temppath;
    char *editor;
    unsigned char hashes[2][MD5_DIGEST_LENGTH];
    int fd_src, fd_dest, ret;

    jourpath = construct_journal_path(jogdir, d);
    temppath = construct_temp_path(d);

    if ((fd_dest = mkstemp(temppath)) < 0) {
        perror("Error on mkstemp");
        goto fail;
    }

    if (file_exist(jourpath)) {
        if ((fd_src = open(jourpath, O_RDONLY)) < 0) {
            fprintf(stderr, "Can't open %s: %s\n", jourpath, strerror(errno));
            goto fail;
        }
        else if (fdcopy(fd_src, fd_dest) < 0) {
            perror("Can't copy the journal");
            goto fail;
        }
        else {
            close(fd_src);
        }
    }
    close(fd_dest);
    md5file(temppath, hashes[0]);

    switch (fork()) {
    case -1:
        perror("Can't fork myself");
        goto fail;
    case 0:
        if ((editor = getenv("EDITOR")) == NULL &&
            (editor = getenv("VISUAL")) == NULL) {
            editor = "vi";
        }
        execlp(editor, editor, temppath, NULL);
        fprintf(stderr, "Can't execute %s: %s\n", editor, strerror(errno));
        return 0;
    default:
        wait(NULL);
    break;
    }

    if (md5file(temppath, hashes[1]) == 0 && md5cmp(hashes[0], hashes[1]) == 0) {
        ret = 0;
        goto success;
    }

    if (!(sprintf(jourpath, "%s/%02d/", jogdir, d.year)
          && (mkdir(jourpath, S_IRWXU) == 0 || errno == EEXIST)
          && sprintf(jourpath, "%s/%02d/", jourpath, d.month)
          && (mkdir(jourpath, S_IRWXU) == 0 || errno == EEXIST)
          && sprintf(jourpath, "%s/%02d",  jourpath, d.day)
          && (fd_src  = open(temppath, O_RDONLY)) >= 0
          && (fd_dest = open(jourpath, O_WRONLY|O_CREAT, S_IRWXU)) >= 0
          && fdcopy(fd_src, fd_dest) == 0
          && close(fd_src)  == 0
          && close(fd_dest) == 0)) {
        perror("Can't create the journal");
        goto fail;
    }
    ret = jog_commit(jogdir, repo, d);

  success:
    remove(temppath);
    return ret;

  fail:
    return -1;
}

int jog_show(char *jogdir, git_repository *repo, Date d) {
    char *jourpath;
    int fd;

    if (file_exist(jourpath = construct_journal_path(jogdir, d))
        && (fd = open(jourpath, O_RDONLY)) >= 0
        && fdcopy(fd, STDOUT_FILENO) == 0
        && close(fd) == 0) {
        return 0;
    }

    fprintf(stderr, "Can't show %s: %s\n", jourpath, strerror(errno));
    return -1;
}

int jog_main(char *command, char *date) {
    Date d;
    char *jogdir;
    git_repository *repo;

    git_libgit2_init();

    if(decide_jog_directory(&jogdir, &repo) < 0) {
        return -1;
    }

    d = strtodate(date);

    if (streq(command, "open"))
        return jog_open(jogdir, repo, d);
    if (streq(command, "show"))
        return jog_show(jogdir, repo, d);

    return -1;
}

int main(int argc, char* argv[]) {
    if (argc == 1) {
        return jog_main("open", "0");
    }

    if (streq(argv[1], "--help") || streq(argv[1], "help")) {
        usage();
        return 0;
    }

    if (streq(argv[1], "open") || streq(argv[1], "show")) {
        if (argc == 2) {
            return jog_main(argv[1], "0");
        }
        if (argc == 3) {
            return jog_main(argv[1], argv[2]);
        }
    }

    if (argc == 2 && (isdigit(argv[1][0]) || argv[1][0] == '-')) {
        return jog_main("open", argv[1]);
    }

    usage();
    return -1;
}
