ssize_t osx_xattr_get
(const char *path, const char *name, void *value, size_t size,
 u_int32_t position, int options);

ssize_t osx_xattr_fget
(int fd, const char *name, void *value, size_t size, u_int32_t position,
 int options);

ssize_t osx_xattr_list
(const char *path, char *namebuf, size_t size, int options);

ssize_t osx_xattr_flist(int fd, char *namebuf, size_t size, int options);

int osx_xattr_set
(const char *path, const char *name, void *value, size_t size,
 u_int32_t position, int options);

int osx_xattr_fset
(int fd, const char *name, void *value, size_t size, u_int32_t position,
 int options);

int osx_xattr_remove(const char *path, const char *name, int options);

int osx_xattr_fremove(int fd, const char *name, int options);
