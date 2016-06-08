ssize_t osx_xattr_getxattr
(const char *path, const char *name, void *value, size_t size,
 u_int32_t position, int options);

ssize_t osx_xattr_fgetxattr
(int fd, const char *name, void *value, size_t size, u_int32_t position,
 int options);

ssize_t osx_xattr_listxattr
(const char *path, char *namebuf, size_t size, int options);

ssize_t osx_xattr_flistxattr(int fd, char *namebuf, size_t size, int options);

int osx_xattr_setxattr
(const char *path, const char *name, void *value, size_t size,
 u_int32_t position, int options);

int osx_xattr_fsetxattr
(int fd, const char *name, void *value, size_t size, u_int32_t position,
 int options);

int osx_xattr_removexattr(const char *path, const char *name, int options);

int osx_xattr_fremovexattr(int fd, const char *name, int options);
