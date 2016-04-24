#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <sys/xattr.h>

ssize_t osx_xattr_get
(const char *path, const char *name, void *value, size_t size,
 u_int32_t position, int options)
{
  ssize_t r;
  caml_release_runtime_system();
  r = getxattr(path, name, value, size, position, options);
  caml_acquire_runtime_system();
  return r;
}

ssize_t osx_xattr_fget
(int fd, const char *name, void *value, size_t size, u_int32_t position,
 int options)
{
  ssize_t r;
  caml_release_runtime_system();
  r = fgetxattr(fd, name, value, size, position, options);
  caml_acquire_runtime_system();
  return r;
}

ssize_t osx_xattr_list
(const char *path, char *namebuf, size_t size, int options)
{
  ssize_t r;
  caml_release_runtime_system();
  r = listxattr(path, namebuf, size, options);
  caml_acquire_runtime_system();
  return r;
}

ssize_t osx_xattr_flist(int fd, char *namebuf, size_t size, int options)
{
  ssize_t r;
  caml_release_runtime_system();
  r = flistxattr(fd, namebuf, size, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_xattr_set
(const char *path, const char *name, void *value, size_t size,
 u_int32_t position, int options)
{
  int r;
  caml_release_runtime_system();
  r = setxattr(path, name, value, size, position, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_xattr_fset
(int fd, const char *name, void *value, size_t size, u_int32_t position,
 int options)
{
  int r;
  caml_release_runtime_system();
  r = fsetxattr(fd, name, value, size, position, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_xattr_remove(const char *path, const char *name, int options)
{
  int r;
  caml_release_runtime_system();
  r = removexattr(path, name, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_xattr_fremove(int fd, const char *name, int options)
{
  int r;
  caml_release_runtime_system();
  r = fremovexattr(fd, name, options);
  caml_acquire_runtime_system();
  return r;
}
