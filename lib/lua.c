#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <string.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

CAMLprim value
caml_new_lua(value code) {
	lua_State *L = luaL_newstate();   /* opens Lua */
	luaL_openlibs(L);

  return (value) L;
}

void
caml_close_lua(value lua) {
  lua_State *L = (lua_State*) lua;
	lua_close(L);
}

CAMLprim value
caml_exec_lua(value lua, value code) {
	int error;
  lua_State *L = (lua_State*) lua;
  char *val = String_val(code);

  error = luaL_loadbuffer(L, val, strlen(val), "line") ||
	lua_pcall(L, 0, 0, 0);

	if (error) {
    printf("Got an error :(\n");
			fprintf(stdout, "Message: %s\n", lua_tostring(L, -1));
			lua_pop(L, 1);  /* pop error message from the stack */
	}

	return 0;
}

CAMLprim value
caml_get_string(value lua, value name) {
  char *object_name = String_val(name);
  lua_State *L = (lua_State*) lua;
  lua_getglobal(L, object_name);
  char *string_value = lua_tostring(L, -1);
  return caml_copy_string(string_value);
}
