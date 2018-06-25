-define(A, "as").
-define(M(Module), list_to_binary(
                          lists:concat(["-module(",Module,").\n"]))).
-define(EXPORT, <<"-compile([export_all]).\n">>).
