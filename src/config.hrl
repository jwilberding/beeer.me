-ifndef(beeerme_config_hrl).
-define(beeerme_config_hrl, ok).


-define(HOST, "localhost").
%%-define(HOST, "beeer.me").
-define(SERVER_PORT, 9630).

%% Protocol
%%

%% Request a segmented hash list of a file
-define(REQ_HASH_LIST, 5).

%% New file being sent from client o server
-define(NEW_FILE, 6).

-endif.
