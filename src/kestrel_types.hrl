-ifndef(_kestrel_types_included).
-define(_kestrel_types_included, yeah).

-define(kestrel_Status_NOT_CONFIGURED, 0).
-define(kestrel_Status_QUIESCENT, 1).
-define(kestrel_Status_READ_ONLY, 2).
-define(kestrel_Status_UP, 3).

%% struct item

-record(item, {data = undefined :: string(), 
               id = undefined :: integer()}).

%% struct queueInfo

-record(queueInfo, {head_item = undefined :: string(), 
                    items = undefined :: integer(), 
                    bytes = undefined :: integer(), 
                    journal_bytes = undefined :: integer(), 
                    age = undefined :: integer(), 
                    waiters = undefined :: integer(), 
                    open_transactions = undefined :: integer()}).

-endif.
