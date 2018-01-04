-module(connection).

-compile({parse_transform, epolymorph_interface_pt}).

-type instance() :: term().

-callback send(instance(), binary()) -> ok|{error, Reason::term()}.

-callback close(instance()) -> ok.
